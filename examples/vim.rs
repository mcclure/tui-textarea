use crossterm::event::{DisableMouseCapture, EnableMouseCapture};
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use futures::StreamExt;
use ratatui::backend::CrosstermBackend;
use ratatui::style::{Color, Modifier, Style};
use ratatui::widgets::{Block, Borders};
use ratatui::Terminal;
use std::env;
use std::fmt;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::io::BufRead;
use tui_textarea::{CursorMove, Input, Key, Scrolling, TextArea};

// Audio help-- for ami
use std::fmt::Display;
use cpal::traits::{HostTrait, DeviceTrait, StreamTrait};
use cpal::{Sample, FromSample, SizedSample};
#[cfg(feature = "audio_log")]
use std::io::Write; // Later we might want this for file writing.
#[cfg(feature = "audio_log")]
type AudioLog = std::fs::File;
#[cfg(not(feature = "audio_log"))]
type AudioLog = ();
#[derive(Debug)]
enum CpalError {
    Build(cpal::BuildStreamError),
    Play(cpal::PlayStreamError),
    NoDevice,
    Unknown
}
impl std::error::Error for CpalError {}
impl Display for CpalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl From<cpal::BuildStreamError> for CpalError { fn from(e: cpal::BuildStreamError) -> Self { CpalError::Build(e) } }
impl From<cpal::PlayStreamError> for CpalError { fn from(e: cpal::PlayStreamError) -> Self { CpalError::Play(e) } }
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use atomicbox::AtomicOptionBox;
// End audio help

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Normal,
    Insert,
    Replace(bool), // Bool for "once only?"
    Visual,
    Command, // "colon mode", what the manual calls Command-line mode
    Operator(char),
}

impl Mode {
    fn block<'a>(&self) -> Block<'a> {
        let help = match self {
            Self::Normal => "type :q to quit, type i to enter insert mode",
            Self::Replace(_) | Self::Insert => "type Esc to back to normal mode",
            Self::Visual => "type y to yank, type d to delete, type Esc to back to normal mode",
            Self::Operator(_) => "move cursor to apply operator",
            Self::Command => "enter command at prompt, or Esc for normal mode",
        };
        let title = format!("{} MODE ({})", self, help);
        Block::default().borders(Borders::ALL).title(title)
    }

    fn cursor_style(&self) -> Style {
        let color = match self {
            Self::Normal => Color::Reset,
            Self::Insert => Color::LightBlue,
            Self::Visual => Color::LightYellow,
            Self::Replace(_) => Color::LightRed,
            Self::Operator(_) => Color::LightGreen,
            // FIXME: This matches behavior of vim, would it be better to underline or something?
            Self::Command => { return Style::default(); }
        };
        Style::default().fg(color).add_modifier(Modifier::REVERSED)
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Normal => write!(f, "NORMAL"),
            Self::Insert => write!(f, "INSERT"),
            Self::Visual => write!(f, "VISUAL"),
            Self::Replace(_) => write!(f, "REPLACE"),
            Self::Operator(c) => write!(f, "OPERATOR({})", c),
            Self::Command => write!(f, "COMMAND"),
        }
    }
}

// How the Vim emulation state transitions
#[derive(Clone)]
enum Transition {
    Nop,
    Mode(Mode),
    Pending(Input),
    Quit,
}

// For ami
struct VimAudioSeed {
    time: std::sync::Arc<AtomicU32>,
    play: std::sync::Arc<AtomicBool>,
}

// Ranges the way TextArea thinks about them
type TextRange = ((usize, usize), (usize, usize));

// Ranges the way pom thinks about them
// Code by NicEastVillage on Github https://github.com/J-F-Liu/pom/issues/43#issuecomment-723645227
// Adjusted for utf8
// No known license, but code appears intended to be used and is fairly minimal

#[derive(Debug, Clone)]
struct ByteSpan {
    begin: usize,
    end: usize,
}

trait WithSpan<'a, O: 'a> {
    fn with_span(self) -> pom::utf8::Parser<'a, (ByteSpan, O)>;
}

impl<'a, O: 'a> WithSpan<'a, O> for pom::utf8::Parser<'a, O> {
    fn with_span(self) -> pom::utf8::Parser<'a, (ByteSpan, O)> {
        (pom::utf8::empty().pos() + self + pom::utf8::empty().pos())
            .map(|((begin, item), end)| (ByteSpan { begin, end }, item))
    }
}

// End NicEastvillage code

// Language

// Number: Play this note
// Parenthesis: Store a pattern, first letter is the label (must be uppercase letter)
// Square bracket: Scope
// Uppercase letter: Play stored pattern
// +Number, -Number: Shift base note by semitones
// ++Number, --Number: Shift base note by octaves (or multiply/divide for non-notes)
// x: rest
// r: reset
// pNumber, p+Number, p++Number etc: Set pitch, do NOT play note
// tNumber, t+Number, t++Number etc: Set tempo (rate, high)
// dNumber, d+Number, d++Number etc: set duty/duration (against basis of 8)
// a&b: One, then the other

// TODOs/document: pv, dv; !
// TODOs/consider: pr, dr; &&, &&&; !!; #, ##; :;

// In comments below: An //AT comment implies audio thread only, a //PT comment implies processing thread only
// There are two forms of this AST, a "raw" form and a "clean" form, but they have the same type.

#[derive(Debug, Clone)]
enum Act {
    Set(i32),
    Increment(i32),
    Double(i32),
    Versus(i32)
}

#[derive(Debug, Clone)]
enum Adjust {
    Pitch(Act, bool), // PT // Argument 2 is for "hard/send", e.g., p+ vs p++
    Tempo(Act),
    Duty(Act),
    Reset // FIXME: Consider partials?
}

// The "primary value" of a note. May or may not literally correspond to pitch.
// I tried really hard to think of a name for this enum other than "pitch" and failed.
#[derive(Debug, Clone)]
enum Pitch {
    Abs(i32), // AT (0 == rest); MUST be sanitized for 0-128 range by time reaches AT
    Rel(i32), // PT
    Rest      // PT
}

#[derive(Debug, Clone)]
struct Note {
    span: ByteSpan,
    adjust: Vec<Adjust>, // No adjustments -> vec len 0
    pitch: Pitch,        // "Note value", may not correpsond to pitch per se
}

#[derive(Debug, Clone)]
enum Node {
    Play(Note),
    Fork(Vec<Node>)
}

#[derive(Default, Debug, Clone)]
struct Song {
    prefix: Vec<Adjust>, // TODO consider tinyvec
    score: Vec<Node>
}

// - TODO: Do the "name" args serve a purpose? Do they create perf issues? Should they be commented out?
// - TODO: use "pom::parser::list()"
fn parse_language(input:String) -> Result<Song, pom::Error> { // FIXME: &String?
    use pom::utf8::*;

    // Whitespace types

    const SPACES:&str = " \t\r\n";
    const SPACES_HASH:&str = " \t\r\n#";
    const INLINE_SPACES:&str = " \t";
    const NEWLINE_SPACES:&str = "\r\n";

    // TODO: Thread in multiline pervasively so we can have t ++ 3
    fn opt_space<'a>() -> Parser<'a, ()> {
        one_of(SPACES).repeat(0..).discard()
            .name("opt_space")
    }

    fn hash_word<'a>() -> Parser<'a, ()> {
        sym('#').discard() * one_of(INLINE_SPACES).repeat(0..).discard()
        * none_of(SPACES_HASH).repeat(1..).discard()
    }

    fn hash_line<'a>() -> Parser<'a, ()> {
        seq("##").discard() * none_of(NEWLINE_SPACES).repeat(0..).discard()
    }

    fn one_blank<'a>(multiline:bool) -> Parser<'a, ()> {
        one_of(if multiline {SPACES} else {INLINE_SPACES}).discard() | hash_word() | hash_line()
    }

    fn inline_opt_blank<'a>() -> Parser<'a, ()> {
        one_blank(false).repeat(0..).discard()
    }

    fn inline_blank<'a>() -> Parser<'a, ()> {
        one_blank(false).repeat(1..).discard()
    }

    fn opt_blank<'a>() -> Parser<'a, ()> {
        one_blank(true).repeat(0..).discard()
    }

    fn blank<'a>() -> Parser<'a, ()> {
        one_blank(true).repeat(1..).discard()
    }

    // Primitive tokens

    fn positive<'a>() -> Parser<'a, i32> {
        let integer = (one_of("123456789").discard() * one_of("0123456789").discard().repeat(0..)).discard()
            | sym('0').discard();
//      sym('-').discard().opt() * // TODO: negative numbers via quotes
//      let integer = digit.discard().repeat(1..);
        integer.collect().convert(|x| x.parse::<i32>())
            .name("positive")
    }

    // Compound tokens

    fn act<'a>(set:bool) -> Parser<'a, Act> {
        {
            let mut p = (sym('v') * positive()).map(|x| Act::Versus(x))
            | (seq("++") * positive()).map(|x| Act::Double(x))
            | (seq("--") * positive()).map(|x| Act::Double(-x))
            | (sym('+') * positive()).map(|x| Act::Increment(x))
            | (sym('-') * positive()).map(|x| Act::Increment(-x));
            if set {
                p = p | positive().map(|x| Act::Set(x))
            }
            p
        }.name("act")
    }

    fn adjust<'a>() -> Parser<'a, Adjust> {
        (
            sym('r').map(|_|Adjust::Reset)
            | (sym('d') * act(true)).map(|act|Adjust::Duty(act))
            | (sym('t') * act(true)).map(|act|Adjust::Tempo(act))
            | (sym('p') * positive()).map(|x|Adjust::Pitch(Act::Set(x), true))
            | (sym('p').opt() + act(false)).map(|(hard, act)|Adjust::Pitch(act, hard.is_some()))
        ).name("adjust")
    }

    fn note_pitch<'a>() -> Parser<'a, Pitch> {
        (
            sym('x').map(|_|Pitch::Rest) | positive().map(Pitch::Rel)
        ).name("pitch")
    }

    fn note<'a>() -> Parser<'a, Note> { // TODO: Collapse Option<Vec<Adjust>> into Vec<Adjust> and use 0..?
        (
            (adjust() - blank()).repeat(0..) + note_pitch()
        ).name("note").with_span().map(|(span, (adjust, pitch))| Note {span, adjust, pitch})
    }

    // Utility
    fn tuple_merge<T>(t:(T, Vec<T>)) -> Vec<T> {
        let (val, vec) = t;
        let mut result = vec![val];
        result.extend(vec);
        result
    }

    fn node<'a>() -> Parser<'a, Node> {
        (
            (
                note().map(Node::Play) + (opt_blank() * sym('&') * opt_blank() * note().map(Node::Play)).repeat(1..)
            ).map(tuple_merge).map(Node::Fork)
            | note().map(Node::Play)
        ).name("node")
    }

//    let upper = one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ");

    // TODO: parser should produce a song
    let parser =
        (
            opt_space() * sym('!') * (
                (
                    inline_opt_blank() *
                    (adjust() + (inline_blank() * adjust()).repeat(0..)).map(tuple_merge)
                )
                | inline_opt_blank().map(|_|vec![])
            ) - sym('\n')
        ).repeat(0..).map(|v|v.into_iter().flatten().collect()) +
        (
            (
                opt_blank() *
                (node() + (blank() * node()).repeat(0..)).map(tuple_merge) - opt_blank()
            )
            | opt_blank().map(|_|vec![]) // Empty file is valid
        )
        - end()
    ;
    parser.map(|(prefix, score)|Song {prefix, score}).parse_str(&input)
}

// Translate PT to AT
fn absolute_language(s:Song) -> Song {
    fn absolute_node(node:Node) -> Node {
        // TODO: I don't want this anymore. I do want () scanned
        // match node {
        //     Node::Play((adjust, pitch)) => {
        //         Node::Play((adjust, match pitch {
        //             Pitch::Rest => Pitch::Abs(0),
        //             Pitch::Rel(x) => Pitch::Abs(fit_range(x + 69 - 12)), // TODO: Remove -12 when it's easier to shift octave
        //             Pitch::Abs(x) => Pitch::Abs(fit_range(x))
        //         }))
        //     },
        //     Node::Fork(all) => Node::Fork(all.into_iter().map(absolute_node).collect())
        // }
        node
    }

    // Todo: Surf prefix for note offsets
    Song{prefix:s.prefix, score:s.score.into_iter().map(absolute_node).collect()}
}

// Command line parser

enum CommandLineTotality {
    Auto,
    All,
    Buffer,
}

enum CommandLineSetType {
    On,
    Off,
    Question
}

enum CommandLineFileOp {
    Read(bool), // Force?
    Write
}

// TODO: reset, reset!, set for tones, play
enum CommandLine {
    File(CommandLineFileOp, bool, String), // write?, "in-buffer"?, name
    Wqae(bool, bool, CommandLineTotality, bool), // Write, Quit, All/Buffer, Exclamation
    Help(String),
    Set(String, CommandLineSetType), // What you set, what you set it to // TODO: Local?
    Beep
//    Play(Option<u32>)
//    Split(Option<String>), // Filename
}

// TODO: ignore surrounding whitespace
fn parse_command_line(input:String) -> Result<CommandLine, pom::Error> { // FIXME: &String?
    use pom::utf8::*;

    fn opt_space<'a>() -> Parser<'a, ()> {
        one_of(" \t").repeat(0..).discard()
    }

    fn space<'a>() -> Parser<'a, ()> {
        one_of(" \t").repeat(1..).discard()
    }

    fn unspace<'a>() -> Parser<'a, ()> {
        none_of(" \t").repeat(1..).discard()
    }

    // wqa! . Gets its own breakout cuz it's complicated
    let wqae = (
            ( // one of w or q must be present
                seq("q").map(|_|(false, true)) // q by itself
                | (                            // w or wq
                    seq("w").map(|_|(true)) +
                    seq("q").opt().map(|x|x.is_some())
                  )
            ) + ( // a/all, or b/buffer?
                (seq("a") * seq("ll").opt() ).map(|_|CommandLineTotality::All)
                | (seq("b") * seq("uffer").opt() ).map(|_|CommandLineTotality::Buffer)
            ).opt().map(|x|x.unwrap_or(CommandLineTotality::Auto))
            + seq("!").opt().map(|x|x.is_some()) // Force?
        ).map(|(((a,b),c),d)| CommandLine::Wqae(a,b,c,d));

    let parser = (
          ((seq("write").discard() | sym('w').discard())
            * (sym('v').discard() | seq("visual").discard()).opt()
            + space() * unspace().collect()).map(|(visual, x)| CommandLine::File(CommandLineFileOp::Write, visual.is_some(), x.to_string()))

        | ((seq("edit").discard() | sym('e').discard())
            * sym('!').discard().opt()
            + space() * unspace().collect()).map(|(force, x)| CommandLine::File(CommandLineFileOp::Read(force.is_some()), false, x.to_string()))

        | ((sym('!').discard() * opt_space()).opt() * (seq("cat"))
            * space() * unspace().collect()).map(|x| CommandLine::File(CommandLineFileOp::Read(false), true, x.to_string()))

        | wqae

        | (seq("help") * space() * unspace().collect().map(|x| CommandLine::Help(x.to_string())))

        | (seq("set") * space()) * (
                unspace().collect() +
                (space() * (
                    seq("0").map(|_|CommandLineSetType::Off)
                    | seq("1").map(|_|CommandLineSetType::On)
                    | seq("?").map(|_|CommandLineSetType::Question)
                ))
            ).map(|(s,t)|CommandLine::Set(s.to_string(), t))

        | (seq("beep").map(|_| CommandLine::Beep))
    ) - end();
    parser.parse_str(&input)
}

// State of Vim emulation
struct Vim {
    mode: Mode,
    pending: Input, // Pending input to handle a sequence with two keys like gg
    current_file: Option<PathBuf>,
    dirty: bool,
    audio:VimAudioSeed
}

// All changes in a single Vim transition
struct VimChanges {
    transition:Transition,
    dirty: bool,
    current_file: Option<PathBuf>, // If it changed
    status_message: Option<String> // If it changed
}

impl Vim {
    fn new(mode: Mode, current_file:Option<PathBuf>, dirty:bool, audio:VimAudioSeed) -> Self {
        Self {
            mode,
            pending: Input::default(),
            current_file,
            dirty,
            audio
        }
    }

    // Dump the contents of a file to a vector.
    // TODO merge with load<>?
    fn load_raw<'a>(&self, file: &PathBuf) -> io::Result<Vec<String>> {
        let file = fs::File::open(file)?;
        io::BufReader::new(file)
            .lines()
            .collect::<io::Result<_>>()
    }

    // Create a textarea containing the contents of a file.
    // If no name is provided, the current file will be loaded.
    fn load<'a>(&self, current_file: Option<&PathBuf>) -> io::Result<TextArea<'a>> {
        let current_file = current_file.or(self.current_file.as_ref());
        if let Some(path) = current_file {
            let file = fs::File::open(path)?;
            io::BufReader::new(file)
                .lines()
                .collect::<io::Result<_>>()
        } else {
            Ok(TextArea::default())
        }
    }

    // Write out some lines to a file.
    // If no name is provided, the current file will be written.
    // FIXME: This is not atomic. Do the write-and-rename trick.
    fn save<'a>(&self, current_file: Option<&PathBuf>, lines:&[String]) -> io::Result<()> {
        let current_file = current_file.or(self.current_file.as_ref());
        if let Some(path) = current_file {
            let mut file = fs::File::create(path)?;

            for line in lines {
                file.write(line.as_bytes())?;
                file.write("\n".as_bytes())?; // Of course it's ok to always add a newline… this is vi
            }

            Ok(())
        } else {
            Err(io::Error::new(io::ErrorKind::InvalidInput, "No file name")) // FIXME: If it ever becomes stable change to InvalidFilename
        }
    }

    fn beep(&self) { // TODO: offer CPAL, blink options? // Note: Takes self but doesn't use it (yet?)
        print!("\x07");
    }

    fn with_pending(self, pending: Input) -> Self {
        Self {
            mode: self.mode,
            pending,
            current_file: self.current_file,
            dirty: self.dirty,
            audio: self.audio
        }
    }

    // True if the textarea cursor is at the end of its given line
    fn is_before_line_end(textarea: &TextArea<'_>) -> bool {
        let (cursor_line, cursor_char) = textarea.cursor();
        let lines = textarea.lines();
        let line = &lines[cursor_line];
        let line_length = line.chars().count(); // FIXME: Not acceptable-- O(N)

        cursor_char < line_length
    }

    // Result: This function should not modify vim, only provide a set of changes to apply to vim
    fn transition(&self, input: Input, textarea: &mut TextArea<'_>, command: &mut TextArea<'_>) -> VimChanges {
        const NOP:VimChanges = VimChanges {
            transition:Transition::Nop, dirty:false, current_file:None, status_message:None
        };

        if input.key == Key::Null {
            return VimChanges { ..NOP };
        }

        match self.mode {
            Mode::Normal | Mode::Visual | Mode::Operator(_) => {
                match input {
                    Input {
                        key: Key::Char('h'),
                        ..
                    } |
                    Input {
                        key: Key::Left,
                        ..
                    } => textarea.move_cursor(CursorMove::Back),

                    Input {
                        key: Key::Char('j'),
                        ..
                    } |
                    Input {
                        key: Key::Down,
                        ..
                    } => textarea.move_cursor(CursorMove::Down),

                    Input {
                        key: Key::Char('k'),
                        ..
                    } |
                    Input {
                        key: Key::Up,
                        ..
                    } => textarea.move_cursor(CursorMove::Up),

                    Input {
                        key: Key::Char('l'),
                        ..
                    } |
                    Input {
                        key: Key::Right,
                        ..
                    } => textarea.move_cursor(CursorMove::Forward),

                    Input {
                        key: Key::Char('w'),
                        ..
                    } => textarea.move_cursor(CursorMove::WordForward),
                    Input {
                        key: Key::Char('e'),
                        ctrl: false,
                        ..
                    } => {
                        textarea.move_cursor(CursorMove::WordEnd);
                        if matches!(self.mode, Mode::Operator(_)) {
                            textarea.move_cursor(CursorMove::Forward); // Include the text under the cursor
                        }
                    }
                    Input {
                        key: Key::Char('b'),
                        ctrl: false,
                        ..
                    } => textarea.move_cursor(CursorMove::WordBack),
                    Input {
                        key: Key::Char('^'),
                        ..
                    } => textarea.move_cursor(CursorMove::Head),
                    Input {
                        key: Key::Char('$'),
                        ..
                    } => textarea.move_cursor(CursorMove::End),
                    Input { // Note: Not sorted with j
                        key: Key::Char('J'),
                        ..
                    } => {
                        let mut cursor = textarea.cursor();
                        let mut line_count = 1;
                        let mut dirty = false;

                        if let Some(((from_line, from_idx), (to_line, _))) = textarea.selection_range() {
                            // J with a selection joins all lines selected
                            // If only one line is selected, it acts like normal J,
                            // except on failure the cursor moves to selection start.
                            line_count = (to_line-from_line).max(1);
                            cursor = (from_line, from_idx);
                            textarea.cancel_selection(); // fixme restore
                            textarea.move_cursor(CursorMove::Jump(from_line as u16, from_idx as u16));
                        }

                        for _ in 0..line_count {
                            textarea.move_cursor(CursorMove::End);
                            let success = textarea.delete_line_by_end();
                            if success { // A line existed
                                textarea.insert_char(' ');
                                dirty = true;
                            } else { // In regular vim, joining on the final line is a noop
                                let (c1, c2) = cursor;
                                textarea.move_cursor(CursorMove::Jump(c1 as u16, c2 as u16));
                                self.beep();
                            }
                        }
                        return VimChanges { dirty, ..NOP };
                    }
                    Input {
                        key: Key::Char('D'),
                        ..
                    } => {
                        textarea.delete_line_by_end();
                        return VimChanges { transition:Transition::Mode(Mode::Normal), dirty:true, ..NOP };
                    }
                    Input {
                        key: Key::Char('C'),
                        ..
                    } => {
                        textarea.delete_line_by_end();
                        textarea.cancel_selection();
                        return VimChanges { transition:Transition::Mode(Mode::Insert), dirty:true, ..NOP };
                    }
                    Input {
                        key: Key::Char('p'),
                        ..
                    } => {
                        textarea.paste();
                        return VimChanges { transition:Transition::Mode(Mode::Normal), dirty:true, ..NOP };
                    }
                    Input {
                        key: Key::Char('u'),
                        ctrl: false,
                        ..
                    } => {
                        textarea.undo();
                        return VimChanges { transition:Transition::Mode(Mode::Normal), dirty:true, ..NOP };
                    }
                    Input {
                        key: Key::Char('r'),
                        ctrl: true,
                        ..
                    } => {
                        textarea.redo();
                        return VimChanges { transition:Transition::Mode(Mode::Normal), dirty:true, ..NOP };
                    }
                    Input {
                        key: Key::Char('x'),
                        ..
                    } => {
                        // FIXME: This check shouldn't be necessary, but the vim example is able to cursor over a terminating newline currently, which real vim can't in normal mode
                        // FIXME: Repeatedly mashing x at the end of a line should delete the entire line right to left
                        if Vim::is_before_line_end(&textarea) {
                            textarea.delete_next_char();
                        }
                        return VimChanges { transition:Transition::Mode(Mode::Normal), dirty:true, ..NOP };
                    }
                    Input {
                        key: Key::Char('i'),
                        ..
                    } => {
                        textarea.cancel_selection();
                        return VimChanges { transition:Transition::Mode(Mode::Insert), ..NOP };
                    }
                    Input {
                        key: Key::Char('a'),
                        ..
                    } => {
                        textarea.cancel_selection();

                        if Vim::is_before_line_end(&textarea) {
                            textarea.move_cursor(CursorMove::Forward);
                        }
                        return VimChanges { transition:Transition::Mode(Mode::Insert), ..NOP };
                    }
                    Input {
                        key: Key::Char('A'),
                        ..
                    } => {
                        textarea.cancel_selection();
                        textarea.move_cursor(CursorMove::End);
                        return VimChanges { transition:Transition::Mode(Mode::Insert), ..NOP };
                    }
                    Input {
                        key: Key::Char('S'),
                        ..
                    } => {
                        let mut line_count = 1;

                        if let Some(((from_line, from_idx), (to_line, _))) = textarea.selection_range() {
                            // S with a selection clears all lines selected
                            line_count = (to_line-from_line).max(1);

                            textarea.cancel_selection(); // fixme restore
                            textarea.move_cursor(CursorMove::Jump(from_line as u16, from_idx as u16));
                        }

                        textarea.move_cursor(CursorMove::Head);
                        for line_idx in 0..line_count {
                            let (cursor_line, _) = textarea.cursor();
                            let lines = textarea.lines();
                            let line = &lines[cursor_line];

                            if line.len() > 0 {
                                // delete_line_by_end has a special behavior where if you are at the end,
                                // it joins the line with the next. Prevent accidentally triggering this on an empty line.
                                textarea.delete_line_by_end();
                            }

                            if line_idx < line_count-1 {
                                // We are now guaranteed at the end of the line.
                                // Join to next line.
                                textarea.delete_line_by_end();
                            }
                        }

                        return VimChanges { transition:Transition::Mode(Mode::Insert), dirty:true, ..NOP };
                    }
                    Input {
                        key: Key::Char('o'),
                        ..
                    } => {
                        textarea.move_cursor(CursorMove::End);
                        textarea.insert_newline();
                        return VimChanges { transition:Transition::Mode(Mode::Insert), dirty:true, ..NOP };
                    }
                    Input {
                        key: Key::Char('O'),
                        ..
                    } => {
                        textarea.move_cursor(CursorMove::Head);
                        textarea.insert_newline();
                        textarea.move_cursor(CursorMove::Up);
                        return VimChanges { transition:Transition::Mode(Mode::Insert), dirty:true, ..NOP };
                    }
                    Input {
                        key: Key::Char('I'),
                        ..
                    } => {
                        textarea.cancel_selection();
                        textarea.move_cursor(CursorMove::Head);
                        return VimChanges { transition:Transition::Mode(Mode::Insert), ..NOP };
                    }
                    Input {
                        key: Key::Char('r'),
                        ..
                    } => {
                        // Notice selection is not cancelled-- it will be used by replace mode
                        return VimChanges { transition:Transition::Mode(Mode::Replace(true)), ..NOP };
                    }
                    Input {
                        key: Key::Char('R'),
                        ..
                    } => {
                        if textarea.selection_range().is_some() {
                            // R with a selection does the same thing as S-- it enters Insert NOT Replace mode.
                            return self.transition(Input { key: Key::Char('S'), ctrl: false, alt: false, shift: true }, textarea, command);
                        } else {
                            return VimChanges { transition:Transition::Mode(Mode::Replace(false)), ..NOP };
                        }
                    }

                    /*
                    // You're not getting out so easily
                    Input {
                        key: Key::Char('q'),
                        ..
                    } => return Transition::Quit,
                    */
                    Input {
                        key: Key::Char('e'),
                        ctrl: true,
                        ..
                    } => textarea.scroll((1, 0)),
                    Input {
                        key: Key::Char('y'),
                        ctrl: true,
                        ..
                    } => textarea.scroll((-1, 0)),
                    Input {
                        key: Key::Char('d'),
                        ctrl: true,
                        ..
                    } => textarea.scroll(Scrolling::HalfPageDown),
                    Input {
                        key: Key::Char('u'),
                        ctrl: true,
                        ..
                    } => textarea.scroll(Scrolling::HalfPageUp),
                    Input {
                        key: Key::Char('f'),
                        ctrl: true,
                        ..
                    } => textarea.scroll(Scrolling::PageDown), // FIXME but don't scroll below end
                    Input {
                        key: Key::Char('b'),
                        ctrl: true,
                        ..
                    } => textarea.scroll(Scrolling::PageUp),
                    Input {
                        key: Key::Char('v'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Normal => {
                        textarea.start_selection();
                        return VimChanges { transition:Transition::Mode(Mode::Visual), ..NOP };
                    }
                    Input {
                        key: Key::Char('V'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Normal => {
                        textarea.move_cursor(CursorMove::Head);
                        textarea.start_selection();
                        textarea.move_cursor(CursorMove::End);
                        return VimChanges { transition:Transition::Mode(Mode::Visual), ..NOP };
                    }
                    Input { key: Key::Esc, .. }
                    | Input {
                        key: Key::Char('['),
                        ctrl: true,
                        ..
                    }
                    | Input {
                        key: Key::Char('v'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.cancel_selection();
                        return VimChanges { transition:Transition::Mode(Mode::Normal), ..NOP };
                    }
                    Input {
                        key: Key::Char('g'),
                        ctrl: false,
                        ..
                    } if matches!(
                        self.pending,
                        Input {
                            key: Key::Char('g'),
                            ctrl: false,
                            ..
                        }
                    ) =>
                    {
                        textarea.move_cursor(CursorMove::Top)
                    }
                    Input {
                        key: Key::Char('G'),
                        ctrl: false,
                        ..
                    } => textarea.move_cursor(CursorMove::Bottom),
                    Input {
                        key: Key::Char(c),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Operator(c) => {
                        // Handle yy, dd, cc. (This is not strictly the same behavior as Vim)
                        textarea.move_cursor(CursorMove::Head);
                        textarea.start_selection();
                        let cursor = textarea.cursor();
                        textarea.move_cursor(CursorMove::Down);
                        if cursor == textarea.cursor() {
                            textarea.move_cursor(CursorMove::End); // At the last line, move to end of the line instead
                        }
                    }
                    Input {
                        key: Key::Char(op @ ('y' | 'd' | 'c')),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Normal => {
                        textarea.start_selection();
                        return VimChanges { transition:Transition::Mode(Mode::Operator(op)), ..NOP };
                    }
                    Input {
                        key: Key::Char('y'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.move_cursor(CursorMove::Forward); // Vim's text selection is inclusive
                        textarea.copy();
                        return VimChanges { transition:Transition::Mode(Mode::Normal), ..NOP };
                    }
                    Input {
                        key: Key::Char('d'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.move_cursor(CursorMove::Forward); // Vim's text selection is inclusive
                        textarea.cut();
                        return VimChanges { transition:Transition::Mode(Mode::Normal), dirty:true, ..NOP };
                    }
                    Input {
                        key: Key::Char('c'),
                        ctrl: false,
                        ..
                    } if self.mode == Mode::Visual => {
                        textarea.move_cursor(CursorMove::Forward); // Vim's text selection is inclusive
                        textarea.cut();
                        return VimChanges { transition:Transition::Mode(Mode::Insert), dirty:true, ..NOP };
                    }
                    Input {
                        key: Key::Char(':'),
                        ..
                    } => {
                        // Notice selection is not canceled
                        // TODO: Factor out
                        command.cancel_selection();
                        command.move_cursor(CursorMove::Jump(0,0));
                        while command.delete_line_by_end() {} // Erase until it fails to erase

                        return VimChanges { transition:Transition::Mode(Mode::Command), ..NOP };
                    }
                    input => return VimChanges { transition:Transition::Pending(input), ..NOP },
                }

                // Handle the pending operator
                match self.mode {
                    Mode::Operator('y') => {
                        textarea.copy();
                        VimChanges { transition:Transition::Mode(Mode::Normal), ..NOP }
                    }
                    Mode::Operator('d') => {
                        textarea.cut();
                        VimChanges { transition:Transition::Mode(Mode::Normal), dirty:true, ..NOP }
                    }
                    Mode::Operator('c') => {
                        textarea.cut();
                        VimChanges { transition:Transition::Mode(Mode::Insert), dirty:true, ..NOP }
                    }
                    _ => VimChanges { ..NOP },
                }
            }
            Mode::Insert => match input {
                Input { key: Key::Esc, .. }
                | Input {
                    key: Key::Char('['),
                    ctrl: true,
                    ..
                }
                | Input {
                    key: Key::Char('c'),
                    ctrl: true,
                    ..
                } => VimChanges { transition:Transition::Mode(Mode::Normal), ..NOP },
                input => {
                    textarea.input(input); // Use default key mappings in insert mode
                    VimChanges { transition:Transition::Mode(Mode::Insert), dirty:true, ..NOP }
                }
            },
            Mode::Replace(once) => match input {
                Input { key: Key::Esc, .. }
                | Input {
                    key: Key::Char('['),
                    ctrl: true,
                    ..
                }
                | Input {
                    key: Key::Char('c'),
                    ctrl: true,
                    ..
                } => {
                    VimChanges { transition: if textarea.selection_range().is_some() {
                        // The user made a selection, hit lowercase 'r', then aborted.
                        // (It shouldn't be possible to get here in non-"once" mode.)
                        Transition::Mode(Mode::Visual)
                    } else {
                        Transition::Mode(Mode::Normal)
                    }, ..NOP }
                },
                Input { key, .. }  => {
                    let dirty =
                        if match key { Key::Down | Key::Up | Key::Left | Key::Right => false, _ => true }
                        && !(once && (key == Key::Backspace || key == Key::Delete)) { // Allowed with R, not r
                            if let Some(((from_line, from_idx), (to_line, to_idx))) = textarea.selection_range() {
                                // Bizarro 'r' with a selection: Replace every non-newline character at once?!
                                let mut next_start_idx = from_idx;
                                for line_idx in from_line..=to_line {
                                    let lines = textarea.lines();
                                    let line = &lines[line_idx];
                                    let line_len = line.len();

                                    textarea.move_cursor(CursorMove::Jump(line_idx as u16, next_start_idx as u16));

                                    // "min" is to handle the odd case where the cursor is on the newline (not possible in real vim)
                                    let end_idx = if line_idx==to_line { line_len.min(to_idx+1) }
                                    else { line_len };

                                    for _ in next_start_idx..end_idx {
                                        textarea.delete_next_char();
                                        textarea.input(input.clone());
                                    }

                                    next_start_idx = 0;
                                }

                                textarea.move_cursor(CursorMove::Jump(from_line as u16, from_idx as u16));
                            } else {
                                // Normal 'r'
                                if Vim::is_before_line_end(&textarea) {
                                    textarea.delete_next_char(); // FIXME: Will eat newlines and join into next line, should act like insert at end of line
                                }
                                textarea.input(input); // Use default key mappings in insert mode
                            }
                            true
                        } else {
                            self.beep();
                            false
                        };
                    VimChanges { transition: if once {
                        Transition::Mode(Mode::Normal)
                    } else {
                        Transition::Mode(Mode::Replace(false))
                    }, dirty, ..NOP }
                }
            },
            Mode::Command => match input {
                // Exit command mode abnormally
                Input { key: Key::Esc, .. }
                | Input {
                    key: Key::Char('c'),
                    ctrl: true,
                    ..
                } => VimChanges { transition:Transition::Mode(Mode::Normal), ..NOP },
                // Investigate history
                // TODO scroll history
                Input { key: Key::Up, .. }
                | Input { key: Key::Down, .. } => {
                    self.beep();
                    VimChanges { transition:Transition::Mode(Mode::Command), ..NOP }
                },
                // Enter command successfully
                Input { key: Key::Enter, .. } => {
                    // Process line… this is heaviweight and maybe should be its own Thing
                    let line0 = command.lines()[0].clone();
                    let entry = parse_command_line(line0);
                    let mut error:Option<String> = None;

                    match entry { // Notice: Currently ! not supported
                        Ok(CommandLine::Wqae(w, q, _totality, _exclamation)) => {
                            if w {
                                match self.save(None, textarea.lines()) {
                                    Ok(()) => {
                                        return VimChanges { transition: if q {Transition::Quit} else {Transition::Mode(Mode::Normal)}, ..NOP }
                                    }
                                    Err(e) => {
                                        error = Some(format!("Error: Cannot w+q: {e}"));
                                        self.beep();
                                    }
                                }
                            } else if q {
                                return VimChanges { transition:Transition::Quit, ..NOP }; // Short circuit
                            }
                        },
                        Ok(CommandLine::File(op, inline, path)) => {
                            let path = Path::new(&path).to_path_buf();

                            match (op, inline) {
                                // Simple file operations
                                (CommandLineFileOp::Read(force), false) =>
                                    match self.load(Some(&path)) {
                                        Ok(new_textarea) => {
                                            *textarea = new_textarea;
                                            return VimChanges { transition:Transition::Mode(Mode::Normal), dirty:true, current_file:Some(path), ..NOP }
                                        },
                                        Err(e) => {
                                            error = Some(format!("Error: Cannot write: {e}"));
                                            self.beep();
                                        }
                                    },
                                (CommandLineFileOp::Write, false) =>
                                    match self.save(None, textarea.lines()) {
                                        Ok(()) => {
                                            return VimChanges { transition:Transition::Mode(Mode::Normal), current_file:Some(path), ..NOP }
                                        },
                                        Err(e) => {
                                            error = Some(format!("Error: Cannot read: {e}"));
                                            self.beep();
                                        }
                                    },

                                // Special "in-buffer" file operations--
                                // activated with :cat and :wv these paste a file *into* a buffer,
                                // or read selected text out of a buffer, rather than whole files.
                                (CommandLineFileOp::Read(_), true) => {
                                    match self.load_raw(&path) {
                                        Ok(file_lines) => {
                                            let old_yank = textarea.yank_text();

                                            if textarea.selection_range().is_some() {
                                                textarea.move_cursor(CursorMove::Forward); // Vim's text selection is inclusive
                                                textarea.cut();
                                            }
                                            let (cursor_line, cursor_char) = textarea.cursor();

                                            textarea.set_yank_text(file_lines.join("\n"));
                                            textarea.paste();
                                            textarea.set_yank_text(old_yank);

                                            textarea.move_cursor(CursorMove::Jump(cursor_line as u16, cursor_char as u16));
                                        },
                                        Err(e) => {
                                            error = Some(format!("Error: Cannot read: {e}"));
                                            self.beep();
                                        }
                                    }
                                },
                                (CommandLineFileOp::Write, true) => {
                                    if let Some(((from_line, from_idx), (to_line, to_idx))) = textarea.selection_range() {
                                        // Kludge: For most range ops we move forward the cursor for inclusion, but here we just made the operations inclusive

                                        // Note, INCLUSIVE // How is this not in a crate somewhere already??
                                        fn char_range(s: &String, from:usize, to:usize) -> String
                                        {
                                            s.chars().skip(from).take(to - from + 1).collect::<String>()
                                        }
                                        fn char_range_open(s: &String, from:usize) -> String
                                        {
                                            s.chars().skip(from).collect::<String>()
                                        }
                                        fn char_range_closed(s: &String, to:usize) -> String
                                        {
                                            s.chars().take(to+1).collect::<String>()
                                        }

                                        let in_lines = textarea.lines();
                                        let mut out_lines: Vec<String> = Default::default();
                                        if from_line == to_line {
                                            out_lines.push(char_range(&in_lines[from_line], from_idx, to_idx))
                                        } else { // TODO split to line-operations.rs?
                                            for line_idx in from_line..=to_line {
                                                let in_line = &in_lines[line_idx];
                                                if line_idx == from_line {
                                                    out_lines.push(char_range_open(in_line, from_idx));
                                                } else if line_idx == to_line {
                                                    out_lines.push(char_range_closed(in_line, to_idx).to_string());
                                                } else {
                                                    out_lines.push(in_line.clone());
                                                }
                                            }
                                        }
                                        match self.save(Some(&path), &out_lines) {
                                            Ok(()) => {
                                                // Not sure I like this behavior, but it's what vi does? --mcc
                                                textarea.cancel_selection();
                                                textarea.move_cursor(CursorMove::Jump(from_line as u16, from_idx as u16));
                                            },
                                            Err(e) => {
                                                error = Some(format!("Error: Cannot write: {e}"));
                                                self.beep();
                                            }
                                        }
                                    } else {
                                        error = Some(format!("Error: No text selected (:writev)."));
                                        self.beep();
                                    }
                                },
                            }
                        },
                        Ok(CommandLine::Beep) => {
                            self.beep();
                        },
                        //Err(e) => { eprintln!("ERR {}", e); },
                        _ => { // : syntax error // TODO: print error if any?
                            error = Some(format!("Not a command: {}", command.lines()[0].clone()));
                            self.beep(); // TODO print useful message
                        }
                    }

                    VimChanges { transition:Transition::Mode(Mode::Normal), status_message:error, ..NOP }
                },
                // Type into command buffer
                _ => {
                    command.input(input);
                    VimChanges { transition:Transition::Mode(Mode::Command), ..NOP }
                }
            },
        }
    }
}

struct AudioSeed {
    time: std::sync::Arc<AtomicU32>,
    play: std::sync::Arc<AtomicBool>,
    song: std::sync::Arc<AtomicOptionBox<Song>>
}

fn audio_write<T>(output: &mut [T], channels: usize, next_sample: &mut dyn FnMut() -> f32, audio_log: &mut AudioLog)
where
    T: Sample + FromSample<f32> + bytemuck::Pod, /* Pod constraint can be removed without audio_log */
{
    // Chop output array into slices of size "channels"
    for frame in output.chunks_mut(channels) {
        let value: T = T::from_sample(next_sample());

        // Take one sample and interleave it into all channels
        for sample in frame.iter_mut() {
            *sample = value;
        }

        #[cfg(feature = "audio_log")]
        {
            audio_log.write_all(bytemuck::cast_slice(&[value]));
        }
    }
}

fn audio_run<T>(device: &cpal::Device, config: &cpal::StreamConfig, audio_additional:AudioSeed) -> Result<cpal::Stream, CpalError>
where
    T: SizedSample + FromSample<f32> + bytemuck::Pod, /* Pod constraint can be removed without audio_log */
{
    // Types for audio engine
    #[derive(Debug, Clone)]
    struct AdjustState {
        root:i32,        // Base note ('notes' index)
        pitch:i32,       // Pitch (index relative to base note)
        rate:i32,        // Note length (samples)
        duty:i32,        // For what portion of the note is it "held"? (per vs)
        duty_vs:i32,     // Radix of duty (scalar)
        duty_as_sample_cache:i32, // duty converted to samples
    }
    #[derive(Debug, Clone)]
    struct PlayState {
        // TODO Separate? Traits?
        synth_at:i32,       // Progress within square cycle (subsamples)
        synth_high:bool,    // Is square high?

        sample_at:i32,      // Progress within beat (samples)
        beat_at:usize,        // Beat ('song.score' index)
    }
    #[derive(Debug, Clone)]
    struct State { // TODO rename "frame"?
        // TODO: CoreState with pitch_vs, rate_vs?
        adjust:AdjustState,
        play:PlayState
    }

    // Constants for audio engine
    const BPM:i32 = 110;
    const SQUARE_RADIX:i32 = 32; // "Subsample" fixed point for better pitch accuracy
    const REST_NODE:Node = Node::Play(Note { span: ByteSpan { begin:0, end:0 }, adjust: vec![], pitch: Pitch::Abs(0) });
    const DEFAULT_RATE:i32 = (60.0*48000.0/(BPM as f64)/4.0) as i32;
    const DEFAULT_ADJUST:AdjustState = AdjustState {
        root:69-12, pitch:0, rate:DEFAULT_RATE, duty:8, duty_vs:8, duty_as_sample_cache:DEFAULT_RATE
    };
    const DEFAULT_PLAY:PlayState = PlayState {
        synth_at: 0, synth_high:false, sample_at:0, beat_at:0
    };

    fn fit_range(x:i32) -> i32 {
        x.clamp(0,127)
    }

    fn do_adjust(v:&Vec<Adjust>, state:&mut AdjustState, default:&AdjustState) {
        for adjust in v {
            match adjust {
                Adjust::Pitch(act, _) => match act {
                    Act::Set(x) => state.root = *x,
                    Act::Increment(x) =>  state.root += *x,
                    Act::Double(x) => state.root += *x * 12,
                    Act::Versus(_) => todo!(),
                },
                Adjust::Tempo(act) => {
                    match act {
                        Act::Set(x) => state.rate = *x,
                        Act::Increment(_) => todo!(),
                        Act::Double(x) => if *x > 0 {
                                state.rate *= *x;
                            } else {
                                state.rate /= -*x;
                            },
                        Act::Versus(_) => todo!(),
                    }
                    state.duty_as_sample_cache = state.duty * state.rate / state.duty_vs;
                },
                Adjust::Duty(act) => {
                    match act {
                        Act::Set(x) => state.duty = *x,
                        Act::Increment(x) => state.duty += *x,
                        Act::Double(x) => if *x > 0 {
                            state.duty *= *x;
                        } else {
                            state.duty /= -*x;
                        },
                        Act::Versus(x) => {
                            // Kludge?: Rescale duty to duty_vs
                            state.duty *= state.duty_vs;
                            state.duty_vs = *x;
                            state.duty /= state.duty_vs;
                            state.duty = state.duty.max(1);
                        },
                    }
                    state.duty = state.duty.min(state.duty_vs);
                    state.duty_as_sample_cache = state.duty * state.rate / state.duty_vs;
                },
                Adjust::Reset => *state = default.clone(),
            }
        }
    }

// FIXME: sample_rate is really pretty important. Currently 44100 is assumed.
//    let sample_rate = config.sample_rate.0 as f32;
    let channels = config.channels as usize;
    let mut reset_adjust = DEFAULT_ADJUST;
    let mut state = State {adjust: DEFAULT_ADJUST, play: DEFAULT_PLAY};

    // Copy cross thread values into closure
    let audio_time = audio_additional.time.clone();
    let audio_playing = audio_additional.play.clone();
    let audio_song = audio_additional.song.clone();

    // Can't have an empty song, so make a default one containing a single rest.
    let mut song:Song = Default::default();
    song.score.push(REST_NODE);

    // Lookup table of MIDI note -> subsample zero-crossing period of square wave
    let notes = {
        let mut notes:Vec<i32> = Default::default();
        let mut base_freq = 440.0 / 32.0;
        let semitone = (2.0_f64).powf(1.0/12.0);
        for _ in 0..=8 { // I REFUSE to play notes below MIDI 9. I just DON'T WANT TO BOTHER.
            notes.push(0x7FFFFFFF);
        }
        for _x in 0..=9 { // Nine octaves above that. Whatever
            let mut freq = base_freq;
            for _y in 0..12 {
                // TODO use real sample rate
                let period = (SQUARE_RADIX as f64*44100.0_f64/freq/2.0).floor() as i32; // div two because half cycles
                //eprintln!("{}: {freq} = {period}", notes.len());
                notes.push(period);
                freq *= semitone;
            }
            base_freq *= 2.0;
        }
        notes
    };

    // Generate exactly 1 mono sample
    let mut next_value = move || {
        // Move forward sample counters
        state.play.synth_at += SQUARE_RADIX;
        state.play.sample_at += 1;

        // Check for new instructions from processing thread
        if let Some(new_song) = audio_song.swap(None, Ordering::AcqRel) {
            song = *new_song;
            //eprintln!("{:#?}", song.clone());

            reset_adjust = DEFAULT_ADJUST;
            do_adjust(&song.prefix, &mut reset_adjust, &DEFAULT_ADJUST);
            state.adjust = reset_adjust.clone();

            if song.score.len() == 0 {
                song.score.push(REST_NODE);
            }
        }

        // Check for end of note
        let mut need_adjustment = false;
        if state.play.sample_at > state.adjust.rate {
            state.play.beat_at += 1;
            state.play.sample_at = 0;

            // Cancel out square wave "overrun" (already dubious)
            if state.adjust.duty < state.adjust.duty_vs {
                state.play.synth_at = 0;
                state.play.synth_high = !state.play.synth_high;
            }

            need_adjustment = true;
        }
        // Check for end of song (loop)
        // Do this outside previous if because song can be replaced "under us"
        if state.play.beat_at >= song.score.len() {
            state.play.beat_at = 0;

            // FIXME: File a bug on what happens with this next line if you remove clone()
            state.adjust = reset_adjust.clone(); // Implicit reset each loop. Consider making customizable?
            need_adjustment = true;
        }

        // What note are we playing?
        // FIXME: There are bugs here:
        // - pitch adjustments always apply relative to 69-12 rather than current value.
        // - tempo adjustments tend to result in the note being skipped, somehow.
        let note = &song.score[state.play.beat_at];
        if need_adjustment {
            match &song.score[state.play.beat_at] {
                Node::Play(Note {adjust:v, ..}) => {
                    do_adjust(v, &mut state.adjust, &reset_adjust);
                },
                _ => unreachable!()
            }
        }
        let pitch_index = match &song.score[state.play.beat_at] {
            Node::Play(Note { pitch:Pitch::Abs(x), ..}) => fit_range(*x), // Never generated?
            Node::Play(Note { pitch:Pitch::Rel(x), ..}) => fit_range(*x + state.adjust.root),
            Node::Play(Note { pitch:Pitch::Rest, ..}) => 0,
            _ => unreachable!()
        };
        // Map note->synth zero crossing peroid
        let period = notes[pitch_index as usize];

        // Time for synth zero crossing?
        if state.play.sample_at < state.adjust.duty_as_sample_cache && state.play.synth_at > period {
            state.play.synth_high = !state.play.synth_high;
            state.play.synth_at -= period;
        }

        // Tell the processing thread where we're at
        audio_time.store(state.play.beat_at as u32, Ordering::Relaxed);

        // Play sound
        if state.play.synth_high {
            0.25
        } else {
            0.0 // TODO: This won't work with mixing. Fix zero_value below
        }
        // -- BOILERPLATE --
    };

    let mut zero_value = || { 0.0 };

    let err_fn = |err| panic!("an error occurred on stream: {}", err); // FIXME: Geez don't panic

    #[cfg(feature = "audio_log")]
    let mut audio_log = std::fs::File::create("audio_log.raw").unwrap();
    #[cfg(not(feature = "audio_log"))]
    let mut audio_log:AudioLog = ();

    let stream = device.build_output_stream(
        config,
        move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
            audio_write(data, channels,
                if audio_playing.load(Ordering::Relaxed) {
                    &mut next_value
                } else {
                    &mut zero_value
                },
            &mut audio_log)
        },
        err_fn,
        None,
    )?;
    stream.play()?;

    Ok(stream)
}


fn ami_boot_audio(audio_additional:AudioSeed) -> Option<cpal::Stream> {
    let host = cpal::default_host();
    if let Some(device) = host.default_output_device() {
        let config = device.default_output_config().unwrap();

        let stream_result = match config.sample_format() {
            cpal::SampleFormat::I8 => audio_run::<i8>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::I16 => audio_run::<i16>(&device, &config.into(), audio_additional),
            // cpal::SampleFormat::I24 => audio_run::<I24>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::I32 => audio_run::<i32>(&device, &config.into(), audio_additional),
            // cpal::SampleFormat::I48 => audio_run::<I48>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::I64 => audio_run::<i64>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::U8 => audio_run::<u8>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::U16 => audio_run::<u16>(&device, &config.into(), audio_additional),
            // cpal::SampleFormat::U24 => audio_run::<U24>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::U32 => audio_run::<u32>(&device, &config.into(), audio_additional),
            // cpal::SampleFormat::U48 => audio_run::<U48>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::U64 => audio_run::<u64>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::F32 => audio_run::<f32>(&device, &config.into(), audio_additional),
            cpal::SampleFormat::F64 => audio_run::<f64>(&device, &config.into(), audio_additional),
            sample_format => panic!("Unsupported sample format '{sample_format}'"),
        };

        match stream_result {
            Err(e) => {
                //warn!("Audio startup failure: {}", e); // TODO : logging
                None
            },
            Ok(v) => {
                //trace!("Audio startup success");
                Some(v)
            }
        }
    } else {
        panic!("Failure: No audio device");
        None
    }
}

#[tokio::main]
async fn main() -> io::Result<()> {
    use clap::Parser;

    #[derive(Parser)]
    struct Cli {
        #[arg(long = "play")]
        play: bool,
        #[arg(long = "loud-error", short='e')]
        loud_error:bool,
        filename: Option<PathBuf>
    }
    let cli = Cli::parse();

    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    let audio_time = std::sync::Arc::new(AtomicU32::new(0));
    let audio_play = std::sync::Arc::new(AtomicBool::new(cli.play));
    let audio_song = std::sync::Arc::new(AtomicOptionBox::<Song>::none());
    let audio_seed = AudioSeed { time: audio_time.clone(), play: audio_play.clone(), song: audio_song.clone() };
    let audio = ami_boot_audio(audio_seed);

    let mut vim = Vim::new(Mode::Normal, cli.filename, false, VimAudioSeed { time:audio_time, play:audio_play.clone() }); // Note: time NOT cloned

    enable_raw_mode()?;
    crossterm::execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut term = Terminal::new(backend)?;

    // This will be the file edit area.
    let mut textarea = vim.load(None)?;

    textarea.set_block(Mode::Normal.block());
    textarea.set_cursor_style(Mode::Normal.cursor_style());
    textarea.set_cursor_line_style(Style::default().bg(/*Color::DarkGray*/Color::Indexed(232) /*236 better on pure black bg*/));

    // This is the command-line-mode :entry box, which is only sometimes visible.
    let mut command = TextArea::default();
    command.set_block(Block::default().borders(Borders::NONE));
    command.set_cursor_style(Style::default().fg(Color::Reset).add_modifier(Modifier::REVERSED));

    const FRAMES_PER_SECOND:f32 = 60.0;
    let period = std::time::Duration::from_secs_f32(1.0 / FRAMES_PER_SECOND);
    let mut interval = tokio::time::interval(period);
    let mut events = crossterm::event::EventStream::new();
    let mut should_quit = false;

    // Extra GUI state
    let mut error_highlight: Option<TextRange> = None;
    let mut song_highlights: Vec<TextRange> = Default::default();
    let mut current_status_message: Option<String> = None;

    while !should_quit {
        tokio::select! {
            // FIXME: rather than this wait on mspc messages or something
            // Used to this happened every loop
            _ = interval.tick() => {
                let play = (*vim.audio.play).load(Ordering::Relaxed);
                let time = (*vim.audio.time).load(Ordering::Relaxed);

                // TODO: This is work! Don't do it every time
                textarea.clear_custom_highlight();
                if let Some(highlight) = error_highlight {
                    textarea.custom_highlight(highlight, Style::default().fg(Color::LightRed).add_modifier(Modifier::REVERSED), 35); // TODO if possible blink 25/35
                }
                if (play || time > 0) && (time as usize) < song_highlights.len() {
                    textarea.custom_highlight(song_highlights[time as usize], Style::default().fg(/*Color::LightCyan*/Color::Indexed(51)).add_modifier(Modifier::UNDERLINED), 35); // TODO if possible blink 25/35
                }

                term.draw(|f| {
                    f.render_widget(&textarea, f.area());
                    let mut bottom_line_area = f.area();
                    bottom_line_area.y = bottom_line_area.height-1;
                    bottom_line_area.height=1;

                    if vim.mode.clone() == Mode::Command {
                        current_status_message = None;

                        let bar = ratatui::widgets::Paragraph::new(":");
                        f.render_widget(bar, bottom_line_area);

                        bottom_line_area.x += 1;
                        bottom_line_area.width -= 1;
                        f.render_widget(&command, bottom_line_area);
                    } else if let Some(status) = &current_status_message {
                        let bar = ratatui::widgets::Paragraph::new(status.clone());
                        let bar = bar.style(Style::default().fg(Color::LightRed).add_modifier(Modifier::REVERSED));
                        f.render_widget(bar, bottom_line_area);
                    } else {
                        let bar = ratatui::widgets::Paragraph::new(format!("{} {}", if play { "PLAYING" } else {"Paused "}, time));
                        f.render_widget(bar, bottom_line_area);
                    }
                })?;
            },

            // Terminal event
            Some(Ok(event)) = events.next() => {
                match event.clone().into() { // Mode-indifferent overrides
                    Input {
                        key: Key::Char('p'),
                        ctrl: true,
                        ..
                    } => {
                        audio_play.fetch_xor(true, Ordering::Relaxed);
                    },
                    _ => { // Mode match
                        let VimChanges { transition, dirty, current_file, status_message } = vim.transition(event.into(), &mut textarea, &mut command);

                        if status_message.is_some() {
                            if cli.loud_error { eprintln!("{}", status_message.clone().unwrap()); }

                            current_status_message = status_message;
                        } else if dirty {
                            current_status_message = None;
                        }

                        // Ugly: This code is written in a super functional style and between here and the vim = my added code just treats it as mutable

                        vim.dirty = vim.dirty || dirty;
                        vim.current_file = current_file.or(vim.current_file);

                        // Throw over the wall, but ONLY if we're in normal mode *and* something changed.
                        match (transition.clone(), vim.mode) {
                            (Transition::Nop, Mode::Normal) |
                            (Transition::Mode(Mode::Normal), _) => {
                                if vim.dirty {
                                    // Completely parse buffer
                                    // TODO: Factor elsewhere
                                    let mut line_starts:Vec<usize> = Default::default(); // notice, in BYTES
                                    let mut all:String = Default::default();
                                    // FIXME: Since I'm scanning twice, why not allocate an early buffer
                                    for line in textarea.lines() {
                                        line_starts.push(all.len());
                                        all = all + line;
                                        all = all + "\n";
                                    }
                                    let all_len = all.len();
                                    let song = if all_len > 0 {parse_language(all)}
                                        else { Ok(Default::default()) }; // Empty string is valid
                                    //eprintln!("D: {:?}", song.clone()); // Before processing

                                    error_highlight = None;

                                    match song {
                                        Ok(song) => {
                                            // I *think* I don't need SeqCst because only one thread writes?
                                            let song = absolute_language(song);

                                            // Calculate highlights map
                                            {
                                                let mut line_at = 0;
                                                let mut char_at = 0;
                                                let mut chars:Option<std::str::Chars> = None;
                                                let lines = &textarea.lines();
                                                song_highlights.clear();
                                                for node in &song.score {
                                                    match node {
                                                        Node::Play(Note { span:ByteSpan { begin, end }, ..}) => {
                                                            use tuple_map::*;
                                                            song_highlights.push((begin,end).map(|idx| {
                                                                let idx = *idx;
                                                                let line_at_was = line_at;
                                                                // Step forward lines until line_starts is *just about* to pass our index.
                                                                while line_at + 1 < line_starts.len() && line_starts[line_at+1] <= idx {
                                                                    line_at+=1;
                                                                }
                                                                // We moved forward a line (or started)
                                                                if chars.is_none() || line_at > line_at_was {
                                                                    char_at = 0;
                                                                    chars = Some(lines[line_at].chars()); // FIXME: fuse()?
                                                                }
                                                                let within_idx = idx - line_starts[line_at];
                                                                let chars = chars.as_mut().unwrap();
                                                                loop {
                                                                    let position = lines[line_at].len() - chars.as_str().len(); // Freakish but works
                                                                    if position >= within_idx { break }; // Implicitly assumes all notes length 1 or greater
                                                                    if chars.next().is_none() { break }
                                                                    char_at += 1;
                                                                }
                                                                (line_at, char_at)
                                                            }));
                                                        },
                                                        _ => (),
                                                    }
                                                }
                                                // Note: All ranges are incidentally +1, but that's the way TextArea wants it
                                            }

                                            audio_song.store(Some(Box::new(song)), Ordering::AcqRel)
                                        },
                                        Err(error) => {
                                            // TODO: Reverse Bad position
                                            if cli.loud_error { eprintln!("Syntax: {}", error.clone()); }

                                            current_status_message = Some(format!("Syntax: {}", error.clone()));

                                            let position = match error {
                                                pom::Error::Incomplete => all_len,
                                                pom::Error::Mismatch { position, .. } |
                                                pom::Error::Conversion { position, .. } |
                                                pom::Error::Expect { position, .. } |
                                                pom::Error::Custom { position, .. } => position
                                            };
                                            let (line_idx, line_char) = match line_starts.binary_search(&position) {
                                                Ok(line_idx) => (line_idx, 0 as usize),
                                                Err(line_idx_plus) => {
                                                    let line_idx = line_idx_plus-1; // It always gives the index after, and line_starts[0] is always 0
                                                    let line_base = line_starts[line_idx];
                                                    let line_byte = position - line_base;
                                                    let line_char = {
                                                        let mut result = 0;
                                                        for (idx, _) in textarea.lines()[line_idx].char_indices() {
                                                            if idx >= line_byte { // Can only be > if something went real wrong with utf-8
                                                                break;
                                                            }
                                                            result += 1;
                                                        }
                                                        result
                                                    };
                                                    (line_idx, line_char) // WRONG FOR UTF-8 FIXME // ALSO: CURSED RETURN
                                                }
                                            };
                                            error_highlight = Some(((line_idx, line_char), (line_idx, line_char+1)));
                                        }
                                    }

                                    vim.dirty = false;
                                }
                            },
                            _ => ()
                        };

                        vim = match transition {
                            // UI mode changed
                            Transition::Mode(mode) if vim.mode != mode => {
                                textarea.set_block(mode.block());
                                textarea.set_cursor_style(mode.cursor_style());
                                Vim::new(mode, vim.current_file, vim.dirty, vim.audio)
                            }

                            // Nothing changed
                            Transition::Nop | Transition::Mode(_) => vim,
                            Transition::Pending(input) => vim.with_pending(input),
                            Transition::Quit => { should_quit = true; vim },
                        }
                    }
                }
            },
        }
    }

    disable_raw_mode()?;
    crossterm::execute!(
        term.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    term.show_cursor()?;

    println!("Lines: {:?}", textarea.lines());

    Ok(())
}
