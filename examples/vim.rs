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

// For orb
use ratatui_image::{picker::Picker, StatefulImage, protocol::Protocol}; // todo: protocol::StatefulProtocol

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Normal,
    Command, // "colon mode", what the manual calls Command-line mode
    Operator(char)
}

impl Mode {
    fn cursor_style(&self) -> Style {
        let color = match self {
            Self::Normal | Self::Operator(_) => Color::Reset,
            // FIXME: This matches behavior of vim, would it be better to underline or something?
            Self::Command => { return Style::default(); }
        };
        Style::default().fg(color).add_modifier(Modifier::REVERSED)
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Normal | Self::Operator(_) => write!(f, "NORMAL"),
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

// Command line parser

enum CommandLineSetType {
    On,
    Off,
    Question
}

// TODO: reset, reset!, set for tones, play
enum CommandLine {
    New,
    Quit,
    Help(String),
    Set(String, CommandLineSetType), // What you set, what you set it to // TODO: Local?
    Beep,
    Taunt(bool),
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
    let wqae = (seq("q").discard() - seq("uit").discard().opt()).map(|_| CommandLine::Quit);

    let parser =
          seq("new").discard().map(|_| CommandLine::New)

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

        | seq("beep").discard().map(|_| CommandLine::Beep)

        | (seq("taunt").discard() * sym('!').discard().opt()).map(|e| CommandLine::Taunt(e.is_some())
    ) - end();
    parser.parse_str(&input)
}

// State of Vim emulation
struct Vim {
    mode: Mode,
    pending: Input, // Pending input to handle a sequence with two keys like gg
}

enum OrbEffect {
    None,
    Reset
}

// All changes in a single Vim transition
struct VimChanges {
    transition:Transition,
    effect:OrbEffect,
    status_message: Option<String> // If it changed
}

impl Vim {
    fn new(mode: Mode) -> Self {
        Self {
            mode,
            pending: Input::default()
        }
    }

    fn beep(&self) { // TODO: offer CPAL, blink options? // Note: Takes self but doesn't use it (yet?)
        print!("\x07");
    }

    fn with_pending(self, pending: Input) -> Self {
        Self {
            mode: self.mode,
            pending,
        }
    }

    // Result: This function should not modify vim, only provide a set of changes to apply to vim
    fn transition(&self, input: Input, command: &mut TextArea<'_>) -> VimChanges {
        const NOP:VimChanges = VimChanges {
            transition:Transition::Nop, effect:OrbEffect::None, status_message:None
        };

        if input.key == Key::Null {
            return VimChanges { ..NOP };
        }

        match self.mode {
            Mode::Normal | Mode::Operator(_) => {
                match input {
                    Input {
                        key: Key::Char('q'),
                        ..
                    } | Input {
                        key: Key::Char('c'),
                        ctrl: true,
                        ..
                    } => {
                        return VimChanges { transition:Transition::Quit, ..NOP }
                    },
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
                    _ => VimChanges { ..NOP },
                }
            }
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
                    const NO_WRITE:&str = "Error: No write since last change (add ! to override)";

                    match entry {
                        Ok(CommandLine::Quit) => {
                            return VimChanges { transition:Transition::Quit, ..NOP }; // Short circuit
                        },
                        Ok(CommandLine::New) =>
                            return VimChanges { transition:Transition::Mode(Mode::Normal), effect:OrbEffect::Reset, ..NOP },
                        Ok(CommandLine::Beep) => {
                            self.beep();
                        },
                        Ok(CommandLine::Taunt(e)) => {
                            if !e {
                                error = Some("The Orb remains implacable".to_string());
                            } else {
                                self.beep();
                                error = Some("Despite your efforts, the Orb remains implacable".to_string());
                            }
                        }
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

#[tokio::main]
async fn main() -> io::Result<()> {
    use clap::Parser;

    #[derive(Parser)]
    struct Cli {
        #[arg(long = "print-error", short='e')]
        print_error:bool,
    }
    let cli = Cli::parse();

    let picker = Picker::from_query_stdio().unwrap(); // Must do this before stdout lock

    let mut stdout = io::stdout().lock();

    let mut vim = Vim::new(Mode::Normal);

    enable_raw_mode()?;
    crossterm::execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut term = Terminal::new(backend)?;

/*
    // Replace with whatever your basic widget is
    // This will be the file edit area.
    let init_textarea = |textarea:&mut TextArea| {
        textarea.set_block(Mode::Normal.block());
        textarea.set_cursor_style(Mode::Normal.cursor_style());
        textarea.set_cursor_line_style(Style::default().bg(/*Color::DarkGray*/Color::Indexed(232) /*236 better on pure black bg*/));
    };
    let mut textarea = vim.load(None)?;
    init_textarea(&mut textarea);
*/

    // This is the command-line-mode :entry box, which is only sometimes visible.
    let mut command = TextArea::default();
    command.set_block(Block::default().borders(Borders::NONE));
    command.set_cursor_style(Style::default().fg(Color::Reset).add_modifier(Modifier::REVERSED));

    const FRAMES_PER_SECOND:f32 = 60.0;
    let period = std::time::Duration::from_secs_f32(1.0 / FRAMES_PER_SECOND);
    let mut interval = tokio::time::interval(period);
    let mut events = crossterm::event::EventStream::new();
    let mut should_quit = false;

    // ORB TECHNOLOGY
    use glam::{UVec2, Vec2};

    // frame-width-in-blocks, frame-height-in-blocks, internal-width-of-block-in-pixels, internal-height-of-block-in-pixels
    type SizeQuad = (u16, u16, u16, u16);

    struct Orb {
        size: SizeQuad,
        pub protocol: Protocol
    };

    impl Orb {
        fn new_with(size:SizeQuad, protocol:Protocol) -> Self {
            Orb { size, protocol }
        }
        pub fn new(picker:&Picker, size:SizeQuad) -> Self {
            let protocol = {
                let (frame_width, frame_height, pixel_width, pixel_height) = {
                    let (fw, fh, iw, ih) = size;
                    (fw as u32, fh as u32, fw as u32*iw as u32, fh as u32*ih as u32)
                };
                let mut data:Vec<u8> = Default::default();

                let size = UVec2::new(pixel_width, pixel_height).as_vec2();
                let center = size/2.0;
                let lesser_axis = size.x.min(size.y);

                for y in 0..pixel_height {
                    for x in 0..pixel_width {
                        let at = UVec2::new(x, y).as_vec2();
                        let relative_at = (at - center)*2.0/lesser_axis;
                        let dist_sq = relative_at.length_squared();
                        //eprintln!("{at}, {relative_at}, {dist_sq}");

                        if dist_sq > 1.0 {
                            data.push(0);
                            data.push(0);
                            data.push(0);
                        } else {
                            data.push(150);
                            data.push(222);
                            data.push(209);
                        }
                    }
                }

                //eprintln!("{pixel_width},{pixel_height} > {frame_width},{frame_height} ... {}", data.len());
                //std::process::exit(0);

                let raw_image = image::DynamicImage::ImageRgb8(image::ImageBuffer::from_raw(pixel_width, pixel_height, data).unwrap());
                picker.new_protocol(raw_image, ratatui::layout::Rect::new( 0, 0, frame_width as u16, frame_height as u16), ratatui_image::Resize::Crop(None)).unwrap()
            };
            Self::new_with(size, protocol)
        }
        pub fn resize(&mut self, picker:&Picker, size:SizeQuad) {
            if size != self.size {
                *self = Self::new(picker, size);
            }
        }
    }
    let mut orb:Option<Orb> = None;

    // Extra GUI state
    let mut current_status_message: Option<String> = None;

    while !should_quit {
        tokio::select! {
            // FIXME: rather than this wait on mspc messages or something
            // Used to this happened every loop
            _ = interval.tick() => {

                term.draw(|f| {
                    let area = f.area();

                    {
                        let mut image_area = area;
                        image_area.height -= 1;

                        let (iw, ih) = picker.font_size();
                        let (fw, fh) = (image_area.width, image_area.height);
                        let size = (fw, fh, iw, ih);
                        if let Some(orb) = orb.as_mut() {
                            orb.resize(&picker, size);
                        } else {
                            orb = Some(Orb::new(&picker, size));
                        }
                        if let Some(orb) = orb.as_mut() {
                            f.render_widget(ratatui_image::Image::new(&mut orb.protocol), f.area());
                        } else {
                            unreachable!();
                        }
                    }
//                    f.render_stateful_widget(StatefulImage::default(), f.area(), &mut image);

                    let mut bottom_line_area = area;
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
//                        let bar = ratatui::widgets::Paragraph::new(format!("{} {}", if play { "PLAYING" } else {"Paused "}, time));
//                        f.render_widget(bar, bottom_line_area);
                    }
                })?;
            },

            // Terminal event
            Some(Ok(event)) = events.next() => {
                match <crossterm::event::Event as Into<Input>>::into(event.clone()) { // Mode-indifferent overrides
                    _ => { // Mode match
                        let VimChanges { transition, effect, status_message } = vim.transition(event.into(), &mut command);

                        if status_message.is_some() {
                            if cli.print_error { eprintln!("{}", status_message.clone().unwrap()); }

                            current_status_message = status_message;
                        } else if true { // TODO
                            current_status_message = None;
                        }

                        // Do anything?
/*
                        match (transition.clone(), vim.mode) {
                            (Transition::Nop, Mode::Normal) |
                            (Transition::Mode(Mode::Normal), _) => {
                                
                            },
                            _ => ()
                        };
*/

                        vim = match transition {
                            // UI mode changed
                            Transition::Mode(mode) if vim.mode != mode => {
                                Vim::new(mode)
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

    Ok(())
}
