use colored::Colorize;
use std::fmt::Display;

use crate::{file::File, lexer::Span};

pub enum MessageKind {
    Error,
    Warning,
}

impl Display for MessageKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Error => "error".red(),
                Self::Warning => "warning".yellow(),
            }
        )
    }
}

pub enum Part {
    Line { line: usize, content: String },
    Point(Span),
}

pub struct MessageBuilder<'a> {
    ctx: &'a Context<'a>,
    kind: MessageKind,
    message: String,
    index: usize,
    parts: Vec<Part>,
}

impl MessageBuilder<'_> {
    pub fn line(&self, index: usize) -> usize {
        self.ctx.file.content[0..index]
            .chars()
            .filter(|c| *c == '\n')
            .count()
            + 1
    }

    pub fn offset(&self, index: usize) -> usize {
        let start = self.ctx.file.content[0..index]
            .chars()
            .enumerate()
            .filter(|(_, c)| *c == '\n')
            .fold(0, |_, (i, _)| i + 1);
        index - start + 1
    }

    pub fn print_line(mut self, index: usize) -> Self {
        self.parts.push(Part::Line {
            line: self.line(index),
            content: self
                .ctx
                .file
                .content
                .split('\n')
                .nth(self.line(index) - 1)
                .unwrap()
                .to_owned(),
        });

        self
    }

    pub fn print_point(mut self, span: Span) -> Self {
        let span = Span {
            from: self.offset(span.from) - 1,
            to: self.offset(span.to) - 1,
        };
        self.parts.push(Part::Point(span));

        self
    }

    pub fn print(&self) {
        println!("{}: {}", self.kind, self.message);
        println!(
            "{} {}:{}:{}",
            "-->".blue(),
            self.ctx.file.path.display(),
            self.line(self.index),
            self.offset(self.index)
        );

        let max_line = self
            .parts
            .iter()
            .filter_map(|part| {
                if let Part::Line { line, .. } = part {
                    Some(line)
                } else {
                    None
                }
            })
            .max()
            .copied()
            .unwrap_or(1);
        let line_width = (max_line as f64).log10().floor() as usize + 1;

        self.parts.iter().for_each(|part| match part {
            Part::Line { line, content } => {
                println!(
                    "{} {}",
                    format!("{:<1$} |", line, line_width).blue(),
                    content
                )
            }
            Part::Point(span) => {
                println!(
                    "{} {: <3$}{:^<4$}",
                    format!("{: <1$} |", "", line_width).blue(),
                    "",
                    "",
                    span.from,
                    span.to - span.from
                )
            }
        });

        println!()
    }
}

pub struct Context<'a> {
    file: &'a File,
}

pub trait Error {
    fn print_error<'a>(&self, ctx: &'a Context) -> MessageBuilder<'a>;
}

impl<'a> Context<'a> {
    pub fn new(file: &'a File) -> Self {
        Self { file }
    }

    pub fn print_errors(&self, errors: &[Box<dyn Error>]) {
        errors
            .iter()
            .for_each(|error| error.print_error(self).print())
    }

    pub fn start<'b>(
        &'b self,
        kind: MessageKind,
        message: &str,
        index: usize,
    ) -> MessageBuilder<'b> {
        MessageBuilder {
            ctx: self,
            kind,
            message: message.to_owned(),
            index,
            parts: Vec::new(),
        }
    }
}
