use std::{fmt::Display, iter::repeat};

use crate::{
    error::{Error, MessageBuilder, MessageKind},
    file::File,
};

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub from: usize,
    pub to: usize,
}

// Brackets
#[derive(Clone, Debug)]
pub struct LeftRound {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct RightRound {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct LeftSquare {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct RightSquare {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct LeftCurly {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct RightCurly {
    span: Span,
}

// Symbols
#[derive(Clone, Debug)]
pub struct Comma {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct DoubleColon {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct Arrow {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct Tick {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct Dot {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct Colon {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct Equals {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct Semicolon {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct Pipe {
    span: Span,
}

// Keywords
#[derive(Clone, Debug)]
pub struct Let {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct Base {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct Pub {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct Type {
    span: Span,
}
#[derive(Clone, Debug)]
pub struct Module {
    span: Span,
}

#[derive(Clone, Debug)]
pub struct Ident {
    span: Span,
    data: String,
}
#[derive(Clone, Debug)]
pub enum LiteralKind {
    String,
    Integer,
    Decimal,
}
#[derive(Clone, Debug)]
pub struct Literal {
    span: Span,
    kind: LiteralKind,
    data: String,
}

#[derive(Clone, Debug)]
pub enum Token {
    LeftRound(LeftRound),
    RightRound(RightRound),
    LeftSquare(LeftSquare),
    RightSquare(RightSquare),
    LeftCurly(LeftCurly),
    RightCurly(RightCurly),

    Comma(Comma),
    DoubleColon(DoubleColon),
    Arrow(Arrow),
    Tick(Tick),
    Dot(Dot),
    Colon(Colon),
    Equals(Equals),
    Semicolon(Semicolon),
    Pipe(Pipe),

    Let(Let),
    Base(Base),
    Pub(Pub),
    Type(Type),
    Module(Module),

    Ident(Ident),
    Literal(Literal),
}

pub struct UnknownCharacter {
    span: Span,
    c: char,
}

impl Error for UnknownCharacter {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> MessageBuilder<'a> {
        ctx.start(
            MessageKind::Error,
            &format!("Unknown character `{}`", self.c),
            self.span.from,
        )
        .print_line(self.span.from)
        .print_point(self.span)
    }
}

pub struct MissingArrowHead {
    span: Span,
}

impl Error for MissingArrowHead {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> MessageBuilder<'a> {
        ctx.start(MessageKind::Error, "Missing arrow head", self.span.from)
            .print_line(self.span.from)
            .print_point(self.span)
    }
}

pub struct UnterminatedString {
    opening: Span,
    eof: Span,
}

impl Error for UnterminatedString {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> MessageBuilder<'a> {
        ctx.start(MessageKind::Error, "Missing closing \"", self.eof.from)
            .print_line(self.eof.from)
            .print_point(self.eof)
    }
}

pub struct TooManyDecimalPoints {
    span: Span,
}

impl Error for TooManyDecimalPoints {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> MessageBuilder<'a> {
        ctx.start(
            MessageKind::Error,
            "Too many decimal points in number literal",
            self.span.from,
        )
        .print_line(self.span.from)
        .print_point(self.span)
    }
}

pub enum LexicalError {
    UnknownCharacter(UnknownCharacter),
    MissingArrowHead(MissingArrowHead),
    UnterminatedString(UnterminatedString),
    TooManyDecimalPoints(TooManyDecimalPoints),
}

impl Error for LexicalError {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> MessageBuilder<'a> {
        match self {
            Self::UnknownCharacter(inner) => inner.print_error(ctx),
            Self::MissingArrowHead(inner) => inner.print_error(ctx),
            Self::UnterminatedString(inner) => inner.print_error(ctx),
            Self::TooManyDecimalPoints(inner) => inner.print_error(ctx),
        }
    }
}

pub struct Cursor<'a> {
    index: usize,
    chars: Vec<char>,
    errors: &'a mut Vec<Box<dyn Error>>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &str, errors: &'a mut Vec<Box<dyn Error>>) -> Self {
        Self {
            index: 0,
            chars: input.chars().collect(),
            errors,
        }
    }

    pub fn first(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    pub fn second(&self) -> Option<char> {
        self.chars.get(self.index + 1).copied()
    }

    pub fn next(&mut self) -> Option<char> {
        let r = self.chars.get(self.index);
        if r.is_some() {
            self.index += 1;
        }
        r.copied()
    }

    pub fn match_next(&mut self, search: char) -> bool {
        let Some(c) = self.first() else { return false };
        if c == search {
            self.index += 1;
        }
        c == search
    }

    fn line_offset(&self, index: usize) -> (usize, usize) {
        let line = self.chars[0..index].iter().filter(|c| **c == '\n').count();
        let offset = index
            - self.chars[0..index]
                .iter()
                .enumerate()
                .filter(|(_, c)| **c == '\n')
                .fold(0, |_, (i, _)| i);
        (line + 1, offset + 1)
    }

    fn error(&mut self, error: LexicalError) {
        self.errors.push(Box::new(error));
    }

    pub fn success(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn get(&mut self) -> Option<Token> {
        let from = self.index;
        let span = |cursor: &Self| Span {
            from,
            to: cursor.index,
        };

        let token = match self.next()? {
            '(' => Token::LeftRound(LeftRound { span: span(self) }),
            ')' => Token::RightRound(RightRound { span: span(self) }),
            '[' => Token::LeftSquare(LeftSquare { span: span(self) }),
            ']' => Token::RightSquare(RightSquare { span: span(self) }),
            '{' => Token::LeftCurly(LeftCurly { span: span(self) }),
            '}' => Token::RightCurly(RightCurly { span: span(self) }),

            ',' => Token::Comma(Comma { span: span(self) }),
            ':' => {
                if self.match_next(':') {
                    Token::DoubleColon(DoubleColon { span: span(self) })
                } else {
                    Token::Colon(Colon { span: span(self) })
                }
            }
            '\'' => Token::Tick(Tick { span: span(self) }),
            '.' => Token::Dot(Dot { span: span(self) }),
            '=' => Token::Equals(Equals { span: span(self) }),
            ';' => Token::Semicolon(Semicolon { span: span(self) }),
            '|' => Token::Pipe(Pipe { span: span(self) }),
            '-' => {
                if self.match_next('>') {
                    Token::Arrow(Arrow { span: span(self) })
                } else {
                    self.error(LexicalError::MissingArrowHead(MissingArrowHead {
                        span: span(self),
                    }));
                    return self.get();
                }
            }
            '"' => {
                let mut data = String::new();
                while !self.match_next('"') {
                    let Some(c) = self.next() else {
                        self.error(LexicalError::UnterminatedString(UnterminatedString {
                            opening: Span { from, to: from + 1 },
                            eof: Span {
                                from: self.index,
                                to: self.index + 1,
                            },
                        }));
                        return self.get();
                    };

                    data.push(c);
                }

                Token::Literal(Literal {
                    span: span(self),
                    kind: LiteralKind::String,
                    data,
                })
            }
            c if c.is_digit(10) => {
                let mut data = String::new();
                data.push(c);

                let mut decimal = false;

                while self.first()?.is_digit(10) || self.first()? == '.' {
                    let c = self.next()?;
                    if c == '.' {
                        if decimal {
                            self.error(LexicalError::TooManyDecimalPoints(TooManyDecimalPoints {
                                span: Span {
                                    from: self.index - 1,
                                    to: self.index,
                                },
                            }));
                            return self.get();
                        }
                        decimal = true
                    }
                    data.push(c);
                }

                Token::Literal(Literal {
                    span: span(self),
                    kind: if decimal {
                        LiteralKind::Decimal
                    } else {
                        LiteralKind::Integer
                    },
                    data,
                })
            }
            c if c.is_alphabetic() => {
                let mut data = String::new();
                data.push(c);

                while self.first()?.is_alphanumeric() {
                    data.push(self.next()?)
                }

                Token::Ident(Ident {
                    span: span(self),
                    data,
                })
            }
            ' ' | '\n' => return self.get(),
            c => {
                self.error(LexicalError::UnknownCharacter(UnknownCharacter {
                    span: span(self),
                    c,
                }));
                return self.get();
            }
        };

        Some(token)
    }

    pub fn iter(mut self) -> impl Iterator<Item = Token> + 'a {
        std::iter::from_fn(move || self.get())
    }
}

pub fn tokenize<'a>(
    file: &File,
    errors: &'a mut Vec<Box<dyn Error>>,
) -> impl Iterator<Item = Token> + 'a {
    let cursor = Cursor::new(&file.content, errors);
    cursor.iter()
}
