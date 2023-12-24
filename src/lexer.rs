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
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct RightRound {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct LeftSquare {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct RightSquare {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct LeftCurly {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct RightCurly {
    pub span: Span,
}

// Symbols
#[derive(Clone, Debug)]
pub struct Comma {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct DoubleColon {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Arrow {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Tick {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Dot {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Colon {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Equals {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Semicolon {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Pipe {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Underscore {
    pub span: Span,
}

// Keywords
#[derive(Clone, Debug)]
pub struct Let {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Base {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Pub {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Type {
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Module {
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Ident {
    pub span: Span,
    pub data: String,
}
#[derive(Clone, Debug)]
pub enum LiteralKind {
    String,
    Integer,
    Decimal,
}
#[derive(Clone, Debug)]
pub struct Literal {
    pub span: Span,
    pub kind: LiteralKind,
    pub data: String,
}

#[derive(Clone, Debug)]
pub struct EOF {
    span: Span,
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
    Underscore(Underscore),

    Let(Let),
    Base(Base),
    Pub(Pub),
    Type(Type),
    Module(Module),

    Ident(Ident),
    Literal(Literal),

    EOF(EOF),
}

impl Token {
    pub fn as_left_round(&self) -> Option<&LeftRound> {
        if let Self::LeftRound(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_right_round(&self) -> Option<&RightRound> {
        if let Self::RightRound(token) = self {
            Some(token)
        } else {
            None
        }
    }
    pub fn as_left_square(&self) -> Option<&LeftSquare> {
        if let Self::LeftSquare(token) = self {
            Some(token)
        } else {
            None
        }
    }
    pub fn as_right_square(&self) -> Option<&RightSquare> {
        if let Self::RightSquare(token) = self {
            Some(token)
        } else {
            None
        }
    }
    pub fn as_left_curly(&self) -> Option<&LeftCurly> {
        if let Self::LeftCurly(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_right_curly(&self) -> Option<&RightCurly> {
        if let Self::RightCurly(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_comma(&self) -> Option<&Comma> {
        if let Self::Comma(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_arrow(&self) -> Option<&Arrow> {
        if let Self::Arrow(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_tick(&self) -> Option<&Tick> {
        if let Self::Tick(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_dot(&self) -> Option<&Dot> {
        if let Self::Dot(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_equals(&self) -> Option<&Equals> {
        if let Self::Equals(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_pipe(&self) -> Option<&Pipe> {
        if let Self::Pipe(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_colon(&self) -> Option<&Colon> {
        if let Self::Colon(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_underscore(&self) -> Option<&Underscore> {
        if let Self::Underscore(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_double_colon(&self) -> Option<&DoubleColon> {
        if let Self::DoubleColon(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_semicolon(&self) -> Option<&Semicolon> {
        if let Self::Semicolon(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_let(&self) -> Option<&Let> {
        if let Self::Let(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_base(&self) -> Option<&Base> {
        if let Self::Base(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_pub(&self) -> Option<&Pub> {
        if let Self::Pub(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_type(&self) -> Option<&Type> {
        if let Self::Type(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_module(&self) -> Option<&Module> {
        if let Self::Module(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        if let Self::Ident(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_literal(&self) -> Option<&Literal> {
        if let Self::Literal(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn as_eof(&self) -> Option<&EOF> {
        if let Self::EOF(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::LeftRound(inner) => inner.span,
            Self::RightRound(inner) => inner.span,
            Self::LeftSquare(inner) => inner.span,
            Self::RightSquare(inner) => inner.span,
            Self::LeftCurly(inner) => inner.span,
            Self::RightCurly(inner) => inner.span,

            Self::Comma(inner) => inner.span,
            Self::DoubleColon(inner) => inner.span,
            Self::Tick(inner) => inner.span,
            Self::Dot(inner) => inner.span,
            Self::Colon(inner) => inner.span,
            Self::Equals(inner) => inner.span,
            Self::Semicolon(inner) => inner.span,
            Self::Pipe(inner) => inner.span,
            Self::Arrow(inner) => inner.span,
            Self::Underscore(inner) => inner.span,

            Self::Let(inner) => inner.span,
            Self::Base(inner) => inner.span,
            Self::Pub(inner) => inner.span,
            Self::Type(inner) => inner.span,
            Self::Module(inner) => inner.span,

            Self::Ident(inner) => inner.span,
            Self::Literal(inner) => inner.span,
            Self::EOF(inner) => inner.span,
        }
    }
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

    fn error(&mut self, error: LexicalError) {
        self.errors.push(Box::new(error));
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
            '_' => Token::Underscore(Underscore { span: span(self) }),
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
                        if !self.first()?.is_digit(10) {
                            // The dot is a dot call
                            self.index -= 1;
                            break;
                        }
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
                match data.as_str() {
                    "let" => Token::Let(Let { span: span(self) }),
                    "base" => Token::Base(Base { span: span(self) }),
                    "pub" => Token::Pub(Pub { span: span(self) }),
                    "type" => Token::Type(Type { span: span(self) }),
                    "module" => Token::Module(Module { span: span(self) }),
                    data => Token::Ident(Ident {
                        span: span(self),
                        data: data.to_owned(),
                    }),
                }
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

pub fn tokenize<'a>(file: &File, errors: &'a mut Vec<Box<dyn Error>>) -> Vec<Token> {
    let cursor = Cursor::new(&file.content, errors);
    let mut tokens = cursor.iter().collect::<Vec<Token>>();
    tokens.push(Token::EOF(EOF {
        span: Span {
            from: file.content.len() - 1,
            to: file.content.len(),
        },
    }));
    tokens
}
