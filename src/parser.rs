use std::fmt::Display;

use crate::{
    error::{Error, MessageKind},
    lexer::{DoubleColon, Ident, Token},
};

pub enum ParseError {
    PathEndsWithDoubleColon(PathEndsWithDoubleColon),
}

impl Error for ParseError {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> crate::error::MessageBuilder<'a> {
        match self {
            Self::PathEndsWithDoubleColon(inner) => inner.print_error(ctx),
        }
    }
}

pub struct Tokens<'a> {
    tokens: Vec<Token>,
    index: usize,
    errors: &'a mut Vec<Box<dyn Error>>,
}

impl<'a> Tokens<'a> {
    pub fn new(tokens: Vec<Token>, errors: &'a mut Vec<Box<dyn Error>>) -> Self {
        Self {
            tokens,
            index: 0,
            errors,
        }
    }

    pub fn first(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    pub fn next(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.index);
        if token.is_some() {
            self.index += 1
        }
        token.cloned()
    }

    pub fn match_next<T: Clone, F: FnOnce(&Token) -> Option<&T>>(&mut self, f: F) -> Option<T> {
        let result = self.first().and_then(f).cloned();
        if result.is_some() {
            self.index += 1;
        }
        result
    }

    pub fn error(&mut self, error: ParseError) {
        self.errors.push(Box::new(error))
    }
}

pub trait Parser: Sized {
    fn try_parse(tokens: &mut Tokens) -> Option<Self>;
    fn parse(tokens: &mut Tokens) -> Option<Self> {
        let start = tokens.index;
        let result = Self::try_parse(tokens);
        if result.is_none() {
            tokens.index = start;
        }
        result
    }
}

pub struct PathEndsWithDoubleColon {
    double_colon: DoubleColon,
}

impl Error for PathEndsWithDoubleColon {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> crate::error::MessageBuilder<'a> {
        ctx.start(
            MessageKind::Error,
            "Path cannot end in a double colon",
            self.double_colon.span.from,
        )
        .print_line(self.double_colon.span.from)
        .print_point(self.double_colon.span)
    }
}

#[derive(Clone, Debug)]
pub struct Path {
    parts: Vec<Ident>,
    separators: Vec<DoubleColon>,
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.parts.first().unwrap().data)?;
        for part in &self.parts[1..] {
            write!(f, "::{}", part.data)?;
        }
        Ok(())
    }
}

impl Parser for Path {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let mut path = Path {
            parts: Vec::new(),
            separators: Vec::new(),
        };
        path.parts.push(tokens.match_next(Token::as_ident)?);

        loop {
            let Some(separator) = tokens.match_next(Token::as_double_colon) else {
                return Some(path);
            };
            path.separators.push(separator);

            let Some(part) = tokens.match_next(Token::as_ident) else {
                tokens.error(ParseError::PathEndsWithDoubleColon(
                    PathEndsWithDoubleColon {
                        double_colon: path.separators.last().unwrap().clone(),
                    },
                ));
                return None;
            };
            path.parts.push(part)
        }
    }
}

pub fn parse(tokens: Vec<Token>, errors: &mut Vec<Box<dyn Error>>) -> Option<Path> {
    let mut tokens = Tokens::new(tokens, errors);
    Path::parse(&mut tokens)
}
