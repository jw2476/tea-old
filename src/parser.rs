use std::{fmt::Display, iter::Sum};

use crate::{
    error::{Error, MessageKind},
    lexer::{
        Arrow, Comma, DoubleColon, Ident, LeftCurly, LeftRound, LeftSquare, RightCurly, RightRound,
        RightSquare, Span, Tick, Token,
    },
};

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

    pub fn first(&self) -> &Token {
        self.tokens.get(self.index).unwrap()
    }

    pub fn next(&mut self) -> Token {
        let token = self.tokens.get(self.index).unwrap();
        if token.as_eof().is_none() {
            self.index += 1
        }
        token.clone()
    }

    pub fn second(&self) -> &Token {
        self.tokens
            .get((self.index + 1).min(self.tokens.len() - 1))
            .unwrap()
    }

    pub fn match_next<T: Clone, F: FnOnce(&Token) -> Option<&T>>(&mut self, f: F) -> Option<T> {
        let result = f(self.first()).cloned();
        if result.is_some() && self.first().as_eof().is_none() {
            self.index += 1;
        }
        result
    }

    pub fn error<E: Error + 'static>(&mut self, error: E) {
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
                tokens.error(PathEndsWithDoubleColon {
                    double_colon: path.separators.last().unwrap().clone(),
                });
                return None;
            };
            path.parts.push(part)
        }
    }
}

pub struct MissingReturnType {
    arrow: Arrow,
}

impl Error for MissingReturnType {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> crate::error::MessageBuilder<'a> {
        ctx.start(
            MessageKind::Error,
            "Function type missing return type",
            self.arrow.span.from,
        )
        .print_line(self.arrow.span.from)
        .print_point(self.arrow.span)
    }
}

pub struct FunctionType {
    args: Args,
    arrow: Arrow,
    return_type: Box<Type>,
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.args, self.return_type)
    }
}

impl Parser for FunctionType {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let args = Args::parse(tokens)?;
        let arrow = tokens.match_next(Token::as_arrow)?;
        let Some(return_type) = Type::parse(tokens) else {
            tokens.error(MissingReturnType { arrow });
            return None;
        };
        Some(Self {
            args,
            arrow,
            return_type: Box::new(return_type),
        })
    }
}

pub struct EmptySumType {
    span: Span,
}

impl Error for EmptySumType {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> crate::error::MessageBuilder<'a> {
        ctx.start(
            MessageKind::Error,
            "Sum type has no variants",
            self.span.from,
        )
        .print_line(self.span.from)
        .print_point(self.span)
    }
}

pub struct ExpectedIdent {
    span: Span,
}

impl Error for ExpectedIdent {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> crate::error::MessageBuilder<'a> {
        ctx.start(MessageKind::Error, "Expected identifier", self.span.from)
            .print_line(self.span.from)
            .print_point(self.span)
    }
}

pub struct SumTypeField(Ident, Option<Box<Type>>);

impl Parser for SumTypeField {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let Some(ident) = tokens.match_next(Token::as_ident) else {
            tokens.error(ExpectedIdent {
                span: Span {
                    from: tokens.index,
                    to: tokens.index + 1,
                },
            });
            return None;
        };
        let ty = if tokens.first().as_comma().is_some() {
            None
        } else {
            Type::parse(tokens)
        };
        Some(Self(ident, ty.map(Box::new)))
    }
}

impl Display for SumTypeField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.data)?;
        if let Some(ty) = &self.1 {
            write!(f, " {}", ty)?;
        } else {
        }
        Ok(())
    }
}

pub struct ExpectedComma {
    span: Span,
}

impl Error for ExpectedComma {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> crate::error::MessageBuilder<'a> {
        ctx.start(MessageKind::Error, "Expected `,`", self.span.from)
            .print_line(self.span.from)
            .print_point(self.span)
    }
}

pub struct ExpectedRightSquare {
    span: Span,
}

impl Error for ExpectedRightSquare {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> crate::error::MessageBuilder<'a> {
        ctx.start(MessageKind::Error, "Expected `]`", self.span.from)
            .print_line(self.span.from)
            .print_point(self.span)
    }
}

pub struct SumType {
    left: LeftSquare,
    fields: Vec<SumTypeField>,
    separators: Vec<Comma>,
    right: RightSquare,
}

impl Display for SumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        if !self.fields.is_empty() {
            write!(f, "{}", self.fields[0])?;
            for field in &self.fields[1..] {
                write!(f, ", {}", field)?;
            }
        }
        write!(f, "]")
    }
}

impl Parser for SumType {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let left = tokens.match_next(Token::as_left_square)?;

        if let Some(right) = tokens.match_next(Token::as_right_square) {
            tokens.error(EmptySumType {
                span: Span {
                    from: left.span.from,
                    to: right.span.to,
                },
            });
            return None;
        }

        let field = SumTypeField::parse(tokens)?;
        let mut fields = vec![field];
        let mut separators = Vec::new();

        loop {
            match tokens.first().clone() {
                Token::Ident(ident) => {
                    tokens.error(ExpectedComma { span: ident.span });
                    return None;
                }
                Token::Comma(separator) => {
                    tokens.index += 1;
                    separators.push(separator);
                    let field = SumTypeField::parse(tokens)?;
                    fields.push(field);
                }
                Token::RightSquare(right) => {
                    tokens.index += 1;
                    return Some(Self {
                        left,
                        fields,
                        separators,
                        right,
                    });
                }
                token => {
                    tokens.error(ExpectedRightSquare { span: token.span() });
                    return None;
                }
            }
        }
    }
}

pub struct ExpectedType {
    span: Span,
}

impl Error for ExpectedType {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> crate::error::MessageBuilder<'a> {
        ctx.start(MessageKind::Error, "Expected type", self.span.from)
            .print_line(self.span.from)
            .print_point(self.span)
    }
}

pub struct LabelledProductTypeField(Ident, Box<Type>);

impl Display for LabelledProductTypeField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.0.data, self.1)
    }
}

impl Parser for LabelledProductTypeField {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let Some(ident) = tokens.match_next(Token::as_ident) else {
            tokens.error(ExpectedIdent {
                span: tokens.second().span(),
            });
            return None;
        };
        let Some(ty) = Type::parse(tokens) else {
            tokens.error(ExpectedType {
                span: tokens.second().span(),
            });
            return None;
        };
        Some(Self(ident, Box::new(ty)))
    }
}

pub struct ExpectedRightCurly {
    span: Span,
}

impl Error for ExpectedRightCurly {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> crate::error::MessageBuilder<'a> {
        ctx.start(MessageKind::Error, "Expected `}`", self.span.from)
            .print_line(self.span.from)
            .print_point(self.span)
    }
}

pub struct LabelledProductType {
    left: LeftCurly,
    fields: Vec<LabelledProductTypeField>,
    separators: Vec<Comma>,
    right: RightCurly,
}

impl Display for LabelledProductType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        if !self.fields.is_empty() {
            write!(f, "{}", self.fields[0])?;
            for field in &self.fields[1..] {
                write!(f, ", {}", field)?;
            }
        }
        write!(f, "}}")
    }
}

impl Parser for LabelledProductType {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let left = tokens.match_next(Token::as_left_curly)?;
        let field = LabelledProductTypeField::parse(tokens)?;
        let mut fields = vec![field];
        let mut separators = Vec::new();

        loop {
            match tokens.first().clone() {
                Token::Ident(ident) => {
                    tokens.error(ExpectedComma { span: ident.span });
                    return None;
                }
                Token::Comma(separator) => {
                    tokens.index += 1;
                    separators.push(separator);
                    let field = LabelledProductTypeField::parse(tokens)?;
                    fields.push(field);
                }
                Token::RightCurly(right) => {
                    tokens.index += 1;
                    return Some(Self {
                        left,
                        fields,
                        separators,
                        right,
                    });
                }
                token => {
                    tokens.error(ExpectedRightCurly { span: token.span() });
                    return None;
                }
            }
        }
    }
}

pub struct UnlabelledProductType {
    left: LeftCurly,
    fields: Vec<Type>,
    separators: Vec<Comma>,
    right: RightCurly,
}

impl Display for UnlabelledProductType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        if !self.fields.is_empty() {
            write!(f, "{}", self.fields[0])?;
            for field in &self.fields[1..] {
                write!(f, ", {}", field)?;
            }
        }
        write!(f, "}}")
    }
}

impl Parser for UnlabelledProductType {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let left = tokens.match_next(Token::as_left_curly)?;
        let mut fields = Vec::new();
        let mut separators = Vec::new();
        if let Some(right) = tokens.match_next(Token::as_right_curly) {
            return Some(Self {
                left,
                fields,
                separators,
                right,
            });
        } else {
        }

        fields.push(Type::parse(tokens)?);

        loop {
            match tokens.first().clone() {
                Token::Comma(separator) => {
                    tokens.index += 1;
                    separators.push(separator.clone());
                    let field = Type::parse(tokens)?;
                    fields.push(field);
                }
                Token::RightCurly(right) => {
                    tokens.index += 1;
                    return Some(Self {
                        left,
                        fields,
                        separators,
                        right: right.clone(),
                    });
                }
                _ => {
                    return None; // It's likely a labelled product type
                }
            }
        }
    }
}

pub struct HoleType {
    tick: Tick,
    ident: Ident,
}

impl Display for HoleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.ident.data)
    }
}

impl Parser for HoleType {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let tick = tokens.match_next(Token::as_tick)?;
        let Some(ident) = tokens.match_next(Token::as_ident) else {
            tokens.error(ExpectedIdent {
                span: Span {
                    from: tokens.index,
                    to: tokens.index + 1,
                },
            });
            return None;
        };
        Some(Self { tick, ident })
    }
}

pub enum ProductType {
    Labelled(LabelledProductType),
    Unlabelled(UnlabelledProductType),
}

impl Display for ProductType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Labelled(inner) => write!(f, "{}", inner),
            Self::Unlabelled(inner) => write!(f, "{}", inner),
        }
    }
}

impl Parser for ProductType {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        if let Some(unlabelled) = UnlabelledProductType::parse(tokens) {
            Some(Self::Unlabelled(unlabelled))
        } else if let Some(labelled) = LabelledProductType::parse(tokens) {
            Some(Self::Labelled(labelled))
        } else {
            None
        }
    }
}

pub struct ExpectedRightRound {
    span: Span,
}

impl Error for ExpectedRightRound {
    fn print_error<'a>(&self, ctx: &'a crate::error::Context) -> crate::error::MessageBuilder<'a> {
        ctx.start(MessageKind::Error, "Expected `)`", self.span.from)
            .print_line(self.span.from)
            .print_point(self.span)
    }
}

pub struct ArgsOther {
    left: LeftRound,
    inner: Box<Type>,
    right: RightRound,
}

impl Display for ArgsOther {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.inner)
    }
}

impl Parser for ArgsOther {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let left = tokens.match_next(Token::as_left_round)?;
        let Some(inner) = Type::parse(tokens) else {
            tokens.error(ExpectedType {
                span: Span {
                    from: tokens.index,
                    to: tokens.index + 1,
                },
            });
            return None;
        };
        let Some(right) = tokens.match_next(Token::as_right_round) else {
            tokens.error(ExpectedRightRound {
                span: Span {
                    from: tokens.index,
                    to: tokens.index + 1,
                },
            });
            return None;
        };
        Some(Self {
            left,
            inner: Box::new(inner),
            right,
        })
    }
}

pub enum Args {
    Sum(SumType),
    Product(ProductType),
    Type(crate::lexer::Type),
    Module(crate::lexer::Module),
    Path(Path),
    Hole(HoleType),
    Other(ArgsOther),
}

impl Display for Args {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sum(inner) => write!(f, "{}", inner),
            Self::Product(inner) => write!(f, "{}", inner),
            Self::Type(_) => write!(f, "type"),
            Self::Module(_) => write!(f, "module"),
            Self::Path(inner) => write!(f, "{}", inner),
            Self::Hole(inner) => write!(f, "{}", inner),
            Self::Other(inner) => write!(f, "{}", inner),
        }
    }
}

impl Parser for Args {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        if let Some(sum) = SumType::parse(tokens) {
            Some(Self::Sum(sum))
        } else if let Some(product) = ProductType::parse(tokens) {
            Some(Self::Product(product))
        } else if let Some(ty) = tokens.match_next(Token::as_type) {
            Some(Self::Type(ty))
        } else if let Some(module) = tokens.match_next(Token::as_module) {
            Some(Self::Module(module))
        } else if let Some(path) = Path::parse(tokens) {
            Some(Self::Path(path))
        } else if let Some(hole) = HoleType::parse(tokens) {
            Some(Self::Hole(hole))
        } else if let Some(other) = ArgsOther::parse(tokens) {
            Some(Self::Other(other))
        } else {
            None
        }
    }
}

pub enum Type {
    Function(FunctionType),
    Sum(SumType),
    Product(ProductType),
    Type(crate::lexer::Type),
    Module(crate::lexer::Module),
    Path(Path),
    Hole(HoleType),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(inner) => write!(f, "{}", inner),
            Self::Sum(inner) => write!(f, "{}", inner),
            Self::Product(inner) => write!(f, "{}", inner),
            Self::Type(_) => write!(f, "type"),
            Self::Module(_) => write!(f, "module"),
            Self::Path(inner) => write!(f, "{}", inner),
            Self::Hole(inner) => write!(f, "{}", inner),
        }
    }
}

impl Parser for Type {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        if let Some(function) = FunctionType::parse(tokens) {
            Some(Self::Function(function))
        } else if let Some(sum) = SumType::parse(tokens) {
            Some(Self::Sum(sum))
        } else if let Some(product) = ProductType::parse(tokens) {
            Some(Self::Product(product))
        } else if let Some(ty) = tokens.match_next(Token::as_type) {
            Some(Self::Type(ty))
        } else if let Some(module) = tokens.match_next(Token::as_module) {
            Some(Self::Module(module))
        } else if let Some(path) = Path::parse(tokens) {
            Some(Self::Path(path))
        } else if let Some(hole) = HoleType::parse(tokens) {
            Some(Self::Hole(hole))
        } else {
            None
        }
    }
}

pub fn parse(tokens: Vec<Token>, errors: &mut Vec<Box<dyn Error>>) -> Option<Type> {
    let mut tokens = Tokens::new(tokens, errors);
    Type::parse(&mut tokens)
}
