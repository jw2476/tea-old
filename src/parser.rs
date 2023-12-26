use std::{fmt::Display, iter::Sum};

use crate::{
    error::{Error, MessageKind},
    lexer::{
        Arrow, Base, Colon, Comma, Dot, DoubleColon, Equals, Ident, LeftCurly, LeftRound,
        LeftSquare, Let, Literal, LiteralKind, Pipe, RightCurly, RightRound, RightSquare,
        Semicolon, Span, Tick, Token, Underscore,
    },
};

pub struct Tokens {
    tokens: Vec<Token>,
    index: usize,
}

impl Tokens {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
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

            let part = tokens.match_next(Token::as_ident)?;
            path.parts.push(part)
        }
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
        let return_type = Type::parse(tokens)?;
        Some(Self {
            args,
            arrow,
            return_type: Box::new(return_type),
        })
    }
}

pub struct SumTypeField(Ident, Option<Box<Type>>);

impl Parser for SumTypeField {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let ident = tokens.match_next(Token::as_ident)?;
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
        }
        Ok(())
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

        let field = SumTypeField::parse(tokens)?;
        let mut fields = vec![field];
        let mut separators = Vec::new();

        loop {
            match tokens.first().clone() {
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
                    return None;
                }
            }
        }
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
        let ident = tokens.match_next(Token::as_ident)?;
        let ty = Type::parse(tokens)?;
        Some(Self(ident, Box::new(ty)))
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
    ident: Option<Ident>,
}

impl Display for HoleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ident {
            Some(ident) => write!(f, "'{}", ident.data),
            None => write!(f, "'_"),
        }
    }
}

impl Parser for HoleType {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let tick = tokens.match_next(Token::as_tick)?;
        if let Some(ident) = tokens.match_next(Token::as_ident) {
            Some(Self {
                tick,
                ident: Some(ident),
            })
        } else if tokens.match_next(Token::as_underscore).is_some() {
            Some(Self { tick, ident: None })
        } else {
            None
        }
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
        let inner = Type::parse(tokens)?;
        let right = tokens.match_next(Token::as_right_round)?;
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
    Call(Call),
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
            Self::Call(inner) => write!(f, "{}", inner),
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
        } else if let Some(call) = Call::parse(tokens) {
            Some(Self::Call(call))
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
    Call(Call),
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
            Self::Call(inner) => write!(f, "{}", inner),
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
        } else if let Some(call) = Call::parse(tokens) {
            Some(Self::Call(call))
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

pub struct SumDecl {
    variant: Path,
    value: Option<Expression>,
}

impl Display for SumDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.variant)?;
        if let Some(value) = &self.value {
            write!(f, " {}", value)?;
        }
        Ok(())
    }
}

impl Parser for SumDecl {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let variant = Path::parse(tokens)?;
        let value = Expression::parse(tokens);
        Some(Self { variant, value })
    }
}

pub struct LabelledProductDeclField {
    field: Ident,
    value: Expression,
}

impl Display for LabelledProductDeclField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.field.data, self.value)
    }
}

impl Parser for LabelledProductDeclField {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let field = tokens.match_next(Token::as_ident)?.clone();
        let value = Expression::parse(tokens)?;
        Some(Self { field, value })
    }
}

pub struct LabelledProductDecl {
    ty: Option<Type>,
    left: LeftCurly,
    right: RightCurly,
    fields: Vec<LabelledProductDeclField>,
    separators: Vec<Comma>,
}

impl Display for LabelledProductDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            Some(ty) => write!(
                f,
                "{}{{{}}}",
                ty,
                self.fields
                    .iter()
                    .map(|field| format!("{}", field))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            None => write!(
                f,
                "_{{{}}}",
                self.fields
                    .iter()
                    .map(|field| format!("{}", field))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Parser for LabelledProductDecl {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let ty = if let Some(_) = tokens.match_next(Token::as_underscore) {
            None
        } else {
            Some(Type::parse(tokens)?)
        };
        let left = tokens.match_next(Token::as_left_curly)?;
        let field = LabelledProductDeclField::parse(tokens)?;
        let mut fields = vec![field];
        let mut separators = Vec::new();

        loop {
            match tokens.first().clone() {
                Token::Comma(separator) => {
                    tokens.index += 1;
                    separators.push(separator);
                    let field = LabelledProductDeclField::parse(tokens)?;
                    fields.push(field);
                }
                Token::RightCurly(right) => {
                    tokens.index += 1;
                    return Some(Self {
                        ty,
                        left,
                        fields,
                        separators,
                        right,
                    });
                }
                token => {
                    return None;
                }
            }
        }
    }
}

pub struct UnlabelledProductDecl {
    ty: Option<Type>,
    left: LeftCurly,
    right: RightCurly,
    fields: Vec<Expression>,
    separators: Vec<Comma>,
}

impl Display for UnlabelledProductDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            Some(ty) => write!(
                f,
                "{}{{{}}}",
                ty,
                self.fields
                    .iter()
                    .map(|field| format!("{}", field))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            None => write!(
                f,
                "_{{{}}}",
                self.fields
                    .iter()
                    .map(|field| format!("{}", field))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Parser for UnlabelledProductDecl {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let ty = if let Some(_) = tokens.match_next(Token::as_underscore) {
            None
        } else {
            Some(Type::parse(tokens)?)
        };
        let left = tokens.match_next(Token::as_left_curly)?;
        let mut fields = Vec::new();
        let mut separators = Vec::new();
        if let Some(right) = tokens.match_next(Token::as_right_curly) {
            return Some(Self {
                ty,
                left,
                fields,
                separators,
                right,
            });
        }

        fields.push(Expression::parse(tokens)?);

        loop {
            match tokens.first().clone() {
                Token::Comma(separator) => {
                    tokens.index += 1;
                    separators.push(separator.clone());
                    let field = Expression::parse(tokens)?;
                    fields.push(field);
                }
                Token::RightCurly(right) => {
                    tokens.index += 1;
                    return Some(Self {
                        ty,
                        left,
                        fields,
                        separators,
                        right,
                    });
                }
                _ => {
                    return None; // It's likely a labelled product decl
                }
            }
        }
    }
}

pub enum ProductDecl {
    Unlabelled(UnlabelledProductDecl),
    Labelled(LabelledProductDecl),
}

impl Display for ProductDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unlabelled(unlabelled) => write!(f, "{}", unlabelled),
            Self::Labelled(labelled) => write!(f, "{}", labelled),
        }
    }
}

impl Parser for ProductDecl {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        if let Some(unlabelled) = UnlabelledProductDecl::parse(tokens) {
            Some(Self::Unlabelled(unlabelled))
        } else if let Some(labelled) = LabelledProductDecl::parse(tokens) {
            Some(Self::Labelled(labelled))
        } else {
            None
        }
    }
}

pub struct Call {
    path: Path,
    left: LeftRound,
    arguments: Vec<Expression>,
    separators: Vec<Comma>,
    right: RightRound,
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.path,
            self.arguments
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl Parser for Call {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let path = Path::parse(tokens)?;
        let left = tokens.next().as_left_round()?.clone();
        let mut arguments = Vec::new();
        let mut separators = Vec::new();
        if let Some(right) = tokens.match_next(Token::as_right_round) {
            return Some(Self {
                path,
                left,
                arguments,
                separators,
                right,
            });
        }

        arguments.push(Expression::parse(tokens)?);

        loop {
            if let Some(separator) = tokens.match_next(Token::as_comma) {
                separators.push(separator.clone());
                arguments.push(Expression::parse(tokens)?);
            } else if let Some(right) = tokens.match_next(Token::as_right_round) {
                return Some(Self {
                    path,
                    left,
                    arguments,
                    separators,
                    right: right.clone(),
                });
            } else {
                return None;
            }
        }
    }
}

pub enum ExpressionPart {
    Literal(Literal),
    Function(Function),
    ProductDecl(ProductDecl),
    Call(Call),
    Type(Type),
}

impl Display for ExpressionPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => match literal.kind {
                LiteralKind::String => write!(f, "\"{}\"", literal.data),
                LiteralKind::Integer | LiteralKind::Decimal => write!(f, "{}", literal.data),
            },
            Self::Function(inner) => write!(f, "{}", inner),
            Self::ProductDecl(product_decl) => write!(f, "{}", product_decl),
            Self::Call(call) => write!(f, "{}", call),
            Self::Type(ty) => write!(f, "{}", ty),
        }
    }
}

impl Parser for ExpressionPart {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        if let Some(literal) = tokens.match_next(Token::as_literal) {
            Some(Self::Literal(literal))
        } else if let Some(function) = Function::parse(tokens) {
            Some(Self::Function(function))
        } else if let Some(product_decl) = ProductDecl::parse(tokens) {
            Some(Self::ProductDecl(product_decl))
        } else if let Some(call) = Call::parse(tokens) {
            Some(Self::Call(call))
        } else if let Some(ty) = Type::parse(tokens) {
            Some(Self::Type(ty))
        } else {
            None
        }
    }
}

pub struct Expression {
    parts: Vec<ExpressionPart>,
    separators: Vec<Dot>,
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.parts
                .iter()
                .map(|part| format!("{}", part))
                .collect::<Vec<String>>()
                .join(".")
        )
    }
}

impl Parser for Expression {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let mut parts = vec![ExpressionPart::parse(tokens)?];
        let mut separators = Vec::new();
        loop {
            if let Some(separator) = tokens.match_next(Token::as_dot) {
                separators.push(separator);
                parts.push(ExpressionPart::parse(tokens)?);
            } else {
                return Some(Self { parts, separators });
            }
        }
    }
}

pub struct UnlabelledDestructure {
    ty: Option<Type>,
    left: LeftCurly,
    fields: Vec<Assignment>,
    separators: Vec<Comma>,
    right: RightCurly,
}

impl Display for UnlabelledDestructure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            Some(ty) => write!(
                f,
                "{}{{{}}}",
                ty,
                self.fields
                    .iter()
                    .map(|field| format!("{}", field))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            None => write!(
                f,
                "_{{{}}}",
                self.fields
                    .iter()
                    .map(|field| format!("{}", field))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Parser for UnlabelledDestructure {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let ty = if let Some(_) = tokens.match_next(Token::as_underscore) {
            None
        } else {
            Some(Type::parse(tokens)?)
        };
        let left = tokens.match_next(Token::as_left_curly)?;
        let mut fields = vec![Assignment::parse(tokens)?];
        let mut separators = Vec::new();

        loop {
            if let Some(separator) = tokens.match_next(Token::as_comma) {
                separators.push(separator);
                fields.push(Assignment::parse(tokens)?);
            } else if let Some(right) = tokens.match_next(Token::as_right_curly) {
                return Some(Self {
                    ty,
                    left,
                    fields,
                    separators,
                    right,
                });
            } else {
                return None;
            }
        }
    }
}

pub struct LabelledDestructure {
    ty: Option<Type>,
    left: LeftCurly,
    fields: Vec<(Ident, Assignment)>,
    separators: Vec<Comma>,
    right: RightCurly,
}

impl Display for LabelledDestructure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            Some(ty) => write!(
                f,
                "{}{{{}}}",
                ty,
                self.fields
                    .iter()
                    .map(|(ident, assignment)| format!("{} {}", ident.data, assignment))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            None => write!(
                f,
                "_{{{}}}",
                self.fields
                    .iter()
                    .map(|(ident, assignment)| format!("{} {}", ident.data, assignment))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Parser for LabelledDestructure {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let ty = if let Some(_) = tokens.match_next(Token::as_underscore) {
            None
        } else {
            Some(Type::parse(tokens)?)
        };
        let left = tokens.match_next(Token::as_left_curly)?;
        let mut fields = vec![(
            tokens.match_next(Token::as_ident)?,
            Assignment::parse(tokens)?,
        )];
        let mut separators = Vec::new();
        loop {
            if let Some(separator) = tokens.match_next(Token::as_comma) {
                separators.push(separator);
                fields.push((
                    tokens.match_next(Token::as_ident)?,
                    Assignment::parse(tokens)?,
                ));
            } else if let Some(right) = tokens.match_next(Token::as_right_curly) {
                return Some(Self {
                    ty,
                    left,
                    fields,
                    separators,
                    right,
                });
            } else {
                return None;
            }
        }
    }
}

pub struct SumMatch {
    path: Path,
    assignment: Box<Assignment>,
}

impl Display for SumMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.path, self.assignment)
    }
}

impl Parser for SumMatch {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let path = Path::parse(tokens)?;
        let assignment = Box::new(Assignment::parse(tokens)?);
        Some(Self { path, assignment })
    }
}

pub struct TypedIdent {
    ident: Ident,
    colon: Colon,
    ty: Type,
}

impl Display for TypedIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.ident.data, self.ty)
    }
}

impl Parser for TypedIdent {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let ident = tokens.match_next(Token::as_ident)?;
        let colon = tokens.match_next(Token::as_colon)?;
        let ty = Type::parse(tokens)?;
        Some(Self { ident, colon, ty })
    }
}

pub enum Assignment {
    SumMatch(SumMatch),
    TypedIdent(TypedIdent),
    Ident(Ident),
    UnlabelledDestructure(UnlabelledDestructure),
    LabelledDestructure(LabelledDestructure),
    Underscore(Underscore),
}

impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SumMatch(inner) => write!(f, "{}", inner),
            Self::TypedIdent(inner) => write!(f, "{}", inner),
            Self::Ident(inner) => write!(f, "{}", inner.data),
            Self::UnlabelledDestructure(inner) => write!(f, "{}", inner),
            Self::LabelledDestructure(inner) => write!(f, "{}", inner),
            Self::Underscore(inner) => write!(f, "_"),
        }
    }
}

impl Parser for Assignment {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        if let Some(sum_match) = SumMatch::parse(tokens) {
            Some(Self::SumMatch(sum_match))
        } else if let Some(typed_ident) = TypedIdent::parse(tokens) {
            Some(Self::TypedIdent(typed_ident))
        } else if let Some(ident) = tokens.match_next(Token::as_ident) {
            Some(Self::Ident(ident))
        } else if let Some(unlabelled) = UnlabelledDestructure::parse(tokens) {
            Some(Self::UnlabelledDestructure(unlabelled))
        } else if let Some(labelled) = LabelledDestructure::parse(tokens) {
            Some(Self::LabelledDestructure(labelled))
        } else if let Some(underscore) = tokens.match_next(Token::as_underscore) {
            Some(Self::Underscore(underscore))
        } else {
            None
        }
    }
}

pub struct MultilineBody {
    left: LeftCurly,
    statements: Vec<Statement>,
    expression: Option<Expression>,
    right: RightCurly,
}

impl Display for MultilineBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expression {
            Some(expression) => write!(
                f,
                "{{\n{}{}\n}}",
                self.statements
                    .iter()
                    .map(|statement| String::from("\t") + &format!("{}", statement) + "\n")
                    .collect::<Vec<String>>()
                    .join(""),
                format!("\t{}", expression)
            ),
            None => write!(
                f,
                "{{\n{}}}",
                self.statements
                    .iter()
                    .map(|statement| String::from("\t") + &format!("{}", statement) + "\n")
                    .collect::<Vec<String>>()
                    .join("")
            ),
        }
    }
}

impl Parser for MultilineBody {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let left = tokens.match_next(Token::as_left_curly)?;
        let mut statements = Vec::new();
        loop {
            let Some(statement) = Statement::parse(tokens) else {
                break;
            };
            statements.push(statement)
        }

        if let Some(right) = tokens.match_next(Token::as_right_curly) {
            Some(Self {
                left,
                statements,
                expression: None,
                right,
            })
        } else {
            let expression = Expression::parse(tokens)?;
            let right = tokens.match_next(Token::as_right_curly)?;
            Some(Self {
                left,
                statements,
                expression: Some(expression),
                right,
            })
        }
    }
}

pub enum Body {
    Inline(Expression),
    Multiline(MultilineBody),
}

impl Display for Body {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Inline(inner) => write!(f, "{}", inner),
            Self::Multiline(inner) => write!(f, "{}", inner),
        }
    }
}

impl Parser for Body {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        if let Some(expression) = Expression::parse(tokens) {
            Some(Self::Inline(expression))
        } else if let Some(multiline) = MultilineBody::parse(tokens) {
            Some(Self::Multiline(multiline))
        } else {
            None
        }
    }
}

pub struct Function {
    left: Pipe,
    assignment: Option<Assignment>,
    right: Pipe,
    body: Body,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.assignment {
            Some(assignment) => write!(f, "|{}| {}", assignment, self.body),
            None => write!(f, "|| {}", self.body),
        }
    }
}

impl Parser for Function {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let left = tokens.match_next(Token::as_pipe)?;
        let (assignment, right) = if let Some(right) = tokens.match_next(Token::as_pipe) {
            (None, right)
        } else {
            let assignment = Assignment::parse(tokens)?;
            let right = tokens.match_next(Token::as_pipe)?;
            (Some(assignment), right)
        };
        let body = Body::parse(tokens)?;
        Some(Self {
            left,
            assignment,
            right,
            body,
        })
    }
}

pub struct BaseDecl {
    lt: Let,
    base: Base,
    ident: Ident,
    colon: Colon,
    ty: Type,
    semicolon: Semicolon,
}

impl Display for BaseDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let base {}: {};", self.ident.data, self.ty)
    }
}

impl Parser for BaseDecl {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let lt = tokens.match_next(Token::as_let)?;
        let base = tokens.match_next(Token::as_base)?;
        let ident = tokens.match_next(Token::as_ident)?;
        let colon = tokens.match_next(Token::as_colon)?;
        let ty = Type::parse(tokens)?;
        let semicolon = tokens.match_next(Token::as_semicolon)?;
        Some(Self {
            lt,
            base,
            ident,
            colon,
            ty,
            semicolon,
        })
    }
}

pub struct VariableDecl {
    lt: Let,
    assignment: Assignment,
    equals: Equals,
    expression: Expression,
    semicolon: Semicolon,
}

impl Display for VariableDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.assignment, self.expression)
    }
}

impl Parser for VariableDecl {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let lt = tokens.match_next(Token::as_let)?;
        let assignment = Assignment::parse(tokens)?;
        let equals = tokens.match_next(Token::as_equals)?;
        let expression = Expression::parse(tokens)?;
        let semicolon = tokens.match_next(Token::as_semicolon)?;
        Some(Self {
            lt,
            assignment,
            equals,
            expression,
            semicolon,
        })
    }
}

pub struct ModuleDecl {
    lt: Let,
    ident: Ident,
    colon: Colon,
    m: crate::lexer::Module,
    equals: Equals,
    left: LeftCurly,
    module: Module,
    right: RightCurly,
    semicolon: Semicolon,
}

impl Display for ModuleDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "let {}: module = {{\n{}\n}}",
            self.ident.data,
            format!("{}", self.module)
                .split('\n')
                .map(|line| String::from("\t") + line)
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl Parser for ModuleDecl {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let lt = tokens.match_next(Token::as_let)?;
        let ident = tokens.match_next(Token::as_ident)?;
        let colon = tokens.match_next(Token::as_colon)?;
        let m = tokens.match_next(Token::as_module)?;
        let equals = tokens.match_next(Token::as_equals)?;
        let left = tokens.match_next(Token::as_left_curly)?;
        let module = Module::parse(tokens)?;
        let right = tokens.match_next(Token::as_right_curly)?;
        let semicolon = tokens.match_next(Token::as_semicolon)?;
        Some(Self {
            lt,
            ident,
            colon,
            m,
            equals,
            left,
            module,
            right,
            semicolon,
        })
    }
}

pub struct Reassignment {
    left: Expression,
    equals: Equals,
    right: Expression,
    semicolon: Semicolon,
}

impl Display for Reassignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.left, self.right)
    }
}

impl Parser for Reassignment {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let left = Expression::parse(tokens)?;
        let equals = tokens.match_next(Token::as_equals)?;
        let right = Expression::parse(tokens)?;
        let semicolon = tokens.match_next(Token::as_semicolon)?;
        Some(Self {
            left,
            equals,
            right,
            semicolon,
        })
    }
}

pub enum Statement {
    ModuleDecl(ModuleDecl),
    BaseDecl(BaseDecl),
    VariableDecl(VariableDecl),
    Reassignment(Reassignment),
    Expression((Expression, Semicolon)),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ModuleDecl(inner) => write!(f, "{}", inner),
            Self::BaseDecl(inner) => write!(f, "{}", inner),
            Self::VariableDecl(inner) => write!(f, "{}", inner),
            Self::Reassignment(inner) => write!(f, "{}", inner),
            Self::Expression((expression, _)) => write!(f, "{};", expression),
        }
    }
}

impl Parser for Statement {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        if let Some(module_decl) = ModuleDecl::parse(tokens) {
            Some(Self::ModuleDecl(module_decl))
        } else if let Some(base_decl) = BaseDecl::parse(tokens) {
            Some(Self::BaseDecl(base_decl))
        } else if let Some(variable_decl) = VariableDecl::parse(tokens) {
            Some(Self::VariableDecl(variable_decl))
        } else if let Some(reassignment) = Reassignment::parse(tokens) {
            Some(Self::Reassignment(reassignment))
        } else {
            let expression = Expression::parse(tokens)?;
            let semicolon = tokens.match_next(Token::as_semicolon)?;
            Some(Self::Expression((expression, semicolon)))
        }
    }
}

pub struct Module {
    declarations: Vec<Statement>,
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.declarations
                .iter()
                .map(|declaration| format!("{}", declaration))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl Parser for Module {
    fn try_parse(tokens: &mut Tokens) -> Option<Self> {
        let mut declarations = Vec::new();
        loop {
            let Some(declaration) = Statement::parse(tokens) else {
                return Some(Self { declarations });
            };
            declarations.push(declaration)
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Option<Module> {
    let mut tokens = Tokens::new(tokens);
    Module::parse(&mut tokens)
}

#[cfg(test)]
mod tests {
    use crate::{file::FileRegistry, lexer::tokenize};

    use super::*;

    fn ignore_whitespace(input: &str) -> String {
        input
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect::<String>()
    }

    #[test]
    pub fn test_hello_world() {
        let mut registry = FileRegistry::new();
        let file = registry.open("tests/hello_world.tea").unwrap();
        let mut errors = Vec::new();
        let tokens = tokenize(file, &mut errors);
        assert!(errors.is_empty());
        assert_eq!(
            ignore_whitespace(&file.content),
            ignore_whitespace(&format!("{}", parse(tokens).unwrap()))
        );
    }
}
