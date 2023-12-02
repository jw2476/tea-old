use std::iter::repeat;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    from: usize,
    to: usize,
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

pub struct Cursor {
    index: usize,
    chars: Vec<char>,
}

impl Cursor {
    pub fn new(input: &str) -> Self {
        Self {
            index: 0,
            chars: input.chars().collect(),
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
        let offset = index - self.chars[0..index].iter().enumerate().filter(|(i, c)| **c == '\n').fold(0, |_, (i, _)| i);
        (line + 1, offset + 1)
    }

    fn error(&self, span: Span, message: &str) {
        let (line, offset) = self.line_offset(span.from);
        println!("error: {message}");
        println!("{line} | {}", self.chars.split(|c| *c == '\n').nth(line - 1).unwrap().iter().cloned().collect::<String>());
        let first_padding = repeat(' ').take((line as f32).log(10.0).floor() as usize + 1).collect::<String>();
        let second_padding = repeat(' ').take(offset).collect::<String>();
        let pointers = repeat('^').take(span.to - span.from).collect::<String>();
        println!("{} | {}{}", first_padding, second_padding, pointers);
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
                    self.error(span(self), "Missing arrow head: >");
                    return self.get();
                }
            }
            '"' => {
                let mut data = String::new();
                while !self.match_next('"') {
                    let Some(c) = self.next() else {
                        self.error(
                            Span {
                                from: self.index,
                                to: self.index + 1,
                            },
                            "Missing closing \"",
                        );
                        return self.get();
                    };

                    data.push(c);
                }

                Token::Literal(Literal { span: span(self), kind: LiteralKind::String, data })
            },
            c if c.is_digit(10) => {
                let mut data = String::new();
                data.push(c);

                let mut decimal = false;

                while self.first()?.is_digit(10) || self.first()? == '.' {
                    let c = self.next()?;
                    if c == '.' {
                        if decimal { self.error(Span { from: self.index, to: self.index }, "More than one decimal point in number"); return self.get() }
                        decimal = true
                    }
                    data.push(c);
                }

                Token::Literal(Literal {
                    span: span(self),
                    kind: if decimal { LiteralKind::Decimal } else { LiteralKind::Integer },
                    data
                })
            },
            c if c.is_alphabetic() => {
                let mut data = String::new();
                data.push(c);

                while self.first()?.is_alphanumeric() {
                    data.push(self.next()?)
                }

                Token::Ident(Ident { span: span(self), data })
            }
            ' ' | '\n' => return self.get(), 
            c => { self.error(span(self), &format!("Unknown character: {c}")); return self.get(); }
        };

        Some(token)
    }

    pub fn iter(mut self) -> impl Iterator<Item = Token> {
        std::iter::from_fn(move || self.get())
    }
}

pub fn tokenize(input: &str) -> impl Iterator<Item = Token> {
    let cursor = Cursor::new(input);
    cursor.iter()
}
