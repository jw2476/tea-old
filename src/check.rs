// Future parse pass just to check for errors

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
