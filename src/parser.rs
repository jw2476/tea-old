use pest::{iterators::Pairs, Parser as PestParser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "tea.pest"]
struct Parser;

pub fn parse(input: &str) -> Result<Pairs<'_, Rule>, pest::error::Error<Rule>> {
    Parser::parse(Rule::File, input)
}
