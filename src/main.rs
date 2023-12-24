mod error;
mod file;
mod lexer;
mod parser;
use clap::Parser;

use crate::file::FileRegistry;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Parser)]
struct Build {
    file: String,
}

#[derive(clap::Subcommand)]
enum Commands {
    Build(Build),
}

fn build(build: Build) -> anyhow::Result<()> {
    let mut file_registry = FileRegistry::new();
    let file = file_registry.open(build.file)?;
    let mut errors = Vec::new();
    let tokens: Vec<lexer::Token> = lexer::tokenize(file, &mut errors);
    if !errors.is_empty() {
        let ctx = error::Context::new(file);
        ctx.print_errors(&errors);
        return Ok(());
    }
    let ast = parser::parse(tokens, &mut errors);
    if !errors.is_empty() {
        let ctx = error::Context::new(file);
        ctx.print_errors(&errors);
        return Ok(());
    }
    if let Some(ast) = ast {
        println!("{ast}")
    } else {
        panic!("Failed to parse");
    }

    Ok(())
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Build(b) => build(b)?,
    };

    Ok(())
}
