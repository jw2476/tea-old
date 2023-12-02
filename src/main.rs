mod parser;
use clap::Parser;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Parser)]
struct Build {
    file: String
}

#[derive(clap::Subcommand)]
enum Commands {
    Build(Build),
}

fn build(build: Build) -> anyhow::Result<()> {
   let file = std::fs::read(build.file)?;
   let input = String::from_utf8(file)?;
   let parsed = parser::parse(&input)?;
   println!("{:?}", parsed);

   Ok(())
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Build(b) => build(b)? 
    };

    Ok(())
}
