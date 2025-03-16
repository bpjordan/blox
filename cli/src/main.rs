use std::{error::Error, io::Write};

use blox_parser::{lexer::Lexer, parser::Parser};
use clap::{Parser as ClapParser, Subcommand};
use miette::GraphicalReportHandler;

#[derive(Debug, ClapParser)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Lex { src: String },
    Ast { src: String },
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Box::leak(Box::new(Args::parse()));

    let mut stdout = std::io::stdout().lock();

    match &args.command {
        Command::Lex { src } => disp_tokens(&src, &mut stdout),
        Command::Ast { src } => disp_ast(&src, &mut stdout),
    }
}

fn disp_tokens(src: &str, f: &mut impl Write) -> Result<(), Box<dyn Error>> {
    let mut lexer = Lexer::for_str(src);
    let reporter = GraphicalReportHandler::new();

    while let Some(r) = lexer.next() {
        match r {
            Ok(t) => writeln!(f, "{:?}", t.kind())?,
            Err(e) => {
                let mut buf = String::new();
                reporter.render_report(&mut buf, &e)?;
                writeln!(f, "{buf}")?;
            }
        }
    }

    Ok(())
}

fn disp_ast<'a>(src: &'a str, f: &mut impl Write) -> Result<(), Box<dyn Error + 'a>> {
    let mut parser = Parser::for_str(src);

    let expr = parser.parse()?;

    writeln!(f, "{expr}")?;

    Ok(())
}
