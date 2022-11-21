#![forbid(unsafe_code)]

mod args;

use args::Args;
use std::{
    io::{stdin, Read},
    process::ExitCode,
};

#[derive(Debug)]
enum Error {
    Args(args::Error),
    BadFilename(std::io::Error),
    BadInput(usize),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Args(error) => error.fmt(f),
            Error::BadFilename(error) => write!(f, "failed to read input: {error}"),
            Error::BadInput(1) => write!(f, "1 error generated."),
            Error::BadInput(count) => write!(f, "{count} errors generated."),
        }
    }
}

impl std::error::Error for Error {}

impl From<args::Error> for Error {
    fn from(error: args::Error) -> Self {
        Error::Args(error)
    }
}

fn help() -> Result<(), Error> {
    version()?;
    println!("{}", env!("CARGO_PKG_DESCRIPTION"));
    println!();
    println!("USAGE");
    println!("    interpreter [OPTIONS] <FILENAME>");
    println!();
    println!("OPTIONS");
    println!("    -h --help       Display this help text");
    println!("    -V --version    Print version info and exit");
    println!("    -v --verbose    Display grammar information");
    Ok(())
}

fn version() -> Result<(), Error> {
    println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
    Ok(())
}

fn read_input(filename: Option<String>) -> Result<String, Error> {
    if let Some(filename) = filename {
        std::fs::read_to_string(filename).map_err(Error::BadFilename)
    } else {
        let mut input = String::new();
        stdin()
            .read_to_string(&mut input)
            .map_err(Error::BadFilename)?;

        Ok(input)
    }
}

fn execute(filename: Option<String>, verbose: bool) -> Result<(), Error> {
    let table = language::parse_table(verbose);

    let input = read_input(filename)?;

    let mut lex_errors = Vec::new();
    let tokens = lexer::lex(&input).filter_map(|(result, info)| match result {
        Ok(token) => Some((token, info)),
        Err(error) => {
            lex_errors.push((error, info));
            None
        }
    });

    let parse_result = table.parse(tokens, language::reduce);

    if !lex_errors.is_empty() {
        for (error, info) in &lex_errors {
            println!(
                "{}:{}: error: {}",
                info.line() + 1,
                info.column() + 1,
                error
            );
        }

        return Err(Error::BadInput(lex_errors.len()));
    }

    let parse_tree = match parse_result {
        Ok(tree) => tree,

        Err((error, None)) => {
            println!("EOF: error: {}", error);
            return Err(Error::BadInput(1));
        }

        Err((error, Some(info))) => {
            println!(
                "{}:{}: error: {}",
                info.line() + 1,
                info.column() + 1,
                error
            );
            return Err(Error::BadInput(1));
        }
    };

    println!("{parse_tree:#?}");

    Ok(())
}

fn run() -> Result<(), Error> {
    let input_args = std::env::args().skip(1);
    let args = args::parse(input_args)?;

    match args {
        Args::Help => help(),
        Args::Version => version(),
        Args::Run { filename, verbose } => execute(filename, verbose),
    }
}

fn main() -> ExitCode {
    if let Err(error) = run() {
        println!("Error: {error}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
