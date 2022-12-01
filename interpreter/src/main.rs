#![forbid(unsafe_code)]

mod args;

use args::Args;
use std::process::ExitCode;

#[derive(Debug)]
enum Error {
    Args(args::Error),
    Driver(driver::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Args(error) => error.fmt(f),
            Error::Driver(error) => error.fmt(f),
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
    println!(env!("CARGO_PKG_DESCRIPTION"));
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
    println!(concat!(
        env!("CARGO_PKG_NAME"),
        " ",
        env!("CARGO_PKG_VERSION")
    ));
    Ok(())
}

fn run() -> Result<(), Error> {
    let input_args = std::env::args().skip(1);
    let args = args::parse(input_args)?;

    match args {
        Args::Help => help(),

        Args::Version => version(),

        Args::Run {
            path: None,
            verbose,
        } => driver::run_stdin(verbose).map_err(Error::Driver),

        Args::Run {
            path: Some(path),
            verbose,
        } => driver::run_file(path, verbose).map_err(Error::Driver),
    }
}

fn main() -> ExitCode {
    if let Err(error) = run() {
        eprintln!("{error}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
