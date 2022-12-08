#![forbid(unsafe_code)]

mod args;

use args::Args;
use std::process::ExitCode;

fn help() -> ExitCode {
    version();
    println!(env!("CARGO_PKG_DESCRIPTION"));
    println!();
    println!("USAGE");
    println!("    interpreter [OPTIONS] <FILENAME>");
    println!();
    println!("OPTIONS");
    println!("    -h --help       Display this help text");
    println!("    -V --version    Print version info and exit");
    println!("    -v --verbose    Display grammar information");
    ExitCode::SUCCESS
}

fn version() -> ExitCode {
    println!(concat!(
        env!("CARGO_PKG_NAME"),
        " ",
        env!("CARGO_PKG_VERSION")
    ));
    ExitCode::SUCCESS
}

fn main() -> ExitCode {
    let input_args = std::env::args().skip(1);
    let args = args::parse(input_args);

    match args {
        Ok(Args::Help) => help(),

        Ok(Args::Version) => version(),

        Ok(Args::Run {
            path: None,
            verbose,
        }) => driver::run_stdin(verbose),

        Ok(Args::Run {
            path: Some(path),
            verbose,
        }) => driver::run_file(path, verbose),

        Err(error) => {
            eprintln!("{error}");
            ExitCode::FAILURE
        }
    }
}
