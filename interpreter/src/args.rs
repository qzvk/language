#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Args {
    Help,
    Version,
    Run { path: Option<String>, verbose: bool },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    NoInputFile,
    TooManyInputFiles,
    UnknownOption(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NoInputFile => write!(f, "error: no input file"),
            Error::TooManyInputFiles => write!(f, "error: too many input files"),
            Error::UnknownOption(option) => write!(f, "error: unknown option {option:?}"),
        }
    }
}

impl std::error::Error for Error {}

pub fn parse(args: impl Iterator<Item = String>) -> Result<Args, Error> {
    let mut input = None;
    let mut accept_options = true;
    let mut verbose = false;

    for arg in args {
        match arg.as_str() {
            "--" => accept_options = false,
            "--help" | "-h" if accept_options => return Ok(Args::Help),
            "--version" | "-V" if accept_options => return Ok(Args::Version),
            "--verbose" | "-v" if accept_options => verbose = true,
            "-" if input.is_some() => return Err(Error::TooManyInputFiles),
            "-" => input = Some(None),
            x if x.starts_with('-') && accept_options => return Err(Error::UnknownOption(arg)),
            _ if input.is_some() => return Err(Error::TooManyInputFiles),
            _ => input = Some(Some(arg)),
        }
    }

    if let Some(filename) = input {
        Ok(Args::Run {
            path: filename,
            verbose,
        })
    } else {
        Err(Error::NoInputFile)
    }
}

#[cfg(test)]
mod tests {
    use super::{parse, Args, Error};

    #[test]
    fn can_parse_empty_input() {
        let input = std::iter::empty();
        let error = parse(input).unwrap_err();
        assert_eq!(Error::NoInputFile, error);
    }

    #[test]
    fn can_parse_just_filename() {
        let input = [String::from("myscript.txt")].into_iter();
        let output = parse(input).unwrap();
        assert_eq!(
            Args::Run {
                path: Some(String::from("myscript.txt")),
                verbose: false,
            },
            output
        );
    }

    #[test]
    fn can_parse_multiple_filename() {
        let input = [String::from("myscript.txt"), String::from("andmore.txt")].into_iter();
        let error = parse(input).unwrap_err();
        assert_eq!(Error::TooManyInputFiles, error);
    }

    #[test]
    fn can_parse_doubledash() {
        let input = [String::from("--"), String::from("--help")].into_iter();
        let output = parse(input).unwrap();
        assert_eq!(
            Args::Run {
                path: Some(String::from("--help")),
                verbose: false,
            },
            output
        );
    }

    #[test]
    fn can_parse_help_flag() {
        let input = [String::from("--help"), String::from("--")].into_iter();
        let output = parse(input).unwrap();
        assert_eq!(Args::Help, output);

        let input = [String::from("-h")].into_iter();
        let output = parse(input).unwrap();
        assert_eq!(Args::Help, output);
    }

    #[test]
    fn can_parse_version_flag() {
        let input = [String::from("--version")].into_iter();
        let output = parse(input).unwrap();
        assert_eq!(Args::Version, output);

        let input = [String::from("-V")].into_iter();
        let output = parse(input).unwrap();
        assert_eq!(Args::Version, output);
    }

    #[test]
    fn can_parse_verbose_flag() {
        let input = [String::from("example.txt"), String::from("--verbose")].into_iter();
        let output = parse(input).unwrap();
        assert_eq!(
            Args::Run {
                path: Some(String::from("example.txt")),
                verbose: true
            },
            output
        );

        let input = [String::from("-v"), String::from("--"), String::from("-v")].into_iter();
        let output = parse(input).unwrap();
        assert_eq!(
            Args::Run {
                path: Some(String::from("-v")),
                verbose: true
            },
            output
        );
    }

    #[test]
    fn can_parse_unknown_flags() {
        let input = [String::from("--unknown")].into_iter();
        let error = parse(input).unwrap_err();
        assert_eq!(Error::UnknownOption(String::from("--unknown")), error);
    }

    #[test]
    fn can_parse_stdin_input() {
        let input = [String::from("-")].into_iter();
        let output = parse(input).unwrap();
        assert_eq!(
            Args::Run {
                path: None,
                verbose: false
            },
            output
        );
    }
}
