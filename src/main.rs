use clap::clap_app;
use std::fmt::{self, Display, Formatter};
use std::fs::File;
use std::io::{self, Read};
use word_search::{Board, Direction, WordSearch};

fn main() {
    std::process::exit({
        let args = clap_app!(word_search =>
	    (version: "1.0.0") // TODO: use crate version instead
	    (author: "Eldred Habert <me@eldred.fr>")
	    (about: "Solves word search puzzles")
	    (@arg allow_backwards: -b --backwards "Enables matching words in \"backwards\" directions (upwards, leftwards, etc.)")
	    (@arg paths: ... "Paths to files to process, \"-\" meaning to read standard input. If unspecfiied, standard input will be read.")
	    )
	    .get_matches();

        let allow_backwards = args.is_present("allow_backwards");
        let paths = args.values_of("paths").map_or_else(
            || PathIterator::None(["-"].iter().map(|rrs| *rrs)),
            |iter| PathIterator::Some(iter),
        );

        let mut exit_status = 0;
        for path in paths {
            let name = match path {
                "-" => "<stdin>",
                path => path,
            };

            match process_file(path, allow_backwards) {
                Err(err) => {
                    exit_status = 1;
                    use ProcessingError::*;
                    match err {
                        Io(err) => eprintln!("Error reading {}: {}", name, err),
                        Parse(err) => eprintln!(
                            "Error parsing {} on line {}: {}",
                            name, err.line_no, err.kind
                        ),
                        WordSearch(errs) => {
                            eprintln!("{} errors searching {} for words:", errs.len(), name);
                            for err in errs {
                                eprintln!("- {}", err);
                            }
                        }
                    }
                }
                Ok((board, words, matches)) => {
                    //
                    todo!()
                }
            }
        }

        exit_status
    });
}

#[derive(Debug)]
enum PathIterator<T, U> {
    Some(T),
    None(U),
}

impl<'a, T: Iterator<Item = &'a str>, U: Iterator<Item = &'a str>> Iterator for PathIterator<T, U> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Some(t) => t.next(),
            Self::None(u) => u.next(),
        }
    }
}

fn process_file(
    path: &str,
    allow_backwards: bool,
) -> Result<(Board, Vec<String>, WordSearch), ProcessingError> {
    let (board, words) = match path {
        "-" => parse_file(&mut io::stdin())?,
        path => parse_file(&mut File::open(path)?)?,
    };
    let ret = (
        board,
        words,
        WordSearch::new(&board, &words, allow_backwards)?,
    );
    Ok(ret)
}

// ===== File input =====

#[derive(Debug)]
enum ProcessingError {
    Io(io::Error),
    Parse(FileParseError),
    WordSearch(Vec<io::Error>),
}

impl From<io::Error> for ProcessingError {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<FileParseError> for ProcessingError {
    fn from(err: FileParseError) -> Self {
        Self::Parse(err)
    }
}

impl From<Vec<io::Error>> for ProcessingError {
    fn from(errs: Vec<io::Error>) -> Self {
        Self::WordSearch(errs)
    }
}

#[derive(Debug)]
struct FileParseError {
    line_no: usize,
    kind: FileParseErrorKind,
}

#[derive(Debug)]
enum FileParseErrorKind {
    NoBoard,
    NoWordList,
}

impl Display for FileParseErrorKind {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        use FileParseErrorKind::*;
        match self {
            NoBoard => write!(fmt, "No board in file"),
            NoWordList => write!(fmt, "No word list after board"),
        }
    }
}

/// File format:
/// ```
/// T h e
/// b o a
/// r d .
/// <blank line(s)>
/// word1
/// word2
/// ...
/// ```
/// Blank lines are allowed in the word list.
fn parse_file<R: Read + ?Sized>(src: &mut R) -> Result<(Board, Vec<String>), ProcessingError> {
    let mut lines = String::new();
    src.read_to_string(&mut lines)?;
    let mut board = Vec::new();

    let mut lines = lines.lines().peekable();
    let mut line_no = 0;
    // Skip blank lines until board (or EOF)
    while lines.peek().map_or(false, |line| line.trim().is_empty()) {
        line_no += 1;
        lines.next();
    }

    // Read board
    loop {
        line_no += 1;
        let line = match lines.next() {
            Some(line) => line,
            None => {
                return Err(FileParseError {
                    line_no,
                    kind: FileParseErrorKind::NoWordList,
                }
                .into())
            }
        };

        if line.trim().is_empty() {
            break;
        }
        board.push(line.split_whitespace().map(|s| s.to_string()).collect());
    }
    if line_no == 1 {
        return Err(FileParseError {
            line_no,
            kind: FileParseErrorKind::NoBoard,
        }
        .into());
    }

    Ok((
        Board::new(board),
        lines
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.to_string())
            .collect(),
    ))
}

#[cfg(test)]
fn assert_boards_equ(lhs: &Board, rhs: &Board) {
    assert_eq!(lhs.height(), rhs.height());
    assert_eq!(lhs.width(), rhs.width());
    for y in 0..lhs.height() {
        for x in 0..lhs.width() {
            assert_eq!(lhs[(x, y)], rhs[(x, y)]);
        }
    }
}

#[test]
fn test_read_file() {
    let (board, words) = read_file("test".as_bytes_mut()).unwrap();
    assert_boards_equ(
        &board,
        &Board::new(vec![
            vec!["à".to_string(), "b".to_string(), "ç".to_string()],
            vec!["d".to_string(), "e".to_string(), "f".to_string()],
            vec!["g".to_string(), "h".to_string(), "ï".to_string()],
        ]),
    );
    assert_eq!(
        words,
        vec![
            "eï".to_string(),
            "dg".to_string(),
            "hé".to_string(),
            "àb".to_string(),
            "fç".to_string(),
        ]
    );
}

#[test]
fn test_read_empty_file() {
    unimplemented!();
}

#[test]
fn test_read_file_no_words() {
    unimplemented!();
}

// ===== Displaying =====

fn print_board(board: &Board) {
    for y in 0..board.height() {
        for x in 0..board.width() - 1 {
            print!("{} ", board[(x, y)]);
        }
        println!("{}", board[(board.width() - 1, y)]);
    }
}

fn print_matches(word: &str, matches: &Vec<(usize, usize, Direction)>) {
    if matches.len() == 0 {
        println!("\"{}\" not found", word);
    } else {
        let nb_matches = matches.len();
        println!("\"{}\" found {}:", word, Times(nb_matches));

        let mut i = nb_matches;
        for (x, y, dir) in matches {
            i = i - 1;
            println!(
                "  {}{} from (x={}, y={}){}",
                if i == 0 && nb_matches != 1 {
                    "and "
                } else {
                    ""
                },
                dir,
                x + 1,
                y + 1,
                if i != 0 { "," } else { "" }
            );
        }
    }
}

struct Times(usize);

impl Display for Times {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        match self.0 {
            1 => write!(fmt, "once"),
            2 => write!(fmt, "twice"),
            n => write!(fmt, "{} times", n),
        }
    }
}
