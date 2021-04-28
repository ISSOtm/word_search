use aho_corasick::AhoCorasickBuilder;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::io::{self, Read};
use std::iter::FusedIterator;
use std::ops::Index;
use std::slice::SliceIndex;

#[derive(Debug)]
pub struct Board(Vec<Vec<String>>);

impl Board {
    pub fn new(board: Vec<Vec<String>>) -> Self {
        let height = board.len();
        assert_ne!(height, 0, "Board may not be empty (height != 0)");
        let width = board[0].len();
        assert_ne!(width, 0, "Board may not be empty (width != 0)");
        for i in 1..height {
            assert_eq!(
                board[i].len(),
                width,
                "All board rows must have width {}, row #{} has {}",
                width,
                i,
                board[i].len()
            );
        }

        Self(board)
    }

    pub fn height(&self) -> usize {
        self.0.len()
    }

    pub fn width(&self) -> usize {
        self.0[0].len()
    }
}

impl<
        I: SliceIndex<[String], Output = String>,
        J: SliceIndex<[Vec<String>], Output = Vec<String>>,
    > Index<(I, J)> for Board
{
    type Output = String;

    fn index(&self, index: (I, J)) -> &Self::Output {
        &self.0[index.1][index.0]
    }
}

#[derive(Debug)]
pub struct WordSearch<'a> {
    board: &'a Board,
    matches: HashMap<usize, Vec<(usize, usize, Direction)>>,
}

impl<'a> WordSearch<'a> {
    pub fn new(
        board: &'a Board,
        dict: &Vec<String>,
        allow_backwards: bool,
    ) -> Result<Self, Vec<io::Error>> {
        let matcher = AhoCorasickBuilder::new().build(dict.iter());
        let mut matches = HashMap::new();

        for i in 0..dict.len() {
            matches.insert(i, vec![]);
        }

        let mut errors = Vec::new();
        for dir in DirIterator::new(Direction::Right, allow_backwards) {
            for line in LinesIter::new(board, dir) {
                for result in matcher.stream_find_iter(line.iter()) {
                    match result {
                        Ok(matched) => {
                            let (x, y, remainder) = line.get_coords_of(matched.start()).unwrap();
                            assert_eq!(remainder, 0);
                            matches
                                .get_mut(&matched.pattern())
                                .unwrap()
                                .push((x, y, dir));
                        }
                        Err(err) => errors.push(err),
                    }
                }
            }
        }

        if errors.len() == 0 {
            Ok(Self { board, matches })
        } else {
            Err(errors)
        }
    }

    pub fn matches(&self) -> &HashMap<usize, Vec<(usize, usize, Direction)>> {
        &self.matches
    }
}

#[test]
fn test_match() {
    use Direction::*;

    let board = Board::new(vec![
        vec!["à".to_string(), "b".to_string(), "ç".to_string()],
        vec!["d".to_string(), "e".to_string(), "f".to_string()],
        vec!["g".to_string(), "h".to_string(), "ï".to_string()],
    ]);
    let search = WordSearch::new(
        &board,
        &vec![
            "eï".to_string(),
            "dg".to_string(),
            "hé".to_string(),
            "àb".to_string(),
            "fç".to_string(),
        ],
        true,
    )
    .expect("Errors occurred during read");

    assert_eq!(search.matches().get(&0), Some(&vec![(1, 1, DownRight)]));
    assert_eq!(search.matches().get(&1), Some(&vec![(0, 1, Down)]));
    assert_eq!(search.matches().get(&2), Some(&vec![]));
    assert_eq!(search.matches().get(&3), Some(&vec![(0, 0, Right)]));
    assert_eq!(search.matches().get(&4), Some(&vec![(2, 1, Up)]));
}

// ===== Directions, and an iterator around them ======

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Direction {
    UpLeft,
    Up,
    UpRight,
    Right,
    DownRight,
    Down,
    DownLeft,
    Left,
}

impl Direction {
    fn is_backwards(&self) -> bool {
        use Direction::*;
        match self {
            UpLeft | Up | UpRight | DownLeft | Left => true,
            Right | DownRight | Down => false,
        }
    }

    fn clockwise_rot(&self, allow_backwards: bool) -> Self {
        let mut dir = *self;

        loop {
            use Direction::*;
            dir = match dir {
                UpLeft => Up,
                Up => UpRight,
                UpRight => Right,
                Right => DownRight,
                DownRight => Down,
                Down => DownLeft,
                DownLeft => Left,
                Left => UpLeft,
            };
            if allow_backwards || !dir.is_backwards() {
                return dir;
            }
        }
    }

    fn step(&self, (x, y): (usize, usize)) -> (usize, usize) {
        use Direction::*;
        (
            match self {
                UpLeft | Left | DownLeft => x.wrapping_sub(1),
                Up | Down => x,
                UpRight | Right | DownRight => x.wrapping_add(1),
            },
            match self {
                UpLeft | Up | UpRight => y.wrapping_sub(1),
                Left | Right => y,
                DownLeft | Down | DownRight => y.wrapping_add(1),
            },
        )
    }
}

impl Display for Direction {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        use Direction::*;

        match self {
            UpLeft => write!(fmt, "up-left"),
            Up => write!(fmt, "up"),
            UpRight => write!(fmt, "up-right"),
            Right => write!(fmt, "right"),
            DownRight => write!(fmt, "down-right"),
            Down => write!(fmt, "down"),
            DownLeft => write!(fmt, "down-left"),
            Left => write!(fmt, "left"),
        }
    }
}

#[derive(Debug)]
struct DirIterator {
    first_dir: Option<Direction>,
    allow_backwards: bool,

    /// Last direction emitted
    cur_dir: Direction,
}

impl DirIterator {
    fn new(mut start_dir: Direction, allow_backwards: bool) -> Self {
        if !allow_backwards && start_dir.is_backwards() {
            start_dir = start_dir.clockwise_rot(allow_backwards);
        }

        Self {
            first_dir: None,
            allow_backwards,

            cur_dir: start_dir,
        }
    }
}

#[test]
fn test_backward_start_correction() {
    let mut iter = DirIterator::new(Direction::Up, false);
    assert_eq!(iter.next(), Some(Direction::Right));
}

impl Iterator for DirIterator {
    type Item = Direction;

    fn next(&mut self) -> Option<Self::Item> {
        let dir = self.cur_dir;
        match self.first_dir {
            // If we haven't iterated yet, register the first direction, and return it
            None => self.first_dir = Some(self.cur_dir),
            // If we made a full circle, return EOF
            Some(dir) if dir == self.cur_dir => return None,
            Some(_) => (),
        }
        self.cur_dir = self.cur_dir.clockwise_rot(self.allow_backwards);
        Some(dir)
    }
}

#[test]
fn test_first_dir() {
    use Direction::*;

    let dirs: Vec<Direction> = DirIterator::new(DownRight, true).collect();
    assert_eq!(
        dirs,
        vec![DownRight, Down, DownLeft, Left, UpLeft, Up, UpRight, Right]
    );
}

impl FusedIterator for DirIterator {}

#[test]
fn test_fused_dir_iter() {
    use Direction::*;

    let expected = vec![
        Some(UpLeft),
        Some(Up),
        Some(UpRight),
        Some(Right),
        Some(DownRight),
        Some(Down),
        Some(DownLeft),
        Some(Left),
        None,
        None,
        None,
        // ...and so on. 3 should be enough to be confident that it's infinite.
    ];
    let mut dir_iter = DirIterator::new(UpLeft, true);
    for i in 0..expected.len() {
        assert_eq!(dir_iter.next(), expected[i], "Mismatch on iter #{}", i);
    }
}

// ===== An iterator over lines in a given direction =====

#[derive(Debug)]
struct LinesIter<'a> {
    board: &'a Board,
    dir: Direction,

    i: usize,
}

impl<'a> LinesIter<'a> {
    fn new(board: &'a Board, dir: Direction) -> Self {
        Self { board, dir, i: 0 }
    }

    fn n(&self) -> usize {
        use Direction::*;
        match self.dir {
            UpLeft | UpRight | DownRight | DownLeft => self.board.width() + self.board.height() - 1,
            Up | Down => self.board.width(),
            Left | Right => self.board.height(),
        }
    }

    fn starting_coords(&self) -> (usize, usize) {
        use Direction::*;

        let hmax = self.board.width() - 1;
        let vmax = self.board.height() - 1;
        let horiz = self.i.saturating_sub(vmax);
        let vert = vmax.saturating_sub(self.i);

        match self.dir {
            UpLeft => (hmax - horiz, vmax - vert),
            Up => (self.i, vmax),
            UpRight => (horiz, vmax - vert),
            Right => (0, self.i),
            DownRight => (horiz, vert),
            Down => (self.i, 0),
            DownLeft => (hmax - horiz, vert),
            Left => (hmax, self.i),
        }
    }
}

impl<'a> Iterator for LinesIter<'a> {
    type Item = Line<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i == self.n() {
            None
        } else {
            let (x, y) = self.starting_coords();
            let line = Line::new(self.board, self.dir, x, y);
            self.i += 1;
            Some(line)
        }
    }
}

#[test]
fn test_starting_coords() {
    let board = Board::new(vec![
        vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
            "0".to_string(),
        ],
        vec![
            "d".to_string(),
            "é".to_string(),
            "f".to_string(),
            "1".to_string(),
        ],
        vec![
            "g".to_string(),
            "h".to_string(),
            "i".to_string(),
            "2".to_string(),
        ],
    ]);

    use Direction::*;
    let results = vec![
        (UpLeft, vec![(3, 0), (3, 1), (3, 2), (2, 2), (1, 2), (0, 2)]),
        (Up, vec![(0, 2), (1, 2), (2, 2), (3, 2)]),
        (
            UpRight,
            vec![(0, 0), (0, 1), (0, 2), (1, 2), (2, 2), (3, 2)],
        ),
        (Right, vec![(0, 0), (0, 1), (0, 2)]),
        (
            DownRight,
            vec![(0, 2), (0, 1), (0, 0), (1, 0), (2, 0), (3, 0)],
        ),
        (Down, vec![(0, 0), (1, 0), (2, 0), (3, 0)]),
        (
            DownLeft,
            vec![(3, 2), (3, 1), (3, 0), (2, 0), (1, 0), (0, 0)],
        ),
        (Left, vec![(3, 0), (3, 1), (3, 2)]),
    ];

    for (dir, expected) in results {
        assert_eq!(
            LinesIter::new(&board, dir)
                .map(|line| (line.start_x, line.start_y))
                .collect::<Vec<_>>(),
            expected,
            "{:?} did not match!",
            dir,
        );
    }
}

impl FusedIterator for LinesIter<'_> {}

#[test]
fn test_fused_lines_iter() {
    let board = Board::new(vec![vec![
        "0".to_string(),
        "1".to_string(),
        "2".to_string(),
    ]]);
    let expected = vec![
        Some(0),
        Some(1),
        Some(2),
        None,
        None,
        None,
        // ...and so on. 3 should be enough to be confident that it's infinite.
    ];
    let mut lines_iter = LinesIter::new(&board, Direction::Down);
    for i in 0..expected.len() {
        assert_eq!(
            lines_iter.next().map(|line| line.start_x),
            expected[i],
            "Mismatch on iter #{}",
            i
        );
    }
}

// ===== An iterator over characters in a given line =====

#[derive(Debug)]
struct Line<'a> {
    board: &'a Board,
    dir: Direction,
    start_x: usize,
    start_y: usize,
}

impl<'a> Line<'a> {
    fn new(board: &'a Board, dir: Direction, start_x: usize, start_y: usize) -> Self {
        Self {
            board,
            dir,
            start_x,
            start_y,
        }
    }

    /// Gets the coords at a given **byte** offset, and the offset within that string (again, as bytes)
    fn get_coords_of(&self, mut step: usize) -> Option<(usize, usize, usize)> {
        let mut coords = (self.start_x, self.start_y);
        while step >= self.board[coords].len() {
            step -= self.board[coords].len();
            coords = self.dir.step(coords);
            if coords.0 >= self.board.width() || coords.1 >= self.board.height() {
                return None;
            }
        }
        Some((coords.0, coords.1, step))
    }

    fn iter(&'a self) -> LineIter<'a> {
        LineIter(self, 0)
    }
}

#[test]
fn test_multibyte_coords() {
    let board = Board::new(vec![
        vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
            "0".to_string(),
        ],
        vec![
            "d".to_string(),
            "é".to_string(),
            "f".to_string(),
            "1".to_string(),
        ],
        vec![
            "g".to_string(),
            "h".to_string(),
            "i".to_string(),
            "2".to_string(),
        ],
    ]);
    let line = Line::new(&board, Direction::DownRight, 0, 0);

    assert_eq!(line.get_coords_of(0), Some((0, 0, 0)));
    assert_eq!(line.get_coords_of(1), Some((1, 1, 0)));
    assert_eq!(line.get_coords_of(2), Some((1, 1, 1)));
    assert_eq!(line.get_coords_of(3), Some((2, 2, 0)));
}

struct LineIter<'a>(&'a Line<'a>, usize);

impl Read for LineIter<'_> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let (mut coords, mut i) = match self.0.get_coords_of(self.1) {
            Some((x, y, i)) => ((x, y), i),
            None => return Ok(0),
        };
        let mut nb_read = 0;

        while nb_read < buf.len() {
            let bytes = self.0.board[coords].as_bytes();
            buf[nb_read] = bytes[i];
            i += 1;
            nb_read += 1;
            if i == self.0.board[coords].len() {
                coords = self.0.dir.step(coords);
                if coords.0 >= self.0.board.width() || coords.1 >= self.0.board.height() {
                    break;
                }
                i = 0;
            }
        }

        self.1 += nb_read;
        Ok(nb_read)
    }
}

#[test]
fn test_line_str() {
    let board = Board::new(vec![
        vec![
            "à".to_string(),
            "b".to_string(),
            "ç".to_string(),
            "d".to_string(),
        ],
        vec![
            "é".to_string(),
            "f".to_string(),
            "g".to_string(),
            "h".to_string(),
        ],
        vec![
            "i".to_string(),
            "j".to_string(),
            "k".to_string(),
            "ù".to_string(),
        ],
    ]);

    for j in 0..board.height() {
        let mut expected = String::new();
        for i in 0..board.width() {
            expected.push_str(&board[(i, j)]);
        }

        let mut line_str = String::new();
        Line::new(&board, Direction::Right, 0, j)
            .iter()
            .read_to_string(&mut line_str)
            .expect(format!("Error reading line {}", j).as_str());
        assert_eq!(line_str, expected, "Line {} does not match", j);
    }
}
