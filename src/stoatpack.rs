use crate::shogi::position::Position;
use crate::shogi::shogimove::Move;
use crate::{FastDeserialise, Outcome, ShogiBoard};
use std::io::{BufRead, Error, ErrorKind, Result};

pub struct StoatpackBase<M: ScoredMove> {
    pub startpos: Position,
    pub wdl: Outcome,
    pub moves: Vec<M>,
}

pub trait ScoredMove: Sized {
    const SIZE: usize;

    fn score(&self) -> i16;
    fn static_eval(&self) -> Option<i16>;
    fn mv(&self) -> Move;

    fn read(reader: &mut impl BufRead) -> Result<Option<Self>>;
    fn read_fast(reader: &mut impl BufRead, buffer: &mut Vec<u8>) -> Result<bool>;
}

macro_rules! read_primitive {
    ($reader:expr, $t:ty) => {{
        let mut buf = [0; size_of::<$t>()];
        $reader.read_exact(&mut buf)?;
        <$t>::from_le_bytes(buf)
    }};
}

macro_rules! read_primitive_into_vec {
    ($reader:expr, $writer:expr, $t:ty) => {{
        let mut buf = [0; size_of::<$t>()];
        $reader.read_exact(&mut buf)?;
        $writer.extend(&buf);
        <$t>::from_le_bytes(buf)
    }};
}

impl<M: ScoredMove> StoatpackBase<M> {
    #[must_use]
    pub fn new(startpos: Position) -> Self {
        Self {
            startpos,
            wdl: Outcome::Draw,
            moves: Vec::new(),
        }
    }

    pub fn deserialise(reader: &mut impl BufRead) -> Result<Self> {
        let wdl_type: u8 = read_primitive!(reader, u8);

        let wdl: Outcome = (wdl_type >> 6)
            .try_into()
            .map_err(|_| Error::new(ErrorKind::InvalidData, "Invalid WDL"))?;

        let startpos_type = wdl_type & 0b111111;
        let mut startpos = match startpos_type {
            0 => Position::startpos(),
            1 => todo!(), // shogi960
            2 => {
                ShogiBoard::deserialise(reader)?
                    .unpack()
                    .map_err(|err| {
                        Error::new(
                            ErrorKind::InvalidData,
                            format!("Invalid packed startpos: {}", err),
                        )
                    })?
                    .0
            }
            _ => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("Invalid startpos type {}", startpos_type),
                ));
            }
        };

        let unscored_count = read_primitive!(reader, u16);
        for _ in 0..unscored_count {
            let raw_move = read_primitive!(reader, u16);

            if raw_move == 0 {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    "Encountered an unscored nullmove",
                ));
            }

            let mv = Move::from_raw(raw_move);
            startpos = startpos.apply_move(mv);
        }

        let mut moves = Vec::new();

        while let Some(mv) = M::read(reader)? {
            moves.push(mv);
        }

        Ok(Self {
            startpos,
            wdl,
            moves,
        })
    }
}

impl<M: ScoredMove> FastDeserialise for StoatpackBase<M> {
    fn deserialise_fast_into_buffer(reader: &mut impl BufRead, buffer: &mut Vec<u8>) -> Result<()> {
        let wdl_type = read_primitive_into_vec!(reader, buffer, u8);

        let startpos_type = wdl_type & 0b111111;
        match startpos_type {
            0 => {}
            1 => todo!(), // shogi960
            2 => ShogiBoard::deserialise_fast_into_buffer(reader, buffer)?,
            _ => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("Invalid startpos type {}", startpos_type),
                ));
            }
        }

        let unscored_count = read_primitive_into_vec!(reader, buffer, u16);
        for _ in 0..unscored_count {
            let _ = read_primitive_into_vec!(reader, buffer, u16);
        }

        while M::read_fast(reader, buffer)? {}

        Ok(())
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ScoredMove1 {
    pub mv: Move,
    pub score: i16,
}

impl ScoredMove for ScoredMove1 {
    const SIZE: usize = size_of::<u16>() + size_of::<i16>();

    fn score(&self) -> i16 {
        self.score
    }

    fn static_eval(&self) -> Option<i16> {
        None
    }

    fn mv(&self) -> Move {
        self.mv
    }

    fn read(reader: &mut impl BufRead) -> Result<Option<Self>> {
        let raw = read_primitive!(reader, u16);
        let score = read_primitive!(reader, i16);

        if raw == 0 {
            if score == 0 {
                Ok(None)
            } else {
                Err(Error::new(
                    ErrorKind::InvalidData,
                    "Encountered a scored nullmove",
                ))
            }
        } else {
            Ok(Some(Self {
                mv: Move::from_raw(raw),
                score,
            }))
        }
    }

    fn read_fast(reader: &mut impl BufRead, buffer: &mut Vec<u8>) -> Result<bool> {
        let raw = read_primitive_into_vec!(reader, buffer, u16);
        let _ = read_primitive_into_vec!(reader, buffer, i16);
        Ok(raw != 0)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ScoredMove2 {
    pub mv: Move,
    pub static_eval: i16,
    pub score: i16,
}

impl ScoredMove for ScoredMove2 {
    const SIZE: usize = size_of::<u16>() + size_of::<i16>() * 2;

    fn score(&self) -> i16 {
        self.score
    }

    fn static_eval(&self) -> Option<i16> {
        Some(self.static_eval)
    }

    fn mv(&self) -> Move {
        self.mv
    }

    fn read(reader: &mut impl BufRead) -> Result<Option<Self>> {
        let score = read_primitive!(reader, i16);
        let static_eval = read_primitive!(reader, i16);
        let raw = read_primitive!(reader, u16);

        if raw == 0 {
            if static_eval == 0 && score == 0 {
                Ok(None)
            } else {
                Err(Error::new(
                    ErrorKind::InvalidData,
                    "Encountered a scored nullmove",
                ))
            }
        } else {
            Ok(Some(Self {
                mv: Move::from_raw(raw),
                static_eval,
                score,
            }))
        }
    }

    fn read_fast(reader: &mut impl BufRead, buffer: &mut Vec<u8>) -> Result<bool> {
        let _ = read_primitive_into_vec!(reader, buffer, i16);
        let _ = read_primitive_into_vec!(reader, buffer, i16);
        let raw = read_primitive_into_vec!(reader, buffer, u16);
        Ok(raw != 0)
    }
}

pub type Stoatpack = StoatpackBase<ScoredMove1>;
pub type Stoatpack2 = StoatpackBase<ScoredMove2>;
