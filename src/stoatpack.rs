use crate::shogi::position::Position;
use crate::shogi::shogimove::Move;
use crate::{FastDeserialise, Outcome, ShogiBoard};
use std::io::{BufRead, Error, ErrorKind};

pub type ScoredMove = (Move, i16);

pub struct Stoatpack {
    pub startpos: Position,
    pub wdl: Outcome,
    pub moves: Vec<ScoredMove>,
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

impl Stoatpack {
    #[must_use]
    pub fn new(startpos: Position) -> Self {
        Self {
            startpos,
            wdl: Outcome::Draw,
            moves: Vec::new(),
        }
    }

    pub fn deserialise(reader: &mut impl BufRead) -> std::io::Result<Self> {
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

        loop {
            let raw_move = read_primitive!(reader, u16);
            let score = read_primitive!(reader, i16);

            if raw_move == 0 && score == 0 {
                break;
            }

            if raw_move == 0 {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    "Encountered a scored nullmove",
                ));
            }

            let mv = Move::from_raw(raw_move);
            moves.push((mv, score));
        }

        Ok(Self {
            startpos,
            wdl,
            moves,
        })
    }
}

impl FastDeserialise for Stoatpack {
    fn deserialise_fast_into_buffer(
        reader: &mut impl BufRead,
        buffer: &mut Vec<u8>,
    ) -> std::io::Result<()> {
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

        loop {
            let mv = read_primitive_into_vec!(reader, buffer, u16);
            let _ = read_primitive_into_vec!(reader, buffer, i16);

            if mv == 0 {
                break;
            }
        }

        Ok(())
    }
}
