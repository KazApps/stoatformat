use crate::shogi::bitboard::Bitboard;
use crate::shogi::core::{Color, PieceType};
use crate::shogi::position::{Hand, Position, PositionBuilder, PositionBuilderError};
use bulletformat::BulletFormat;
use std::io::BufRead;
use std::str::FromStr;

pub mod shogi;
pub mod stoatpack;

pub trait FastDeserialise {
    fn deserialise_fast_into_buffer(
        reader: &mut impl BufRead,
        buffer: &mut Vec<u8>,
    ) -> std::io::Result<()>;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum Outcome {
    SenteLoss = 0,
    Draw,
    SenteWin,
}

impl Outcome {
    pub fn flip(self) -> Self {
        match self {
            Self::SenteLoss => Self::SenteWin,
            Self::Draw => Self::Draw,
            Self::SenteWin => Self::SenteLoss,
        }
    }
}

impl TryFrom<u8> for Outcome {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::SenteLoss),
            1 => Ok(Self::Draw),
            2 => Ok(Self::SenteWin),
            _ => Err(()),
        }
    }
}

impl FromStr for Outcome {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "1.0" | "[1.0]" | "1" => Ok(Self::SenteWin),
            "0.5" | "[0.5]" | "1/2" => Ok(Self::Draw),
            "0.0" | "[0.0]" | "0" => Ok(Self::SenteLoss),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(C, packed)]
pub struct ShogiBoard {
    pub occ: [u128; 2],
    pub pieces: U4Array<20>,
    pub score: i16,
    pub ply_count: u16,
    pub rel_king_squares: [u8; 2],
    pub _unused: [u8; 6],
}

const _SHOGI_BOARD_SIZE: () = assert!(size_of::<ShogiBoard>() == 64);

impl ShogiBoard {
    pub fn occ(&self) -> u128 {
        (self.occ[0] | self.occ[1]) & Bitboard::ALL.0
    }

    pub fn stm(&self) -> Color {
        let stm = (self.occ[0] >> 90) & 0x1;
        Color::from_raw(stm as u8)
    }

    pub fn set_stm(&mut self, stm: Color) {
        const STM_MASK: u128 = 0x1 << 90;
        self.occ[0] = (self.occ[0] & !STM_MASK) | ((stm.raw() as u128) << 90);
    }

    pub fn wdl(&self) -> Outcome {
        let wdl = (self.occ[0] >> 88) & 0x3;
        match wdl {
            0 => Outcome::SenteLoss,
            1 => Outcome::Draw,
            2 => Outcome::SenteWin,
            _ => panic!("Invalid WDL"),
        }
    }

    pub fn wdl_idx(&self) -> usize {
        ((self.occ[0] >> 88) & 0x3) as usize
    }

    pub fn set_wdl(&mut self, wdl: Outcome) {
        const WDL_MASK: u128 = 0x3 << 88;
        self.occ[0] = (self.occ[0] & !WDL_MASK) | ((wdl as u128) << 88);
    }

    pub fn king_sq(&self, c: Color) -> u8 {
        self.rel_king_squares[c.idx()]
    }

    pub fn stm_king_sq(&self) -> u8 {
        self.rel_king_squares[self.stm().idx()]
    }

    pub fn nstm_king_sq(&self) -> u8 {
        self.rel_king_squares[self.stm().flip().idx()]
    }

    pub fn unpack(&self) -> Result<(Position, i16, Outcome), PositionBuilderError> {
        let mut builder = PositionBuilder::new();

        let sente_occ = Bitboard(self.occ[0] & Bitboard::ALL.0);
        let gote_occ = Bitboard(self.occ[1] & Bitboard::ALL.0);

        let mut piece_idx = 0;

        for sq in sente_occ {
            let pt = PieceType::from_raw(self.pieces.get(piece_idx));
            piece_idx += 1;

            builder.add_piece(sq, pt.with_color(Color::SENTE));
        }

        for sq in gote_occ {
            let pt = PieceType::from_raw(self.pieces.get(piece_idx));
            piece_idx += 1;

            builder.add_piece(sq, pt.with_color(Color::GOTE));
        }

        let sente_hand = Hand::from_raw((self.occ[0] >> 96) as u32);
        let gote_hand = Hand::from_raw((self.occ[1] >> 96) as u32);

        builder.set_hand(Color::SENTE, sente_hand)?;
        builder.set_hand(Color::GOTE, gote_hand)?;

        let stm = self.stm();
        builder.set_stm(stm);

        builder.set_ply_count(self.ply_count)?;

        let wdl = self.wdl();
        Ok((builder.build()?, self.score, wdl))
    }

    pub fn deserialise(reader: &mut impl BufRead) -> std::io::Result<Self> {
        let mut buf = [0; size_of::<Self>()];
        reader.read_exact(&mut buf)?;
        // SAFETY: this type entirely comprises primitives
        Ok(unsafe { std::mem::transmute::<[u8; size_of::<Self>()], Self>(buf) })
    }

    pub const fn as_bytes(self) -> [u8; size_of::<Self>()] {
        unsafe { std::mem::transmute(self) }
    }

    pub const fn from_bytes(bytes: [u8; size_of::<Self>()]) -> Self {
        unsafe { std::mem::transmute(bytes) }
    }
}

impl FastDeserialise for ShogiBoard {
    fn deserialise_fast_into_buffer(
        reader: &mut impl BufRead,
        buffer: &mut Vec<u8>,
    ) -> std::io::Result<()> {
        let mut buf = [0; size_of::<Self>()];
        reader.read_exact(&mut buf)?;
        buffer.extend(&buf);
        Ok(())
    }
}

impl Position {
    pub fn pack(&self, sente_score: i16, wdl: Outcome) -> ShogiBoard {
        let mut sente_occ = self.color_bb(Color::SENTE).0;
        let mut gote_occ = self.color_bb(Color::GOTE).0;

        let sente_hand = self.hand(Color::SENTE);
        let gote_hand = self.hand(Color::GOTE);

        sente_occ |= (sente_hand.raw() as u128) << 96;
        gote_occ |= (gote_hand.raw() as u128) << 96;

        let stm = if self.stm() == Color::SENTE { 0 } else { 1 };
        let stm_wdl = (stm << 90) | ((wdl as u128) << 88);
        sente_occ |= stm_wdl;

        let mut pieces = U4Array::new();
        let mut piece_idx = 0;

        for sq in self.color_bb(Color::SENTE) {
            let pt = self.piece_on(sq).unwrap().piece_type();
            pieces.set(piece_idx, pt.raw());
            piece_idx += 1;
        }

        for sq in self.color_bb(Color::GOTE) {
            let pt = self.piece_on(sq).unwrap().piece_type();
            pieces.set(piece_idx, pt.raw());
            piece_idx += 1;
        }

        let sente_king_sq = self
            .piece_bb(PieceType::KING.with_color(Color::SENTE))
            .lsb()
            .expect("Missing sente king");
        let gote_king_sq = self
            .piece_bb(PieceType::KING.with_color(Color::GOTE))
            .lsb()
            .expect("Missing gote king")
            .rotate();

        ShogiBoard {
            occ: [sente_occ, gote_occ],
            pieces,
            score: sente_score,
            ply_count: self.ply_count(),
            rel_king_squares: [sente_king_sq.raw(), gote_king_sq.raw()],
            _unused: [0; 6],
        }
    }
}

impl FromStr for ShogiBoard {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split: Vec<_> = s.split('|').collect();

        let sfen = split[0];
        let score: i16 = split
            .get(1)
            .ok_or("Incomplete record")?
            .trim()
            .parse()
            .map_err(|_| "Invalid score")?;
        let wdl: Outcome = split
            .get(2)
            .ok_or("Incomplete record")?
            .trim()
            .parse()
            .map_err(|_| "Invalid WDL")?;

        let position = sfen.parse::<Position>().map_err(|err| format!("{}", err))?;

        Ok(position.pack(score, wdl))
    }
}

impl IntoIterator for ShogiBoard {
    type Item = (u8, u8);
    type IntoIter = BoardIter;

    fn into_iter(self) -> Self::IntoIter {
        let sente_to_move = self.stm() == Color::SENTE;

        let sente_occ = Bitboard(self.occ[0] & Bitboard::ALL.0);
        let gote_occ = Bitboard(self.occ[1] & Bitboard::ALL.0);

        let mut pieces = [0; 40];
        let mut piece_idx = 0;

        for _ in 0..sente_occ.popcount() {
            pieces[piece_idx] = self.pieces.get(piece_idx) << 1;
            piece_idx += 1;
        }

        for _ in 0..gote_occ.popcount() {
            pieces[piece_idx] = self.pieces.get(piece_idx) << 1;
            piece_idx += 1;
        }

        let fill_hand = |hand: Hand| {
            let mut hand_occ = 0;
            let mut hand_counts = [0; 7];

            for (i, pt) in [
                PieceType::PAWN,
                PieceType::LANCE,
                PieceType::KNIGHT,
                PieceType::SILVER,
                PieceType::GOLD,
                PieceType::BISHOP,
                PieceType::ROOK,
            ]
            .iter()
            .enumerate()
            {
                let count = hand.count(*pt);
                if count > 0 {
                    hand_occ |= 1 << i;
                    hand_counts[i] = count as u8;
                }
            }

            (hand_occ, hand_counts)
        };

        let sente_hand = Hand::from_raw((self.occ[0] >> 96) as u32);
        let gote_hand = Hand::from_raw((self.occ[1] >> 96) as u32);

        let (sente_hand_occ, sente_hand_counts) = fill_hand(sente_hand);
        let (gote_hand_occ, gote_hand_counts) = fill_hand(gote_hand);

        BoardIter {
            sente_to_move,
            sente_occ,
            gote_occ,
            pieces,
            piece_idx: 0,
            sente_hand_occ,
            sente_hand_counts,
            gote_hand_occ,
            gote_hand_counts,
        }
    }
}

pub struct BoardIter {
    sente_to_move: bool,
    sente_occ: Bitboard,
    gote_occ: Bitboard,
    pieces: [u8; 40],
    piece_idx: usize,
    sente_hand_occ: u8,
    sente_hand_counts: [u8; 7],
    gote_hand_occ: u8,
    gote_hand_counts: [u8; 7],
}

impl Iterator for BoardIter {
    type Item = (u8, u8);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(sq) = self.sente_occ.pop_lsb().map(|sq| sq.raw()) {
            let piece = self.pieces[self.piece_idx];
            self.piece_idx += 1;

            return if self.sente_to_move {
                Some((piece, sq))
            } else {
                Some((piece ^ 0x1, 80 - sq))
            };
        }

        if self.sente_hand_occ != 0 {
            let piece = self.sente_hand_occ.trailing_zeros() as usize;
            let count = &mut self.sente_hand_counts[piece];

            let piece_feature = (piece as u8) << 1;
            let count_feature = 81 + *count;

            *count -= 1;
            if *count == 0 {
                self.sente_hand_occ &= self.sente_hand_occ - 1;
            }

            return if self.sente_to_move {
                Some((piece_feature, count_feature))
            } else {
                Some((piece_feature ^ 0x1, count_feature))
            };
        }

        if let Some(sq) = self.gote_occ.pop_lsb().map(|sq| sq.raw()) {
            let piece = self.pieces[self.piece_idx];
            self.piece_idx += 1;

            return if self.sente_to_move {
                Some((piece ^ 0x1, sq))
            } else {
                Some((piece, 80 - sq))
            };
        }

        if self.gote_hand_occ != 0 {
            let piece = self.gote_hand_occ.trailing_zeros() as usize;
            let count = &mut self.gote_hand_counts[piece];

            let piece_feature = (piece as u8) << 1;
            let count_feature = 81 + *count;

            *count -= 1;
            if *count == 0 {
                self.gote_hand_occ &= self.gote_hand_occ - 1;
            }

            return if self.sente_to_move {
                Some((piece_feature ^ 0x1, count_feature))
            } else {
                Some((piece_feature, count_feature))
            };
        }

        None
    }
}

impl BulletFormat for ShogiBoard {
    type FeatureType = (u8, u8);

    const HEADER_SIZE: usize = 0;

    fn set_result(&mut self, _result: f32) {
        todo!()
    }

    fn score(&self) -> i16 {
        if self.stm() == Color::SENTE {
            self.score
        } else {
            -self.score
        }
    }

    fn result(&self) -> f32 {
        (self.result_idx() as f32) / 2.0
    }

    fn result_idx(&self) -> usize {
        if self.stm() == Color::SENTE {
            self.wdl() as usize
        } else {
            2 - self.wdl() as usize
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(C)]
pub struct U4Array<const SIZE_OVER_TWO: usize> {
    data: [u8; SIZE_OVER_TWO],
}

impl<const SIZE_OVER_TWO: usize> U4Array<SIZE_OVER_TWO> {
    pub fn new() -> Self {
        Self {
            data: [0; SIZE_OVER_TWO],
        }
    }

    pub fn get(&self, idx: usize) -> u8 {
        (self.data[idx / 2] >> ((idx % 2) * 4)) & 0xF
    }

    pub fn set(&mut self, idx: usize, value: u8) {
        assert!(value <= 0xF);
        let slot = &mut self.data[idx / 2];
        if idx % 2 == 1 {
            *slot = (*slot & 0x0F) | (value << 4);
        } else {
            *slot = (*slot & 0xF0) | value;
        }
    }
}

impl<const SIZE_OVER_TWO: usize> Default for U4Array<SIZE_OVER_TWO> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::shogi::position::Position;
    use super::*;

    #[test]
    fn pack_roundtrip() {
        let pos = Position::startpos();
        let score = 64;
        let wdl = Outcome::SenteWin;

        let packed = pos.pack(score, wdl);
        let (unpacked_pos, unpacked_score, unpacked_wdl) = packed.unpack().unwrap();

        assert_eq!(pos, unpacked_pos);
        assert_eq!(score, unpacked_score);
        assert_eq!(wdl, unpacked_wdl);
    }
}
