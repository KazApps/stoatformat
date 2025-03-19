use crate::shogi::core::{PieceType, Square};
use std::fmt::{Display, Formatter};
use std::num::NonZeroU16;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Move(NonZeroU16);

impl Move {
    const TO_SHIFT: u32 = 0;

    const NORMAL_FROM_SHIFT: u32 = 7;
    const NORMAL_PROMO_FLAG_SHIFT: u32 = 14;

    const DROP_PIECE_SHIFT: u32 = 7;

    const DROP_FLAG_SHIFT: u32 = 15;

    const SQUARE_MASK: u16 = 0b1111111;
    const PIECE_MASK: u16 = 0b111;
    const FLAG_MASK: u16 = 0b1;

    const DROP_PIECE_INDICES: [Option<u16>; PieceType::COUNT] = {
        let mut result = [None; PieceType::COUNT];

        result[PieceType::PAWN.idx()] = Some(0);
        result[PieceType::LANCE.idx()] = Some(1);
        result[PieceType::KNIGHT.idx()] = Some(2);
        result[PieceType::SILVER.idx()] = Some(3);
        result[PieceType::GOLD.idx()] = Some(4);
        result[PieceType::BISHOP.idx()] = Some(5);
        result[PieceType::ROOK.idx()] = Some(6);

        result
    };

    const DROP_PIECES: [PieceType; 7] = [
        PieceType::PAWN,
        PieceType::LANCE,
        PieceType::KNIGHT,
        PieceType::SILVER,
        PieceType::GOLD,
        PieceType::BISHOP,
        PieceType::ROOK,
    ];

    #[must_use]
    pub const fn from_raw(mv: u16) -> Self {
        Self(NonZeroU16::new(mv).unwrap())
    }

    #[must_use]
    pub const fn make_normal(from: Square, to: Square) -> Self {
        assert!(from.raw() != to.raw());

        let mut mv = 0;

        mv |= (to.raw() as u16) << Self::TO_SHIFT;
        mv |= (from.raw() as u16) << Self::NORMAL_FROM_SHIFT;

        Self(NonZeroU16::new(mv).unwrap())
    }

    #[must_use]
    pub const fn make_promo(from: Square, to: Square) -> Self {
        assert!(from.raw() != to.raw());

        let mut mv = 0;

        mv |= (to.raw() as u16) << Self::TO_SHIFT;
        mv |= (from.raw() as u16) << Self::NORMAL_FROM_SHIFT;
        mv |= 1 << Self::NORMAL_PROMO_FLAG_SHIFT;

        Self(NonZeroU16::new(mv).unwrap())
    }

    #[must_use]
    pub const fn make_drop(pt: PieceType, to: Square) -> Self {
        let mut mv = 0;

        mv |= (to.raw() as u16) << Self::TO_SHIFT;
        mv |= Self::DROP_PIECE_INDICES[pt.idx()].unwrap() << Self::DROP_PIECE_SHIFT;
        mv |= 1 << Self::DROP_FLAG_SHIFT;

        Self(NonZeroU16::new(mv).unwrap())
    }

    #[must_use]
    pub const fn is_drop(self) -> bool {
        self.get(Self::DROP_FLAG_SHIFT, Self::FLAG_MASK) != 0
    }

    #[must_use]
    pub const fn is_promo(self) -> bool {
        assert!(!self.is_drop());
        self.get(Self::NORMAL_PROMO_FLAG_SHIFT, Self::FLAG_MASK) != 0
    }

    #[must_use]
    pub const fn from(self) -> Square {
        assert!(!self.is_drop());
        Square::from_raw(self.get(Self::NORMAL_FROM_SHIFT, Self::SQUARE_MASK) as u8)
    }

    #[must_use]
    pub const fn to(self) -> Square {
        Square::from_raw(self.get(Self::TO_SHIFT, Self::SQUARE_MASK) as u8)
    }

    #[must_use]
    pub const fn drop_piece(self) -> PieceType {
        assert!(self.is_drop());
        Self::DROP_PIECES[self.get(Self::DROP_PIECE_SHIFT, Self::PIECE_MASK) as usize]
    }

    #[must_use]
    pub const fn is_null(self) -> bool {
        self.0.get() == 0
    }

    #[must_use]
    const fn get(self, shift: u32, mask: u16) -> u16 {
        (self.0.get() >> shift) & mask
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_drop() {
            return write!(f, "{}*{}", self.drop_piece(), self.to());
        }

        write!(f, "{}{}", self.from(), self.to())?;

        if self.is_promo() {
            write!(f, "+")?;
        }

        Ok(())
    }
}
