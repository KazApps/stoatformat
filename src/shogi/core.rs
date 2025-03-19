use crate::shogi::bitboard::Bitboard;
use std::fmt::{Display, Formatter};
use std::num::NonZeroU8;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Color {
    id: NonZeroU8,
}

impl Color {
    pub const SENTE: Self = Self::from_raw(0);
    pub const GOTE: Self = Self::from_raw(1);

    pub const COUNT: usize = 2;

    #[must_use]
    pub const fn from_raw(id: u8) -> Self {
        assert!(id < Self::COUNT as u8);
        Self {
            id: NonZeroU8::new(id + 1).unwrap(),
        }
    }

    #[must_use]
    pub const fn raw(self) -> u8 {
        self.id.get() - 1
    }

    #[must_use]
    pub const fn idx(self) -> usize {
        self.raw() as usize
    }

    #[must_use]
    pub const fn flip(self) -> Self {
        Self::from_raw(self.raw() ^ 1)
    }

    #[must_use]
    pub const fn relative_score(self, score: i16) -> i16 {
        match self {
            Self::SENTE => score,
            Self::GOTE => -score,
            _ => unreachable!(),
        }
    }

    #[must_use]
    pub const fn eq(self, other: Self) -> bool {
        self.id.get() == other.id.get()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PieceType {
    id: NonZeroU8,
}

impl PieceType {
    pub const PAWN: Self = Self::from_raw(0);
    pub const PROMOTED_PAWN: Self = Self::from_raw(1);
    pub const LANCE: Self = Self::from_raw(2);
    pub const KNIGHT: Self = Self::from_raw(3);
    pub const PROMOTED_LANCE: Self = Self::from_raw(4);
    pub const PROMOTED_KNIGHT: Self = Self::from_raw(5);
    pub const SILVER: Self = Self::from_raw(6);
    pub const PROMOTED_SILVER: Self = Self::from_raw(7);
    pub const GOLD: Self = Self::from_raw(8);
    pub const BISHOP: Self = Self::from_raw(9);
    pub const ROOK: Self = Self::from_raw(10);
    pub const PROMOTED_BISHOP: Self = Self::from_raw(11);
    pub const PROMOTED_ROOK: Self = Self::from_raw(12);
    pub const KING: Self = Self::from_raw(13);

    pub const COUNT: usize = 14;

    #[must_use]
    pub const fn from_raw(id: u8) -> Self {
        assert!(id < Self::COUNT as u8);
        Self {
            id: NonZeroU8::new(id + 1).unwrap(),
        }
    }

    #[must_use]
    pub const fn raw(self) -> u8 {
        self.id.get() - 1
    }

    #[must_use]
    pub const fn idx(self) -> usize {
        self.raw() as usize
    }

    #[must_use]
    pub const fn is_promoted(self) -> bool {
        self.id.get() == Self::PROMOTED_PAWN.id.get()
            || self.id.get() == Self::PROMOTED_LANCE.id.get()
            || self.id.get() == Self::PROMOTED_KNIGHT.id.get()
            || self.id.get() == Self::PROMOTED_SILVER.id.get()
            || self.id.get() == Self::PROMOTED_BISHOP.id.get()
            || self.id.get() == Self::PROMOTED_ROOK.id.get()
    }

    #[must_use]
    pub const fn can_promote(self) -> bool {
        self.id.get() == Self::PAWN.id.get()
            || self.id.get() == Self::LANCE.id.get()
            || self.id.get() == Self::KNIGHT.id.get()
            || self.id.get() == Self::SILVER.id.get()
            || self.id.get() == Self::BISHOP.id.get()
            || self.id.get() == Self::ROOK.id.get()
    }

    #[must_use]
    pub const fn with_color(self, c: Color) -> Piece {
        Piece::from_raw((self.raw() << 1) | c.raw())
    }

    #[must_use]
    pub const fn promoted(self) -> Option<Self> {
        match self {
            Self::PAWN => Some(Self::PROMOTED_PAWN),
            Self::LANCE => Some(Self::PROMOTED_LANCE),
            Self::KNIGHT => Some(Self::PROMOTED_KNIGHT),
            Self::SILVER => Some(Self::PROMOTED_SILVER),
            Self::BISHOP => Some(Self::PROMOTED_BISHOP),
            Self::ROOK => Some(Self::PROMOTED_ROOK),
            _ => None,
        }
    }

    #[must_use]
    pub const fn unpromoted(self) -> Self {
        match self {
            Self::PROMOTED_PAWN => Self::PAWN,
            Self::PROMOTED_LANCE => Self::LANCE,
            Self::PROMOTED_KNIGHT => Self::KNIGHT,
            Self::PROMOTED_SILVER => Self::SILVER,
            Self::PROMOTED_BISHOP => Self::BISHOP,
            Self::PROMOTED_ROOK => Self::ROOK,
            _ => self,
        }
    }

    #[must_use]
    pub const fn max_in_hand(self) -> u32 {
        match self {
            Self::PAWN => 18,
            Self::LANCE => 4,
            Self::KNIGHT => 4,
            Self::SILVER => 4,
            Self::GOLD => 4,
            Self::BISHOP => 2,
            Self::ROOK => 2,
            _ => 0,
        }
    }

    #[must_use]
    pub const fn eq(self, other: Self) -> bool {
        self.id.get() == other.id.get()
    }

    #[must_use]
    pub fn all() -> PieceTypeIterator {
        PieceTypeIterator::new()
    }
}

impl Display for PieceType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::PAWN => "P",
                Self::PROMOTED_PAWN => "+P",
                Self::LANCE => "L",
                Self::KNIGHT => "N",
                Self::PROMOTED_LANCE => "+L",
                Self::PROMOTED_KNIGHT => "+N",
                Self::SILVER => "S",
                Self::PROMOTED_SILVER => "+S",
                Self::GOLD => "G",
                Self::BISHOP => "B",
                Self::ROOK => "R",
                Self::PROMOTED_BISHOP => "+B",
                Self::PROMOTED_ROOK => "+R",
                Self::KING => "K",
                _ => unreachable!(),
            }
        )
    }
}

pub struct PieceTypeIterator {
    curr: u8,
}

impl PieceTypeIterator {
    #[must_use]
    fn new() -> Self {
        Self { curr: 0 }
    }
}

impl Iterator for PieceTypeIterator {
    type Item = PieceType;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr >= Piece::COUNT as u8 {
            None
        } else {
            let v = PieceType::from_raw(self.curr);
            self.curr += 1;
            Some(v)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Piece {
    id: NonZeroU8,
}

impl Piece {
    pub const SENTE_PAWN: Self = Self::from_raw(0);
    pub const GOTE_PAWN: Self = Self::from_raw(1);
    pub const SENTE_PROMOTED_PAWN: Self = Self::from_raw(2);
    pub const GOTE_PROMOTED_PAWN: Self = Self::from_raw(3);
    pub const SENTE_LANCE: Self = Self::from_raw(4);
    pub const GOTE_LANCE: Self = Self::from_raw(5);
    pub const SENTE_KNIGHT: Self = Self::from_raw(6);
    pub const GOTE_KNIGHT: Self = Self::from_raw(7);
    pub const SENTE_PROMOTED_LANCE: Self = Self::from_raw(8);
    pub const GOTE_PROMOTED_LANCE: Self = Self::from_raw(9);
    pub const SENTE_PROMOTED_KNIGHT: Self = Self::from_raw(10);
    pub const GOTE_PROMOTED_KNIGHT: Self = Self::from_raw(11);
    pub const SENTE_SILVER: Self = Self::from_raw(12);
    pub const GOTE_SILVER: Self = Self::from_raw(13);
    pub const SENTE_PROMOTED_SILVER: Self = Self::from_raw(14);
    pub const GOTE_PROMOTED_SILVER: Self = Self::from_raw(15);
    pub const SENTE_GOLD: Self = Self::from_raw(16);
    pub const GOTE_GOLD: Self = Self::from_raw(17);
    pub const SENTE_BISHOP: Self = Self::from_raw(18);
    pub const GOTE_BISHOP: Self = Self::from_raw(19);
    pub const SENTE_ROOK: Self = Self::from_raw(20);
    pub const GOTE_ROOK: Self = Self::from_raw(21);
    pub const SENTE_PROMOTED_BISHOP: Self = Self::from_raw(22);
    pub const GOTE_PROMOTED_BISHOP: Self = Self::from_raw(23);
    pub const SENTE_PROMOTED_ROOK: Self = Self::from_raw(24);
    pub const GOTE_PROMOTED_ROOK: Self = Self::from_raw(25);
    pub const SENTE_KING: Self = Self::from_raw(26);
    pub const GOTE_KING: Self = Self::from_raw(27);

    pub const COUNT: usize = 28;

    #[must_use]
    pub const fn from_raw(id: u8) -> Self {
        assert!(id < Self::COUNT as u8);
        Self {
            id: NonZeroU8::new(id + 1).unwrap(),
        }
    }

    #[must_use]
    pub const fn raw(self) -> u8 {
        self.id.get() - 1
    }

    #[must_use]
    pub const fn idx(self) -> usize {
        self.raw() as usize
    }

    #[must_use]
    pub const fn piece_type(self) -> PieceType {
        PieceType::from_raw(self.raw() >> 1)
    }

    #[must_use]
    pub const fn color(self) -> Color {
        Color::from_raw(self.raw() & 1)
    }

    #[must_use]
    pub const fn eq(self, other: Self) -> bool {
        self.id.get() == other.id.get()
    }

    #[must_use]
    pub fn all() -> PieceIterator {
        PieceIterator::new()
    }
}

impl TryFrom<char> for Piece {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'P' => Ok(Self::SENTE_PAWN),
            'p' => Ok(Self::GOTE_PAWN),
            'L' => Ok(Self::SENTE_LANCE),
            'l' => Ok(Self::GOTE_LANCE),
            'N' => Ok(Self::SENTE_KNIGHT),
            'n' => Ok(Self::GOTE_KNIGHT),
            'S' => Ok(Self::SENTE_SILVER),
            's' => Ok(Self::GOTE_SILVER),
            'G' => Ok(Self::SENTE_GOLD),
            'g' => Ok(Self::GOTE_GOLD),
            'B' => Ok(Self::SENTE_BISHOP),
            'b' => Ok(Self::GOTE_BISHOP),
            'R' => Ok(Self::SENTE_ROOK),
            'r' => Ok(Self::GOTE_ROOK),
            'K' => Ok(Self::SENTE_KING),
            'k' => Ok(Self::GOTE_KING),
            _ => Err(()),
        }
    }
}

impl Display for Piece {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::SENTE_PAWN => "P",
                Self::GOTE_PAWN => "p",
                Self::SENTE_PROMOTED_PAWN => "+P",
                Self::GOTE_PROMOTED_PAWN => "+p",
                Self::SENTE_LANCE => "L",
                Self::GOTE_LANCE => "l",
                Self::SENTE_KNIGHT => "N",
                Self::GOTE_KNIGHT => "n",
                Self::SENTE_PROMOTED_LANCE => "+L",
                Self::GOTE_PROMOTED_LANCE => "+l",
                Self::SENTE_PROMOTED_KNIGHT => "+N",
                Self::GOTE_PROMOTED_KNIGHT => "+n",
                Self::SENTE_SILVER => "S",
                Self::GOTE_SILVER => "s",
                Self::SENTE_PROMOTED_SILVER => "+S",
                Self::GOTE_PROMOTED_SILVER => "+s",
                Self::SENTE_GOLD => "G",
                Self::GOTE_GOLD => "g",
                Self::SENTE_BISHOP => "B",
                Self::GOTE_BISHOP => "b",
                Self::SENTE_ROOK => "R",
                Self::GOTE_ROOK => "r",
                Self::SENTE_PROMOTED_BISHOP => "+B",
                Self::GOTE_PROMOTED_BISHOP => "+b",
                Self::SENTE_PROMOTED_ROOK => "+R",
                Self::GOTE_PROMOTED_ROOK => "+r",
                Self::SENTE_KING => "K",
                Self::GOTE_KING => "k",
                _ => unreachable!(),
            }
        )
    }
}

pub struct PieceIterator {
    curr: u8,
}

impl PieceIterator {
    #[must_use]
    fn new() -> Self {
        Self { curr: 0 }
    }
}

impl Iterator for PieceIterator {
    type Item = Piece;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr >= Piece::COUNT as u8 {
            None
        } else {
            let v = Piece::from_raw(self.curr);
            self.curr += 1;
            Some(v)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Square {
    id: NonZeroU8,
}

impl Square {
    pub const _9I: Self = Self::from_raw(0);
    pub const _8I: Self = Self::from_raw(1);
    pub const _7I: Self = Self::from_raw(2);
    pub const _6I: Self = Self::from_raw(3);
    pub const _5I: Self = Self::from_raw(4);
    pub const _4I: Self = Self::from_raw(5);
    pub const _3I: Self = Self::from_raw(6);
    pub const _2I: Self = Self::from_raw(7);
    pub const _1I: Self = Self::from_raw(8);
    pub const _9H: Self = Self::from_raw(9);
    pub const _8H: Self = Self::from_raw(10);
    pub const _7H: Self = Self::from_raw(11);
    pub const _6H: Self = Self::from_raw(12);
    pub const _5H: Self = Self::from_raw(13);
    pub const _4H: Self = Self::from_raw(14);
    pub const _3H: Self = Self::from_raw(15);
    pub const _2H: Self = Self::from_raw(16);
    pub const _1H: Self = Self::from_raw(17);
    pub const _9G: Self = Self::from_raw(18);
    pub const _8G: Self = Self::from_raw(19);
    pub const _7G: Self = Self::from_raw(20);
    pub const _6G: Self = Self::from_raw(21);
    pub const _5G: Self = Self::from_raw(22);
    pub const _4G: Self = Self::from_raw(23);
    pub const _3G: Self = Self::from_raw(24);
    pub const _2G: Self = Self::from_raw(25);
    pub const _1G: Self = Self::from_raw(26);
    pub const _9F: Self = Self::from_raw(27);
    pub const _8F: Self = Self::from_raw(28);
    pub const _7F: Self = Self::from_raw(29);
    pub const _6F: Self = Self::from_raw(30);
    pub const _5F: Self = Self::from_raw(31);
    pub const _4F: Self = Self::from_raw(32);
    pub const _3F: Self = Self::from_raw(33);
    pub const _2F: Self = Self::from_raw(34);
    pub const _1F: Self = Self::from_raw(35);
    pub const _9E: Self = Self::from_raw(36);
    pub const _8E: Self = Self::from_raw(37);
    pub const _7E: Self = Self::from_raw(38);
    pub const _6E: Self = Self::from_raw(39);
    pub const _5E: Self = Self::from_raw(40);
    pub const _4E: Self = Self::from_raw(41);
    pub const _3E: Self = Self::from_raw(42);
    pub const _2E: Self = Self::from_raw(43);
    pub const _1E: Self = Self::from_raw(44);
    pub const _9D: Self = Self::from_raw(45);
    pub const _8D: Self = Self::from_raw(46);
    pub const _7D: Self = Self::from_raw(47);
    pub const _6D: Self = Self::from_raw(48);
    pub const _5D: Self = Self::from_raw(49);
    pub const _4D: Self = Self::from_raw(50);
    pub const _3D: Self = Self::from_raw(51);
    pub const _2D: Self = Self::from_raw(52);
    pub const _1D: Self = Self::from_raw(53);
    pub const _9C: Self = Self::from_raw(54);
    pub const _8C: Self = Self::from_raw(55);
    pub const _7C: Self = Self::from_raw(56);
    pub const _6C: Self = Self::from_raw(57);
    pub const _5C: Self = Self::from_raw(58);
    pub const _4C: Self = Self::from_raw(59);
    pub const _3C: Self = Self::from_raw(60);
    pub const _2C: Self = Self::from_raw(61);
    pub const _1C: Self = Self::from_raw(62);
    pub const _9B: Self = Self::from_raw(63);
    pub const _8B: Self = Self::from_raw(64);
    pub const _7B: Self = Self::from_raw(65);
    pub const _6B: Self = Self::from_raw(66);
    pub const _5B: Self = Self::from_raw(67);
    pub const _4B: Self = Self::from_raw(68);
    pub const _3B: Self = Self::from_raw(69);
    pub const _2B: Self = Self::from_raw(70);
    pub const _1B: Self = Self::from_raw(71);
    pub const _9A: Self = Self::from_raw(72);
    pub const _8A: Self = Self::from_raw(73);
    pub const _7A: Self = Self::from_raw(74);
    pub const _6A: Self = Self::from_raw(75);
    pub const _5A: Self = Self::from_raw(76);
    pub const _4A: Self = Self::from_raw(77);
    pub const _3A: Self = Self::from_raw(78);
    pub const _2A: Self = Self::from_raw(79);
    pub const _1A: Self = Self::from_raw(80);

    pub const COUNT: usize = 81;

    #[must_use]
    pub const fn from_raw(id: u8) -> Self {
        assert!(id < Self::COUNT as u8);
        Self {
            id: NonZeroU8::new(id + 1).unwrap(),
        }
    }

    #[must_use]
    pub const fn from_file_rank(file: u32, rank: u32) -> Self {
        assert!(file < 9);
        assert!(rank < 9);

        Self::from_raw((rank * 9 + file) as u8)
    }

    #[must_use]
    pub const fn raw(self) -> u8 {
        self.id.get() - 1
    }

    #[must_use]
    pub const fn idx(self) -> usize {
        self.raw() as usize
    }

    #[must_use]
    pub const fn rank(self) -> u32 {
        self.raw() as u32 / 9
    }

    #[must_use]
    pub const fn file(self) -> u32 {
        self.raw() as u32 % 9
    }

    #[must_use]
    pub const fn rotate(self) -> Self {
        Self::from_raw(80 - self.raw())
    }

    #[must_use]
    pub const fn bit(self) -> Bitboard {
        Bitboard(1u128 << self.raw())
    }

    #[must_use]
    pub const fn eq(self, other: Self) -> bool {
        self.id.get() == other.id.get()
    }
}

impl Display for Square {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let file = (b'1' + 8 - self.file() as u8) as char;
        let rank = (b'a' + 8 - self.rank() as u8) as char;
        write!(f, "{}{}", file, rank)
    }
}
