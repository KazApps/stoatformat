use crate::shogi::core::{Color, Square};
use std::fmt::{Display, Formatter};
use std::ops::*;

pub mod offsets {
    pub const NORTH: i32 = 9;
    pub const SOUTH: i32 = -9;
    pub const WEST: i32 = -1;
    pub const EAST: i32 = 1;

    pub const NORTH_WEST: i32 = 8;
    pub const NORTH_EAST: i32 = 10;
    pub const SOUTH_WEST: i32 = -10;
    pub const SOUTH_EAST: i32 = -8;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Bitboard(pub u128);

impl Bitboard {
    pub const ALL: Self = Self(0x1ffffffffffffffffffff);
    pub const EMPTY: Self = Self(0);

    pub const RANK_I: Self = Self(0x0000000000000000001ff);
    pub const RANK_H: Self = Self(0x00000000000000003fe00);
    pub const RANK_G: Self = Self(0x000000000000007fc0000);
    pub const RANK_F: Self = Self(0x000000000000ff8000000);
    pub const RANK_E: Self = Self(0x0000000001ff000000000);
    pub const RANK_D: Self = Self(0x00000003fe00000000000);
    pub const RANK_C: Self = Self(0x000007fc0000000000000);
    pub const RANK_B: Self = Self(0x000ff8000000000000000);
    pub const RANK_A: Self = Self(0x1ff000000000000000000);

    pub const FILE_9: Self = Self(0x001008040201008040201);
    pub const FILE_8: Self = Self(0x002010080402010080402);
    pub const FILE_7: Self = Self(0x004020100804020100804);
    pub const FILE_6: Self = Self(0x008040201008040201008);
    pub const FILE_5: Self = Self(0x010080402010080402010);
    pub const FILE_4: Self = Self(0x020100804020100804020);
    pub const FILE_3: Self = Self(0x040201008040201008040);
    pub const FILE_2: Self = Self(0x080402010080402010080);
    pub const FILE_1: Self = Self(0x100804020100804020100);

    #[must_use]
    pub const fn get_sq(self, sq: Square) -> bool {
        (self.0 & sq.bit().0) != 0
    }

    pub const fn set_sq(&mut self, sq: Square) -> &mut Self {
        self.0 |= sq.bit().0;
        self
    }

    pub const fn clear_sq(&mut self, sq: Square) -> &mut Self {
        self.0 &= !sq.bit().0;
        self
    }

    pub const fn toggle_sq(&mut self, sq: Square) -> &mut Self {
        self.0 ^= sq.bit().0;
        self
    }

    pub const fn assign_sq(&mut self, sq: Square, value: bool) -> &mut Self {
        if value {
            self.set_sq(sq)
        } else {
            self.clear_sq(sq)
        }
    }

    pub const fn clear(&mut self) -> &mut Self {
        self.0 = 0;
        self
    }

    #[must_use]
    pub const fn popcount(self) -> u32 {
        self.0.count_ones()
    }

    #[must_use]
    pub const fn empty(self) -> bool {
        self.0 == 0
    }

    #[must_use]
    pub const fn multiple(self) -> bool {
        (self.0 & (self.0 - 1)) != 0
    }

    #[must_use]
    pub const fn one(self) -> bool {
        !self.empty() && !self.multiple()
    }

    #[must_use]
    pub const fn lsb(self) -> Option<Square> {
        let idx = self.0.trailing_zeros();
        if idx == 128 {
            None
        } else {
            Some(Square::from_raw(idx as u8))
        }
    }

    #[must_use]
    pub const fn isolate_lsb(self) -> Self {
        Self(self.0 & self.0.wrapping_neg())
    }

    pub const fn pop_lsb(&mut self) -> Option<Square> {
        let lsb = self.lsb();
        self.0 &= self.0.wrapping_sub(1);
        lsb
    }

    #[must_use]
    pub const fn and(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }

    #[must_use]
    pub const fn or(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }

    #[must_use]
    pub const fn xor(self, rhs: Self) -> Self {
        Self(self.0 ^ rhs.0)
    }

    #[must_use]
    pub const fn inverse(self) -> Self {
        Self(!self.0)
    }

    #[must_use]
    pub const fn bit_shl(self, rhs: u32) -> Self {
        Self(self.0 << rhs)
    }

    #[must_use]
    pub const fn bit_shr(self, rhs: u32) -> Self {
        Self(self.0 >> rhs)
    }

    #[must_use]
    pub const fn shift_north(self) -> Self {
        Self((self.0 & !Self::RANK_A.0) << offsets::NORTH)
    }

    #[must_use]
    pub const fn shift_south(self) -> Self {
        Self(self.0 >> -offsets::SOUTH)
    }

    #[must_use]
    pub const fn shift_west(self) -> Self {
        Self((self.0 & !Self::FILE_9.0) >> -offsets::WEST)
    }

    #[must_use]
    pub const fn shift_east(self) -> Self {
        Self((self.0 & !Self::FILE_1.0) << offsets::EAST)
    }

    #[must_use]
    pub const fn shift_north_west(self) -> Self {
        Self((self.0 & !(Self::RANK_A.0 | Self::FILE_9.0)) << offsets::NORTH_WEST)
    }

    #[must_use]
    pub const fn shift_north_east(self) -> Self {
        Self((self.0 & !(Self::RANK_A.0 | Self::FILE_1.0)) << offsets::NORTH_EAST)
    }

    #[must_use]
    pub const fn shift_south_west(self) -> Self {
        Self((self.0 & !Self::FILE_9.0) >> -offsets::SOUTH_WEST)
    }

    #[must_use]
    pub const fn shift_south_east(self) -> Self {
        Self((self.0 & !Self::FILE_1.0) >> -offsets::SOUTH_EAST)
    }

    #[must_use]
    pub const fn shift_north_relative(self, c: Color) -> Self {
        if c.eq(Color::SENTE) {
            self.shift_north()
        } else {
            self.shift_south()
        }
    }

    #[must_use]
    pub const fn shift_south_relative(self, c: Color) -> Self {
        if c.eq(Color::SENTE) {
            self.shift_south()
        } else {
            self.shift_north()
        }
    }

    #[must_use]
    pub const fn shift_west_relative(self, c: Color) -> Self {
        if c.eq(Color::SENTE) {
            self.shift_west()
        } else {
            self.shift_east()
        }
    }

    #[must_use]
    pub const fn shift_east_relative(self, c: Color) -> Self {
        if c.eq(Color::SENTE) {
            self.shift_east()
        } else {
            self.shift_west()
        }
    }

    #[must_use]
    pub const fn shift_north_west_relative(self, c: Color) -> Self {
        if c.eq(Color::SENTE) {
            self.shift_north_west()
        } else {
            self.shift_south_east()
        }
    }

    #[must_use]
    pub const fn shift_north_east_relative(self, c: Color) -> Self {
        if c.eq(Color::SENTE) {
            self.shift_north_east()
        } else {
            self.shift_south_west()
        }
    }

    #[must_use]
    pub const fn shift_south_west_relative(self, c: Color) -> Self {
        if c.eq(Color::SENTE) {
            self.shift_south_west()
        } else {
            self.shift_north_east()
        }
    }

    #[must_use]
    pub const fn shift_south_east_relative(self, c: Color) -> Self {
        if c.eq(Color::SENTE) {
            self.shift_south_east()
        } else {
            self.shift_north_west()
        }
    }

    #[must_use]
    pub const fn fill_up(self) -> Self {
        let mut bb = self.0;
        bb |= bb << 9;
        bb |= bb << 18;
        bb |= bb << 36;
        bb |= bb << 72;
        Self(bb)
    }

    #[must_use]
    pub const fn fill_down(self) -> Self {
        let mut bb = self.0;
        bb |= bb >> 9;
        bb |= bb >> 18;
        bb |= bb >> 36;
        bb |= bb >> 72;
        Self(bb)
    }

    #[must_use]
    pub const fn fill_file(self) -> Self {
        Self(self.fill_up().0 | self.fill_down().0)
    }
}

impl BitAnd for Bitboard {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl BitOr for Bitboard {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl BitXor for Bitboard {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.0 ^ rhs.0)
    }
}

impl BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0;
    }
}

impl Not for Bitboard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0) & Self::ALL
    }
}

impl Shl<i32> for Bitboard {
    type Output = Self;

    fn shl(self, rhs: i32) -> Self::Output {
        assert!(rhs >= 0);
        Self(self.0 << rhs) & Self::ALL
    }
}

impl ShlAssign<i32> for Bitboard {
    fn shl_assign(&mut self, rhs: i32) {
        assert!(rhs >= 0);
        self.0 = (self.0 << rhs) & Self::ALL.0;
    }
}

impl Shr<i32> for Bitboard {
    type Output = Self;

    fn shr(self, rhs: i32) -> Self::Output {
        assert!(rhs >= 0);
        Self(self.0 >> rhs)
    }
}

impl ShrAssign<i32> for Bitboard {
    fn shr_assign(&mut self, rhs: i32) {
        assert!(rhs >= 0);
        self.0 <<= rhs;
    }
}

impl Display for Bitboard {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for rank in (0..9).rev() {
            for file in 0..9 {
                if file > 0 {
                    write!(f, " ")?;
                }
                write!(
                    f,
                    "{}",
                    if self.get_sq(Square::from_file_rank(file, rank)) {
                        '1'
                    } else {
                        '.'
                    }
                )?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

impl IntoIterator for Bitboard {
    type Item = Square;
    type IntoIter = Biterator;

    fn into_iter(self) -> Self::IntoIter {
        Biterator { board: self }
    }
}

pub struct Biterator {
    board: Bitboard,
}

impl Iterator for Biterator {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        self.board.pop_lsb()
    }
}
