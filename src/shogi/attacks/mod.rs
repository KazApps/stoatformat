#![allow(long_running_const_eval)]

use crate::shogi::bitboard::{offsets, Bitboard};
use crate::shogi::core::{Color, Square};

macro_rules! attack_init {
    (| mut $a:ident, $s:ident, $b:ident | $($r:tt)+) => {{
        let mut _res = [Bitboard::EMPTY; Square::COUNT];
        let mut _sq_idx = 0;
        while _sq_idx < Square::COUNT as u8 {
            let $s = Square::from_raw(_sq_idx);
            let $b = $s.bit();
            let mut $a = Bitboard::EMPTY;
            {$($r)+};
            _res[$s.idx()] = $a;
            _sq_idx += 1;
        }
        _res
    }}
}

macro_rules! sided_attack_init {
    (| mut $a:ident, $c:ident, $s:ident, $b:ident | $($r:tt)+) => {{
        let mut _res = [[Bitboard::EMPTY; Square::COUNT]; Color::COUNT];
        let mut _c_idx = 0;
        while _c_idx < Color::COUNT as u8 {
            let $c = Color::from_raw(_c_idx);
            let mut _sq_idx = 0;
            while _sq_idx < Square::COUNT as u8 {
                let $s = Square::from_raw(_sq_idx);
                let $b = $s.bit();
                let mut $a = Bitboard::EMPTY;
                {$($r)+};
                _res[$c.idx()][$s.idx()] = $a;
                _sq_idx += 1;
            }
            _c_idx += 1;
        }
        _res
    }}
}

const PAWN_ATTACKS: [[Bitboard; Square::COUNT]; Color::COUNT] =
    sided_attack_init!(|mut attacks, c, _sq, bit| {
        attacks = attacks.or(bit.shift_north_relative(c));
    });

const KNIGHT_ATTACKS: [[Bitboard; Square::COUNT]; Color::COUNT] =
    sided_attack_init!(|mut attacks, c, _sq, bit| {
        attacks = attacks.or(bit.shift_north_relative(c).shift_north_west_relative(c));
        attacks = attacks.or(bit.shift_north_relative(c).shift_north_east_relative(c));
    });

const SILVER_ATTACKS: [[Bitboard; Square::COUNT]; Color::COUNT] =
    sided_attack_init!(|mut attacks, c, _sq, bit| {
        attacks = attacks.or(bit.shift_north_west());
        attacks = attacks.or(bit.shift_north_east());
        attacks = attacks.or(bit.shift_south_west());
        attacks = attacks.or(bit.shift_south_east());

        attacks = attacks.or(bit.shift_north_relative(c));
    });

const GOLD_ATTACKS: [[Bitboard; Square::COUNT]; Color::COUNT] =
    sided_attack_init!(|mut attacks, c, _sq, bit| {
        attacks = attacks.or(bit.shift_north());
        attacks = attacks.or(bit.shift_south());
        attacks = attacks.or(bit.shift_west());
        attacks = attacks.or(bit.shift_east());

        attacks = attacks.or(bit.shift_north_west_relative(c));
        attacks = attacks.or(bit.shift_north_east_relative(c));
    });

const KING_ATTACKS: [Bitboard; Square::COUNT] = attack_init!(|mut attacks, _sq, bit| {
    attacks = attacks.or(bit.shift_north());
    attacks = attacks.or(bit.shift_south());
    attacks = attacks.or(bit.shift_west());
    attacks = attacks.or(bit.shift_east());

    attacks = attacks.or(bit.shift_north_west());
    attacks = attacks.or(bit.shift_north_east());
    attacks = attacks.or(bit.shift_south_west());
    attacks = attacks.or(bit.shift_south_east());
});

#[must_use]
const fn edges(dir: i32) -> Bitboard {
    match dir {
        offsets::NORTH => Bitboard::RANK_A,
        offsets::SOUTH => Bitboard::RANK_I,
        offsets::WEST => Bitboard::FILE_9,
        offsets::EAST => Bitboard::FILE_1,
        offsets::NORTH_WEST => Bitboard(Bitboard::RANK_A.0 | Bitboard::FILE_9.0),
        offsets::NORTH_EAST => Bitboard(Bitboard::RANK_A.0 | Bitboard::FILE_1.0),
        offsets::SOUTH_WEST => Bitboard(Bitboard::RANK_I.0 | Bitboard::FILE_9.0),
        offsets::SOUTH_EAST => Bitboard(Bitboard::RANK_I.0 | Bitboard::FILE_1.0),
        _ => unreachable!(),
    }
}

#[must_use]
const fn generate_sliding_attacks(offset: i32, sq: Square, occ: Bitboard) -> Bitboard {
    let mut blockers = edges(offset);
    let mut bit = sq.bit();

    if !blockers.and(bit).empty() {
        return Bitboard::EMPTY;
    }

    blockers = blockers.or(occ);

    let right = offset < 0;
    let shift = offset.unsigned_abs();

    let mut dst = Bitboard::EMPTY;

    loop {
        if right {
            bit = bit.bit_shr(shift);
        } else {
            bit = bit.bit_shl(shift);
        }

        dst = dst.or(bit);

        if !blockers.and(bit).empty() {
            break;
        }
    }

    dst
}

#[must_use]
const fn generate_multi_sliding_attacks(offsets: &[i32; 4], sq: Square, occ: Bitboard) -> Bitboard {
    let mut dst = Bitboard::EMPTY;

    let mut i = 0;
    while i < offsets.len() {
        dst = dst.or(generate_sliding_attacks(offsets[i], sq, occ));
        i += 1;
    }

    dst
}

#[must_use]
pub const fn pawn_attacks(sq: Square, c: Color) -> Bitboard {
    PAWN_ATTACKS[c.idx()][sq.idx()]
}

#[must_use]
pub const fn lance_attacks(sq: Square, c: Color, occ: Bitboard) -> Bitboard {
    if c.eq(Color::SENTE) {
        generate_sliding_attacks(offsets::NORTH, sq, occ)
    } else {
        generate_sliding_attacks(offsets::SOUTH, sq, occ)
    }
}

#[must_use]
pub const fn knight_attacks(sq: Square, c: Color) -> Bitboard {
    KNIGHT_ATTACKS[c.idx()][sq.idx()]
}

#[must_use]
pub const fn silver_attacks(sq: Square, c: Color) -> Bitboard {
    SILVER_ATTACKS[c.idx()][sq.idx()]
}

#[must_use]
pub const fn gold_attacks(sq: Square, c: Color) -> Bitboard {
    GOLD_ATTACKS[c.idx()][sq.idx()]
}

#[must_use]
pub const fn bishop_attacks(sq: Square, occ: Bitboard) -> Bitboard {
    generate_multi_sliding_attacks(
        &[
            offsets::NORTH_WEST,
            offsets::NORTH_EAST,
            offsets::SOUTH_WEST,
            offsets::SOUTH_EAST,
        ],
        sq,
        occ,
    )
}

#[must_use]
pub const fn rook_attacks(sq: Square, occ: Bitboard) -> Bitboard {
    generate_multi_sliding_attacks(
        &[offsets::NORTH, offsets::SOUTH, offsets::WEST, offsets::EAST],
        sq,
        occ,
    )
}

#[must_use]
pub const fn king_attacks(sq: Square) -> Bitboard {
    KING_ATTACKS[sq.idx()]
}

#[must_use]
pub const fn promoted_bishop_attacks(sq: Square, occ: Bitboard) -> Bitboard {
    bishop_attacks(sq, occ).or(king_attacks(sq))
}

#[must_use]
pub const fn promoted_rook_attacks(sq: Square, occ: Bitboard) -> Bitboard {
    rook_attacks(sq, occ).or(king_attacks(sq))
}
