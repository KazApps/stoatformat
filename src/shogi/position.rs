use crate::shogi::attacks::*;
use crate::shogi::bitboard::Bitboard;
use crate::shogi::core::*;
use crate::shogi::shogimove::Move;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct Hand(u32);

impl Hand {
    const PAWN_BITS: i32 = 5;
    const LANCE_BITS: i32 = 3;
    const KNIGHT_BITS: i32 = 3;
    const SILVER_BITS: i32 = 3;
    const GOLD_BITS: i32 = 3;
    const BISHOP_BITS: i32 = 2;
    const ROOK_BITS: i32 = 2;

    const PAWN_OFFSET: i32 = 0;
    const LANCE_OFFSET: i32 = Self::PAWN_OFFSET + Self::PAWN_BITS;
    const KNIGHT_OFFSET: i32 = Self::LANCE_OFFSET + Self::LANCE_BITS;
    const SILVER_OFFSET: i32 = Self::KNIGHT_OFFSET + Self::KNIGHT_BITS;
    const GOLD_OFFSET: i32 = Self::SILVER_OFFSET + Self::SILVER_BITS;
    const BISHOP_OFFSET: i32 = Self::GOLD_OFFSET + Self::GOLD_BITS;
    const ROOK_OFFSET: i32 = Self::BISHOP_OFFSET + Self::BISHOP_BITS;

    const OFFSETS: [Option<i32>; PieceType::COUNT] = {
        let mut result = [None; PieceType::COUNT];

        result[PieceType::PAWN.idx()] = Some(Self::PAWN_OFFSET);
        result[PieceType::LANCE.idx()] = Some(Self::LANCE_OFFSET);
        result[PieceType::KNIGHT.idx()] = Some(Self::KNIGHT_OFFSET);
        result[PieceType::SILVER.idx()] = Some(Self::SILVER_OFFSET);
        result[PieceType::GOLD.idx()] = Some(Self::GOLD_OFFSET);
        result[PieceType::BISHOP.idx()] = Some(Self::BISHOP_OFFSET);
        result[PieceType::ROOK.idx()] = Some(Self::ROOK_OFFSET);

        result
    };

    const MASKS: [u32; PieceType::COUNT] = {
        let mut result = [0; PieceType::COUNT];

        result[PieceType::PAWN.idx()] = ((1u32 << Self::PAWN_BITS) - 1) << Self::PAWN_OFFSET;
        result[PieceType::LANCE.idx()] = ((1u32 << Self::LANCE_BITS) - 1) << Self::LANCE_OFFSET;
        result[PieceType::KNIGHT.idx()] = ((1u32 << Self::KNIGHT_BITS) - 1) << Self::KNIGHT_OFFSET;
        result[PieceType::SILVER.idx()] = ((1u32 << Self::SILVER_BITS) - 1) << Self::SILVER_OFFSET;
        result[PieceType::GOLD.idx()] = ((1u32 << Self::GOLD_BITS) - 1) << Self::GOLD_OFFSET;
        result[PieceType::BISHOP.idx()] = ((1u32 << Self::BISHOP_BITS) - 1) << Self::BISHOP_OFFSET;
        result[PieceType::ROOK.idx()] = ((1u32 << Self::ROOK_BITS) - 1) << Self::ROOK_OFFSET;

        result
    };

    #[must_use]
    pub const fn from_raw(raw: u32) -> Self {
        Self(raw)
    }

    #[must_use]
    pub fn raw(self) -> u32 {
        self.0
    }

    #[must_use]
    pub fn empty(self) -> bool {
        self.0 == 0
    }

    #[must_use]
    pub fn count(self, pt: PieceType) -> u32 {
        let offset = Self::OFFSETS[pt.idx()].unwrap();
        let mask = Self::MASKS[pt.idx()];
        (self.0 & mask) >> offset
    }

    pub fn increment_get(&mut self, pt: PieceType) -> u32 {
        let curr = self.count(pt);
        assert!(curr < (Self::MASKS[pt.idx()] >> Self::OFFSETS[pt.idx()].unwrap()));
        assert!(curr < pt.max_in_hand());
        self.set(pt, curr + 1);
        curr + 1
    }

    pub fn decrement_get(&mut self, pt: PieceType) -> u32 {
        let curr = self.count(pt);
        assert!(curr > 0);
        self.set(pt, curr - 1);
        curr - 1
    }

    pub fn set(&mut self, pt: PieceType, count: u32) {
        let offset = Self::OFFSETS[pt.idx()].unwrap();
        let mask = Self::MASKS[pt.idx()];

        assert!(count <= (mask >> offset));
        assert!(count <= pt.max_in_hand());

        self.0 = (self.0 & !mask) | (count << offset);
    }
}

const _HAND_FITS_IN_U32: () = assert!(Hand::ROOK_OFFSET + Hand::ROOK_BITS <= 32);

impl Display for Hand {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut first = true;

        for pt in [
            PieceType::ROOK,
            PieceType::BISHOP,
            PieceType::GOLD,
            PieceType::SILVER,
            PieceType::KNIGHT,
            PieceType::LANCE,
            PieceType::PAWN,
        ] {
            let count = self.count(pt);

            if count == 0 {
                continue;
            }

            if !first {
                write!(f, " ")?;
            } else {
                first = false;
            }

            if count > 1 {
                write!(f, "{}", count)?;
            }

            write!(f, "{}", pt)?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SfenError {
    NotEnoughParts,
    TooManyParts,
    NotEnoughRanks,
    TooManyRanks,
    NotEnoughFiles(u32),
    TooManyFiles(u32),
    InvalidChar(char),
    InvalidPromotion(PieceType, u32),
    DanglingPromotion(u32),
    InvalidKings,
    InvalidStm,
    InvalidHandPiece(char),
    DuplicateHandPiece(Color, PieceType),
    TooManyPiecesInHand(Color, PieceType, u32),
    DanglingHandPieceCount,
    ZeroPlyCount,
    InvalidPlyCount,
    TooManyPieces(u32),
}

impl Display for SfenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SfenError::NotEnoughParts => write!(f, "Incomplete SFEN"),
            SfenError::TooManyParts => write!(f, "Too many SFEN parts"),
            SfenError::NotEnoughRanks => write!(f, "Not enough ranks"),
            SfenError::TooManyRanks => write!(f, "Too many ranks"),
            SfenError::NotEnoughFiles(rank) => write!(f, "Not enough files in rank {}", rank + 1),
            SfenError::TooManyFiles(rank) => write!(f, "Too many files in rank {}", rank + 1),
            SfenError::InvalidChar(c) => write!(f, "Invalid character '{}'", c),
            SfenError::InvalidPromotion(pt, rank) => {
                write!(f, "Invalid promoted piece {pt} in rank {}", rank + 1)
            }
            SfenError::DanglingPromotion(rank) => {
                write!(f, "Dangling promotion character '+' in rank {}", rank + 1)
            }
            SfenError::InvalidKings => {
                write!(f, "Invalid kings (each side must have exactly 1)")
            }
            SfenError::InvalidStm => write!(f, "Invalid side to move"),
            SfenError::InvalidHandPiece(c) => write!(f, "Invalid piece character in hand '{}'", c),
            SfenError::TooManyPiecesInHand(c, pt, count) => {
                write!(
                    f,
                    "Too many {pt} in {} hand: max {}, count {count}",
                    if *c == Color::SENTE { "sente" } else { "gote" },
                    pt.max_in_hand()
                )
            }
            SfenError::DuplicateHandPiece(c, pt) => write!(
                f,
                "Duplicate count for {pt} in {} hand",
                if *c == Color::SENTE { "sente" } else { "gote" },
            ),
            SfenError::DanglingHandPieceCount => write!(f, "Dangling piece count in hand"),
            SfenError::ZeroPlyCount => write!(f, "Ply count must be greater than 0"),
            SfenError::InvalidPlyCount => write!(f, "Invalid ply count"),
            SfenError::TooManyPieces(count) => {
                write!(f, "Too many pieces: max 40, count {}", count)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Position {
    colors: [Bitboard; Color::COUNT],
    pieces: [Bitboard; PieceType::COUNT],

    mailbox: [Option<Piece>; Square::COUNT],

    hands: [Hand; Color::COUNT],

    stm: Color,

    ply_count: u16,
}

pub const POS_SIZE: usize = size_of::<Position>();

impl Position {
    #[must_use]
    pub fn startpos() -> Self {
        let mut pos = Self::default();

        *pos.color_bb_mut(Color::SENTE) = Bitboard(0x7fd05ff);
        *pos.color_bb_mut(Color::GOTE) = Bitboard(0x1ff417fc0000000000000);

        *pos.piece_type_bb_mut(PieceType::PAWN) = Bitboard(0x7fc0000007fc0000);
        *pos.piece_type_bb_mut(PieceType::LANCE) = Bitboard(0x101000000000000000101);
        *pos.piece_type_bb_mut(PieceType::KNIGHT) = Bitboard(0x82000000000000000082);
        *pos.piece_type_bb_mut(PieceType::SILVER) = Bitboard(0x44000000000000000044);
        *pos.piece_type_bb_mut(PieceType::GOLD) = Bitboard(0x28000000000000000028);
        *pos.piece_type_bb_mut(PieceType::KING) = Bitboard(0x10000000000000000010);
        *pos.piece_type_bb_mut(PieceType::BISHOP) = Bitboard(0x400000000000000400);
        *pos.piece_type_bb_mut(PieceType::ROOK) = Bitboard(0x10000000000010000);

        pos.regen();

        pos
    }

    pub fn from_sfen_parts(parts: &[&str]) -> Result<Self, SfenError> {
        if parts.len() < 3 {
            return Err(SfenError::NotEnoughParts);
        }

        if parts.len() > 4 {
            return Err(SfenError::TooManyParts);
        }

        let mut builder = PositionBuilder::new();

        let mut rank_idx = 0;

        for rank in parts[0].split('/') {
            if rank_idx >= 9 {
                return Err(SfenError::TooManyRanks);
            }

            let mut file_idx = 0;
            let mut next_promoted = false;

            for c in rank.chars() {
                if file_idx >= 9 {
                    return Err(SfenError::TooManyFiles(rank_idx));
                }

                if let Some(empty_squares) = c.to_digit(10) {
                    file_idx += empty_squares;
                } else if c == '+' {
                    next_promoted = true;
                } else {
                    let mut piece: Piece = c.try_into().map_err(|_| SfenError::InvalidChar(c))?;
                    if next_promoted {
                        let pt = piece.piece_type();
                        match pt.promoted() {
                            Some(promoted) => piece = promoted.with_color(piece.color()),
                            None => return Err(SfenError::InvalidPromotion(pt, rank_idx)),
                        }
                        next_promoted = false;
                    }
                    let sq = Square::from_file_rank(file_idx, 8 - rank_idx);
                    builder.add_piece(sq, piece);
                    file_idx += 1;
                }
            }

            if next_promoted {
                return Err(SfenError::DanglingPromotion(rank_idx));
            }

            if file_idx > 9 {
                return Err(SfenError::TooManyFiles(rank_idx));
            } else if file_idx < 9 {
                return Err(SfenError::NotEnoughFiles(rank_idx));
            }

            rank_idx += 1;
        }

        if rank_idx < 9 {
            return Err(SfenError::NotEnoughRanks);
        }

        match parts[1] {
            "b" => builder.set_stm(Color::SENTE),
            "w" => builder.set_stm(Color::GOTE),
            _ => return Err(SfenError::InvalidStm),
        };

        let mut hands = [Hand::default(); 2];

        if parts[2] != "-" {
            let mut next_count = 1;
            let mut has_next_count = false;

            for c in parts[2].chars() {
                if let Some(digit) = c.to_digit(10) {
                    if has_next_count {
                        next_count *= 10;
                        next_count += digit;
                    } else {
                        next_count = digit;
                    }
                    has_next_count = true;
                } else {
                    let piece: Piece = c.try_into().map_err(|_| SfenError::InvalidHandPiece(c))?;
                    let max = piece.piece_type().max_in_hand();

                    if max == 0 {
                        return Err(SfenError::InvalidHandPiece(c));
                    }

                    if next_count > max {
                        return Err(SfenError::TooManyPiecesInHand(
                            piece.color(),
                            piece.piece_type(),
                            next_count,
                        ));
                    }

                    let hand = &mut hands[piece.color().idx()];

                    if hand.count(piece.piece_type()) > 0 {
                        return Err(SfenError::DuplicateHandPiece(
                            piece.color(),
                            piece.piece_type(),
                        ));
                    }

                    hand.set(piece.piece_type(), next_count);

                    next_count = 1;
                    has_next_count = false;
                }
            }

            if has_next_count {
                return Err(SfenError::DanglingHandPieceCount);
            }

            builder.set_hand(Color::SENTE, hands[0])?;
            builder.set_hand(Color::GOTE, hands[1])?;
        }

        if parts.len() == 4 {
            let ply_count: u16 = parts[3].parse().map_err(|_| SfenError::InvalidPlyCount)?;
            builder.set_ply_count(ply_count)?;
        }

        let pos = builder.build()?;

        if pos.piece_bb(Piece::SENTE_KING).popcount() != 1
            || pos.piece_bb(Piece::GOTE_KING).popcount() != 1
        {
            return Err(SfenError::InvalidKings);
        }

        Ok(pos)
    }

    #[must_use]
    pub fn occupancy(&self) -> Bitboard {
        self.colors[0] | self.colors[1]
    }

    #[must_use]
    pub fn color_bb(&self, c: Color) -> Bitboard {
        self.colors[c.idx()]
    }

    #[must_use]
    fn color_bb_mut(&mut self, c: Color) -> &mut Bitboard {
        &mut self.colors[c.idx()]
    }

    #[must_use]
    pub fn piece_type_bb(&self, pt: PieceType) -> Bitboard {
        self.pieces[pt.idx()]
    }

    #[must_use]
    fn piece_type_bb_mut(&mut self, pt: PieceType) -> &mut Bitboard {
        &mut self.pieces[pt.idx()]
    }

    #[must_use]
    pub fn piece_bb(&self, piece: Piece) -> Bitboard {
        self.color_bb(piece.color()) & self.piece_type_bb(piece.piece_type())
    }

    #[must_use]
    pub fn hand(&self, c: Color) -> Hand {
        self.hands[c.idx()]
    }

    #[must_use]
    fn hand_mut(&mut self, c: Color) -> &mut Hand {
        &mut self.hands[c.idx()]
    }

    #[must_use]
    pub fn stm(&self) -> Color {
        self.stm
    }

    #[must_use]
    pub fn ply_count(&self) -> u16 {
        self.ply_count
    }

    pub fn piece_on(&self, sq: Square) -> Option<Piece> {
        self.mailbox[sq.idx()]
    }

    #[must_use]
    pub fn is_capture(&self, mv: Move) -> bool {
        self.piece_on(mv.to()).is_some()
    }

    #[must_use]
    pub fn is_in_check(&self) -> bool {
        let nstm = self.stm.flip();

        let king = self
            .piece_bb(PieceType::KING.with_color(self.stm))
            .lsb()
            .unwrap();

        let pawns = self.piece_bb(PieceType::PAWN.with_color(nstm));
        let pawn_attacks = pawn_attacks(king, self.stm);

        if !(pawns & pawn_attacks).empty() {
            return true;
        }

        let knights = self.piece_bb(PieceType::KNIGHT.with_color(nstm));
        let attacks = knight_attacks(king, self.stm);

        if !(knights & attacks).empty() {
            return true;
        }

        let silvers = self.piece_bb(PieceType::SILVER.with_color(nstm));
        let attacks = silver_attacks(king, self.stm);

        if !(silvers & attacks).empty() {
            return true;
        }

        let golds = self.piece_bb(PieceType::GOLD.with_color(nstm))
            | self.piece_bb(PieceType::PROMOTED_PAWN.with_color(nstm))
            | self.piece_bb(PieceType::PROMOTED_LANCE.with_color(nstm))
            | self.piece_bb(PieceType::PROMOTED_KNIGHT.with_color(nstm))
            | self.piece_bb(PieceType::PROMOTED_SILVER.with_color(nstm));
        let attacks = gold_attacks(king, self.stm);

        if !(golds & attacks).empty() {
            return true;
        }

        let promoted_bishops = self.piece_bb(PieceType::PROMOTED_BISHOP.with_color(nstm));
        let promoted_rooks = self.piece_bb(PieceType::PROMOTED_ROOK.with_color(nstm));

        let kings =
            promoted_bishops | promoted_rooks | self.piece_bb(PieceType::KING.with_color(nstm));
        let attacks = king_attacks(king);

        if !(kings & attacks).empty() {
            return true;
        }

        let occ = self.occupancy();

        let rooks = promoted_rooks | self.piece_bb(PieceType::ROOK.with_color(nstm));

        let lances = rooks | self.piece_bb(PieceType::LANCE.with_color(nstm));
        let attacks = lance_attacks(king, self.stm, occ);

        if !(lances & attacks).empty() {
            return true;
        }

        let bishops = promoted_bishops | self.piece_bb(PieceType::BISHOP.with_color(nstm));
        let attacks = bishop_attacks(king, occ);

        if !(bishops & attacks).empty() {
            return true;
        }

        let attacks = rook_attacks(king, occ);

        if !(rooks & attacks).empty() {
            return true;
        }

        false
    }

    #[must_use]
    pub fn apply_move(&self, mv: Move) -> Self {
        let mut new_pos = *self;

        if mv.is_drop() {
            let piece = mv.drop_piece().with_color(self.stm);
            new_pos.drop_piece(mv.to(), piece);
        } else {
            let piece = self.piece_on(mv.from()).unwrap();
            if mv.is_promo() {
                new_pos.promote_piece(mv.from(), mv.to(), piece);
            } else {
                new_pos.move_piece(mv.from(), mv.to(), piece);
            }
        }

        new_pos.ply_count += 1;
        new_pos.stm = self.stm.flip();

        new_pos
    }

    fn add_piece(&mut self, sq: Square, piece: Piece) {
        assert!(self.piece_on(sq).is_none());

        *self.color_bb_mut(piece.color()) |= sq.bit();
        *self.piece_type_bb_mut(piece.piece_type()) |= sq.bit();

        self.mailbox[sq.idx()] = Some(piece);
    }

    fn move_piece(&mut self, from: Square, to: Square, piece: Piece) {
        assert_ne!(from, to);

        if let Some(captured) = self.piece_on(to) {
            self.capture_piece(to, captured);
        }

        *self.color_bb_mut(piece.color()) ^= from.bit() ^ to.bit();
        *self.piece_type_bb_mut(piece.piece_type()) ^= from.bit() ^ to.bit();

        self.mailbox[from.idx()] = None;
        self.mailbox[to.idx()] = Some(piece);
    }

    fn promote_piece(&mut self, from: Square, to: Square, piece: Piece) {
        assert_ne!(from, to);

        if let Some(captured) = self.piece_on(to) {
            self.capture_piece(to, captured);
        }

        let promoted_pt = piece.piece_type().promoted().unwrap();

        *self.color_bb_mut(piece.color()) ^= from.bit() ^ to.bit();

        *self.piece_type_bb_mut(piece.piece_type()) ^= from.bit() ^ to.bit();
        *self.piece_type_bb_mut(promoted_pt) ^= from.bit() ^ to.bit();

        self.mailbox[from.idx()] = None;
        self.mailbox[to.idx()] = Some(promoted_pt.with_color(piece.color()));
    }

    fn capture_piece(&mut self, sq: Square, piece: Piece) {
        assert_ne!(piece.piece_type(), PieceType::KING);

        *self.color_bb_mut(piece.color()) ^= sq.bit();
        *self.piece_type_bb_mut(piece.piece_type()) ^= sq.bit();

        let hand_pt = piece.piece_type().unpromoted();
        self.hand_mut(piece.color().flip()).increment_get(hand_pt);
    }

    fn drop_piece(&mut self, sq: Square, piece: Piece) {
        assert!(!piece.piece_type().is_promoted());
        assert_ne!(piece.piece_type(), PieceType::KING);

        self.add_piece(sq, piece);
        self.hand_mut(self.stm).decrement_get(piece.piece_type());
    }

    pub fn sfen(&self) -> String {
        use std::fmt::Write as _;

        let mut sfen = String::new();

        for rank in (0..9).rev() {
            let mut file = 0;

            while file < 9 {
                let sq = Square::from_file_rank(file, rank);
                match self.piece_on(sq) {
                    None => {
                        let mut empty_squares = 1;

                        while file < 8
                            && self
                                .piece_on(Square::from_file_rank(file + 1, rank))
                                .is_none()
                        {
                            empty_squares += 1;
                            file += 1;
                        }

                        write!(sfen, "{}", empty_squares).unwrap();
                    }
                    Some(piece) => write!(sfen, "{}", piece).unwrap(),
                }

                file += 1;
            }

            if rank > 0 {
                sfen.push('/');
            }
        }

        if self.stm == Color::SENTE {
            write!(sfen, " b ").unwrap();
        } else {
            write!(sfen, " w ").unwrap();
        }

        let sente_hand = self.hand(Color::SENTE);
        let gote_hand = self.hand(Color::GOTE);

        if sente_hand.empty() && gote_hand.empty() {
            write!(sfen, "-").unwrap();
        } else {
            let mut write_hand = |c, hand: &Hand| {
                for pt in [
                    PieceType::ROOK,
                    PieceType::BISHOP,
                    PieceType::GOLD,
                    PieceType::SILVER,
                    PieceType::KNIGHT,
                    PieceType::LANCE,
                    PieceType::PAWN,
                ] {
                    let count = hand.count(pt);

                    if count == 0 {
                        continue;
                    }

                    if count > 1 {
                        write!(sfen, "{}", count).unwrap();
                    }

                    write!(sfen, "{}", pt.with_color(c)).unwrap();
                }
            };

            write_hand(Color::SENTE, &sente_hand);
            write_hand(Color::GOTE, &gote_hand);
        }

        write!(sfen, " {}", self.ply_count).unwrap();

        sfen
    }

    fn regen(&mut self) {
        self.mailbox.fill(None);

        for piece in Piece::all() {
            for sq in self.piece_bb(piece) {
                assert!(self.mailbox[sq.idx()].is_none());
                self.mailbox[sq.idx()] = Some(piece);
            }
        }
    }
}

impl Default for Position {
    fn default() -> Self {
        Position {
            colors: [Bitboard::EMPTY; Color::COUNT],
            pieces: [Bitboard::EMPTY; PieceType::COUNT],
            mailbox: [None; Square::COUNT],
            hands: [Hand::default(); Color::COUNT],
            stm: Color::SENTE,
            ply_count: 1,
        }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "   9   8   7   6   5   4   3   2   1")?;
        writeln!(f, " +---+---+---+---+---+---+---+---+---+")?;

        for rank in (0..9).rev() {
            for file in 0..9 {
                if let Some(piece) = self.piece_on(Square::from_file_rank(file, rank)) {
                    if piece.piece_type().is_promoted() {
                        write!(f, " |{}", piece)?;
                    } else {
                        write!(f, " | {}", piece)?;
                    }
                } else {
                    write!(f, " |  ")?;
                }
            }

            writeln!(f, " | {}", char::from(b'a' + 8 - rank as u8))?;
            writeln!(f, " +---+---+---+---+---+---+---+---+---+")?;
        }

        writeln!(f)?;

        writeln!(f, "Sente pieces in hand: {}", self.hand(Color::SENTE))?;
        writeln!(f, "Gote pieces in hand: {}", self.hand(Color::GOTE))?;

        writeln!(f)?;

        if self.stm == Color::SENTE {
            writeln!(f, "Sente to move")?;
        } else {
            writeln!(f, "Gote to move")?;
        }

        Ok(())
    }
}

impl FromStr for Position {
    type Err = SfenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split = s.split_ascii_whitespace().collect::<Vec<&str>>();
        Self::from_sfen_parts(&split)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PositionBuilderError {
    MissingStm,
    TooManyPiecesInHand(Color, PieceType, u32),
    ZeroPlyCount,
    TooManyPieces(u32),
}

impl Display for PositionBuilderError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PositionBuilderError::MissingStm => write!(f, "Missing side to move"),
            PositionBuilderError::TooManyPiecesInHand(c, pt, count) => {
                write!(
                    f,
                    "Too many {pt} in {} hand! max: {}, count: {count}",
                    if *c == Color::SENTE { "sente" } else { "gote" },
                    pt.max_in_hand()
                )
            }
            PositionBuilderError::ZeroPlyCount => write!(f, "Ply count must be greater than 0"),
            PositionBuilderError::TooManyPieces(count) => {
                write!(f, "Too many pieces: max 40, count {}", count)
            }
        }
    }
}

impl From<PositionBuilderError> for SfenError {
    fn from(value: PositionBuilderError) -> Self {
        match value {
            PositionBuilderError::TooManyPiecesInHand(c, pt, count) => {
                SfenError::TooManyPiecesInHand(c, pt, count)
            }
            PositionBuilderError::ZeroPlyCount => SfenError::ZeroPlyCount,
            PositionBuilderError::TooManyPieces(count) => SfenError::TooManyPieces(count),
            _ => unreachable!(),
        }
    }
}

pub struct PositionBuilder {
    pos: Position,
    stm: Option<Color>,
    piece_count: u32,
}

impl PositionBuilder {
    pub fn new() -> Self {
        Self {
            pos: Position::default(),
            stm: None,
            piece_count: 0,
        }
    }

    pub fn add_piece(&mut self, sq: Square, piece: Piece) -> &mut Self {
        self.pos.add_piece(sq, piece);
        self.piece_count += 1;
        self
    }

    pub fn set_hand_count(&mut self, c: Color, pt: PieceType, count: u32) -> &mut Self {
        assert_eq!(self.pos.hand(c).count(pt), 0);
        self.pos.hand_mut(c).set(pt, count);
        self.piece_count += count;
        self
    }

    pub fn set_hand(&mut self, c: Color, hand: Hand) -> Result<&mut Self, PositionBuilderError> {
        assert!(self.pos.hand(c).empty());
        *self.pos.hand_mut(c) = hand;

        for pt in [
            PieceType::PAWN,
            PieceType::LANCE,
            PieceType::KNIGHT,
            PieceType::SILVER,
            PieceType::GOLD,
            PieceType::BISHOP,
            PieceType::ROOK,
        ] {
            let count = hand.count(pt);
            if count > pt.max_in_hand() {
                return Err(PositionBuilderError::TooManyPiecesInHand(
                    c,
                    pt,
                    pt.max_in_hand(),
                ));
            }
            self.piece_count += count;
        }

        Ok(self)
    }

    pub fn set_stm(&mut self, stm: Color) -> &mut Self {
        self.stm = Some(stm);
        self
    }

    pub fn set_ply_count(&mut self, ply_count: u16) -> Result<&mut Self, PositionBuilderError> {
        if ply_count == 0 {
            return Err(PositionBuilderError::ZeroPlyCount);
        }
        self.pos.ply_count = ply_count;
        Ok(self)
    }

    pub fn build(self) -> Result<Position, PositionBuilderError> {
        if self.stm.is_none() {
            return Err(PositionBuilderError::MissingStm);
        }

        if self.piece_count > 40 {
            return Err(PositionBuilderError::TooManyPieces(self.piece_count));
        }

        let mut pos = self.pos;
        pos.stm = self.stm.unwrap();
        Ok(pos)
    }
}

impl Default for PositionBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sfen_roundtrip() {
        let sfen = "8l/1l+R2P3/p2pBG1pp/kps1p4/Nn1P2G2/P1P1P2PP/1PS6/1KSG3+r1/LN2+p3L w Sbgn3p 124";
        let pos = sfen.parse::<Position>().unwrap();

        assert_eq!(sfen, pos.sfen());
    }

    #[test]
    fn sfen_ok() {
        assert!(
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1"
                .parse::<Position>()
                .is_ok()
        );

        assert!(
            "8l/1l+R2P3/p2pBG1pp/kps1p4/Nn1P2G2/P1P1P2PP/1PS6/1KSG3+r1/LN2+p3L w Sbgn3p 124"
                .parse::<Position>()
                .is_ok()
        );
    }

    #[test]
    fn sfen_err() {
        // not enough parts
        assert!(
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b"
                .parse::<Position>()
                .is_err()
        );

        // too many parts
        assert!(
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1 2"
                .parse::<Position>()
                .is_err()
        );

        // not enough ranks
        assert!("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1 b - 1"
            .parse::<Position>()
            .is_err());

        // too many ranks
        assert!(
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL/LNSGKGSNL b - 1"
                .parse::<Position>()
                .is_err()
        );

        // too many files (piece)
        assert!(
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPPR/1B5R1/LNSGKGSNL b - 1"
                .parse::<Position>()
                .is_err()
        );

        // too many files (empty)
        assert!(
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP1/1B5R1/LNSGKGSNL b - 1"
                .parse::<Position>()
                .is_err()
        );

        // not enough files
        assert!(
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPP/1B5R1/LNSGKGSNL b - 1"
                .parse::<Position>()
                .is_err()
        );

        // too many pieces
        assert!(
            "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL/LNSGKGSNL b 1P 1"
                .parse::<Position>()
                .is_err()
        );
    }

    #[test]
    fn check() {
        assert!(!Position::startpos().is_in_check());

        assert!(
            !"8l/1l+R2P3/p2pBG1pp/kps1p4/Nn1P2G2/P1P1P2PP/1PS6/1KSG3+r1/LN2+p3L w Sbgn3p 124"
                .parse::<Position>()
                .unwrap()
                .is_in_check()
        );

        assert!("k8/9/9/4p4/4K4/9/9/9/9 b - 1"
            .parse::<Position>()
            .unwrap()
            .is_in_check());

        assert!(!"k8/9/9/4K4/4p4/9/9/9/9 b - 1"
            .parse::<Position>()
            .unwrap()
            .is_in_check());

        assert!("k8/9/2b6/9/4K4/9/9/9/9 b - 1"
            .parse::<Position>()
            .unwrap()
            .is_in_check());

        assert!(!"k8/9/9/9/4K4/9/9/4l4/9 b - 1"
            .parse::<Position>()
            .unwrap()
            .is_in_check());

        assert!("k8/4l4/9/9/4K4/9/9/9/9 b - 1"
            .parse::<Position>()
            .unwrap()
            .is_in_check());
    }
}
