#include <iostream>
#include <iomanip>
#include <string>
#include <cstring>
#include <cassert>

//#define EXAMPLE 1		// Run easy short example; comment it to switch to issue solving
#define CALC_LEVEL 2	// Run the issue with specifed level (1..3); comment if want an example

#define B 0			// Black
#define W 0x40		// White
#define ______ 0	// Empty square
#define XXXXXX 0xFF	// Denied placement (no figure inside but also cannot move there)

enum {PAWN=1, ROOK, KNGT, BSHP, QUEE, KING};	// Figure types
typedef unsigned char P;	// Placement: Presence value (0 if not present) | color | figure type

// Start board of the game
const P start_board_data[] = {

#if CALC_LEVEL == 1
	// Only 5 figures (the most suspected) on the board
	XXXXXX, XXXXXX, XXXXXX, XXXXXX, XXXXXX, XXXXXX, XXXXXX, XXXXXX,
	XXXXXX, XXXXXX, B|PAWN, B|PAWN, B|PAWN, XXXXXX, XXXXXX, XXXXXX,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	XXXXXX, XXXXXX, XXXXXX, XXXXXX, W|PAWN, XXXXXX, XXXXXX, XXXXXX,
	XXXXXX, XXXXXX, XXXXXX, XXXXXX, XXXXXX, W|BSHP, XXXXXX, XXXXXX,
#elif CALC_LEVEL == 2
	// Block all figures that just can't move
	XXXXXX, B|KNGT, B|BSHP, B|QUEE, B|KING, B|BSHP, B|KNGT, XXXXXX,
	XXXXXX, XXXXXX, B|PAWN, B|PAWN, B|PAWN, XXXXXX, XXXXXX, XXXXXX,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	XXXXXX, XXXXXX, XXXXXX, XXXXXX, W|PAWN, XXXXXX, XXXXXX, XXXXXX,
	XXXXXX, XXXXXX, XXXXXX, W|QUEE, W|KING, W|BSHP, XXXXXX, XXXXXX,
#else
	// Full(classic) start chess-board. Will be used also for EXAMPLE
	B|ROOK, B|KNGT, B|BSHP, B|QUEE, B|KING, B|BSHP, B|KNGT, B|ROOK,
	B|PAWN, B|PAWN, B|PAWN, B|PAWN, B|PAWN, B|PAWN, B|PAWN, B|PAWN,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	W|PAWN, W|PAWN, W|PAWN, W|PAWN, W|PAWN, W|PAWN, W|PAWN, W|PAWN,
	W|ROOK, W|KNGT, W|BSHP, W|QUEE, W|KING, W|BSHP, W|KNGT, W|ROOK,
#endif

};

#define TARGET_STEPS 8	// Steps to solve the issue

const P target_board_data[] = {
#if defined(EXAMPLE)
#undef TARGET_STEPS
#define TARGET_STEPS 6	// Example has less steps - for performance
	// Example - quite easy to guess possible moves
	B|ROOK, ______, B|BSHP, B|QUEE, B|KING, B|BSHP, B|KNGT, B|ROOK,
	______, ______, B|PAWN, B|PAWN, B|PAWN, B|PAWN, B|PAWN, B|PAWN,
	______, B|PAWN, B|KNGT, ______, ______, ______, ______, ______,
	B|PAWN, ______, ______, ______, ______, ______, ______, W|QUEE,
	______, ______, W|BSHP, ______, ______, ______, ______, ______,
	______, ______, ______, ______, W|PAWN, ______, ______, ______,
	W|PAWN, W|PAWN, W|PAWN, W|PAWN, ______, W|PAWN, W|PAWN, W|PAWN,
	W|ROOK, W|KNGT, W|BSHP, ______, W|KING, ______, W|KNGT, W|ROOK,
#elif CALC_LEVEL == 1
	// Only 5 figures are on board
	XXXXXX, XXXXXX, XXXXXX, XXXXXX, XXXXXX, XXXXXX, XXXXXX, XXXXXX,
	XXXXXX, XXXXXX, ______, ______, ______, XXXXXX, XXXXXX, XXXXXX,
	______, ______, B|PAWN, ______, B|PAWN, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, W|PAWN, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	XXXXXX, XXXXXX, XXXXXX, XXXXXX, ______, XXXXXX, XXXXXX, XXXXXX,
	XXXXXX, XXXXXX, XXXXXX, XXXXXX, XXXXXX, ______, XXXXXX, XXXXXX,
#elif CALC_LEVEL == 2
	// Figures that MUST NOT move (pawns, rooks, etc) are denied 
	XXXXXX, B|KNGT, B|BSHP, B|QUEE, B|KING, B|BSHP, B|KNGT, XXXXXX,
	XXXXXX, XXXXXX, ______, ______, ______, XXXXXX, XXXXXX, XXXXXX,
	______, ______, B|PAWN, ______, B|PAWN, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, W|PAWN, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	XXXXXX, XXXXXX, XXXXXX, XXXXXX, ______, XXXXXX, XXXXXX, XXXXXX,
	XXXXXX, XXXXXX, XXXXXX, W|QUEE, W|KING, ______, XXXXXX, XXXXXX,
#else
	// All figures are on the board
	B|ROOK, B|KNGT, B|BSHP, B|QUEE, B|KING, B|BSHP, B|KNGT, B|ROOK,
	B|PAWN, B|PAWN, ______, ______, ______, B|PAWN, B|PAWN, B|PAWN,
	______, ______, B|PAWN, ______, B|PAWN, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	______, ______, ______, ______, W|PAWN, ______, ______, ______,
	______, ______, ______, ______, ______, ______, ______, ______,
	W|PAWN, W|PAWN, W|PAWN, W|PAWN, ______, W|PAWN, W|PAWN, W|PAWN,
	W|ROOK, W|KNGT, W|BSHP, W|QUEE, W|KING, ______, W|KNGT, W|ROOK,
#endif
};

#define COLOR_MASK 0x40		// Mask to extract color from Placement
#define FIGURE_MASK 0x07	// Mask to extract figure type from Placement

#define MAX_OFFSET 63	// Offset = 0..63
#define MAX_POS 7		// Pos = 0..7

// Operations with Placement
#define IS_EMPTY(p) (p == 0)
#define COLOR(p) (p & COLOR_MASK)
#define FIGURE(p) (p & FIGURE_MASK)

typedef signed short Offset;	// Always in range [0,63]

struct Pos {
	signed short _x, _y;	// Can be out board (either negative or more than MAX_POS)

	inline Pos(Offset o)
	: _x(/* o%8 */ o&0x07 )
	, _y(/* o/8 */ o>>3 )	// Performance optimization?!
	{}

	inline Pos(signed short x, signed short y)
	: _x(x)
	, _y(y)
	{}

	inline bool is_onboard() const
	{
		return (_x >=0 && _y >=0 && _x <= MAX_POS && _y <= MAX_POS);
	}

	inline Pos operator+(const Pos& b)
	{
		Pos res(_x + b._x, _y + b._y);
		return res;
	}

	inline operator Offset()
	{
		// All on-board positions can be converted to Offset
		assert(is_onboard());

		// return _x + _y*8;
		return _x + (_y<<3);	// Performance optimization?!
	}
};

struct Board {
	inline Board(const P *board)
	{
		memcpy(_board, board, sizeof(_board));
	}

	inline bool operator==(const Board& b2)
	{
		return (memcmp(_board, b2._board, sizeof(_board)) == 0);
	}

	inline P operator[](signed short offset) const
	{
		assert(offset >=0);
		assert(offset <= MAX_OFFSET);

		return _board[offset];
	}

	inline P move_figure_to(Offset from, Offset to)
	{
		P beaten_figure = _board[to];
		_board[to] = _board[from];
		_board[from] = ______;

		return beaten_figure;
	}

	inline void undo_beat(Offset beaten_offset, Offset old_offset, P beaten_figure)
	{
		_board[old_offset] = _board[beaten_offset];
		_board[beaten_offset] = beaten_figure;
	}

	P _board[64];
};

class GameStep {
private:
	Board& _board;
	unsigned short _step;
	bool _completed;

public:
	GameStep(Board& board, unsigned short step)
	: _board(board)
	, _step(step)
	, _completed(false)
	{ }

	void calculate_step();
	inline bool completed() const { return _completed; }

private:
	void move_figure(Offset offset);

	bool try_move(Offset from_offset, Pos to_pos);
	bool try_beat(Offset from_offset, Pos to_pos);
	bool try_move_or_beat(Offset from_offset, Pos to_pos, bool* was_beat=NULL);


	inline bool is_complete()
	{
#if 1
		// Check step and board state
		if ( _step != TARGET_STEPS+1) return false;

		static const Board target_board(target_board_data);
		return (_board == target_board);
#else
		// Test
		return (_step == TARGET_STEPS+1);
#endif
	}

	inline bool is_continue_iterate()
	{
		// Stop iterating on desired step
		return (_step < TARGET_STEPS+1);
	}
};

static long long combinations = 0;

void GameStep::calculate_step()
{
	if (is_complete()) {
		std::cout << "\nSolved on " << combinations << "-th combination!\n";
		_completed = true;
		return;
	}
	if (!is_continue_iterate()) {
		combinations++;
		// Print dots to indicate the progress....
		if (combinations % 1000000 == 0) std::cout << '.' << std::flush;
		return;
	}

	P moving_color = (_step%2) ? W : B;
	for (signed short offset=0; offset <= MAX_OFFSET; offset++) {
		P p = _board[offset];
		if (p != ______ && p != XXXXXX && COLOR(p) == moving_color) {
			move_figure(offset);
			if (_completed) return;
		}
	}
}

// Can beat only figures of different color
#define CAN_BEAT(p, my_color) (COLOR(p) != my_color)

bool GameStep::try_move(Offset from_offset, Pos to_pos)
{
	if (!to_pos.is_onboard()) return false;
	if (IS_EMPTY(_board[to_pos])) {
		_board.move_figure_to(from_offset, to_pos);
		calculate_step();
		_board.move_figure_to(to_pos, from_offset);	// move figure back
		return true;
	}
	return false;
}

bool GameStep::try_beat(Offset from_offset, Pos to_pos)
{
	if (!to_pos.is_onboard()) return false;
	P beaten_figure = _board[to_pos];
	if (beaten_figure == ______ || beaten_figure == XXXXXX) return false;

	P figure_color = COLOR(_board[from_offset]);
	if (CAN_BEAT(beaten_figure, figure_color)) {
		_board.move_figure_to(from_offset, to_pos);
		calculate_step();
		_board.undo_beat(to_pos, from_offset, beaten_figure);	// resurrect beaten figure
		return true;
	}
	return false;
}

bool GameStep::try_move_or_beat(Offset from_offset, Pos to_pos, bool* was_move /* = NULL */)
{
	if (!to_pos.is_onboard()) return false;
	P beaten_figure = _board[to_pos];
	if (beaten_figure == XXXXXX) return false;	// Deny to beat this special mark

	P figure_color = COLOR(_board[from_offset]);
	bool can_move = IS_EMPTY(beaten_figure);
	if (can_move || CAN_BEAT(beaten_figure, figure_color)) {
		if (was_move) *was_move = can_move;

		_board.move_figure_to(from_offset, to_pos);
		calculate_step();
		_board.undo_beat(to_pos, from_offset, beaten_figure);   // restore old state

		return true;
	}
	return false;
}

//--------------------------------------------------------------------------------------
// Output-related
const char *figure_name(P p)
{
	switch(FIGURE(p)) {
		case PAWN: return "PAWN";
		case ROOK: return "ROOK";
		case KNGT: return "KNGT";
		case BSHP: return "BSHP";
		case QUEE: return "QUEE";
		case KING: return "KING";
	}
	return "????";
}


// Print Pos in 'E2' format. Note: _y is reversed to use common numeration of chessboard
inline std::ostream& operator<< (std::ostream& stm, const Pos& p)
{
	stm << static_cast<char>('A'+p._x) << (8-p._y);
	return stm;
}

// Convenience macro to watch successful result
#define WATCH(to_pos, try_action)							\
do {														\
	try_action;												\
	if (next_step.completed()) {							\
		std::cout << "Step#" << _step << ": " <<			\
			((figure_color == B) ? "BLACK" : "WHITE") <<	\
			" " << figure_name(_board[offset]) << 			\
			", from " << Pos(offset) <<						\
			" to " << to_pos << "\n";						\
		_completed = true;									\
		return;												\
	}														\
} while(0)

//---------------------------------------------------------------------------------------

void GameStep::move_figure(Offset offset)
{
#if 0
	Board next_board(_board);	// Copy board bitwise: we can solve the issue in parallel threads?!
	GameStep next_step(next_board, _step + 1);
#else
	// Use the same board (can be run only in single thread!)
	GameStep next_step(_board, _step + 1);
#endif

	P figure = _board[offset];
	P figure_color = COLOR(figure);
	P figure_type = FIGURE(figure);

	switch(figure_type) {
		case PAWN:
		{
			// Black pawn moves down, white pawn moves up
			signed short direction = ((figure_color == B) ? 1 : -1);

			// Move (not beat) 1 step forward
			Pos p1 = Pos(offset) + Pos(0, direction);
			bool can_move_forward;
			WATCH(p1, can_move_forward = next_step.try_move(offset, p1));

			if (can_move_forward) {
				Pos p(offset);
				// On the first step pawn can go for two squares
				bool first_step = (figure_color == B && p._y == 1) || (figure_color == W && p._y == 6);
				if (first_step) {
					// Move (not beat) 2 steps forward
					Pos p2 = Pos(offset) + Pos(0, direction*2);
					WATCH(p2, next_step.try_move(offset, p2));
				}
			}

			Pos beat_left = Pos(offset) + Pos(-1, direction);
			WATCH(beat_left, next_step.try_beat(offset, beat_left));

			Pos beat_right = Pos(offset) + Pos(1, direction);
			WATCH(beat_right, next_step.try_beat(offset, beat_right));

			// TODO? PAWN can become a QUEEN when reaching the last row
		}
		break;

		case ROOK:
		case QUEE:	// Queen can move the same as Rook
		{
			bool was_move;
			bool can_move_or_beat;
			signed short i;

			// Rook can move horizontally/vertically until reach figure of the same color or beat figure of other color
			for(i=1, was_move=true; was_move; i++) {
				Pos p = Pos(offset) + Pos(i, 0);
				WATCH(p, can_move_or_beat = next_step.try_move_or_beat(offset, p, &was_move));
				if (!can_move_or_beat) break;
			}
			for(i=1, was_move=true; was_move; i++) {
				Pos p = Pos(offset) + Pos(-i, 0);
				WATCH(p, can_move_or_beat = next_step.try_move_or_beat(offset, p, &was_move));
				if (!can_move_or_beat) break;
			}
			for(i=1, was_move=true; was_move; i++) {
				Pos p = Pos(offset) + Pos(0, i);
				WATCH(p, can_move_or_beat = next_step.try_move_or_beat(offset, p, &was_move));
				if (!can_move_or_beat) break;
			}
			for(i=1, was_move=true; was_move; i++) {
				Pos p = Pos(offset) + Pos(0, -i);
				WATCH(p, can_move_or_beat = next_step.try_move_or_beat(offset, p, &was_move));
				if (!can_move_or_beat) break;
			}
		}
		if (figure_type == ROOK) break;

		// Walk-throught for QUEE: Queen can move the same as Bishop
		case BSHP:
		{
			bool was_move;
			bool can_move_or_beat;
			signed short i;

			// Bishop can move on diagonal in 4 directions until reach figure of the same color or beat figure of other color
			for(i=1, was_move=true; was_move; i++) {
				Pos p = Pos(offset) + Pos(i, i);
				WATCH(p, can_move_or_beat = next_step.try_move_or_beat(offset, p, &was_move));
				if (!can_move_or_beat) break;
			}
			for(i=1, was_move=true; was_move; i++) {
				Pos p = Pos(offset) + Pos(-i, -i);
				WATCH(p, can_move_or_beat = next_step.try_move_or_beat(offset, p, &was_move));
				if (!can_move_or_beat) break;
			}
			for(i=1, was_move=true; was_move; i++) {
				Pos p = Pos(offset) + Pos(-i, i);
				WATCH(p, can_move_or_beat = next_step.try_move_or_beat(offset, p, &was_move));
				if (!can_move_or_beat) break;
			}
			for(i=1, was_move=true; was_move; i++) {
				Pos p = Pos(offset) + Pos(i, -i);
				WATCH(p, can_move_or_beat = next_step.try_move_or_beat(offset, p, &was_move));
				if (!can_move_or_beat) break;
			}
		}
		break;

		case KNGT:
		{
			// Possible moves/beats of Knight (8): it can jump over other figures
			static const Pos knight_moves[] = {
				Pos(1,2), Pos(2,1), Pos(-1,-2), Pos(-2,-1), Pos(-1,2), Pos(2,-1), Pos(1,-2), Pos(-2,1)
			};
			for(int i=0; i<sizeof(knight_moves)/sizeof(*knight_moves); i++) {
				Pos p = Pos(offset) + knight_moves[i];
				WATCH(p, next_step.try_move_or_beat(offset, p));
			}
		}
		break;
		case KING:
		{
			// Possible moves/beats of King (9)
			static const Pos king_moves[] = {
				Pos(0,1), Pos(0,-1), Pos(1,0), Pos(-1,0), Pos(1,1), Pos(-1,-1), Pos(1,-1), Pos(-1,1)
			};
			for(int i=0; i<sizeof(king_moves)/sizeof(*king_moves); i++) {
				Pos p = Pos(offset) + king_moves[i];
				WATCH(p, next_step.try_move_or_beat(offset, p));
			}
		}
		break;
	}
}

int main()
{
	Board start_board(start_board_data);
	GameStep first_step(start_board, 1);
	first_step.calculate_step();
	if (!first_step.completed()) {
		std::cout << "\nEnumerated " << combinations << " combinations but solution was not found\n";
	}

	return (first_step.completed() ? 0 : 1);
}

