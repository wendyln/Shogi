(** A module that encapsulates information and functions on the state of the
    game at each point throughout the course of the Shogi match. *)

exception Invalid_Move
(**Raised when the move is invalid*)

exception Invalid_Pos
(**Raised when a position is invalid*)

type turn_type
(**[turn_type] refers to which player's turn it is*)

type game = {
  turn : turn_type;
  cap_B : Piece.t list;
  cap_W : Piece.t list;
  board : Board.board_type;
  check : Board.check_type;
  game_over : bool;
  log : string ref;
}
(**[game] is the state of the game. It includes the whose turn, captured list
   for Black, captured list for White, and whether anyone is under check - game
   over*)

val new_game : unit -> game
(**[new_game] initialize a new game*)

val swap_turns : game -> game
(** [swap_turns game] is the same State.game with the turn swapped. *)

val change_turn : turn_type -> turn_type
(** [change_turn t] toggles the turn [t] between each turn *)

val end_game : game -> bool
(**[end_game] is true if [game] represents a game that has ended*)

val get_board : game -> Board.board_type
(**[get_board game] returns the current board of the [game]*)

val get_turn : game -> int
(** [get_turn game] returns which player's turn *)

val get_piece : string -> Board.board_type -> string
(**[get_piece board loc ] takes in [Board] and [Location] and returns the string
   representation of piece at that location *)

val get_captured : game -> Piece.t list * Piece.t list
(**[get_captured game] returns the tuple of captured items for White and Black
   in [game]*)

val interpret_move : string -> (int * char) * (int * char)
(**[interpret_move] takes the command and return*)

val move_piece_board : ?admin_mode:bool -> game -> string -> game
(** [move_piece_board game command] moves piece according to command *)

val drop_piece_board : game -> string -> game
(**[move_piece game command] drops piece from captured pieces back into board *)

val string_of_state : game -> string
(** [string_of_state] is a function that, once applied to a game state, returns
    #q the string representation of the current game state.*)

val tagged_possible_states : game -> int -> (string * game) list
(** [tagged_possible_states game player] is a list of possible game states,
    associated with the command that takes the user from the original game to
    each game.*)

val possible_moves : game -> int -> game list
(** [ possible_moves game player] return a list of possible games states of the
    next move*)
