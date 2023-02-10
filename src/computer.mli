(** A module to encapsulate the functions required on the bot for single and
    zero-player games. *)

val piece_value_eval : State.game -> int
(**[ piece_value_eval game] takes in the game and returns the evaluation of
   piece_values of game board*)

val king_safety_eval : State.game -> int
(**[king_safety_eval st] is an integer representing an evaluated value of how
   safe the king is in the board state [st]*)

val square_control : State.game -> int
(**[square_control st] is an integer representing an evaluated value of how mamy
   squares the current player controls in the board state [st]*)

val bot_move : State.game -> State.game
(**[bot_move board] takes in the current state of the board and find a strategy
   to move a piece to a location *)

val one_player_game : State.game -> string -> State.game
(** [one_player_game game] plays the game against a computer *)

val zero_player_game : State.game -> State.game
(** [zero_player_game game] plays the game with two computers *)

(* Methods I need to make : 1) list of possible game states from current game
   state -> valid_move_list : Piece , board : returns list of possible positions

   1. king safety -> Escape Square -> Attackers ( count captured list ) ->
   Defenders

   2. Square Control -> numbers of squares controlled + number of captured
   pieces for each player *)
