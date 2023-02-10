(** A module to encapsulate functions required to be performed on the board
    within each state. *)

type board_type = ((int * char) * Piece.t) list
(**[board_type] is a type representing a board *)

val empty_board : board_type
(** [empty_board] is an empty board with no pieces on it*)

val add_piece : board_type -> Piece.t -> int * char -> board_type
(** [add_piece board p pos] adds a piece [p] to location [pos] on [board]*)

val new_board : board_type
(**[new_board] creates a new board for a new game with all the pieces set up*)

val move_piece :
  ?admin_mode:bool ->
  ?log:string ref ->
  int * char ->
  int * char ->
  board_type ->
  int ->
  bool ->
  board_type
(**[move_piece admin_mode log curr_loc final_loc board promote] is the board
   after moving a piece (while promoting or not) from [curr_loc] to
   [final _loc]. Requires: Moves are valid. If Admin Mode is true then allows
   any move otherwise ifillegal move is played, raises Invalid Move failes and
   writes to Log *)

val capture :
  int * char ->
  board_type ->
  Piece.t list ->
  Piece.t list ->
  Piece.t list * Piece.t list
(**[capture final_loc board capt_B capt_W] is a tuple of captured Black list and
   captured White list. Each list represents the piece which the respective
   player has captured *)

val drop_piece :
  ?log:string ref ->
  Piece.t ->
  int * char ->
  board_type ->
  Piece.t list ->
  board_type
(**[drop_piece p final_loc board] is the result of dropping a [piece] from your
   [captured list] back on to the board. Requires: Piece is from captured list
   and dropped location is valid. Raises: Invalid Drop and writes to the log*)

val new_cap : Piece.t -> Piece.t list -> Piece.t list
(**[new_cap piece cap_lst] is the new captured list after [piece] is used*)

val pos_piece : board_type -> Piece.t -> int * char
(** [pos_piece board piece] returns the position of the piece on the board. RI:
    Piece is on the board *)

val find_king : int -> board_type -> int * char
(** [find_king player board] finds the position of the the player's king on the
    board returning a location*)

(**[check_type] is the type of check a player is under*)
type check_type =
  | None
  | Check
  | Checkmate

val check : board_type -> int -> check_type
(** [check board player] checks if the given player's king is under check or
    checkmate*)

val filled_positions :
  board_type -> int -> bool -> ((int * char) * Piece.t) list
(** [filled_positions board player include_king] is an association list
    containing mappings from a location on the board to a piece, all belonging
    to player and does/does not include the king, as specified.*)

val empty_positions : board_type -> (int * char) list
(** [empty_positions board] is a [(int * char) list] that represents all
    positions on the board where no pieces exist. *)

val order_list : board_type -> ((int * char) * string) list
(** [order_list game] is an association list that maps each position,
    incrementally left-to-right, up-to-down, to a [char] that represents the
    piece/empty space at that position.*)

val valid_move_list : Piece.t -> int * char -> board_type -> (int * char) list
(**[valid_move_list p board] is the list of valid positions piece [p] can move
   to on [board]*)

val remove_player : board_type -> int -> board_type
(**[remove_player board player] removes the specified player from the board*)
