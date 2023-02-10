(** A module that encapsulates information and functions on each piece within
    each Game state. *)

exception InvalidPromotion of string
(**[InvalidPromotion s] is raised when a piece tries to promote when it is
   invalid to do so. s containts the piece that is trying to promote *)

(**[piecetype] is a type represeting the type of the piece, e.g. pawn*)
type piecetype =
  | Pawn
  | Lance
  | Knight
  | Silver
  | Gold
  | King
  | Rook
  | Bishop

type t = {
  player : int;
  piecerepr : string;
  piecetype : piecetype;
  promoted : bool;
}
(** The abstract type of values representing pieces*)

val create_piece : int -> string -> t
(** [create_piece p name] is the piece of type [name] and player [p]*)

val repr_piece : t -> string
(** [repr_piece p] is the char representation of piece [p]*)

val get_player : t -> int
(** [get_player p] is the player in possession of piece [p]*)

(* val moves_piece : t -> int * char -> (int * char) list *)
(** [move_piece p pos ] is the list containing all possible moves of piece [p]
    at position [pos].*)

val promote_piece : t -> t
(** [promote_piece p] is the result of attempting to promote the piece [p].
    Raises [InvalidPromotion] if it is not legal to promote*)

val is_king : t -> bool
(**[is_king piece] returns true if the given piece is a king*)

val value_of_piece : t -> int
(** [ value_of_piece p] is the value assigned to each piece based on its
    strength*)

val compare : t -> t -> int
(**[compare p1 p2] returns 0 if pieces [p1] is equal to [p2], a negative integer
   if [p1] is worth less than [p2], and a positive integer if [p1] is greater
   than [p2]. The value of the pieces are that of the formalization developed by
   Tanigawa (NHK 2006)*)
