exception InvalidPromotion of string

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

(**[char_to_piecetype p] is the Piecetype corresponding to the string [p].
   Raises Failure if string p does not correspond to any piecetype*)
let str_to_piecetype = function
  | "p" | "P" | "+p" | "+P" -> Pawn
  | "l" | "L" | "+l" | "+L" -> Lance
  | "n" | "N" | "+n" | "+N" -> Knight
  | "s" | "S" | "+s" | "+S" -> Silver
  | "g" | "G" -> Gold
  | "k" | "K" -> King
  | "r" | "R" | "+r" | "+R" -> Rook
  | "b" | "B" | "+b" | "+B" -> Bishop
  | s -> raise (Failure ("Precondition violated: No such piece" ^ s))

let create_piece p name =
  {
    player = p;
    piecerepr = name;
    piecetype = str_to_piecetype name;
    promoted = (if String.get name 0 = '+' then true else false);
  }

let repr_piece p =
  if p.player = 0 then String.uppercase_ascii p.piecerepr
  else String.lowercase_ascii p.piecerepr

let get_player = function
  | p -> p.player

(**[is_king piece] returns true if the given piece is a king*)
let is_king p = if p.piecetype = King then true else false

let promote_piece piece =
  match piece.piecetype with
  | Pawn | Lance | Knight | Silver | Bishop | Rook ->
      { piece with piecerepr = "+" ^ piece.piecerepr; promoted = true }
  | _ ->
      raise
        (InvalidPromotion ("'" ^ piece.piecerepr ^ "'" ^ " Cannot be Promoted"))

let value_of_piece p =
  match str_to_piecetype p.piecerepr with
  | Pawn -> if p.promoted then 6 else 1
  | Lance -> if p.promoted then 6 else 3
  | Knight -> if p.promoted then 6 else 4
  | Silver -> if p.promoted then 6 else 5
  | Gold -> 6
  | Bishop -> if p.promoted then 10 else 8
  | Rook -> if p.promoted then 12 else 10
  | King -> 10000

let compare p1 p2 =
  let val1 = value_of_piece p1 in
  let val2 = value_of_piece p2 in
  val1 - val2
