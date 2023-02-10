open Piece

exception End_of_Board
(**Raised when the square a piece is trying to move to is out of the board*)

exception Occupied of (int * char)
(**Raised when the square a piece is trying to move to is occupied*)

type location = int * char
(**type location represents the coordinates of a square on the board. *)

type piece_rep = location * Piece.t
(**type piece_rep is a tuple containing the coordinate of the piece and the
   piece.*)

type board_type = piece_rep list
(** type board_type represents the board, with elements representing the pieces
    that are currently on the board *)

(** type direction represents the possible directions a piece can move in*)
type direction =
  | Up
  | Down
  | Left
  | Right
  | Upleft
  | Upright
  | Downleft
  | Downright

(*For rows that are fully populated, I created a method to make adding pieces
  easier, it iterates through the list adding pieces in positions moving left to
  right *)
let rec add_across_row ((column, row) : location) (color : int) = function
  | [] -> []
  | h :: t ->
      ((column, row), create_piece color h)
      :: add_across_row (column + 1, row) color t

(**[empty_board] creates an empty board*)
let empty_board = []

(**[add_piece board p pos] adds piece p to board at pos *)
let add_piece board p pos = (pos, p) :: board

let remove_player board player : board_type =
  List.filter (fun (_, p) -> get_player p != player) board

(**[only_player board player] remvoes all pieces on the board not associated
   with the given player and returns the new board*)
let only_player board player : board_type =
  List.filter (fun (_, p) -> get_player p = player) board

let new_board : board_type =
  ((8, 'b'), create_piece 0 "R") (*Black Rook*)
  :: ((2, 'b'), create_piece 0 "B") (*Black Bishop*)
  :: ((2, 'h'), create_piece 1 "R") (*White Rook*)
  :: ((8, 'h'), create_piece 1 "B") (*White Bishop*)
  :: add_across_row (1, 'a') 0 [ "L"; "N"; "S"; "G"; "K"; "G"; "S"; "N"; "L" ]
  (*Black Last Row*)
  @ add_across_row (1, 'c') 0 [ "P"; "P"; "P"; "P"; "P"; "P"; "P"; "P"; "P" ]
    (*Black Pawn Row*)
  @ add_across_row (1, 'i') 1 [ "L"; "N"; "S"; "G"; "K"; "G"; "S"; "N"; "L" ]
    (*White Last Row*)
  @ add_across_row (1, 'g') 1 [ "P"; "P"; "P"; "P"; "P"; "P"; "P"; "P"; "P" ]
(*Black Last Row*)

(* BEGIN FUNCTIONS TO HELP WITH PRINTING *)

(** [next_position (i, c)] is an [int * char] that represents the following
    board position to the given one, taken left-to-right and up-to-down.*)
let next_position ((i, c) : location) : location =
  match i with
  | 1 -> begin
      match c with
      | 'i' -> (0, 'z')
      | c' ->
          let new_c = c' |> int_of_char |> ( + ) 1 |> char_of_int in
          (9, new_c)
    end
  | i' -> (i' - 1, c)

(** [filled_positions_aux board loc acc player include_king] is an association
    list mapping a location on the board to its corresponding piece, ensuring
    the space is non-empty, the piece belongs to the specified player, and the
    king is/isn't included. *)
let rec filled_positions_aux (board : board_type) (loc : int * char)
    (acc : ((int * char) * Piece.t) list) (player : int) (include_king : bool) =
  match loc with
  | 0, 'z' -> acc
  | loc ->
      filled_positions_aux board (next_position loc)
        (if
         List.mem loc (* ensures space isn't empty *)
           (let a, _ = List.split board in
            a)
         && player = get_player (List.assoc loc board)
         (* ensures player corresponds to piece*)
        then
         let piece = List.assoc loc board in
         match piece.piecetype with
         (* ensures king is/isn't included*)
         | King -> if include_king then (loc, piece) :: acc else acc
         | _ -> (loc, piece) :: acc
        else acc)
        player include_king

let filled_positions board player include_king =
  filled_positions_aux board (9, 'a') [] player include_king

(** [empty_positions_aux board loc acc] is a [(int * char) list] that represents
    all positions on the board where no pieces exist, starting the search from
    loc and accumulating to acc. *)
let rec empty_positions_aux board (loc : int * char) (acc : (int * char) list) :
    (int * char) list =
  match loc with
  | 0, 'z' -> acc
  | tup ->
      empty_positions_aux board (next_position loc)
        (if
         List.mem tup
           (let a, _ = List.split board in
            a)
        then acc
        else tup :: acc)

let empty_positions (board : board_type) = empty_positions_aux board (9, 'a') []

(** [order_list_helper accum assoc_lst] is an association list that maps each
    position, incrementally left-to-right, up-to-down, to a [char] that
    represents the piece/empty space at that position.*)
let rec order_list_helper accum assoc_lst = function
  | 0, 'z' -> accum
  | tup ->
      let accum' =
        (try (tup, repr_piece (List.assoc tup assoc_lst))
         with Not_found -> (tup, " "))
        :: accum
      in
      order_list_helper accum' assoc_lst (next_position tup)

let order_list board = List.rev (order_list_helper [] board (9, 'a'))

(**[capture final_loc board capt_B capt_W] returns a tuple of captured list for
   both players*)
let rec capture (final_loc : int * char) (board : board_type) (capt_B : t list)
    (capt_W : t list) =
  match board with
  | [] -> (capt_B, capt_W)
  | (x1, x2) :: _ when x1 = final_loc ->
      let player = get_player x2 in
      let repr =
        let string_rep = String.uppercase_ascii (repr_piece x2) in
        if String.length string_rep = 1 then
          Char.escaped (String.get string_rep 0)
        else Char.escaped (String.get string_rep 1)
      in
      if player = 0 then
        let p = create_piece 1 repr in
        (capt_B, p :: capt_W)
      else
        let p = create_piece 0 repr in
        (p :: capt_B, capt_W)
  | _ :: t -> capture final_loc t capt_B capt_W

(**[drop_piece p final_loc board cap_list] drops piece p onto the board at
   final_loc*)
let drop_piece ?(log = ref "") (p : t) (final_loc : int * char)
    (board : board_type) (cap_list : t list) : board_type =
  if List.mem p cap_list |> not then (
    log := "Cannot drop make believe piece";
    failwith "Cannot drop make believe piecee")
  else if List.mem_assoc final_loc board then (
    log := "Cannot drop onto occupied location";
    failwith "Cannot drop onto occupied location")
  else (final_loc, p) :: board

(*[new_cap p cap_lst] returns the new captured list after piece p is dropped*)
let rec new_cap (p : t) (cap_lst : t list) =
  match cap_lst with
  | [] -> []
  | x :: t -> if x = p then t else x :: new_cap p t

(*[pos_piece board piece] returns the pos of piece on the board*)
let rec pos_piece (board : board_type) (piece : Piece.t) =
  match board with
  | [] -> raise (Failure "Piece is not on board")
  | h :: t -> if snd h = piece then fst h else pos_piece t piece

(** [same_side p1 p2] is true if both pieces belong to the same player and false
    otherwise*)
let same_side p1 p2 = Piece.get_player p1 = Piece.get_player p2

(**[move_one dir pos board] tries to move the piece at [pos] in the direction
   [dir] by one. Raises Illegal if the move is invalid, and returns the new
   position if it is valid. [dir] is the direction of movement. Raises
   End_of_Board if the move will take the piece out of the constraints of the
   board*)
let move_one dir (lx, ly) board =
  match dir with
  | Right -> begin
      match lx with
      | 1 -> raise End_of_Board
      | _ ->
          if List.mem_assoc (lx - 1, ly) board then
            raise (Occupied (lx - 1, ly))
          else (lx - 1, ly)
    end
  | Left -> begin
      match lx with
      | 9 -> raise End_of_Board
      | _ ->
          if List.mem_assoc (lx + 1, ly) board then
            raise (Occupied (lx + 1, ly))
          else (lx + 1, ly)
    end
  | Up -> begin
      match ly with
      | 'a' -> raise End_of_Board
      | _ ->
          let ly_tocode = Char.code ly in
          if List.mem_assoc (lx, Char.chr (ly_tocode - 1)) board then
            raise (Occupied (lx, Char.chr (ly_tocode - 1)))
          else (lx, Char.chr (ly_tocode - 1))
    end
  | Down -> begin
      match ly with
      | 'i' -> raise End_of_Board
      | _ ->
          let ly_tocode = Char.code ly in
          if List.mem_assoc (lx, Char.chr (ly_tocode + 1)) board then
            raise (Occupied (lx, Char.chr (ly_tocode + 1)))
          else (lx, Char.chr (ly_tocode + 1))
    end
  | Upleft -> begin
      match (lx, ly) with
      | _, 'a' | 9, _ -> raise End_of_Board
      | _ ->
          let ly_tocode = Char.code ly in
          if List.mem_assoc (lx + 1, Char.chr (ly_tocode - 1)) board then
            raise (Occupied (lx + 1, Char.chr (ly_tocode - 1)))
          else (lx + 1, Char.chr (ly_tocode - 1))
    end
  | Upright -> begin
      match (lx, ly) with
      | _, 'a' | 1, _ -> raise End_of_Board
      | _ ->
          let ly_tocode = Char.code ly in
          if List.mem_assoc (lx - 1, Char.chr (ly_tocode - 1)) board then
            raise (Occupied (lx - 1, Char.chr (ly_tocode - 1)))
          else (lx - 1, Char.chr (ly_tocode - 1))
    end
  | Downleft -> begin
      match (lx, ly) with
      | _, 'i' | 9, _ -> raise End_of_Board
      | _ ->
          let ly_tocode = Char.code ly in
          if List.mem_assoc (lx + 1, Char.chr (ly_tocode + 1)) board then
            raise (Occupied (lx + 1, Char.chr (ly_tocode + 1)))
          else (lx + 1, Char.chr (ly_tocode + 1))
    end
  | Downright -> begin
      match (lx, ly) with
      | _, 'i' | 1, _ -> raise End_of_Board
      | _ ->
          let ly_tocode = Char.code ly in
          if List.mem_assoc (lx - 1, Char.chr (ly_tocode + 1)) board then
            raise (Occupied (lx - 1, Char.chr (ly_tocode + 1)))
          else (lx - 1, Char.chr (ly_tocode + 1))
    end

(**[handle_occ f dir pos board piece] handles exceptions for pieces that only
   move once. [f] is a function direction -> (int*char) -> (int*char) list ->
   (int*char) that tries to move a piece and return a position. [dir] is the
   direction of movement. If f raises [End_of_Board], the result is []. If f
   raises [Occupied p], the piece at p on board is checked, if it belongs to the
   same player as [piece], the output is []. Otherwise, the output is
   [f dir pos board piece]. *)
let handle_occ f dir (pos : int * char) board piece =
  try [ f dir pos board ] with
  | Occupied newpos ->
      if same_side piece (List.assoc newpos board) then [] else [ newpos ]
  | End_of_Board -> []

(**[knight_move_one dir pos b] tries to move the knight at [pos] in direction
   [dir] on [board]. If the destination square [d] is occupied, raise
   [Occupied d]. If the destination square is out of the board, raise
   [End_of_Board]. Otherwise, return the destination square. *)
let knight_move_one dir (x, y) board =
  let piece = List.assoc (x, y) board in
  match dir with
  | Upleft ->
      let newx, newy = (x + 1, Char.chr (Char.code y - 2)) in
      if newx > 9 || Char.code newy > Char.code 'i' then raise End_of_Board
      else if List.mem_assoc (newx, newy) board then
        let new_piece = List.assoc (newx, newy) board in
        if same_side piece new_piece then raise (Occupied (newx, newy))
        else (newx, newy)
      else (newx, newy)
  | Upright ->
      let newx, newy = (x - 1, Char.chr (Char.code y - 2)) in
      if newx < 1 || Char.code newy < Char.code 'a' then raise End_of_Board
      else if List.mem_assoc (newx, newy) board then
        let new_piece = List.assoc (newx, newy) board in
        if same_side piece new_piece then raise (Occupied (newx, newy))
        else (newx, newy)
      else (newx, newy)
  | Downleft ->
      let newx, newy = (x + 1, Char.chr (Char.code y + 2)) in
      if newx > 9 || Char.code newy > Char.code 'i' then raise End_of_Board
      else if List.mem_assoc (newx, newy) board then
        let new_piece = List.assoc (newx, newy) board in
        if same_side piece new_piece then raise (Occupied (newx, newy))
        else (newx, newy)
      else (newx, newy)
  | Downright ->
      let newx, newy = (x - 1, Char.chr (Char.code y + 2)) in
      if newx > 9 || Char.code newy > Char.code 'i' then raise End_of_Board
      else if List.mem_assoc (newx, newy) board then
        let new_piece = List.assoc (newx, newy) board in
        if same_side piece new_piece then raise (Occupied (newx, newy))
        else (newx, newy)
      else (newx, newy)
  | Up | Down | Left | Right -> raise (Failure "Precondition violated")

(**[knight_move_helper dir pos b] is the list of all possible moves of a knight
   at [pos] on [board] in direction [dir] *)
let knight_moves_helper dir pos board =
  let piece = List.assoc pos board in
  match dir with
  | Up ->
      handle_occ knight_move_one Upleft pos board piece
      @ handle_occ knight_move_one Upright pos board piece
  | Down ->
      handle_occ knight_move_one Downleft pos board piece
      @ handle_occ knight_move_one Downright pos board piece
  | _ -> raise (Failure "Precondition violated")

(**[move dir pos b p] tries to move the piece [p] at [pos] in direction [dir] on
   board [b] until stopped by a piece or end of board. Returns the list of all
   possible destination squares in that direction *)
let rec move dir (lx, ly) board piece =
  try
    let next_square = move_one dir (lx, ly) board in
    next_square :: move dir next_square board piece
  with
  | End_of_Board -> []
  | Occupied pos ->
      let occ_pice = List.assoc pos board in
      let curr_piece = piece in
      if same_side occ_pice curr_piece then [] else [ pos ]

(**[king_moves_helper pos b p] is the list of all possible moves of a king piece
   [p] at [pos] on [board] *)
let king_moves_helper (lx, ly) board piece =
  handle_occ move_one Upleft (lx, ly) board piece
  @ handle_occ move_one Up (lx, ly) board piece
  @ handle_occ move_one Upright (lx, ly) board piece
  @ handle_occ move_one Right (lx, ly) board piece
  @ handle_occ move_one Downleft (lx, ly) board piece
  @ handle_occ move_one Down (lx, ly) board piece
  @ handle_occ move_one Downright (lx, ly) board piece
  @ handle_occ move_one Left (lx, ly) board piece

(**[gold_moves_helper dir pos b p] is the list of all possible moves of a gold
   general at [pos] on [board] in direction [dir] *)
let gold_moves_helper dir (lx, ly) board piece =
  match dir with
  | Up ->
      handle_occ move_one Upleft (lx, ly) board piece
      @ handle_occ move_one Up (lx, ly) board piece
      @ handle_occ move_one Upright (lx, ly) board piece
      @ handle_occ move_one Right (lx, ly) board piece
      @ handle_occ move_one Down (lx, ly) board piece
      @ handle_occ move_one Left (lx, ly) board piece
  | Down ->
      handle_occ move_one Downleft (lx, ly) board piece
      @ handle_occ move_one Up (lx, ly) board piece
      @ handle_occ move_one Downright (lx, ly) board piece
      @ handle_occ move_one Right (lx, ly) board piece
      @ handle_occ move_one Down (lx, ly) board piece
      @ handle_occ move_one Left (lx, ly) board piece
  | _ -> raise (Failure "Precondition violated")

(**[silver_moves_helper dir pos b p] is the list of all possible moves of a
   silver general at [pos] on [board] in direction [dir] *)
let silver_moves_helper dir (lx, ly) board piece =
  match dir with
  | Up ->
      handle_occ move_one Upleft (lx, ly) board piece
      @ handle_occ move_one Up (lx, ly) board piece
      @ handle_occ move_one Upright (lx, ly) board piece
      @ handle_occ move_one Downleft (lx, ly) board piece
      @ handle_occ move_one Downright (lx, ly) board piece
  | Down ->
      handle_occ move_one Downleft (lx, ly) board piece
      @ handle_occ move_one Down (lx, ly) board piece
      @ handle_occ move_one Downright (lx, ly) board piece
      @ handle_occ move_one Upleft (lx, ly) board piece
      @ handle_occ move_one Upright (lx, ly) board piece
  | _ -> raise (Failure "Precondition violated")

(**[rook_moves_helper pos b p] is the list of all possible moves of a rook [p]
   at [pos] on [board] *)
let rook_moves_helper (lx, ly) board piece =
  move Down (lx, ly) board piece
  @ move Up (lx, ly) board piece
  @ move Right (lx, ly) board piece
  @ move Left (lx, ly) board piece

(**[bishop_moves_helper pos b p] is the list of all possible moves of a bishop
   [p] at [pos] on [board] *)
let bishop_moves_helper (lx, ly) board piece =
  move Upleft (lx, ly) board piece
  @ move Upright (lx, ly) board piece
  @ move Downleft (lx, ly) board piece
  @ move Downright (lx, ly) board piece

(**[valid_move_list p pos b] is a list containing all possible positions a piece
   [p] can move to on board [b]*)
let valid_move_list piece (lx, ly) board : (int * char) list =
  let ptype, promotion_status, player =
    (piece.piecetype, piece.promoted, piece.player)
  in
  match (ptype, promotion_status, player) with
  | Pawn, false, 1 -> handle_occ move_one Up (lx, ly) board piece
  | Pawn, false, 0 -> handle_occ move_one Down (lx, ly) board piece
  | Rook, false, _ -> rook_moves_helper (lx, ly) board piece
  | Lance, false, 1 -> move Up (lx, ly) board piece
  | Lance, false, 0 -> move Down (lx, ly) board piece
  | Bishop, false, _ -> bishop_moves_helper (lx, ly) board piece
  | Knight, false, 1 -> knight_moves_helper Up (lx, ly) board
  | Knight, false, 0 -> knight_moves_helper Down (lx, ly) board
  | Silver, false, 1 -> silver_moves_helper Up (lx, ly) board piece
  | Silver, false, 0 -> silver_moves_helper Down (lx, ly) board piece
  | King, false, _ -> king_moves_helper (lx, ly) board piece
  | Gold, false, 1
  | Silver, true, 1
  | Lance, true, 1
  | Knight, true, 1
  | Pawn, true, 1 -> gold_moves_helper Up (lx, ly) board piece
  | Gold, false, 0
  | Silver, true, 0
  | Lance, true, 0
  | Knight, true, 0
  | Pawn, true, 0 -> gold_moves_helper Down (lx, ly) board piece
  | Rook, true, _ ->
      king_moves_helper (lx, ly) board piece
      @ rook_moves_helper (lx, ly) board piece
  | Bishop, true, _ ->
      king_moves_helper (lx, ly) board piece
      @ bishop_moves_helper (lx, ly) board piece
      @ rook_moves_helper (lx, ly) board piece
  | _ -> raise (Failure "Precondition violated at valid_move_list")

(**[list_printer lst elem_printer] returns a string of a list given a printer to
   print individual elements*)
let list_printer lst elem_printer =
  List.fold_left (fun acc elem -> acc ^ "[" ^ elem_printer elem ^ "] ") "" lst

(**[promotion_rows] returns list of avaliable rows for promotion given the
   player. Representation Invariant: Returns last row as head of list. Raises
   Reoresentation Invariant Failed if Invalid Player is given*)
let promotion_rows player =
  match player with
  | 0 -> [ 'i'; 'h'; 'g' ]
  | 1 -> [ 'a'; 'b'; 'c' ]
  | _ -> failwith "Representation Invariant Violated: Invalid Player"

(**[promote_check] checks if location is promotable, if not then raises Invalid
   Promotion. Returns the final location*)
let promote_check player (col, row) log =
  let valid_rows = promotion_rows player in
  if List.mem row valid_rows then (col, row)
  else (
    log :=
      "Invalid Promotion: Valid Rows to Promote are "
      ^ list_printer valid_rows (String.make 1);
    failwith "Invalid Promotion")

(**[auto_promote_check player piece location will_promote log] returns true if
   piece is already being promoted or if it is not and a promototion is required
   : pawn, lance in last row and knight is second*)
let auto_promote_check player piece (_, row) will_promote log =
  if will_promote && not piece.promoted then true
  else
    let last_rows = promotion_rows player in
    let last_row, second_last_row =
      (List.nth last_rows 0, List.nth last_rows 1)
    in
    match piece.piecetype with
    | (Pawn | Lance) when row = last_row ->
        if not piece.promoted then (
          log := "Piece auto-promoted due to being in last row";
          true)
        else false
    | Knight when row = second_last_row ->
        if not piece.promoted then (
          log := "Knight auto-promoted due to being in second to last row";
          true)
        else false
    | _ -> false

let move_piece ?(admin_mode = false) ?(log = ref "") (curr_loc : location)
    (final_loc : location) (board : board_type) (player : int)
    (will_promote : bool) : board_type =
  match List.assoc_opt curr_loc board with
  | None -> board
  | Some piece when admin_mode || get_player piece = player ->
      if
        admin_mode |> not
        && List.mem final_loc (valid_move_list piece curr_loc board) |> not
      then (
        log :=
          "Piece Cannot Move To This Location \nValid Moves are "
          ^ list_printer (valid_move_list piece curr_loc board) (fun (lx, ly) ->
                string_of_int lx ^ ", " ^ String.make 1 ly);
        failwith "Piece Cannot Move To This Location")
      else
        let remove_current_loc =
          List.filter (fun x -> if fst x = curr_loc then false else true) board
        in
        let remove_final_loc =
          List.filter
            (fun x -> if fst x = final_loc then false else true)
            remove_current_loc
        in
        let will_promote =
          auto_promote_check player piece final_loc will_promote log
        in
        if will_promote then
          (promote_check player final_loc log, promote_piece piece)
          :: remove_final_loc
        else (final_loc, piece) :: remove_final_loc
  | _ ->
      log := "Cannot Move Other Player's Piece";
      failwith "Cannot Move Other Player's Piece"

(* CODE TO IMPLEMENT CHECK *)

(**Ordered type on locations*)
module Location = struct
  type t = int * char

  let compare (x0, y0) (x1, y1) =
    Stdlib.compare
      (string_of_int x0 ^ String.make 1 y0)
      (string_of_int x1 ^ String.make 1 y1)
end

module PossibleMoves = Set.Make (Location)

(**[possible_moves piece board] returns set of all the posssible moves piece can
   take on the board*)
let possible_moves (((lx, ly), p) : piece_rep) (board : board_type) =
  PossibleMoves.of_list (valid_move_list p (lx, ly) board)

(** The type that determines the current state of check *)
type check_type =
  | None
  | Check
  | Checkmate

(** [check_helper board empty player] iterates through all the squares the king
    can move and the kings square determining if there is a check or checkmate*)
let rec check_helper (board : board_type) opposing_players king_position =
  match opposing_players with
  | [] -> false
  | h :: t ->
      let move_set = possible_moves h board in
      if PossibleMoves.mem king_position move_set then true
      else check_helper board t king_position

(** [find_king player] finds the position of the the player's king on the board
    returning a location*)
let rec find_king player = function
  | [] -> failwith "Representation Invariant Violated: King Not Found"
  | (location, piece) :: _
    when Piece.get_player piece = player && Piece.is_king piece -> location
  | _ :: t -> find_king player t

(**[check_bool board player] checks whether the corresponding player's king has
   a check or checkmate agains tthem or not*)
let check_bool board player =
  let king_position = find_king player board in
  check_helper board (remove_player board player) king_position

(**[check board player] checks if the correspondng player's king is in a check,
   checkmate, or neither'*)
let rec check board player =
  match check_bool board player with
  | true -> checkmate (only_player board player) board player
  | false -> None

(**[checkmate move_list board player] determines whether the player is in check
   or checkmate. Requires: Player is either in check or checkmate*)
and checkmate player_move_list board player =
  match checkmate_helper player_move_list board player with
  | true -> Checkmate
  | false -> Check

(**[checkmate_helper move_list board player] iterates through each piece
   checking if moving it could get the player out of check. Returns true if
   player cannot get out of check*)
and checkmate_helper player_move_list board player =
  match player_move_list with
  | [] -> true
  | (loc, p) :: t ->
      checkmate_piece_helper loc (valid_move_list p loc board) board player
      && checkmate_helper t board player

(**[checkmate_piece_helper og_position moves board player] iterates through a
   piece's moves checking if that move could get the player out of check.
   Returns true if player is unable to get out of check by moving given piece*)
and checkmate_piece_helper og_position moves board player =
  match moves with
  | [] -> true
  | h :: t ->
      let new_board = move_piece og_position h board player false in
      check_bool new_board player
      && checkmate_piece_helper og_position t board player
