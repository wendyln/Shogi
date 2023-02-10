open Piece
open State
open Board (*one player game : player - white(1), bot - black(0) *)

(** [white player] returns true if the player is white, returns false if player
    is black*)
let white player = if player = 1 then true else false

(** [value_piece p ] returns the value of each piece, black is negative, white
    is positive *)
let value_piece (p : Piece.t) (player : int) =
  if player = 0 then -1 * value_of_piece p else value_of_piece p

(** [capture_value_game game] returns the total value of captured list for each
    game *)
let capture_value_game game =
  let captured_B = fst (get_captured game) in
  let captured_W = snd (get_captured game) in
  let f acc x =
    let player = get_player x in
    value_piece x player + acc
  in
  List.fold_left f 0 captured_B + List.fold_left f 0 captured_W

(* [piece_value_eval game] evalutes the total value of pieces present in the
   current game. Higher rank piece has higher value. Black pieces have negative
   value, white pieces have positive value. *)
let piece_value_eval game =
  let board = get_board game in
  let captured_val = capture_value_game game in
  let board_val =
    let f acc x =
      let piece = snd x in
      let player = get_player piece in
      value_piece piece player + acc
    in
    List.fold_left f 0 board
  in
  captured_val + board_val

(** KING SAFETY = escape square - attacker + defender*)

(**[escape_square board player] returns the number of empty squares around the
   player's king*)

let escape_square board player =
  let pos_king = find_king player board in
  let king = create_piece player "K" in
  let empty_spots = valid_move_list king pos_king board in
  List.length empty_spots

(**[escape_square_B board] returns the number of escape squares for player black *)
let escape_square_B board = escape_square board 0

(**[escape_square_W board] returns the number of escape squares for player white *)
let escape_square_W board = escape_square board 1

(** [b_attack_w pos] is a helper function for finding the number of attackers
    attacking white. returns true if the piece at the position pos is at the 3
    bottom rows of the white's side*)

let b_attack_w pos =
  let charac = snd pos in
  let asciii = Char.code charac in
  if asciii >= 66 && asciii <= 69 then true else false

(** [check_w_defend] returns true if a white's piece is at the bottom 3 rows of
    white's side*)
let check_w_defend = b_attack_w

(** [w_attack_b pos] is a helper function for finding the number of attackers
    attacking black. returns true if the piece at the position pos is at the top
    3 rows of the black's side*)

let w_attack_b pos =
  let charac = snd pos in
  let asciii = Char.code charac in
  if asciii >= 61 && asciii <= 64 then true else false

(** [check_b_defend] returns true if a black's piece is at the top 3 rows
    defending black*)
let check_b_defend = w_attack_b

(**[capture_attacks captured player] returns the value of potential attacking
   pieces from opponent's captured list *)

let capture_attacks captured player =
  let attackers = if white player then snd captured else fst captured in
  let f acc x = value_of_piece x + acc in
  List.fold_left f 0 attackers

(**[attackes board captured player] returns the value of the attackers *)

let attackers board captured player =
  let captured_attack = capture_attacks captured player in
  let opponent = if white player then 0 else 1 in
  let attack_fn = if white player then b_attack_w else w_attack_b in
  let f acc x =
    let loc = fst x in
    let p = snd x in
    if get_player p = opponent && attack_fn loc then acc + 1 else acc
  in
  List.fold_left f 0 board + captured_attack

(* let attackers board captured player = let captured_attack = capture_attacks
   captured player in let opponent = if white player then 0 else 1 in let
   attack_fn = if white player then b_attack_w else w_attack_b in let f acc x =
   let loc = fst x in let p = snd x in if get_player p = opponent && attack_fn
   loc then acc + Int.abs (value_piece p opponent) else acc in List.fold_left f
   0 board + captured_attack *)

(**[defenders board  player] returns the value of the defenders *)

let defenders board player =
  let defend_fn = if white player then check_w_defend else check_b_defend in
  let f acc x =
    let loc = fst x in
    let p = snd x in
    if get_player p = player && defend_fn loc then acc + 1 else acc
  in
  List.fold_left f 0 board

(** [king_state board captured player] returns the king's safety value of each
    player *)

let king_state board captured player =
  if white player then
    (-4 * escape_square_W board)
    + (-3 * attackers board captured 1)
    + (2 * defenders board 1)
  else
    (-4 * escape_square_B board)
    + (-3 * attackers board captured 0)
    + (2 * defenders board 0)

(** [king_safety_eval game] returns the king's safety value of the game *)

let king_safety_eval game =
  (* let _ = print_endline ("king_safety_eval_game" ^ string_of_state game)
     in *)
  let board = get_board game in
  let captured_list = get_captured game in
  king_state board captured_list 1 - king_state board captured_list 0

(** [square_control game] returns the absolute difference of the number of
    squares controlled by each player *)

(**= squares controlled by white (minus) squares controlled by black *)

let square_control game =
  let board = get_board game in
  let f acc x =
    let p = snd x in
    if white (get_player p) then acc + 1 else acc - 1
  in

  List.fold_left f 0 board

(*[evaluation game] returns the value of the game. Black aims to minimize this
  value, White aims to maximize this value. Evaluation = piece_value +
  king_safety + square_control*)
let evaluation game =
  (* let _ = print_endline ("value of piece_value " ^ string_of_int (piece_value
     game)) in let _ = print_endline ("value of king_safety " ^ string_of_int
     (king_safety_eval game)) in let _ = print_endline ("value of square " ^
     string_of_int (square_control game)) in *)
  (3 * piece_value_eval game) + king_safety_eval game + (5 * square_control game)

(** [minimax_aux game depth maximizer] returns (value, game ) the best game and
    its evaluation value under minimax algorithm. *)

let rec minimax_aux game depth (maximizer : bool) =
  let value = evaluation game in
  (*value of the game *)
  if depth = 0 then (value, game)
  else if maximizer then
    let player = 1 in
    let maxeval = -100000 in
    (*States -> every game possible *)
    let states = State.possible_moves game player in
    let f acc state =
      (*for each game, calculate the value of the game and find the game that
        maximize value*)
      try
        let local_acc = minimax_aux state (depth - 1) false in
        let local_value = fst local_acc in
        (* max_eval = max (acc, value of new game) *)
        if local_value > fst acc then (local_value, state) else acc
      with _ -> acc
    in
    List.fold_left f (maxeval, game) states
  else
    let player = 0 in
    let mineval = 100000 in
    let states = State.possible_moves game player in
    (*States -> every game possible *)
    let f acc state =
      (*for each game, calculate the value and find the game that minimize
        value*)
      try
        let local_acc = minimax_aux state (depth - 1) true in

        let local_value = fst local_acc in
        if local_value < fst acc then (local_value, state) else acc
      with _ -> acc
    in
    List.fold_left f (mineval, game) states

(**[minimax game depth] calls minimax_aux for one player bot. bot is player
   black thus maximizer = false*)

let minimax game depth = minimax_aux game depth false

(**[bot_move game] returns the game that bot will move to *)
let bot_move game = snd (minimax game 2)

(**[one_player game command] starts a one player game *)

let one_player_game game command =
  if get_turn game = 0 then bot_move game else move_piece_board game command

(**[bot_move_white game] returns the game if bot is white *)
let bot_move_white game = snd (minimax_aux game 2 true)

(**[zero_player game command] starts a two bot game *)
let zero_player_game game =
  if get_turn game = 0 then bot_move game else bot_move_white game
