open Piece
open Board

(* Shogi Board Marking
   https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Shogiban.png/525px-Shogiban.png*)

exception Invalid_Move
exception Invalid_Pos

exception Invalid_Move_Player
(**Raised when the move is invalid because the active player is incorrect*)

exception Invalid_Move_Check
(**Raised when the move is invalid because there is a check*)

exception Invalid_Drop
(**Raised when the drop is invalid*)

exception Invalid_Promotion
(**Raised when the proomotion is invalid*)

(*Black : player 0 , White: player 1 *)
type turn_type =
  | Black
  | White

type game = {
  turn : turn_type;
  cap_B : t list;
  cap_W : t list;
  board : board_type;
  check : check_type;
  game_over : bool;
  log : string ref;
}

let new_game () =
  {
    turn = White;
    cap_B = [];
    cap_W = [];
    board = new_board;
    check = None;
    game_over = false;
    log = ref "";
  }

let end_game state = state.game_over
let get_board game = game.board

let get_turn game =
  match game.turn with
  | Black -> 0
  | White -> 1

let change_turn = function
  | Black -> White
  | White -> Black

(**[get_piece loc board] returns the piece at loc on board*)
let rec get_piece (loc : string) (board : board_type) =
  let int_char_loc =
    (int_of_string (String.sub loc 0 1), String.get (String.sub loc 1 1) 0)
  in
  match board with
  | [] -> " "
  | (x1, x2) :: t ->
      if x1 = int_char_loc then repr_piece x2 else get_piece loc t

(** [get_captured g] is the tuple of (l1,l1) where l1 is the list of black
    pieces captured and l2 is the list of white pieces captured.*)
let get_captured game = (game.cap_B, game.cap_W)

(** [interpret_move command] interpret the type of move given a command*)
let interpret_move command : (int * char) * (int * char) =
  try
    let comm = String.sub command 0 4 in
    let int1 = int_of_string (String.sub comm 0 1) in
    let char1 = String.get (String.sub comm 1 1) 0 in
    let int2 = int_of_string (String.sub comm 2 1) in
    let char2 = String.get (String.sub comm 3 1) 0 in
    if
      int1 < 1 && int1 > 9 && int2 < 1 && int2 > 9
      && Char.code char1 - Char.code 'a' < 0
      && Char.code char1 - Char.code 'a' > 8
      && Char.code char2 - Char.code 'a' < 0
      && Char.code char1 - Char.code 'a' > 8
    then raise Invalid_Pos
    else ((int1, char1), (int2, char2))
  with _ -> raise Invalid_Move

type move =
  | Promote
  | Drop
  | Standard

(** [move_type command] returns the type of move given a command*)
let move_type command =
  if String.length command = 5 && String.sub command 4 1 = "+" then Promote
  else if String.length command = 4 && String.sub command 1 1 = "*" then Drop
  else if String.length command = 4 then Standard
  else raise Invalid_Move

(**[comm_drop command] interprets the command for dropping of piece *)
let comm_drop command =
  try
    ( Char.escaped (String.get (String.sub command 0 1) 0),
      ( int_of_string (String.sub command 2 1),
        String.get (String.sub command 3 1) 0 ) )
  with _ -> raise Invalid_Move

(* let rec check_drop cap_list piece = match cap_list with | [] -> false | h ::
   t -> if h = piece then true else check_drop t piece *)

(** [drop_piece_board] try find the piece in capture if yes then drop it if no
    raise exn *)
let drop_piece_board { turn; cap_B; cap_W; check = _; board; game_over; log }
    command =
  try
    log := "";
    let int_turn = if turn = Black then 0 else 1 in
    let drop_p = create_piece int_turn (fst (comm_drop command)) in
    let final_loc = snd (comm_drop command) in
    let cap_list = if int_turn = 0 then cap_B else cap_W in
    let new_board = drop_piece ~log drop_p final_loc board cap_list in
    let check = Board.check new_board int_turn in
    match turn with
    | White ->
        {
          turn = change_turn turn;
          cap_B;
          cap_W = new_cap drop_p cap_W;
          board = new_board;
          check;
          game_over;
          log;
        }
    | Black ->
        {
          turn = change_turn turn;
          cap_B = new_cap drop_p cap_B;
          cap_W;
          board = new_board;
          check;
          game_over;
          log;
        }
  with _ -> raise Invalid_Drop

let swap_turns game =
  {
    game with
    turn =
      (match game.turn with
      | Black -> White
      | White -> Black);
  }

(** [n_chars c n] is a string with the string c printed n times. *)
let rec n_chars c = function
  | 0 -> ""
  | n -> c ^ n_chars c (n - 1)

(** [grab_starting a lst] is a string that grabs the next three elements
    starting at index a of a list and makes them a string *)
let grab_starting a lst =
  let len = List.length lst in
  if (* grab 0 *) a >= len then n_chars " " 14
  else if (* grab 1 *) a = len - 1 then
    n_chars " " 3 ^ List.nth lst a ^ n_chars " " 10
  else if (* grab 2 *) a = len - 2 then
    n_chars " " 3 ^ List.nth lst a ^ n_chars " " 2
    ^ List.nth lst (a + 1)
    ^ n_chars " " 7
  else
    (* grab 3 *)
    n_chars " " 3 ^ List.nth lst a ^ n_chars " " 2
    ^ List.nth lst (a + 1)
    ^ n_chars " " 2
    ^ List.nth lst (a + 2)
    ^ n_chars " " 4

let string_of_state_aux = function
  | { board; cap_W; cap_B; turn; log; _ } ->
      let left_spaces = n_chars " " 14 in
      let delineator = left_spaces ^ n_chars "-" 37 in
      let lst = Board.order_list board in

      let represent_captured piece_lst compare_function =
        piece_lst |> List.sort compare_function |> List.map Piece.repr_piece
      in

      let captured_uppers =
        List.map String.lowercase_ascii
          (List.rev (represent_captured cap_B Piece.compare))
      in
      let captured_lowers =
        List.map String.uppercase_ascii
          (List.rev (represent_captured cap_W Piece.compare))
      in

      let return_str, _, _ =
        List.fold_left
          (fun (prev_str, row_char, captured_lists_index) (i, c) ->
            let piece_string = List.assoc (i, c) lst in
            let piece =
              if String.length piece_string = 1 then piece_string ^ " "
              else piece_string
            in

            if i = 1 then
              ( prev_str ^ "| " ^ piece ^ "| " ^ Char.escaped row_char
                ^ n_chars " " 1
                ^ grab_starting captured_lists_index captured_uppers
                ^ "\n" ^ delineator ^ "\n",
                row_char |> int_of_char |> ( + ) 1 |> char_of_int,
                captured_lists_index + 3 )
            else if i = 9 then
              ( prev_str
                ^ grab_starting captured_lists_index captured_lowers
                ^ "| " ^ piece,
                row_char,
                captured_lists_index )
            else (prev_str ^ "| " ^ piece, row_char, captured_lists_index))
          ( "\n" ^ n_chars " " 1 ^ "PIECES WHITE" ^ n_chars " " 42
            ^ "PIECES BLACK" ^ "\n" ^ n_chars " " 3 ^ "CAPTURED" ^ n_chars " " 5
            ^ "9   8   7   6   5   4   3   2   1" ^ n_chars " " 8 ^ "CAPTURED"
            ^ "\n" ^ delineator ^ "\n",
            'a',
            0 )
          (let keys, _ = List.split lst in
           keys)
      in
      let player_turn =
        match turn with
        | Black -> "\nBlack Turn (Top)\n"
        | White -> "\nWhite Turn (Bottom)\n"
      in
      let int_turn = if turn = Black then 0 else 1 in
      let check = Board.check board int_turn in
      let check_status =
        let string_turn = if turn = Black then "Black" else "White" in
        match check with
        | None -> ""
        | Check -> "\nKing is under check for player " ^ string_turn ^ " \n"
        | Checkmate ->
            "\nKing is under checkmate for player " ^ string_turn ^ " \n"
      in
      return_str ^ player_turn ^ check_status ^ "\n" ^ !log ^ "\n"

(**[string_of_state game] returns the string of the game state*)
let string_of_state game = string_of_state_aux game

(**[promote_piece_board gm cmd] promotes a piece given the command*)
let promote_piece_board gm cmd =
  let current_pos, new_pos = interpret_move cmd in
  let int_turn = if gm.turn = Black then 0 else 1 in
  let log = gm.log in
  {
    gm with
    turn = change_turn gm.turn;
    cap_B = fst (capture new_pos gm.board gm.cap_B gm.cap_W);
    cap_W = snd (capture new_pos gm.board gm.cap_B gm.cap_W);
    board = move_piece ~log current_pos new_pos gm.board int_turn true;
  }

(** [move_piece_board game command] takes in a command and return the new game.
    Invalid_Move or Invalid_Pos returns the same board *)
let move_piece_board ?(admin_mode = false)
    { turn; cap_B; cap_W; board; check; game_over; log } command =
  try
    log := "";
    match move_type command with
    | Drop ->
        drop_piece_board
          { turn; cap_B; cap_W; board; game_over; log; check }
          command
    | Promote -> (
        try
          promote_piece_board
            { turn; cap_B; cap_W; board; game_over; log; check }
            command
        with _ -> raise Invalid_Promotion)
    | Standard -> (
        match interpret_move command with
        | current_pos, new_pos -> (
            let int_turn = if turn = Black then 0 else 1 in
            (*Throws exception if try to change wrong players*)
            let new_board =
              try
                move_piece ~admin_mode ~log current_pos new_pos board int_turn
                  false
              with _ -> raise Invalid_Move_Player
            in
            let turn = change_turn turn in
            let int_turn = if turn = Black then 0 else 1 in
            let check_after_move = Board.check new_board int_turn in
            match (check, check_after_move) with
            | Check, Check -> raise Invalid_Move_Check
            | Checkmate, _ ->
                failwith "Representation Invariant Failed: Move After Checkmate"
            | _ ->
                let game_over =
                  match check_after_move with
                  | Checkmate -> true
                  | _ -> false
                in
                {
                  turn;
                  cap_B = fst (capture new_pos board cap_B cap_W);
                  cap_W = snd (capture new_pos board cap_B cap_W);
                  board = new_board;
                  check = check_after_move;
                  game_over;
                  log;
                }))
  with
  | Invalid_Move ->
      log := "Invalid Move Command, try again";
      { turn; cap_B; cap_W; board; check; game_over; log }
  | Invalid_Pos ->
      log := "Invalid Position, try again";
      { turn; cap_B; cap_W; board; check; game_over; log }
  | Invalid_Move_Check ->
      log := "Player is under check, must defend their king";
      { turn; cap_B; cap_W; board; check; game_over; log }
  | Invalid_Move_Player ->
      log := "Invalid Move: " ^ !log;
      { turn; cap_B; cap_W; board; check; game_over; log }
  | Invalid_Drop ->
      log := "Invalid Drop: " ^ !log;
      { turn; cap_B; cap_W; board; check; game_over; log }
  | Invalid_Promotion ->
      log := "Invalid Promotion: " ^ !log;
      { turn; cap_B; cap_W; board; check; game_over; log }

(**Turn curr_loc, final_loc into string command - helper for possible_moves *)
let create_command curr_loc final_loc =
  string_of_int (fst curr_loc)
  ^ Char.escaped (snd curr_loc)
  ^ string_of_int (fst final_loc)
  ^ Char.escaped (snd final_loc)

(**for each piece, return the games of all possible moves of that piece - helper*)

(* IMPLEMENT VALID DROP *)
(* let possible_drops game player = let two_captured = get_captured game in let
   captured = if player = 1 then (snd two_captured) else (fst two_captured)
   in *)
(* let valid_drop_list game player = let captured = get_captured game in let
   c_list = if player = 0 then fst captured else snd captured in *)

let possible_games_piece game piece player =
  let board = get_board game in
  let curr_loc = pos_piece board piece in
  let check_filter =
    if game.check = Check then fun new_loc ->
      match
        check (move_piece curr_loc new_loc game.board player false) player
      with
      | None -> true
      | _ -> false
    else fun _ -> true
  in
  let moves = List.filter check_filter (valid_move_list piece curr_loc board) in
  let f acc move =
    let command = create_command curr_loc move in
    (command, move_piece_board game command) :: acc
  in
  List.fold_left f [] moves

(* for each board, return all possible next states of the game with associated
   commands *)
let tagged_possible_states game player =
  let board = get_board game in
  let f acc piece_rep =
    let piece = snd piece_rep in
    if get_player piece = player then
      possible_games_piece game piece player :: acc
    else acc
  in
  board |> List.fold_left f [] |> List.flatten

(* for each board, return all possible next state of the game *)
let possible_moves (game : game) (player : int) : game list =
  let _, games = List.split (tagged_possible_states game player) in
  games
