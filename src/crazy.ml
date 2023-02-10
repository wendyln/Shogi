exception Impossible of string

type phrase =
  | Crazy_Capture of string
  | Crazy_Drop of string
  | Crazy_Move of string
  | No_Crazy

(** [range_of_probability prob] is an integer n that represents the range [0, n]
    out of 100, given a probability prob <= 1.0 *)
let range_of_probability prob = prob |> ( *. ) 100. |> int_of_float |> ( + ) 1

(** [calculate_constant lst] provides a constant by which to multiply each 1/v,
    where v is the value of each piece in lst. *)
let calculate_constant lst =
  let rec calculate_constant_aux = function
    | [] -> 0.
    | (_, p) :: t ->
        (* sums over all pieces belong to player *)
        (1. /. float_of_int (Piece.value_of_piece p))
        +. calculate_constant_aux t
  in
  1. /. calculate_constant_aux lst

(** [get_distribution lst] is an association list that maps each (int * char)
    pair to a tuple of float values between 0 and 1, indicating where on the
    distribution function the board space lies. *)
let get_distribution (lst : ((int * char) * float) list) =
  lst
  |> List.fold_left
       (fun acc (tup, f) ->
         match acc with
         | [] -> [ (tup, (0., f)) ]
         | (prev_tup, (lb, ub)) :: t ->
             (tup, (ub, ub +. f)) :: (prev_tup, (lb, ub)) :: t)
       []
  |> List.rev

(** [probabilities_of_locs const lst] is an association list that binds each int
    * char pair to a probability that that square will get chosen.*)
let rec probabilities_of_locs const = function
  | [] -> []
  | (loc, p) :: t ->
      (loc, 1. /. float_of_int (Piece.value_of_piece p) *. const)
      :: probabilities_of_locs const t

(** [get_board_position lst rand_float] is a (int * char) that corresponds to
    the space on the board randomly selected by the probability distribution
    function. *)
let rec get_board_position (lst : ((int * char) * (float * float)) list)
    rand_float =
  match lst with
  | [] ->
      raise (Impossible "distribution must always have at least one element")
  | [ (tup, _) ] -> tup
  | (tup, (lb, ub)) :: t ->
      if rand_float >= lb && rand_float <= ub then tup
      else get_board_position t rand_float

(** [choose_from_three ()] randomly provides one of the three crazy_phrases *)
let choose_from_three () =
  Random.self_init ();
  match Random.int 3 with
  | 0 -> Crazy_Capture " Has Been Randomly Captured!"
  | 1 -> Crazy_Drop " Has Been Randomly Dropped At "
  | _ ->
      Crazy_Capture
        "Your Move Has Been Overridden! This Random Move Was Played Instead: "

(** [choose_from_two phr1 phr2] randomly provides one of the two given
    crazy_phrases *)
let choose_from_two phr1 phr2 =
  Random.self_init ();
  let cstr = " Has Been Randomly Captured!" in
  let dstr = " Has Been Randomly Dropped At " in
  let mstr =
    "Your Move Has Been Overridden! This Random Move Was Played Instead: "
  in
  let nstr = "No_Crazy will never be passed" in
  match Random.int 2 with
  | 0 -> begin
      match phr1 with
      | Crazy_Capture _ -> Crazy_Capture cstr
      | Crazy_Drop _ -> Crazy_Drop dstr
      | Crazy_Move _ -> Crazy_Move mstr
      | No_Crazy -> raise (Impossible nstr)
    end
  | _ -> begin
      match phr2 with
      | Crazy_Capture _ -> Crazy_Capture cstr
      | Crazy_Drop _ -> Crazy_Drop dstr
      | Crazy_Move _ -> Crazy_Move mstr
      | No_Crazy -> raise (Impossible nstr)
    end

(** [generate_craziness prob_capt prob_drop prob_move] is a phrase that
    represents whether a crazy capture, crazy drop, or crazy move has occured
    given input probabilities of each. If more than one are decided to happen,
    another random choice selects the returned crazy action. *)
let generate_crazy_action prob_capt prob_drop prob_move =
  Random.self_init ();
  let capt_range = range_of_probability prob_capt in
  let drop_range = range_of_probability prob_drop in
  let move_range = range_of_probability prob_move in

  let capt_choice = Random.int 101 in
  let drop_choice = Random.int 101 in
  let move_choice = Random.int 101 in

  let do_capt = capt_choice <= capt_range in
  let do_drop = drop_choice <= drop_range in
  let do_move = move_choice <= move_range in

  if do_capt && do_drop && do_move then choose_from_three ()
  else if do_capt && do_drop then
    choose_from_two (Crazy_Capture "") (Crazy_Drop "")
  else if do_capt && do_move then
    choose_from_two (Crazy_Capture "") (Crazy_Move "")
  else if do_drop && do_move then
    choose_from_two (Crazy_Drop "") (Crazy_Move "")
  else if do_capt then Crazy_Capture " Has Been Randomly Captured!"
  else if do_drop then Crazy_Drop " Has Been Randomly Dropped At "
  else if do_move then
    Crazy_Move
      "Your Move Has Been Overridden! This Random Move Was Played Instead: "
  else No_Crazy

(* USED FOR VERIFY PROBABILITY MASS FUNCTION *)
(* let string_of_float_lst lst = List.fold_left (fun x ((i, c), f) -> if x = ""
   then "(" ^ string_of_int i ^ ", " ^ Char.escaped c ^ ") : " ^ string_of_float
   f else x ^ ", " ^ "(" ^ string_of_int i ^ ", " ^ Char.escaped c ^ ") : " ^
   string_of_float f) "" lst *)

(** [crazy_capture_aux old_state] is a tuple containing the randomly-chosen
    location to be captured, the piece at that location, and the corresponding
    next State.game, which is the old board with an arbitrary piece captured and
    added to the corresponding captured list. *)
let crazy_capture_aux (old_state : State.game) =
  match old_state with
  | { turn; cap_B; cap_W; board; _ } -> (
      Random.self_init ();
      let locs_to_pieces =
        Board.filled_positions board (State.get_turn old_state)
          true (* include king because value is too high to ever get chosen *)
      in
      let locs_to_probs =
        (* Assigns a weighted probability to each space with one of current
           player's piece on it of being selected (excluding kings).
           Higher-valued pieces are less likely to be selected than lower-valued
           pieces. *)
        probabilities_of_locs (calculate_constant locs_to_pieces) locs_to_pieces
      in
      let locs_to_distribution =
        (* print_endline (string_of_float_lst locs_to_probs); *)
        get_distribution locs_to_probs
      in
      let random_float = Random.float 1. in

      (* chosen spot with a piece *)
      let selected_spot =
        get_board_position locs_to_distribution random_float
      in
      let new_board = List.remove_assoc selected_spot board in
      let new_turn = State.change_turn turn in
      let piece = List.assoc selected_spot locs_to_pieces in
      ( selected_spot,
        piece,
        match State.get_turn old_state with
        | 0 ->
            {
              old_state with
              turn = new_turn;
              board = new_board;
              cap_W = piece :: cap_W;
            }
        | _ ->
            {
              old_state with
              turn = new_turn;
              board = new_board;
              cap_B = piece :: cap_B;
            } ))

(** [crazy_capture game command cstr] is a State.game that corresponds to the
    random action of automatically capturing a piece belonging to the
    currently-playing player. *)
let crazy_capture (game : State.game) (command : string) (cstr : string) =
  try
    let (i, c), piece, new_game =
      command
      |> State.move_piece_board game
      |> State.swap_turns |> crazy_capture_aux
    in
    print_endline
      ("\n" ^ "!!! ATTENTION !!! : \n" ^ Piece.repr_piece piece ^ " at " ^ "("
     ^ string_of_int i ^ ", " ^ Char.escaped c ^ ")" ^ cstr);
    new_game
  with _ -> State.move_piece_board game command

(** [crazy_drop_aux old_state] is a State.game with the current player having
    moved, and an arbitrary piece from the player's opponent's captured pieces
    reappearing at a random location on the board. If the captured pieces list
    is empty, then simply return the old state. *)
let crazy_drop_aux (old_state : State.game) =
  Random.self_init ();
  match old_state with
  | { cap_B; cap_W; board; _ } ->
      let captured_of_mover =
        (* selects captured pieces of the mover *)
        if State.get_turn old_state = 0 then cap_B else cap_W
      in
      let piece_to_drop =
        (* selects a random piece to drop within the captured pieces of mover *)
        List.nth captured_of_mover (Random.int (List.length captured_of_mover))
      in
      let i, c =
        (* selects a random empty spot on the board in which to drop the random
           piece *)
        let empty_spots = Board.empty_positions board in
        let index = Random.int (List.length empty_spots) in
        List.nth empty_spots index
      in
      (* constructs command *)
      let str_rep = Piece.repr_piece piece_to_drop in
      let command =
        String.uppercase_ascii str_rep ^ "*" ^ string_of_int i ^ Char.escaped c
      in
      (State.drop_piece_board old_state command, str_rep, (i, c))

(** [crazy_drop game command dstr] is a State.game that encodes the game with a
    random piece drop from the captured list of the player currently playing.*)
let crazy_drop (game : State.game) (command : string) (dstr : string) =
  try
    let st, str_rep, (i, c) =
      State.move_piece_board game command |> State.swap_turns |> crazy_drop_aux
    in
    print_endline
      ("\n" ^ "!!! ATTENTION !!! : \n" ^ str_rep ^ dstr ^ "(" ^ string_of_int i
     ^ Char.escaped c ^ ")");
    st
  with _ -> State.move_piece_board game command

(** [crazy_move_aux old_state] is a State.game that represents a game with a
    random move having taken place instead of the move the current player was
    going to make. *)
let crazy_move_aux (old_state : State.game) =
  Random.self_init ();
  let next_states =
    State.tagged_possible_states old_state (State.get_turn old_state)
  in
  let index = Random.int (List.length next_states) in
  List.nth next_states index

(**[crazy_move game command mstr] is a State.game that represents a random move
   having taken place instead of the original move, based on the current game,
   the player's command given, and the random move message mstr. If the next
   step is the same as the step given by the user, then the game proceeds
   without any messages. *)
let crazy_move (game : State.game) (command : string) (mstr : string) =
  try
    let new_command, new_game = crazy_move_aux game in
    if new_command <> command then (
      print_endline ("\n" ^ "!!! ATTENTION !!! : \n" ^ mstr ^ new_command);
      new_game)
    else failwith "command and new_command are the same; do normal action"
  with _ -> State.move_piece_board game command

let crazy_step game command =
  (* Change these probabilities if you want. 1) Furthest left corresponds to
     probability of random capture of playing player 2) Middle corresponds to
     probabilty of random drop from captured list 3) Furthest right corresponds
     to probability of random move replacement *)
  match generate_crazy_action 0.1 0.1 0.1 with
  | No_Crazy -> State.move_piece_board game command
  | Crazy_Capture str -> crazy_capture game command str
  | Crazy_Drop str -> crazy_drop game command str
  | Crazy_Move str -> crazy_move game command str