open Game

(* if s = "1" then Game.State.one_player_game state s else if s = "2" then
   Game.State.move_piece_board state s *)

(* let delay (sec : float) : unit Lwt.t = Lwt_unix.sleep sec *)

(** [delay s] is a promise that resolves after about [s] seconds. *)

let rec make_move (state : State.game) mode =
  if mode = 0 then Game.Computer.zero_player_game state
  else
    let _ = print_endline " " in
    print_endline
      "Please input your move in the format of [int*char*int*char] (e.g. 4g4f) \
       followed by enter\n";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> make_move state mode
    | m ->
        if mode = 3 then Game.Crazy.crazy_step state m
        else if mode = 2 then Game.State.move_piece_board state m
        else if mode = 1 && Game.State.get_turn state = 1 then (
          let _ = print_endline "player" in
          let player = Game.State.move_piece_board state m in
          player |> Game.State.string_of_state |> print_endline;
          Unix.sleepf 1.5;
          Game.Computer.bot_move player)
        else raise (Failure "Precondition violated for make_move")

let rec play_game state mode _ =
  Unix.sleepf 0.1;
  try
    state |> Game.State.string_of_state |> print_endline;
    play_game (make_move state mode) mode false
  with Failure s ->
    let _ = print_endline s in
    ()

let rec choose_mode () =
  print_endline
    "Please input '1' for one player game, '2' for 2 player game, '3' for 2 \
     player Crazy Shogi game";
  print_string "> ";
  match read_line () with
  | "0" ->
      print_endline
        "Zero player game selected, you will be watching two bots play!";
      play_game (Game.State.new_game ()) 0 false
  | "1" ->
      print_endline
        "One player game selected, you will be controlling the pieces at the \
         bottom!";
      play_game (Game.State.new_game ()) 1 false
  | "2" ->
      print_endline
        "Two player game selected, player one will be controlling the pieces \
         at the bottom!";
      play_game (Game.State.new_game ()) 2 false
  | "3" ->
      print_endline
        "Two player, Crazy Shogi game selected! Player one will be controlling \
         the pieces at the bottom!";
      play_game (Game.State.new_game ()) 3 false
  | _ -> choose_mode ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the the Shogi game.\n";
  choose_mode ()

(* Execute the game engine. *)
let () = main ()