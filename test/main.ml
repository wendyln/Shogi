open OUnit2
open Game
open Board
open State

(*Test Plan: We will be describing our testing plan module by module. For
  modules Piece, Board, Computer and State, these are all modules that we can
  test in OUnit, so we have decided to make use of OUnit to write test cases to
  test our modules. However, in the main game module, Crazy module (which
  implements an alternate, random-move game mode) and Computer module (which is
  our chess AI), it is harder to test in OUnit as it is not apparent what the
  outputs are. Hence, we mostly made use of testing through playing. In
  addition, we introduced a 0 player mode for testing, which is a hidden option
  in which we can pit two AIs (both using the code in the Computer module)
  against each other. This allows us to look out for any unexpected behavior
  both in the Computer module and in the main game logic modules in general.

  The test cases were developed through a mixture of glass box testing and black
  box testing. One of the first things we tested was the comparision function
  between pieces, which, once verified to be correct, allowed us to use the
  function in future functions to test our other functions more easily. For
  modules in which the branches are limited, we used glass box testing to try to
  ensure every branch is tested, hence ensuring the correctness of the
  functions. On the other hand, for the modules which have more intricate
  branching we instead did black box testing. We would test edge cases such as
  end of board, collisions, valid and invalid captures to try to ensure that the
  edge cases are covered and tested.

  Through the combination of black and glass box testing, in addition to
  intensive playtesting through the use of 0 player mode, we believe that the
  correctness of our system has been verified. *)

let move_piece_board = move_piece_board ~admin_mode:true
let move_piece = move_piece ~admin_mode:true
(* game = new_game *)
(* game1 = white pawn moved forward 8g8e*)
(* game2 = black bishop move diagonal 2b5e*)
(* game3 = white pawn capture black bishop 5g5e*)
(* game4 = Black Gold capture white pawn  6a8e*)
(* game5 = White drop Bishop to 7d*)
(* empty_board = board with no pieces*)

let game = State.new_game ()
let start_board = get_board game
let game1 = move_piece_board game "8g8e"
let board1 = get_board game1
let game2 = move_piece_board game1 "2b5e"
let board2 = get_board game2
let game3 = move_piece_board game2 "5g5e"
let board3 = get_board game3
let game4 = move_piece_board game3 "6a2h"

(* let board4 = get_board game4 *)
let game5 = move_piece_board game4 "9i9a"
let game6 = move_piece_board game5 "8a9a"

(* let game4 = State.move_piece_board game3 "B*6d" ~admin_mode:true *)
let game_no_pawn_on_rank9_1 = move_piece_board game "9g8g"
let bishop_on_5e = move_piece_board game "8h5e"
let empty_board = Board.empty_board

let lone_gold_bot =
  Board.add_piece empty_board (Piece.create_piece 1 "g") (5, 'e')

let lone_silver_bot =
  Board.add_piece empty_board (Piece.create_piece 1 "s") (5, 'e')

let lone_knight_bot =
  Board.add_piece empty_board (Piece.create_piece 1 "n") (5, 'e')

let lone_bishop_bot =
  Board.add_piece empty_board (Piece.create_piece 1 "b") (5, 'e')

let lone_pawn_bot =
  Board.add_piece empty_board (Piece.create_piece 1 "p") (5, 'e')

let lone_pawn_bot_rowb =
  Board.add_piece empty_board (Piece.create_piece 1 "p") (5, 'b')

let lone_rook_bot =
  Board.add_piece empty_board (Piece.create_piece 1 "r") (5, 'e')

let lone_gold_top =
  Board.add_piece empty_board (Piece.create_piece 0 "G") (5, 'e')

let lone_silver_top =
  Board.add_piece empty_board (Piece.create_piece 0 "S") (5, 'e')

let string_repr final_loc =
  match final_loc with
  | x1, x2 -> string_of_int x1 ^ Char.escaped x2

let move_piece_board_test (name : string) (curr_loc : int * char)
    (final_loc : int * char) (board : board_type) (expected_output : string)
    (player : int) =
  let new_board = move_piece curr_loc final_loc board player false in
  name >:: fun _ ->
  assert_equal expected_output
    (get_piece (string_repr final_loc) new_board)
    ~printer:String.escaped

let compare_piece_test (name : string) (p1 : Piece.t) (p2 : Piece.t)
    (expected_output : int) =
  name >:: fun _ -> assert_equal (Piece.compare p1 p2) expected_output

let promote_piece_test name (p : Piece.t) (expected_player, expected_name) =
  name >:: fun _ ->
  assert_equal (Piece.promote_piece p)
    (Piece.create_piece expected_player expected_name)

let list_of_pieces =
  [
    ("l0", Piece.create_piece 0 "l");
    ("n0", Piece.create_piece 0 "n");
    ("s0", Piece.create_piece 0 "s");
    ("g0", Piece.create_piece 0 "g");
    ("k0", Piece.create_piece 0 "k");
    ("r0", Piece.create_piece 0 "r");
    ("b0", Piece.create_piece 0 "b");
    ("p0", Piece.create_piece 0 "p");
    ("l1", Piece.create_piece 1 "l");
    ("n1", Piece.create_piece 1 "n");
    ("s1", Piece.create_piece 1 "s");
    ("g1", Piece.create_piece 1 "g");
    ("k1", Piece.create_piece 1 "k");
    ("r1", Piece.create_piece 1 "r");
    ("b1", Piece.create_piece 1 "b");
    ("p1", Piece.create_piece 1 "p");
  ]

let printer_moveslist lst =
  let rec print_helper = function
    | [] -> ""
    | (i, c) :: t ->
        "(" ^ Int.to_string i ^ Char.escaped c ^ ");" ^ print_helper t
  in
  "[" ^ print_helper lst ^ "]"

let move_list_test name pos board expected_output =
  let piece = List.assoc pos board in
  let uniq1 = List.sort_uniq compare (Board.valid_move_list piece pos board) in
  let uniq2 = List.sort_uniq compare expected_output in
  name >:: fun _ -> assert_equal uniq1 uniq2 ~printer:printer_moveslist

let move_piece_board_tests =
  [
    move_piece_board_test "white move piece to empty 8h7h" (8, 'h') (7, 'h')
      start_board "b" 1;
    move_piece_board_test "white move piece to empty 8h3b" (8, 'h') (3, 'b')
      board1 "b" 1;
    move_piece_board_test "black move piece to empty 9a8d" (9, 'a') (8, 'd')
      start_board "L" 0;
    move_piece_board_test "black move piece to empty 5a9h" (5, 'a') (9, 'h')
      start_board "K" 0;
    move_piece_board_test "piece over piece" (8, 'h') (2, 'h') start_board "b" 1;
    move_piece_board_test "piece over piece game 3" (5, 'c') (5, 'e') board3 "P"
      0;
    move_piece_board_test "game2" (5, 'e') (8, 'h') board2 "B" 0;
  ]

let str_capture lst = List.fold_left (fun acc x -> x ^ ", " ^ acc) "" lst

let str_cap_test t =
  match t with
  | x, y -> "( " ^ "[" ^ str_capture x ^ "] , [" ^ str_capture y ^ " ]"

let rec cap_list_repr lst =
  match lst with
  | [] -> []
  | h :: t -> Piece.repr_piece h :: cap_list_repr t

let capture_test (name : string) (final_loc : int * char) (board : board_type)
    (cB : Piece.t list) (cW : Piece.t list) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    ( cap_list_repr (fst (capture final_loc board cB cW)),
      cap_list_repr (snd (capture final_loc board cB cW)) )
    ~printer:str_cap_test

let capture_state_test (name : string) (game : State.game) command
    (expected_output : string list * string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    ( cap_list_repr (fst (State.get_captured (move_piece_board game command))),
      cap_list_repr (snd (State.get_captured (move_piece_board game command)))
    )
    ~printer:str_cap_test

let capture_tests =
  [
    capture_test "cB empty, cW empty, capture black" (8, 'c') start_board [] []
      ([], [ "p" ]);
    capture_test "empty, empty, game1" (7, 'i') board1 [] [] ([ "S" ], []);
    capture_test "capture white rook game1" (2, 'h') board2 [] [] ([ "R" ], []);
    capture_test "captured list nonempty, game1" (7, 'i') board1
      [ Piece.create_piece 0 "S" ]
      []
      ([ "S"; "S" ], []);
    capture_test "game 3, white list has Bishop" (8, 'a') start_board []
      [ Piece.create_piece 1 "B" ]
      ([], [ "n"; "b" ]);
    capture_state_test "game 0 5g5e" game "5g5e" ([], []);
    capture_state_test "game 0 5g5c" game "5g5c" ([], [ "p" ]);
    capture_state_test "game 1 8h2b" game1 "8h2b" ([], [ "b" ]);
    capture_state_test "game 1 5g5c" game1 "5g5c" ([], [ "p" ]);
    capture_state_test "game 1 5c1i" game1 "5c1i" ([ "L" ], []);
    capture_state_test "game 1 8b8e" game1 "8b8e" ([ "P" ], []);
    capture_state_test "game 2 5g5e" game2 "5g5e" ([], [ "b" ]);
    capture_state_test "game 2 5c1i" game2 "5c1i" ([ "L" ], []);
    capture_state_test "game 3 4g5e" game3 "4g5e" ([ "P" ], [ "b" ]);
    capture_state_test "game 3 5e5c" game3 "5e5c" ([], [ "p"; "b" ]);
    capture_state_test "game 3 8b8h" game3 "8b8h" ([ "B" ], [ "b" ]);
    capture_state_test "game 4 9a9g" game4 "5g5e" ([ "P"; "R" ], [ "b" ]);
    capture_state_test "game 4 5e5c" game4 "5e5c" ([ "R" ], [ "p"; "b" ]);
    capture_state_test "game 4 5e4a" game4 "5e4a" ([ "R" ], [ "g"; "b" ]);
    capture_state_test "game 5 6e6d" game5 "6e6d" ([ "R" ], [ "l"; "b" ]);
    capture_state_test "game 5 8b8i" game5 "8b8i" ([ "N"; "R" ], [ "l"; "b" ]);
    capture_state_test "game 6 5i5h" game6 "5i5h" ([ "L"; "R" ], [ "l"; "b" ]);
  ]

let compare_piece_tests =
  [
    compare_piece_test "pawn pawn eq"
      (List.assoc "p0" list_of_pieces)
      (List.assoc "p0" list_of_pieces)
      0;
    compare_piece_test "pawn rook lt"
      (List.assoc "p0" list_of_pieces)
      (List.assoc "r0" list_of_pieces)
      (-9);
    compare_piece_test "pawn lance lt"
      (List.assoc "p0" list_of_pieces)
      (List.assoc "l0" list_of_pieces)
      (-2);
    compare_piece_test "pawn knight lt"
      (List.assoc "p0" list_of_pieces)
      (List.assoc "n0" list_of_pieces)
      (-3);
    compare_piece_test "pawn silver lt"
      (List.assoc "p0" list_of_pieces)
      (List.assoc "s0" list_of_pieces)
      (-4);
    compare_piece_test "pawn gold lt"
      (List.assoc "p0" list_of_pieces)
      (List.assoc "g0" list_of_pieces)
      (-5);
    compare_piece_test "pawn bishop lt"
      (List.assoc "p0" list_of_pieces)
      (List.assoc "b0" list_of_pieces)
      (-7);
    compare_piece_test "pawn king lt"
      (List.assoc "p0" list_of_pieces)
      (List.assoc "k0" list_of_pieces)
      (-9999);
    compare_piece_test "pawn pawn eq diff owner"
      (List.assoc "p0" list_of_pieces)
      (List.assoc "p1" list_of_pieces)
      0;
    compare_piece_test "lance rook lt"
      (List.assoc "l0" list_of_pieces)
      (List.assoc "r0" list_of_pieces)
      (-7);
    compare_piece_test "lance lance eq"
      (List.assoc "l0" list_of_pieces)
      (List.assoc "l0" list_of_pieces)
      0;
    compare_piece_test "lance knight lt"
      (List.assoc "l0" list_of_pieces)
      (List.assoc "n0" list_of_pieces)
      (-1);
    compare_piece_test "lance silver lt"
      (List.assoc "l0" list_of_pieces)
      (List.assoc "s0" list_of_pieces)
      (-2);
    compare_piece_test "lance gold lt"
      (List.assoc "l0" list_of_pieces)
      (List.assoc "g0" list_of_pieces)
      (-3);
    compare_piece_test "lance bishop lt"
      (List.assoc "l0" list_of_pieces)
      (List.assoc "b0" list_of_pieces)
      (-5);
    compare_piece_test "lance king lt"
      (List.assoc "l0" list_of_pieces)
      (List.assoc "k0" list_of_pieces)
      (-9997);
  ]

let move_list_tests =
  [
    move_list_test "pawn moves new board" (3, 'g') (get_board game) [ (3, 'f') ];
    move_list_test "king moves new board" (5, 'i') (get_board game)
      [ (6, 'h'); (5, 'h'); (4, 'h') ];
    move_list_test "king top moves new board" (5, 'a') (get_board game)
      [ (6, 'b'); (5, 'b'); (4, 'b') ];
    move_list_test "lance moves new board" (9, 'i') (get_board game)
      [ (9, 'h') ];
    move_list_test "knight moves new board" (8, 'i') (get_board game) [];
    move_list_test "bishop moves new board" (2, 'b') (get_board game) [];
    move_list_test "bishop moves new board" (2, 'b') (get_board game) [];
    move_list_test "bishop moves center board" (5, 'e') (get_board bishop_on_5e)
      [ (6, 'd'); (7, 'c'); (4, 'd'); (3, 'c'); (6, 'f'); (4, 'f') ];
    move_list_test "lance moves new board no pawn" (9, 'i')
      (get_board game_no_pawn_on_rank9_1)
      [ (9, 'h'); (9, 'g'); (9, 'f'); (9, 'e'); (9, 'd'); (9, 'c') ];
    move_list_test "bot gold moves empty board center" (5, 'e') lone_gold_bot
      [ (6, 'd'); (5, 'd'); (4, 'd'); (6, 'e'); (4, 'e'); (5, 'f') ];
    move_list_test "top gold moves empty board center" (5, 'e') lone_gold_top
      [ (6, 'f'); (5, 'f'); (4, 'f'); (6, 'e'); (4, 'e'); (5, 'd') ];
    move_list_test "bot silver moves empty board center" (5, 'e')
      lone_silver_bot
      [ (6, 'd'); (5, 'd'); (4, 'd'); (4, 'f'); (6, 'f') ];
    move_list_test "top silver moves empty board center" (5, 'e')
      lone_silver_top
      [ (6, 'f'); (5, 'f'); (4, 'f'); (4, 'd'); (6, 'd') ];
    move_list_test "bot knight moves empty board center" (5, 'e')
      lone_knight_bot
      [ (6, 'c'); (4, 'c') ];
    move_list_test "bot bishop moves empty board center" (5, 'e')
      lone_bishop_bot
      [
        (1, 'i');
        (2, 'h');
        (3, 'g');
        (4, 'f');
        (6, 'd');
        (7, 'c');
        (8, 'b');
        (9, 'a');
        (1, 'a');
        (2, 'b');
        (3, 'c');
        (4, 'd');
        (6, 'f');
        (7, 'g');
        (8, 'h');
        (9, 'i');
      ];
    move_list_test "bot rook moves empty board center" (5, 'e') lone_rook_bot
      [
        (5, 'a');
        (5, 'b');
        (5, 'c');
        (5, 'd');
        (5, 'f');
        (5, 'g');
        (5, 'h');
        (5, 'i');
        (1, 'e');
        (2, 'e');
        (3, 'e');
        (4, 'e');
        (6, 'e');
        (7, 'e');
        (8, 'e');
        (9, 'e');
      ];
    move_list_test "pawn rook empty board center" (5, 'e') lone_pawn_bot
      [ (5, 'd') ];
    move_list_test "pawn in empty row b" (5, 'b') lone_pawn_bot_rowb
      [ (5, 'a') ];
  ]

let promote_piece_tests =
  [
    promote_piece_test "lance 0" (List.assoc "l0" list_of_pieces) (0, "+l");
    promote_piece_test "rook 0" (List.assoc "r0" list_of_pieces) (0, "+r");
    promote_piece_test "rook 1" (List.assoc "r1" list_of_pieces) (1, "+r");
    promote_piece_test "pawn 1" (List.assoc "p1" list_of_pieces) (1, "+p");
  ]

let suite =
  "test suite for shogi"
  >::: List.flatten
         [
           move_piece_board_tests;
           capture_tests;
           compare_piece_tests;
           move_list_tests;
           promote_piece_tests;
         ]

let _ = run_test_tt_main suite
