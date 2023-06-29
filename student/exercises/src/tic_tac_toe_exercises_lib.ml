open Core
open Tic_tac_toe_2023_common
open Protocol

module Evaluation = struct
  type t =
    | Illegal_state
    | Game_over of { winner : Piece.t option }
    | Game_continues
  [@@deriving sexp_of]

  let to_string (t : t) = t |> sexp_of_t |> Sexp.to_string
end

(* Here are some functions which know how to create a couple different kinds
   of games *)
let empty_game =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let place_piece (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  { game with pieces }
;;

let win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let non_win =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let board_len = Game_kind.board_length game_kind in
  let square_board_list =
    List.init board_len ~f:(fun row ->
      List.init board_len ~f:(fun column -> { Position.row; column }))
  in
  let board_list = List.concat square_board_list in
  List.filter board_list ~f:(fun pos -> not (Map.mem pieces pos))
;;

(* Checking the horizontal options for a set of keys. For a given type of
   piece, we want to check win_count boxes in a row to see if they all
   match *)
let rec check_line
  ~(position : Position.t)
  ~(positions : Position.Set.t)
  ~(game_kind : Game_kind.t)
  ~(dir : Position.t -> Position.t)
  ~(count : int)
  =
  (* print_endline "check_line call"; print_endline (Int.to_string count); *)
  let next_pos = dir position in
  if count = 0
  then true (* check if the next position exists*)
  else if Set.exists positions ~f:(fun pos -> Position.equal next_pos pos)
          && Position.in_bounds next_pos ~game_kind
  then
    check_line
      ~position:next_pos
      ~positions
      ~game_kind
      ~dir
      ~count:(count - 1)
  else false
;;

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
(* evaluate a game board *)
let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  (* Method of solving: iterate through the pieces positions a number of
     pieces at at time and check horizontally, vertically, and diagonally *)
  let x_keys = Map.filter pieces ~f:(Piece.equal X) in
  let o_keys = Map.filter pieces ~f:(Piece.equal O) in
  let x_set = Map.key_set x_keys in
  let o_set = Map.key_set o_keys in
  let directions =
    [ Position.right; Position.down; Position.down_right; Position.up_right ]
  in
  if Set.exists x_set ~f:(fun pos ->
       List.exists directions ~f:(fun dir ->
         check_line
           ~position:pos
           ~positions:x_set
           ~game_kind
           ~dir
           ~count:(Game_kind.win_length game_kind - 1)))
  then Game_over { winner = Some X }
  else if Set.exists o_set ~f:(fun pos ->
            List.exists directions ~f:(fun dir ->
              check_line
                ~position:pos
                ~positions:o_set
                ~game_kind
                ~dir
                ~count:(Game_kind.win_length game_kind - 1)))
  then Game_over { winner = Some O }
  else if List.is_empty (available_moves ~game_kind ~pieces)
  then Game_over { winner = None }
  else Game_continues
;;

let rec w_check_opp_line
  ~(position : Position.t)
  ~(positions : Position.Set.t)
  ~(game_kind : Game_kind.t)
  ~(dir : Position.t -> Position.t)
  ~(count : int)
  =
  let next_pos = dir position in
  if count = 0
  then true
  else if Set.exists positions ~f:(fun pos -> Position.equal next_pos pos)
  then
    w_check_opp_line
      ~position:next_pos
      ~positions
      ~game_kind
      ~dir
      ~count:(count - 1)
  else false
;;

let rec w_check_line
  ~(position : Position.t)
  ~(og_position : Position.t)
  ~(positions : Position.Set.t)
  ~(game_kind : Game_kind.t)
  ~(dir : Position.t -> Position.t)
  ~(op_dir : Position.t -> Position.t)
  ~(count : int)
  =
  let next_pos = dir position in
  if count = 0
  then true
  else if Set.exists positions ~f:(fun pos -> Position.equal next_pos pos)
          && Position.in_bounds next_pos ~game_kind
  then
    w_check_line
      ~position:next_pos
      ~og_position
      ~positions
      ~game_kind
      ~dir
      ~op_dir
      ~count:(count - 1)
  else
    w_check_opp_line
      ~position:og_position
      ~positions
      ~game_kind
      ~dir:op_dir
      ~count
;;

(* Exercise 3. *)
let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let directions =
    [ Position.right, Position.left
    ; Position.down, Position.up
    ; Position.down_right, Position.up_left
    ; Position.up_right, Position.down_left
    ]
  in
  let me_pieces = Map.filter pieces ~f:(Piece.equal me) in
  let me_list = Map.keys me_pieces in
  let pos_set = Position.Set.of_list me_list in
  let moves = available_moves ~game_kind ~pieces in
  List.filter moves ~f:(fun move ->
    List.exists directions ~f:(fun (dir, op_dir) ->
      w_check_line
        ~position:move
        ~og_position:move
        ~positions:pos_set
        ~game_kind
        ~dir
        ~op_dir
        ~count:(Game_kind.win_length game_kind - 1)))
;;

(* Exercise 4. *)
let losing_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let other_piece = Piece.flip me in
  winning_moves ~me:other_piece ~game_kind ~pieces
;;

let exercise_one =
  Command.basic
    ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves =
         available_moves
           ~game_kind:win_for_x.game_kind
           ~pieces:win_for_x.pieces
       in
       print_s [%sexp (moves : Position.t list)];
       let moves =
         available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
       in
       print_s [%sexp (moves : Position.t list)])
;;

let exercise_two =
  Command.basic
    ~summary:"Exercise 2: Did is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation =
         evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
       in
       print_s [%sexp (evaluation : Evaluation.t)])
;;

let exercise_three =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let winning_moves =
         winning_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (winning_moves : Position.t list)];
       ())
;;

let exercise_four =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let losing_moves =
         losing_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (losing_moves : Position.t list)];
       ())
;;

let%expect_test "print_win_for_x" =
  print_endline (Game_state.to_string_hum win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XOX
    OOX
    OXX |}]
;;

let%expect_test "print_non_win" =
  print_endline (Game_state.to_string_hum non_win);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    X
    O
    O X |}]
;;

(* After you've implemented [available_moves], uncomment these tests! *)
let%expect_test "yes available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect
    {| 
   (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 1))
    ((row 1) (column 2)) ((row 2) (column 1))) |}]
;;

let%expect_test "no available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [evaluate] function, uncomment the next two
   tests! *)
let%expect_test "evalulate_win_for_x" =
  print_endline
    (evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evalulate_non_win" =
  print_endline
    (evaluate ~game_kind:non_win.game_kind ~pieces:non_win.pieces
     |> Evaluation.to_string);
  [%expect {| Game_continues |}]
;;

(* When you've implemented the [winning_moves] function, uncomment this
   test! *)
let%expect_test "winning_move" =
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| (((row 1) (column 1)))
  |}];
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
let%expect_test "print_losing" =
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}];
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect
    {|
  (((row 1) (column 1))) |}]
;;
