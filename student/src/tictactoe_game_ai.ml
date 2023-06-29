open! Core
open Tic_tac_toe_2023_common
open Tic_tac_toe_exercises_lib
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  List.random_element_exn
    (Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces)
;;

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let win_list =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  if not (List.is_empty win_list)
  then List.random_element_exn win_list
  else
    List.random_element_exn
      (Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces)
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_if_possible_strategy

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let win_list =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  if not (List.is_empty win_list)
  then List.random_element_exn win_list
  else (
    let lose_list =
      Tic_tac_toe_exercises_lib.losing_moves ~me ~game_kind ~pieces
    in
    if not (List.is_empty lose_list)
    then List.random_element_exn lose_list
    else
      List.random_element_exn
        (Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces))
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_or_block_if_possible_strategy

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  let win_list =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  let lose_list =
    Tic_tac_toe_exercises_lib.losing_moves ~me ~game_kind ~pieces
  in
  if List.length win_list > 1 && List.length lose_list = 0
  then Float.infinity
  else if List.length lose_list > 1 && List.length win_list = 0
  then Float.neg_infinity
  else Float.zero
;;

let _ = score

(* let rec minimax ~(position : Position.t) ~(game_kind : Game_kind.t)
   ~(depth : int) ~(maximizing_player : bool) ~(pieces : Piece.t
   Position.Map.t) ~(piece : Piece.t) ~(player : Piece.t) : float = let
   new_map = Map.set pieces ~key:position ~data:piece in (* let sc = score
   ~me:piece ~game_kind ~pieces in *) if depth = 0 || match evaluate
   ~game_kind ~pieces with | Game_continues -> false | _ -> true then score
   ~me:piece ~game_kind ~pieces else if maximizing_player then ( let value =
   ref Float.neg_infinity in let av_moves = available_moves ~game_kind
   ~pieces:new_map in List.iter av_moves ~f:(fun move -> value := Float.max
   !value (minimax ~position:move ~depth:(depth - 1) ~game_kind
   ~pieces:new_map ~maximizing_player:false ~piece:(Piece.flip piece)
   ~player)); !value) else ( let value = ref Float.infinity in let av_moves =
   available_moves ~game_kind ~pieces in List.iter av_moves ~f:(fun move ->
   value := Float.min !value (minimax ~position:move ~depth:(depth - 1)
   ~game_kind ~pieces:new_map ~maximizing_player:true ~piece ~player));
   !value) ;; *)

let rec minimax
  pos
  depth
  maximizingPlayer
  me
  pieces
  (game_state : Game_state.t)
  player
  : float
  =
  let game_kind = game_state.game_kind in
  let new_pieces = Map.set pieces ~key:pos ~data:me in
  if depth = 0
     ||
     match evaluate ~game_kind ~pieces:new_pieces with
     | Game_continues -> false
     | _ -> true
  then
    (* print_endline "Score Check"; print_s [%message "" (new_pieces :
       Piece.t Position.Map.t)]; print_endline (Game_state.to_string_hum
       game_state); *)
    score ~me:player ~game_kind ~pieces:new_pieces
  else if maximizingPlayer
  then (
    let avmoves = available_moves ~game_kind ~pieces:new_pieces in
    avmoves
    |> List.map ~f:(fun new_pos ->
         minimax
           new_pos
           (depth - 1)
           false
           (Piece.flip me)
           new_pieces
           game_state
           player)
    |> List.fold ~init:Float.neg_infinity ~f:Float.max)
  else (
    let avmoves = available_moves ~game_kind ~pieces:new_pieces in
    avmoves
    |> List.map ~f:(fun new_pos ->
         minimax new_pos (depth - 1) true me new_pieces game_state player)
    |> List.fold ~init:Float.infinity ~f:Float.min)
;;

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)

(* let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t) :
   Position.t = let pieces = game_state.pieces in let game_kind =
   game_state.game_kind in let avmoves = available_moves ~game_kind ~pieces
   in match avmoves |> List.map ~f:(fun pos -> ( minimax ~position:pos
   ~depth:11 ~maximizing_player:true ~piece:me ~pieces ~game_kind ~player:me
   , pos )) |> List.max_elt ~compare:(fun (v1, _pos1) (v2, _pos2) -> print_s
   [%message "" (v1 : Float.t)]; print_s [%message "" (_pos1 : Position.t)];
   print_s [%message "" (v2 : Float.t)]; print_s [%message "" (_pos2 :
   Position.t)]; Float.compare v1 v2) with | Some (_v1, pos) -> print_endline
   (Game_state.to_string_hum game_state); (* print_s [%message "" (v1 :
   Float.t)]; *) pos | None -> random_move_strategy ~game_kind ~pieces ;; *)

let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  let pieces = game_state.pieces in
  let game_kind = game_state.game_kind in
  let avmoves = available_moves ~game_kind ~pieces in
  match
    avmoves
    |> List.map ~f:(fun pos ->
         minimax pos 11 true me pieces game_state me, pos)
    |> List.max_elt ~compare:(fun (v1, _pos1) (v2, _pos2) ->
         print_s [%message "" (v1 : Float.t)];
         print_s [%message "" (_pos1 : Position.t)];
         print_s [%message "" (v2 : Float.t)];
         print_s [%message "" (_pos2 : Position.t)];
         Float.compare v1 v2)
  with
  | Some (_v1, pos) ->
    print_endline (Game_state.to_string_hum game_state);
    (* print_s [%message "" (v1 : Float.t)]; *)
    pos
  | None -> random_move_strategy ~game_kind ~pieces
;;
