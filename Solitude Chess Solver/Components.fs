module Components

(* Piece types *) 
type Piece = 
   Pawn
   | Knight
   | Bishop
   | Rook
   | Queen 
   | King

(*
 * Represents position on the board. NOTE, the origin (0, 0) is always in the
 * BOTTOM-LEFT CORNER! This is different than the usual TOP-LEFT corner
 * 
 * File = The X position
 * Rank = The Y position
 *)
type Position = { File : int; Rank : int }

(* Represents one move *)
type Move = { From : Position; To : Position } 

(* Represents a game board *)
type Board = { MaxFile : int; MaxRank : int; PieceState : (Piece * Position) list }