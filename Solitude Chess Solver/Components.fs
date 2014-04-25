namespace SolitudeChessSolver

module Components = 
   open System
   open System.Runtime.Serialization
   open System.Reflection
   open Microsoft.FSharp.Reflection

   (* Piece types *)
   [<Serializable>]
   [<KnownType("KnownTypes")>]
   type Piece = 
      | Pawn
      | Knight
      | Bishop
      | Rook
      | Queen 
      | King

      static member KnownTypes() = 
         typeof<Piece>.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic) |> Array.filter FSharpType.IsUnion

   (*
    * Represents position on the board. NOTE, the origin (0, 0) is always in the
    * BOTTOM-LEFT CORNER! This is different than the usual TOP-LEFT corner
    * 
    * File = The X position
    * Rank = The Y position
    *)
   [<Serializable>]
   type Position = 
      { File : int; Rank : int }
      member this.ToString() = sprintf "(%A, %A)" this.File this.Rank

   (* Represents one move *)
   [<Serializable>]
   type Move = 
      { From : Position; To : Position } 
      member this.ToString() = sprintf "%A -> %A" (this.From.ToString()) (this.To.ToString())

   (* Represents a game board *)
   [<Serializable>]
   type Board = { MaxFile : int; MaxRank : int; PieceState : (Piece * Position) list }

module InternalComponents =
   open Components
   open InternalHelpers

   (* Represents the state of a single round of processing *)
   type internal ProcessStatus =
      | Unsolved of ContinuationStep<Move>
      | NoSolutionPossible
      | Solved

   (* Represents the current state of processing a board *)
   type internal SolutionState = { Board : Board; Status : ProcessStatus; Accumulator : Move list }