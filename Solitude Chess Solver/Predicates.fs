module Predicates

open SolitudeChessSolver.Components

(* Query to determine whether or not a position has a piece *)
let internal doesPositionHavePiece (board : Board) (position : Position) =
   query { 
      for state in board.PieceState do
      exists ((snd state) = position)
   }

(* Determines whether or not we actually capture a piece with this move *)
let internal doesCapture (board : Board) (move : Move) =
   doesPositionHavePiece board move.To

(* Determines whether or not a move will stay within the bounds of the board *)
let internal doesStayInBounds (board : Board) (move : Move) =
   let toMove = move.To
   let maxFile = board.MaxFile
   let maxRank = board.MaxRank
   toMove.File >= 0 && toMove.Rank >= 0 && toMove.File <= maxFile && toMove.Rank <= maxRank

(* Standard predicate for board boundaries when generating moves from a polynomial *)
let internal standardBoundaryPredicate newPosition oldPosition maxFile maxRank =
   let x = fst newPosition
   let y = snd newPosition
   let oldX = fst oldPosition
   let oldY = snd oldPosition
   x >= 0 && x <= maxFile && y >= 0 && y <= maxRank && not(x = oldX && y = oldY)

(* Detects whether or not the board has been solved *)
let internal isSolved (board : Board) = board.PieceState.Length = 1