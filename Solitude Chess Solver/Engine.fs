module Engine

open SolitudeChessSolver.Components
open Predicates
open Moves

(* Finds all possible NEXT moves for just one piece, regardless of whether or not we capture another piece *)
let internal findAllPossibleMoves (board : Board) (piece : (Piece * Position)) =
   match (fst piece) with
   | Pawn -> pawnMoves piece
   | Knight -> knightMoves piece
   | Bishop -> bishopMoves piece board.MaxFile board.MaxRank
   | Rook -> rookMoves piece board.MaxFile board.MaxRank
   | Queen -> queenMoves piece board.MaxFile board.MaxRank
   | King -> kingMoves piece

(* Find all available NEXT moves for just one piece *)
let internal findPieceAvailableMoves (board : Board) (piece : (Piece * Position)) =
   let originalPosition = snd piece
   findAllPossibleMoves board piece
   |> List.map (fun position -> { From = originalPosition; To = position })
   |> List.filter (fun entry -> (doesStayInBounds board entry) && (doesCapture board entry))

(* Fold accumulator function to help findAvailableNextMoves *)
let internal availableMoveAccumFunc board accumulator pieceState =
   List.append accumulator (findPieceAvailableMoves board pieceState) 

(* Find all available NEXT moves for every single piece *)
let internal findAvailableNextMoves (board : Board) =
   board.PieceState //Go through all pieces
   |> List.fold (availableMoveAccumFunc board) [] //Send board and selected piece to the calculator function

(* Selects the pieces of a board that don't have any part in the execution of a move *) 
let internal nonParticipatingPiecePartitioner (move : Move) (pieceTuple : (Piece * Position)) =
   let position = snd pieceTuple
   not(position = move.To || position = move.From)

(* Performs a move and returns a new board to represent the new state *)
let internal executeMove (board : Board) (move : Move) =
   let piecesNotParticipating, piecesParticipating = List.partition (nonParticipatingPiecePartitioner move) board.PieceState
   let movingPiece = 
      query {
         for piece in piecesParticipating do
         where ((snd piece) = move.From)
         select (fst piece)
         exactlyOne
      }

   let newPieceState = (movingPiece, move.To)::piecesNotParticipating

   { board with PieceState = newPieceState }

(* Solve helper *)
let rec internal solveHelper (board : Board) (nextMove : Move) (previousMoves : Move list) =
   let newBoardState = executeMove board nextMove
   if isSolved newBoardState then
      (true, nextMove::previousMoves)
   else
      let nextMoves = findAvailableNextMoves newBoardState
      match nextMoves with
      | l when List.isEmpty l -> (false, previousMoves)
      | _ -> 
               let mappingOfSolutions = List.map (fun moveInList -> solveHelper newBoardState moveInList previousMoves) nextMoves
               let anySolvedSolutions = query {
                                          for item in mappingOfSolutions do
                                          where ((true, previousMoves) = item)
                                          select item
                                        }
               if Seq.isEmpty anySolvedSolutions then (false, previousMoves)
               else Seq.head anySolvedSolutions

(* Solve the Solitary Chess puzzle *)
let internal solve (board : Board) =
   let initialSolutions = findAvailableNextMoves board
                          |> List.map (fun move -> solveHelper board move)
                          |> List.map (fun cont -> cont [])
   let solution = query {
                     for possibleSolution in initialSolutions do
                     where (fst possibleSolution)
                     select possibleSolution
                  }
   if Seq.isEmpty solution then None
   else Some(snd (Seq.head solution))