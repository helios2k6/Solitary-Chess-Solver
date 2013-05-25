module Engine

open SolitudeChessSolver.Components
open SolitudeChessSolver.InternalComponents
open InternalHelpers
open Predicates
open Moves

(* Finds all possible NEXT moves for just one piece, regardless of whether or not we capture another piece *)
let private findAllPossibleMoves (board : Board) (piece : (Piece * Position)) =
   match (fst piece) with
   | Pawn -> pawnMoves piece
   | Knight -> knightMoves piece
   | Bishop -> bishopMoves piece board.MaxFile board.MaxRank
   | Rook -> rookMoves piece board.MaxFile board.MaxRank
   | Queen -> queenMoves piece board.MaxFile board.MaxRank
   | King -> kingMoves piece

(* Find all available NEXT moves for just one piece *)
let private findPieceAvailableMoves (board : Board) (piece : (Piece * Position)) =
   let originalPosition = snd piece
   findAllPossibleMoves board piece
   |> List.map (fun position -> { From = originalPosition; To = position })
   |> List.filter (fun entry -> (doesStayInBounds board entry) && (doesCapture board entry))

(* Fold accumulator function to help findAvailableNextMoves *)
let private availableMoveAccumFunc board accumulator pieceState =
   List.append accumulator (findPieceAvailableMoves board pieceState) 

(* Find all available NEXT moves for every single piece *)
let private findAvailableNextMoves (board : Board) =
   board.PieceState //Go through all pieces
   |> List.fold (availableMoveAccumFunc board) [] //Send board and selected piece to the calculator function

(* Selects the pieces of a board that don't have any part in the execution of a move *) 
let private nonParticipatingPiecePartitioner (move : Move) (pieceTuple : (Piece * Position)) =
   let position = snd pieceTuple
   not(position = move.To || position = move.From)

(* Performs a move and returns a new board to represent the new state *)
let private executeMove (board : Board) (move : Move) =
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

(* Process one step *)
let private processSingleMove board move =
   let newBoardState = executeMove board move

   if isSolved newBoardState then
      Solved
   else
      let availableMoves = findAvailableNextMoves newBoardState
      match availableMoves with
      | [] -> NoSolutionPossible
      | moves -> Unsolved(moves)

(* Process a list of moves, represented by a continuation *)
let rec private processListOfMoves board (nextMoves : ContinuationStep<Move>) (accum : Move list) =
   //Process all continuations
   let resultList = mapCont (fun a -> processSingleMove board a) nextMoves
   //Segregate the results
   let rec seg resultList (accum : (ProcessStatus list) * (ProcessStatus list) * (ProcessStatus list)) =
      match resultList with
      | [] -> accum
      | h::t -> match accum with
                | (unsolvedList, noSolutionList, solvedList) ->
                     match h with
                     | Unsolved(_) -> seg t (h::unsolvedList, noSolutionList, solvedList)
                     | NoSolutionPossible -> seg t (unsolvedList, h::noSolutionList, solvedList)
                     | Solved -> seg t (unsolvedList, noSolutionList, h::solvedList)

   let segregatedItems = seg resultList ([], [], [])
   0

(* Solve helper *)
let rec private solveHelper (board : Board) (nextMove : Move) (previousMoves : Move list) =
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