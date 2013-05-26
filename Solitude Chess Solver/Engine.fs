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

(* The actual work to process a solution state *)
let private processStateHelper board move (accum : Move list) =
   if accum.Length > ((board.MaxRank + 1) * (board.MaxFile + 1)) then
      { Board = board; Status = NoSolutionPossible; Accumulator = [] }
   else
      let newBoardState = executeMove board move

      if isSolved newBoardState then
         { Board = newBoardState; Status = Solved; Accumulator = move::accum }
      else
         let availableMoves = findAvailableNextMoves newBoardState
         match availableMoves with
         | [] -> { Board = newBoardState; Status = NoSolutionPossible; Accumulator = [] }
         | moves -> { Board = newBoardState; Status = Unsolved(createContForList moves); Accumulator = move::accum }

(* Process a single solution state *)
let private processState (solutionState : SolutionState) =
   match solutionState.Status with
   | NoSolutionPossible | Solved -> failwith "No solution possible or solved"
   | Unsolved(nextMoves) -> mapCont (fun a -> processStateHelper solutionState.Board a solutionState.Accumulator) nextMoves

(* Process the list of solution states *)
let private processStates (solutionStates : SolutionState list) =
   List.collect processState solutionStates

(* Partitioner for solution states *)
let private solutionStateMapper (accum : (SolutionState list * SolutionState list)) item =
   match accum with
   | (unsolvedList, solvedList) -> 
      match item.Status with
      | Unsolved(_) -> (item::unsolvedList, solvedList)
      | NoSolutionPossible -> (unsolvedList, solvedList)
      | Solved -> (unsolvedList, item::solvedList)

(* Solves the puzzle given a list of solution states *)
let rec private solveStates (solutionStates : SolutionState list) =
   let processResults = processStates solutionStates 
                        |> List.fold solutionStateMapper ([], [])
   
   match processResults with
   | (unsolvedList, solvedList) ->
      (* Did we find a solution? *)
      if not(solvedList.IsEmpty) then
         solvedList
      (* Can we still keep looking? *)
      elif not(unsolvedList.IsEmpty) then
         solveStates unsolvedList
      (* There isn't a solution *)
      else
         []

(* Solver with CPS instead *)
let internal solve (board : Board) =
   (* Start with the first solution state *)
   let firstContinuations = findAvailableNextMoves board |> createContForList
   let firstSolutionState = { Board = board; Status = Unsolved(firstContinuations); Accumulator = [] }

   solveStates [firstSolutionState]
   |> List.map (fun state -> { Board = state.Board; Status = state.Status; Accumulator = List.rev state.Accumulator }) 