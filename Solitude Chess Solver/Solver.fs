module Solver

open Components

(* Query to determine whether or not a position has a piece *)
let private doesPositionHavePiece (board : Board) (position : Position) =
   query { 
      for state in board.PieceState do
      exists ((snd state) = position)
   }

(* Determines whether or not we actually capture a piece with this move *)
let private doesCapture (board : Board) (move : Move) =
   let destination = move.To
   doesPositionHavePiece board destination

(* Determines whether or not a move will stay within the bounds of the board *)
let private doesStayInBounds (board : Board) (move : Move) =
   let toMove = move.To
   let maxFile = board.MaxFile
   let maxRank = board.MaxRank

   toMove.File >= 0 && toMove.Rank >= 0 && toMove.File <= maxFile && toMove.Rank <= maxRank

(* Validates a piece type *)
let private validatePiece (piece : (Piece * Position)) pieceType =
   match (fst piece) with
   | pieceType -> true
   | _ -> false

(* Calculates the forward movement of a pawn *)
let private pawnMoves (piece : (Piece * Position)) =
   if validatePiece piece Pawn then
         let currentPosition = snd piece
         let leftForward = { File = currentPosition.File - 1; Rank = currentPosition.Rank + 1 }
         let rightForward = { File = currentPosition.File + 1; Rank = currentPosition.Rank + 1 }

         [leftForward; rightForward]
   else
      failwith "Piece is not a pawn"

(* Calculates the movements of a knight *)
let private knightMoves (piece : (Piece * Position)) = 
   if validatePiece piece Knight then
      let currentPosition = snd piece

      let tupleComprehension = 
            [
               let preCannedTuples = [(1, 2); (2, 1)]
               for t in preCannedTuples do
                  yield t
                  yield (-(fst t), (snd t))
                  yield ((fst t), -(snd t))
                  yield (-(fst t), -(snd t))
            ]
      
      tupleComprehension
      |> List.map (fun t -> { File = (fst t); Rank = (snd t) })
   else
      failwith "Piece is not a knight"

(* Generates a linear function with the given slope, x and y *)
let private generateLinearFunction slope xCoord yCoord =
   let yIntercept = -(slope * xCoord) + yCoord
   (fun x -> (x, (slope * x) + yIntercept))

(* Generates all of the points on the line up to the maxXCoordinate *)
let private generateLine slope xCoord yCoord maxXCoordinate =
   let generator = generateLinearFunction slope xCoord yCoord
   [
      for currentX in 0..maxXCoordinate do
         yield generator currentX
   ]

(* Standard predicate for board boundaries *)
let private standardBoundaryPredicate x y file rank maxFile maxRank =
   x >= 0 && x <= maxFile && y >= 0 && y <= maxRank && x <> file && y <> rank

(* Generates all possible diagonal positions *)
let private generateAllDiagonalPositions file rank maxFile maxRank =
   let positiveSlopeLine = generateLine 1 file rank maxFile
   let negativeSlopeLine = generateLine -1 file rank maxFile
   let bothLines = List.append positiveSlopeLine negativeSlopeLine

   bothLines
   |> List.filter (fun tuple -> 
                     let x = fst tuple
                     let y = snd tuple
                     standardBoundaryPredicate x y file rank maxFile maxRank)
   |> List.map    (fun tuple -> { File = (fst tuple); Rank = (snd tuple) } )

(* Generates all horizontal and vertical positions *)
let private generateAllHorizontalVerticalPositions file rank maxFile maxRank =
   let horizontalLine = generateLine 0 file rank maxFile
   let verticalLine = [ for y in 0..maxRank do yield (file, y) ]
   let bothLines = List.append horizontalLine verticalLine

   bothLines
   |> List.filter (fun tuple ->
                     let x = fst tuple
                     let y = snd tuple
                     standardBoundaryPredicate x y file rank maxFile maxRank)
   |> List.map    (fun tuple -> { File = (fst tuple); Rank = (snd tuple) })

(* Calculates the movements of a bishop *)
let private bishopMoves (piece : (Piece * Position)) (maxFile : int) (maxRank : int) =
   if validatePiece piece Bishop then
      let xPosition = (snd piece).File
      let yPosition = (snd piece).Rank

      generateAllDiagonalPositions xPosition yPosition maxFile maxRank
   else
      failwith "Piece is not a bishop"

(* Calculates the movements of a rook *) 
let private rookMoves (piece : (Piece * Position)) (maxFile : int) (maxRank : int) =
   if validatePiece piece Rook then
      let xPosition = (snd piece).File
      let yPosition = (snd piece).Rank

      generateAllHorizontalVerticalPositions xPosition yPosition maxFile maxRank
   else
      failwith "Piece is not a rook"

(* Calculates the movements of a queen *) 
let private queenMoves (piece : (Piece * Position)) (maxFile : int) (maxRank : int) =
   if validatePiece piece Queen then
      let xPosition = (snd piece).File
      let yPosition = (snd piece).Rank
      let diagPositions = generateAllDiagonalPositions xPosition yPosition maxFile maxRank
      let horizVertPositions = generateAllHorizontalVerticalPositions xPosition yPosition maxFile maxRank

      List.append diagPositions horizVertPositions
   else
      failwith "Piece is not a queen"

(* Calculates the movements of a king *) 
let private kingMoves (piece : (Piece * Position)) =
   if validatePiece piece King then
      let position = snd piece
      let xPosition = position.File
      let yPosition = position.Rank
      [
         { File = xPosition + 1; Rank = yPosition }
         { File = xPosition; Rank = yPosition + 1 }
         { File = xPosition + 1; Rank = yPosition + 1}
         { File = xPosition - 1; Rank = yPosition }
         { File = xPosition - 1; Rank = yPosition + 1 }
         { File = xPosition - 1; Rank = yPosition - 1 }
         { File = xPosition; Rank = yPosition - 1 }
         { File = xPosition + 1; Rank = yPosition - 1 }
      ]
   else
      failwith "Piece is not a king"

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
   |> List.filter (doesStayInBounds board)
   |> List.filter (doesCapture board)

(* Find all available NEXT moves for every single piece *)
let private findAvailableNextMoves (board : Board) =
   board.PieceState //Go through all pieces
   |> List.map (findPieceAvailableMoves board) //Send board and selected piece to the calculator function

(* Performs a move and returns a new board to represent the new state *)
let private executeMove (board : Board) (move : Move) =
   let movingPiece = 
      query {
         for piece in board.PieceState do
         where ((snd piece) = move.From)
         select (fst piece)
         exactlyOne
      }


   0

(* Solve the Solitary Chess puzzle *)
let solve (board : Board) =
   0