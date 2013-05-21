module Moves

open SolitudeChessSolver.Components
open Predicates

(* Calculates the forward movement of a pawn *)
let internal pawnMoves (piece : (Piece * Position)) =
   if validatePiece piece Pawn then
         let currentPosition = snd piece
         let leftForward = { File = currentPosition.File - 1; Rank = currentPosition.Rank + 1 }
         let rightForward = { File = currentPosition.File + 1; Rank = currentPosition.Rank + 1 }

         [leftForward; rightForward]
   else
      failwith "Piece is not a pawn"

(* Calculates the movements of a knight *)
let internal knightMoves (piece : (Piece * Position)) = 
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
let internal generateLinearFunction slope xCoord yCoord =
   let yIntercept = -(slope * xCoord) + yCoord
   (fun x -> (x, (slope * x) + yIntercept))

(* Generates all of the points on the line up to the maxXCoordinate *)
let internal generateLine slope xCoord yCoord maxXCoordinate =
   let generator = generateLinearFunction slope xCoord yCoord
   [
      for currentX in 0..maxXCoordinate do
         yield generator currentX
   ]

(* Generates all possible diagonal positions *)
let internal generateAllDiagonalPositions file rank maxFile maxRank =
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
let internal generateAllHorizontalVerticalPositions file rank maxFile maxRank =
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
let internal bishopMoves (piece : (Piece * Position)) (maxFile : int) (maxRank : int) =
   if validatePiece piece Bishop then
      let xPosition = (snd piece).File
      let yPosition = (snd piece).Rank

      generateAllDiagonalPositions xPosition yPosition maxFile maxRank
   else
      failwith "Piece is not a bishop"

(* Calculates the movements of a rook *) 
let internal rookMoves (piece : (Piece * Position)) (maxFile : int) (maxRank : int) =
   if validatePiece piece Rook then
      let xPosition = (snd piece).File
      let yPosition = (snd piece).Rank

      generateAllHorizontalVerticalPositions xPosition yPosition maxFile maxRank
   else
      failwith "Piece is not a rook"

(* Calculates the movements of a queen *) 
let internal queenMoves (piece : (Piece * Position)) (maxFile : int) (maxRank : int) =
   if validatePiece piece Queen then
      let xPosition = (snd piece).File
      let yPosition = (snd piece).Rank
      let diagPositions = generateAllDiagonalPositions xPosition yPosition maxFile maxRank
      let horizVertPositions = generateAllHorizontalVerticalPositions xPosition yPosition maxFile maxRank

      List.append diagPositions horizVertPositions
   else
      failwith "Piece is not a queen"

(* Calculates the movements of a king *) 
let internal kingMoves (piece : (Piece * Position)) =
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