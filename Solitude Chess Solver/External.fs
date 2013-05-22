namespace SolitudeChessSolver

open System
open System.Collections.Generic
open SolitudeChessSolver.Components
open Engine

type ExternalHelpers() =
   //C# translation layer
   static member CreateBoard((maxFile : int), (maxRank : int), (pieceStateEnumerable : IEnumerable<Tuple<Piece, Position>>)) =
      let seqTrans = query {
                        for tupleItem in pieceStateEnumerable do
                        select (tupleItem.Item1, tupleItem.Item2)
                     }
      { MaxFile = maxFile; MaxRank = maxRank; PieceState = Seq.toList(seqTrans) }

   static member Solve((board : Board)) =
      let result = solve board
      match result with
      | Some(r) -> r
      | None -> []