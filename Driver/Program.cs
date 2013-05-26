using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using System.Runtime.Serialization.Json;
using ChessComponent = SolitudeChessSolver.Components;

namespace Driver
{
	public static class Program
	{
		private static Tuple<ChessComponent.Piece, ChessComponent.Position> CreatePieceState(ChessComponent.Piece piece, ChessComponent.Position position)
		{
			return Tuple.Create<ChessComponent.Piece, ChessComponent.Position>(piece, position);
		}

		private static List<Tuple<ChessComponent.Piece, ChessComponent.Position>> StandardChessBoard = new List<Tuple<ChessComponent.Piece, ChessComponent.Position>>
		{
			CreatePieceState(ChessComponent.Piece.Pawn, new ChessComponent.Position(0, 0)),
			CreatePieceState(ChessComponent.Piece.Bishop, new ChessComponent.Position(1, 0)),
			CreatePieceState(ChessComponent.Piece.Pawn, new ChessComponent.Position(2, 0)),
			CreatePieceState(ChessComponent.Piece.Rook, new ChessComponent.Position(1, 1)),
			CreatePieceState(ChessComponent.Piece.Bishop, new ChessComponent.Position(0, 2)),
			CreatePieceState(ChessComponent.Piece.King, new ChessComponent.Position(1, 2)),
			CreatePieceState(ChessComponent.Piece.Knight, new ChessComponent.Position(2, 2)),
			CreatePieceState(ChessComponent.Piece.Rook, new ChessComponent.Position(3, 3))
		};

		private static List<Tuple<ChessComponent.Piece, ChessComponent.Position>> SmallChessBoard = new List<Tuple<ChessComponent.Piece, ChessComponent.Position>>
		{
			CreatePieceState(ChessComponent.Piece.Pawn, new ChessComponent.Position(0, 0)),
			CreatePieceState(ChessComponent.Piece.Pawn, new ChessComponent.Position(1, 1)),
		};

		public static void CheckSerialization()
		{
			var expectedBoard = SolitudeChessSolver.ExternalHelpers.CreateBoard(3, 3, StandardChessBoard);
			var memoryStream = new MemoryStream();
			var serializer = new DataContractJsonSerializer(typeof(ChessComponent.Board));

			serializer.WriteObject(memoryStream, expectedBoard);
			memoryStream.Position = 0;

			var deserializedBoard = serializer.ReadObject(memoryStream) as ChessComponent.Board;

			if (!object.Equals(expectedBoard, deserializedBoard))
			{
				throw new Exception("Expected board and actual board not the same");
			}
		}

		public static void Main(string[] args)
		{
			var expectedBoard = SolitudeChessSolver.ExternalHelpers.CreateBoard(3, 3, StandardChessBoard);
			var resultCPS = SolitudeChessSolver.ExternalHelpers.Solve(expectedBoard);

			int i = 0;
			foreach (var k in resultCPS)
			{
				i++;
				Console.WriteLine(string.Format("Solution {0}", i));
				foreach (var l in k)
				{
					Console.WriteLine(l.ToString());
				}
				Console.WriteLine("=====");
			}
		}
	}
}
