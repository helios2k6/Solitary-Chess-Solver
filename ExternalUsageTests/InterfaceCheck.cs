using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization.Json;
using ChessComponent = SolitudeChessSolver.Components;

namespace ExternalUsageTests
{
	[TestClass]
	public class InterfaceCheck
	{
		private static Tuple<ChessComponent.Piece, ChessComponent.Position> CreatePieceState(ChessComponent.Piece piece, ChessComponent.Position position)
		{
			return Tuple.Create<ChessComponent.Piece, ChessComponent.Position>(piece, position);
		}

		[TestMethod]
		public void CheckExternalInterface()
		{
			var listOfPieceStates = new List<Tuple<ChessComponent.Piece, ChessComponent.Position>>
			{
				CreatePieceState(ChessComponent.Piece.Bishop, new ChessComponent.Position(0, 0))
			};

			var expectedBoard = SolitudeChessSolver.ExternalHelpers.CreateBoard(0, 0, listOfPieceStates);
			var memoryStream = new MemoryStream();
			var serializer = new DataContractJsonSerializer(typeof(ChessComponent.Board));

			serializer.WriteObject(memoryStream, expectedBoard);
			memoryStream.Position = 0;

			var deserializedBoard = serializer.ReadObject(memoryStream) as ChessComponent.Board;
			Assert.AreEqual<ChessComponent.Board>(expectedBoard, deserializedBoard);
		}
	}
}
