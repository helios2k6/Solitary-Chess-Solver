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
		private static string SerializeBoard(ChessComponent.Board board)
		{
			using (var memoryStream = new MemoryStream())
			{
				var serializer = new DataContractJsonSerializer(typeof(ChessComponent.Board));
				serializer.WriteObject(memoryStream, board);

				memoryStream.Position = 0;
				using (var streamReader = new StreamReader(memoryStream))
				{
					return streamReader.ReadToEnd();
				}
			}
		}

		private static ChessComponent.Board GetBoard(string serializedBoard)
		{
			using (var streamReader = new MemoryStream(Encoding.Unicode.GetBytes(serializedBoard)))
			{
				var serializer = new DataContractJsonSerializer(typeof(ChessComponent.Board));
				return serializer.ReadObject(streamReader) as ChessComponent.Board;
			}
		}

		public static void Main(string[] args)
		{
			var expertBoardOne = GetBoard(SampleBoards.ExpertBoardOne);
			var expertBoardTwo = GetBoard(SampleBoards.ExpertBoardTwo);

			var expertResultOne = SolitudeChessSolver.ExternalHelpers.Solve(expertBoardOne);
			var expertResultTwo = SolitudeChessSolver.ExternalHelpers.Solve(expertBoardTwo);

			var expertResults = new[] { expertResultOne, expertResultTwo };

			foreach (var result in expertResults)
			{
				Console.WriteLine("====Results for board====");
				int i = 0;
				foreach (var k in result)
				{
					i++;
					Console.WriteLine(string.Format("Solution {0}", i));
					foreach (var l in k)
					{
						Console.WriteLine(l.ToString());
					}
					Console.WriteLine("=====");
				}

				Console.WriteLine();
			}
		}
	}
}
