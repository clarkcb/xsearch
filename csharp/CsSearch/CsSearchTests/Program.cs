using System;
using System.Reflection;

namespace CsSearchTests
{
	class Program
	{
		[STAThread]
		static void Main(string[] args)
		{
			string[] my_args = { Assembly.GetExecutingAssembly().Location };

			int returnCode = NUnit.ConsoleRunner.Runner.Main(my_args);

			if (returnCode != 0)
				Console.Beep();
		}
	}
}
