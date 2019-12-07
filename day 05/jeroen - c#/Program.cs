using System;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;

namespace AdventOfCode
{
    static class Program
    {
        public static async Task Main()
        {
            var lines = await File.ReadAllLinesAsync("input.txt");

            Measure(() => string.Join(",", AoC.Part1(lines)));

            Measure(() => string.Join(",", AoC.Part2(lines)));
        }

        static void Measure<T>(Func<T> f)
        {
            var sw = Stopwatch.StartNew();
            var result = f();
            Console.WriteLine($"result = {result} - {sw.Elapsed}");
        }
        
    }
}
