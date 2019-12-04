using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode
{
    static class AoC
    {
        public static int Part1(string[] input) => input.Select(int.Parse).Select(CalculateFuel1).Sum();

        public static int Part2(string[] input) => input.Select(int.Parse).Select(CalculateFuel2).Sum();
        public static int CalculateFuel1(int mass) => mass / 3 - 2;
        public static int CalculateFuel2(int mass) => Fuel(mass).Sum();

        static IEnumerable<int> Fuel(int mass)
        {
            var fuel = mass;
            while (true)
            {
                fuel = CalculateFuel1(fuel);
                if (fuel < 0) break;
                yield return fuel;
            }
        }

    }
}
