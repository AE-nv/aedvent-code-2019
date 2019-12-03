using System.Linq;

namespace AdventOfCode
{
    static class AoC
    {
        public static int Part1(string[] input) => input.Select(int.Parse).Select(CalculateFuel1).Sum();

        public static int Part2(string[] input) => input.Select(int.Parse).Select(CalculateFuel2).Sum();
        public static int CalculateFuel1(int input) => input / 3 - 2;
        public static int CalculateFuel2(int input) 
        {
            int sum = 0;

            var fuel = CalculateFuel1(input);

            while (fuel > 0)
            {
                sum += fuel;
                fuel = CalculateFuel1(fuel);
            }

            return sum;
        }

    }
}
