using System;
using System.Linq;

namespace AdventOfCode
{
    static class AoC
    {
        public static long[] Parse(string[] input) => input[0].Split(',').Select(long.Parse).ToArray();
        public static long Part1(string[] input) => Part1(input, 12, 2)[0];
        public static long[] Part1(string[] input, int p1, int p2) => Run(Parse(input), p1, p2);
        public static int Part2(string[] input) => Part2(input, 19690720);
        public static int Part2(string[] input, long target)
        {
            ReadOnlySpan<long> array = Parse(input);

            for (var p1 = 0; p1 < 100; p1++)
            {
                for (var p2 = 0; p2 < 100; p2++)
                {
                    var result = Run(array.ToArray(), p1, p2)[0];
                    if (result == target)
                        return p1 * 100 + p2;
                }
            }

            return 0;
        }

        public static long[] Run(long[] range, int p1, int p2)
        {
            range[1] = p1;
            range[2] = p2;
            int index = 0;

            while (true)
            {
                var result = Step(range, index);

                if (result < 0) 
                    break;

                var position = range[index + 3];

                range[position] = result;

                index += 4;
            }

            return range;
        }

        public static long Step(long[] range, int index) => range[index] switch
        {
            1 => range[range[index + 1]] + range[range[index + 2]],
            2 => range[range[index + 1]] * range[range[index + 2]],
            99 => -1
        };

    }
}
