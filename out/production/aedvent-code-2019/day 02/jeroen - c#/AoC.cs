using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode
{
    static class AoC
    {
        public static long Part1(string[] input) => Part1(input, 12, 2)[0];
        public static long[] Part1(string[] input, int p1, int p2) => Run(Parse(input), p1, p2);
        public static int Part2(string[] input) => Part2(Parse(input), 19690720);
        public static int Part2(string[] input, long target) => Part2(Parse(input), target);
        static int Part2(IReadOnlyCollection<long> array, long target) => (
                from p1 in Enumerable.Range(0, 100)
                from p2 in Enumerable.Range(0, 100)
                let result = Run(array.ToArray(), p1, p2)[0]
                where result == target
                select p1 * 100 + p2
                ).First();

        static long[] Parse(string[] input) => input[0].Split(',').Select(long.Parse).ToArray();

        static long[] Run(long[] range, int p1, int p2)
        {
            range[1] = p1;
            range[2] = p2;

            int index = 0;
            while (range[index] != 99)
            {
                var result = range[index] switch
                {
                    1 => range[range[index + 1]] + range[range[index + 2]],
                    2 => range[range[index + 1]] * range[range[index + 2]],
                    _ => throw new Exception()
                };

                var position = range[index + 3];
                range[position] = result;

                index += 4;
            }

            return range;
        }


    }
}
