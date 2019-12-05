using System;
using System.Collections.Generic;
using System.Linq;
using static System.Math;

namespace AdventOfCode
{
    static class AoC
    {
        public static int Part1(string[] input)
        {
            var points = input[0].Points().ToHashSet();
            var crossings = input[1].Points().Where(p => points.Contains(p));
            return crossings.Min(x => Abs(x.x) + Abs(x.y));
        }

        public static int Part2(string[] input)
        {
            var points1 = input[0].Points().Select((p, i) => (p, steps: i + 1)).ToLookup(x => x.p, x => x.steps);
            var points2 = input[1].Points().Select((p, i) => (p, steps: i + 1)).ToLookup(x => x.p, x => x.steps);
            var crossings = points2.Select(p => p.Key).Where(p => points1.Contains(p));

            var steps = crossings
                .Select(p => points1[p].Min() + points2[p].Min())
                .Min();

            return steps;
        }

        public static IEnumerable<(int x, int y)> Points(this string input)
        {
            (int x, int y) = (0, 0);
            foreach (var item in input.Split(','))
            {
                Func<int, int, (int, int)> f = item[0] switch
                {
                    'R' => (x, y) => (x + 1, y),
                    'L' => (x, y) => (x - 1, y),
                    'U' => (x, y) => (x, y + 1),
                    'D' => (x, y) => (x, y - 1),
                    _ => throw new Exception()
                };
                var d = int.Parse(item.AsSpan().Slice(1));
                for (int i = 0; i < d; i++)
                {
                    (x, y) = f(x, y);
                    yield return (x, y);
                }
            }
        }
    }
}
