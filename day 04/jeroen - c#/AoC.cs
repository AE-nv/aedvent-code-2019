using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using static System.Linq.Enumerable;

namespace AdventOfCode
{
    static class AoC
    {
        public static int Part1(string[] input) 
            => (
            from i in ParseInput(input)
            let d = i.ToDigits()
            where d.IsAscending() && d.HasAtLeastOneGroupOfAtLeast2AdjacentSameDigits()
            select d
            ).Count();

        public static int Part2(string[] input)
            => (
            from i in ParseInput(input)
            let d = i.ToDigits()
            where d.IsAscending() && d.HasAtLeastOneGroupOfExactly2AdjacentSameDigits()
            select d
            ).Count();

        static IEnumerable<int> ParseInput(this string[] input)
            => input[0]
            .Split('-')
            .Select(int.Parse)
            .ToArray()
            .AsRange();

        static IEnumerable<int> AsRange(this int[] ints) => Range(ints[0], ints[1] - ints[0]);

        public static bool HasAtLeastOneGroupOfAtLeast2AdjacentSameDigits(this int[] digits)
            => digits.GroupBy(i => i).Any(g => g.Count() >= 2);
        public static bool HasAtLeastOneGroupOfExactly2AdjacentSameDigits(this int[] digits)
            => digits.GroupBy(i => i).Any(g => g.Count() == 2);


        public static int[] ToDigits(this int n)
        {
            int[] digits = new int[6];
            int t = n;
            for (int i = 5; i >= 0; i--)
            {
                digits[i] = t % 10;
                t /= 10;
            }
            return digits;
        }

        public static bool IsAscending(this int[] digits)
            => digits.Take(digits.Length - 1).Zip(digits.Skip(1)).All(x => x.First <= x.Second);

    }
}
