using System;
using Xunit;

namespace AdventOfCode
{
    public class Specs
    {
        string[] input = new[] { "12", "14", "1969", "100756" };

        [Fact]
        public void TestPart1()
        {
            var result = AoC.Part1(input);
            Assert.Equal(2 + 2 + 654 + 33583, result);
        }

        [Fact]
        public void TestPart2()
        {
            var result = AoC.Part2(input);
            Assert.Equal(2 + 2 + 966 + 50346, result);
        }

        [Theory]
        [InlineData(12, 2)]
        [InlineData(14, 2)]
        [InlineData(1969, 654)]
        [InlineData(100756, 33583)]
        public void Fuel(int input, int result)
        {
            Assert.Equal(result, AoC.CalculateFuel1(input));
        }

        [Theory]
        [InlineData(12, 2)]
        [InlineData(14, 2)]
        [InlineData(1969, 966)]
        [InlineData(100756, 50346)]
        public void Fuel2(int input, int result)
        {
            Assert.Equal(result, AoC.CalculateFuel2(input));
        }
    }
}
