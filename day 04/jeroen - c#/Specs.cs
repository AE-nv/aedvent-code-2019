using System;
using System.Linq;
using Xunit;

namespace AdventOfCode
{
    public class Specs
    {
        string[] input = new[] { "1-100" };

        [Fact]
        public void TestPart1()
        {
            var result = AoC.Part1(input);
        }

        [Fact]
        public void TestPart2()
        {
            var result = AoC.Part2(input);
        }
        [Theory]
        [InlineData(111111)]
        [InlineData(223456)]
        [InlineData(123455)]
        [InlineData(123789)]
        public void IsAscending(int input)
        {
            Assert.True(input.ToDigits().IsAscending());
        }

        [Theory]
        [InlineData(223450)]
        [InlineData(113787)]
        [InlineData(212345)]
        public void IsNotAscending(int input)
        {
            Assert.False(input.ToDigits().IsAscending());
        }

        [Theory]
        [InlineData(111111, true)]
        [InlineData(223456, true)]
        [InlineData(123455, true)]
        [InlineData(123789, false)]
        public void IsValid1(int input, bool expected)
        {
            Assert.Equal(expected, input.ToDigits().HasAtLeastOneGroupOfAtLeast2AdjacentSameDigits());
        }
        [Theory]
        [InlineData(111111, false)]
        [InlineData(111122, true)]
        [InlineData(112233, true)]
        [InlineData(123444, false)]
        [InlineData(123455, true)]
        [InlineData(123789, false)]
        [InlineData(113789, true)]
        [InlineData(111337, true)]
        [InlineData(111377, true)]
        [InlineData(122377, true)]
        public void IsValid2(int input, bool expected)
        {
            Assert.Equal(expected, input.ToDigits().HasAtLeastOneGroupOfExactly2AdjacentSameDigits());
        }
    }
}
