using System.IO;
using Xunit;

namespace AdventOfCode
{
    public class Specs
    {
        [Theory]
        [InlineData("1,9,10,3,2,3,11,0,99,30,40,50", 9, 10, 3500, 0)]
        [InlineData("1,0,0,0,99", 0, 0, 2, 0)]
        [InlineData("2,3,0,3,99", 3, 0, 6, 3)]
        [InlineData("2,4,4,5,99,0", 4, 4, 9801, 5)]
        [InlineData("1,1,1,4,99,5,6,0,99", 1, 1, 30, 0)]
        public void TestPart1(string input, int p1, int p2, int expected, int index)
        {
            var result = AoC.Part1(new[] { input }, p1, p2);
            Assert.Equal(expected, result[index]);
        }

        [Fact]
        public void TestPart2()
        {
            var result = AoC.Part2(File.ReadAllLines("input.txt"), 3895705);
            Assert.Equal(1202, result);
        }
    }
}
