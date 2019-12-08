using System.Linq;
using Xunit;

namespace AdventOfCode
{
    public class Specs
    {
        string[] input = new[]
        {
            "COM)B",
            "B)C",
            "C)D",
            "D)E",
            "E)F",
            "B)G",
            "G)H",
            "D)I",
            "E)J",
            "J)K",
            "K)L",
            "K)YOU",
            "I)SAN"
        };


        [Fact]
        public void TestPart1()
        {
            var result = AoC.Part1(input.Take(11).ToArray());
            Assert.Equal(42, result);

        }
        [Theory]
        [InlineData("D", 3)]
        [InlineData("L", 7)]
        public void TestSomePaths(string node, int expectedDistance)
        {
            var result = input.CreateGraph().CountDistance("COM", node);
            Assert.Equal(expectedDistance, result);
        }

        [Fact]
        public void TestPart2()
        {
            var result = AoC.Part2(input);
            Assert.Equal(4, result);
        }
    }
}
