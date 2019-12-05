using System.Linq;
using Xunit;

namespace AdventOfCode
{
    public class Specs
    {
        [Theory]
        [InlineData("R8,U5,L5,D3", "U7,R6,D4,L4", 6)]
        [InlineData("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 159)]
        [InlineData("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135)]
        public void TestPart1(string line1, string line2, int expected)
        {
            var result = AoC.Part1(new[] { line1, line2 });
            Assert.Equal(expected, result);
        }

        [Theory]
        [InlineData("R8,U5,L5,D3", "U7,R6,D4,L4", 30)]
        [InlineData("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 610)]
        [InlineData("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 410)]
        public void TestPart2(string line1, string line2, int expected)
        {
            var result = AoC.Part2(new[] { line1, line2 });
            Assert.Equal(expected, result);
        }

        [Fact]
        public void TestExtractPoints()
        {
            var input = "R2,U2,L1,D3";

            var points = AoC.Points(input).ToArray();

            Assert.Equal(new[] { (1,0), (2,0), (2, 1), (2,2), (1,2), (1,1), (1,0), (1, -1) }, points);

        }
    }
}
