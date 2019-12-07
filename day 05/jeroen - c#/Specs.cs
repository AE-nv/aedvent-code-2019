using System.Diagnostics;
using System.IO;
using System.Linq;
using Xunit;
using Xunit.Abstractions;

namespace AdventOfCode
{
    class XUnitTraceListener : TraceListener
    {
        private ITestOutputHelper output;

        public XUnitTraceListener(ITestOutputHelper output) { this.output = output; }

        public override void Write(string message) => output.WriteLine(message);
        public override void WriteLine(string message) => output.WriteLine(message);
    }

    public class Specs
    {
        private readonly ITestOutputHelper output;
        public Specs(ITestOutputHelper output)
        {
            this.output = output;
            Trace.Listeners.Add(new XUnitTraceListener(output));
        }

        [Theory]
        [InlineData("1,9,10,3,2,3,11,0,99,30,40,50", 1)]
        [InlineData("1,0,0,0,99", 1)]
        [InlineData("2,3,0,3,99", 1)]
        [InlineData("2,4,4,5,99,0", 1)]
        [InlineData("1,1,1,4,99,5,6,0,99", 1)]
        [InlineData("1101,100,101,5,104,0,99", 1, 201)]
        [InlineData("1101,0,0,5,4,0,99", 1, 1101)]
        [InlineData("1102,100,101,5,104,0,99", 1, 10100)]
        [InlineData("2,0,0,5,4,0,99", 1, 4)]
        [InlineData("3,0,4,0,99", 123, 123)]
        [InlineData("4,0,99", 1, 4)]
        [InlineData("1105,0,6,4,0,99,4,3,99", 1, 1105)]
        [InlineData("1105,1,6,4,0,99,4,3,99", 1, 4)]
        [InlineData("1106,1,6,4,0,99,4,3,99", 1, 1106)]
        [InlineData("1106,0,6,4,0,99,4,3,99", 1, 4)]
        [InlineData("3,9,8,9,10,9,4,9,99,-1,8", 7, 0)]
        [InlineData("3,9,8,9,10,9,4,9,99,-1,8", 8, 1)]
        [InlineData("3,9,8,9,10,9,4,9,99,-1,8", 9, 0)]
        [InlineData("3,9,7,9,10,9,4,9,99,-1,8", 7, 1)]
        [InlineData("3,9,7,9,10,9,4,9,99,-1,8", 8, 0)]
        [InlineData("3,9,7,9,10,9,4,9,99,-1,8", 9, 0)]
        [InlineData("3,3,1108,-1,8,3,4,3,99", 7, 0)]
        [InlineData("3,3,1108,-1,8,3,4,3,99", 8, 1)]
        [InlineData("3,3,1108,-1,8,3,4,3,99", 9, 0)]
        [InlineData("3,3,1107,-1,8,3,4,3,99", 7, 1)]
        [InlineData("3,3,1107,-1,8,3,4,3,99", 8, 0)]
        [InlineData("3,3,1107,-1,8,3,4,3,99", 9, 0)]
        [InlineData("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", 0, 0)]
        [InlineData("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", 9, 1)]
        [InlineData("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", 0, 0)]
        [InlineData("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", 9, 1)]
        [InlineData("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", 0, 999)]
        [InlineData("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", 7, 999)]
        [InlineData("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", 8, 1000)]
        [InlineData("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", 9, 1001)]
        public void TestPart1(string program, int input, params int[] expectedOutput)
        {
            var result = AoC.Part1(new[] { program }, input).ToArray();
            Assert.Equal(expectedOutput, result);
        }

        [Theory]
        [InlineData(1, 1, Mode.Position, Mode.Position, Mode.Position)]
        [InlineData(199, 99, Mode.Immediate, Mode.Position, Mode.Position)]
        [InlineData(1199, 99, Mode.Immediate, Mode.Immediate, Mode.Position)]
        [InlineData(11143, 43, Mode.Immediate, Mode.Immediate, Mode.Immediate)]
        public void DecodeTest(int value, int expectedOpCode, params Mode[] expectedModes)
        {
            (var opcode, var modes) = AoC.Decode(value);
            Assert.Equal(expectedOpCode, opcode);
            Assert.Equal(expectedModes, modes);
        }


        [Fact]
        public void TestPart2()
        {
            //var result = AoC.Part2(File.ReadAllLines("input.txt"), 3895705);
            //Assert.Equal(1202, result);
        }
    }
}
