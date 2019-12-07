using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace AdventOfCode
{
    public enum Mode
    {
        Position,
        Immediate
    }
    static class AoC
    {
        public static IEnumerable<int> Part1(string[] program) => Run(Parse(program), 1);
        public static IEnumerable<int> Part1(string[] program, int input) => Run(Parse(program), input);
        public static IEnumerable<int> Part2(string[] program) => Run(Parse(program), 5);

        static int[] Parse(string[] input) => input[0].Split(',').Select(int.Parse).ToArray();

        static void SetValue(this int[] program, int value, (int, Mode) parameter)
        {
            (var index, var mode) = parameter;
            program[index] = value;
        }
        static int GetValue(this int[] program, (int, Mode) parameter)
        {
            (var index, var mode) = parameter;
            return mode switch
            {
                Mode.Immediate => index,
                Mode.Position => program[index]
            };
        }

        static IEnumerable<int> GetValues(this int[] program, IEnumerable<(int, Mode)> parameters)
        {
            return parameters.Select(p => GetValue(program, p));
        }

        public static IEnumerable<int> Run(int[] program, int input)
        {
            int index = 0;
            int opcode; 
            Mode[] modes;
            do
            {
                Trace.WriteLine(string.Join(",", program));
                (opcode, modes) = Decode(program[index]);
                Trace.WriteLine($"{opcode} - {string.Join(",", modes)}");
                switch (opcode)
                {
                    case 1:
                    {
                        var parameters = GetParameters(program, index, modes, 3);
                        var values = parameters.Take(2).Select(p => program.GetValue(p)).ToArray();
                        var result =  values[0] + values[1];
                        program.SetValue(result, parameters.Last());
                        var jump = parameters.Length + 1;
                        index += jump;
                    }
                    break;
                    case 2 :
                    {
                        var parameters = GetParameters(program, index, modes, 3);
                        var values = parameters.Take(2).Select(p => program.GetValue(p)).ToArray();
                        var result = values[0] * values[1];
                        program.SetValue(result, parameters.Last());
                        var jump = parameters.Length + 1;
                        index += jump;

                    }
                    break;
                    case 3 :
                    {
                        var parameters = GetParameters(program, index, modes, 1);
                        program.SetValue(input, parameters.Last());
                        var jump = parameters.Length + 1;
                        index += jump;
                    }
                    break;
                    case 4:
                    {
                        var parameters = GetParameters(program, index, modes, 1);
                        var value = parameters.Take(1).Select(p => program.GetValue(p)).First();
                        yield return value;
                        var jump = parameters.Length + 1;
                        index += jump;
                    }
                    break;
                    case 5:
                    {
                        var parameters = GetParameters(program, index, modes, 2);
                        var values = parameters.Take(2).Select(p => program.GetValue(p)).ToArray();
                        index = values[0] == 0 ? index + parameters.Length + 1 : values[1];
                    }
                    break;
                    case 6:
                    {
                        var parameters = GetParameters(program, index, modes, 2);
                        var values = parameters.Take(2).Select(p => program.GetValue(p)).ToArray();
                        index = values[0] == 0 ? values[1] : index + parameters.Length + 1;
                    }
                    break;
                    case 7:
                    {
                        var parameters = GetParameters(program, index, modes, 3);
                        var values = parameters.Take(2).Select(p => program.GetValue(p)).ToArray();
                        var result = values[0] < values[1] ? 1 : 0;
                        program.SetValue(result, parameters.Last());
                        var jump = parameters.Length + 1;
                        index += jump;
                    }
                    break;
                    case 8:
                    {
                        var parameters = GetParameters(program, index, modes, 3);
                        var values = parameters.Take(2).Select(p => program.GetValue(p)).ToArray();
                        var result = values[0] == values[1] ? 1 : 0;
                        program.SetValue(result, parameters.Last());
                        var jump = parameters.Length + 1;
                        index += jump;
                    }
                    break;
                    case 99:
                        break;
                    default: 
                        throw new Exception();
                }
            }
            while (opcode != 99);
        }

        static (int value, Mode mode)[] GetParameters(int[] program, int index, Mode[] modes, int n)
        {
            return program.AsSpan().Slice(index + 1, n).ToArray().Zip(modes, (l, r) => (value: l, mode: r)).ToArray();
        }

        internal static (int opcode, Mode[] modes) Decode(int value)
        {
            Mode[] modes = new Mode[3];
            var opcode = value % 100;
            value /= 100;
            for (int i = 0; i < 3; i++)
            {
                modes[i] = (Mode)(value % 10);
                value /= 10;
            }
            return (opcode, modes);
        }


    }
}
