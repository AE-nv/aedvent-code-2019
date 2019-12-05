import { readFileSync } from 'fs';

const input = readFileSync('./input.txt', 'utf8')
	.split(',')
	.map(input => parseInt(input));

function doCalculation(currentPosition: number, numbers:number[]): number {
	const operator = numbers[currentPosition];
	switch (operator) {
		case 99:
			return -1;
		case 1:
				numbers[numbers[currentPosition + 3]] = numbers[numbers[currentPosition + 1]] + numbers[numbers[currentPosition + 2]];
			return currentPosition + 4;
		case 2:
				numbers[numbers[currentPosition + 3]] = numbers[numbers[currentPosition + 1]] * numbers[numbers[currentPosition + 2]];
			return currentPosition + 4;
		default:
			throw new Error('Kapot');
	}
}

function restoreProgram(numbers:number[]):void{
	numbers[1] = 12;
	numbers[2] = 2;
}
let numbers = input.map(i => i);
restoreProgram(numbers);
let nextPosition = doCalculation(0, numbers);
while(nextPosition >= 0){
	nextPosition = doCalculation(nextPosition, numbers);
}

console.log(`The puzzle output is ${numbers.join(',')}`);
console.log(`The value left at position 0 is ${numbers[0]}`)
