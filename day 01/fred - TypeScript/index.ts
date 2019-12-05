import { readFileSync } from 'fs';

const input = readFileSync('./input.txt', 'utf8')
	.split('\r\n')
	.map(input => parseInt(input));

function getFuel(mass: number): number {
	return Math.floor(mass / 3) - 2;
}

const calculation = input.map(i => getFuel(i));
const result1 = calculation.reduce((total, current) => total + current, 0);

console.log(`The sum of the fuel requirements for all of the modules on your spacecraft is ${result1}`);

function getFuelRec(mass: number): number {
	const extraMass = Math.floor(mass / 3) - 2;
	return extraMass <= 0 ? 0 : extraMass + getFuelRec(extraMass);
}

const calculation2 = input.map(i => getFuelRec(i));
const result2 = calculation2.reduce((total, current) => total + current, 0);

console.log(`The sum of the total fuel requirements for all of the modules and the extra fuel on your spacecraft is ${result2}`);
