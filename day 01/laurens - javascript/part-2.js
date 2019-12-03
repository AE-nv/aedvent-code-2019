// https://alfie.prodo.ai/repulsive-yorkshire-pudding/1

function calculateFuelRequirement(mass) {
  let fuel = Math.floor(mass / 3) - 2;
  if (fuel <= 0) return 0;
  return fuel + calculateFuelRequirement(fuel);
}

// input;
let totalFuel = JSON.parse(input)
	.reduce((fuel, mass) => fuel + calculateFuelRequirement(mass), 0);
console.log(totalFuel);

