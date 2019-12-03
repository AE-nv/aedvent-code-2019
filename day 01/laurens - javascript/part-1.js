// https://alfie.prodo.ai/luminous-reunion

function calculateFuelRequirement(mass) {
  return Math.floor(mass / 3) - 2;
}

// input;
let totalFuel = JSON.parse(input)
	.reduce((fuel, mass) => calculateFuelRequirement(mass) + fuel, 0);
console.log(totalFuel);
