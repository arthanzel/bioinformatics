(* ::Package:: *)

(* This file contains a bunch of handy helper functions. Not all of them are used in the final product. *)

BeginPackage["summative`"];

(* Exports *)
mConcat::usage="concatenates all arguments into a flattened array. Arrays within arrays are completely flattened.";
mDistanceMatrix::usage="creates a matrix with a given radius where each element is the Euclidean distance between its cell and the centre cell.";
mEffectMatrix::usage="returns a matrix of effect sizes in 2 dimensions diminishing with the square of the distance from the centre.";
mListNeighbours::usage="gets a matrix containing a list of neighbours within a certain radius for each element.";
mMaskCircle::usage="creates a matrix with a given radius that contains a circular mask. 1s represent cells within the circle.";
mMaskManhattan::usage="creates a matrix with a given radius that contains a diamond-shaped mask.";
mMaskSquare::usage="creates a matrix with a given radius that contains a square-shaped mask excluding the centre cell.";
mNumNeighbours::usage="creates a matrix containing the number of neighbours that each cell has of a given species.";
mProbSeeding::usage="computes the chance that a patch is successfully
seeded if each seeding event has chance p of successfully seeding and there
are n seeding events.";
mRandomBinomial::usage="performs a number of independent Bernoulli trials and counts the number of successes.";
mRandomStart::usage = "generates a random starting matrix";

Begin["`Private`"];

mConcat[elements__] := Flatten[{elements}];

mDistanceMatrix[radius_] := mDistanceMatrix[radius] =
	(* This is about 30 % faster than CoordinateBoundsArray and EuclideanDistance. *)
	Table[
		Sqrt[x^2 + y^2],
		{x, -radius, radius}, {y, -radius, radius}
	];

mEffectMatrix[radius_] := mEffectMatrix[radius] = Module[{},
	m = Quiet[1 / mDistanceMatrix[radius]];
	m[[radius + 1, radius + 1]] = 0;
	m
];

mListNeighbours[m_, radius_] := With[
	{ width = 2 radius + 1 },
	kernel = CenterArray[0, {width, width}, 1];
	ListConvolve[kernel, m, radius + 1 (*center*), m (*wrap around*), Times, mConcat]
];

mMaskCircle[radius_] := mMaskCircle[radius] =
	Map[Boole[# <= radius + 0.5] &, mDistanceMatrix[radius], {2}];
	
mMaskManhattan[radius_] := mMaskManhattan[radius] =
	Table[
		Boole[Abs[x] + Abs[y] <= radius],
		{x, -radius, radius}, {y, -radius, radius}
	];

mMaskSquare[radius_] := mMaskSquare[radius] = With[
	{ width = 2 radius + 1 },
	CenterArray[0, {width, width}, 1]
];

mNumNeighbours[m_, type_, radius_] := With[
	{ kernel = CenterArray[0, {2 radius + 1, 2 radius + 1}, 1] },
	ListConvolve[kernel, m, radius + 1, m, #1 * Boole[#2 == type] &, Plus]
	(* In ListConvolve functions, #1 is kernel element, #2 is matrix element. *)
];

mProbSeeding[p_,events_] := 1-((1-p)^events);

(* Select+RandomReal is a heck of a lot faster than InverseCDF. *)
mRandomBinomial[n_, p_] := Select[RandomReal[1, n],# < p &] // Length;

mRandomStart[r_, n_, s_] := (
    m = ConstantArray[0, {2r + 1, 2r + 1}];
    Do[m[[RandomInteger[{1, 2r + 1}], RandomInteger[{1, 2r + 1}]]] = RandomInteger[{1, s}], n];
    m
);

End[]
EndPackage[];



