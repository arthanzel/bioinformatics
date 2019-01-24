(* ::Package:: *)

BeginPackage["disease`"];
mRunModel::usage = "Simulates a CA.";

Begin["`Private`"];
mCoordinateMatrix::usage = "Creates a matrix with given dimensions whose elements are coordinates of each element.";
mCoordinateMatrix = Function[{dims},
	If[Length[Dimensions[dims]] > 1,
		mCoordinateMatrix[Dimensions[dims]],
	   Array[{##} &, dims]
	]
];

mForEachInMatrix::usage = "Applies a function to each element in a matrix. The first argument of the function is the element. The second argument is the coordinate of the element.";
mForEachInMatrix=Function[{matrix, fn},
	(* TODO: Parallelize *)
    MapThread[fn, {matrix, mCoordinateMatrix[Dimensions[matrix]]}, Length[Dimensions[matrix]]]
];

mRunModel=Function[{initialMatrix, cellUpdatingFunction, neighborFunction, iterations},
	NestList[ (*runs as discrete time function*)
		Function[{currentMatrix},
			mForEachInMatrix[currentMatrix, (*apply some function to every cell in matrix*)
				Function[{cell,coords},
				    cellUpdatingFunction[cell, neighborFunction[currentMatrix, coords[[1]], coords[[2]]]]
				]
			]
		],
		initialMatrix,
		iterations
    ]
];
End[];
EndPackage[];
