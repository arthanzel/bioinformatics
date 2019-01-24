(* ::Package:: *)

(* This file contains functions that deal with displaying model results and creating figures. *)

BeginPackage["summative`"];

mPlotPlants::usage = "plots plant stuff.";
mPlotPlantsSimple::usage = "Plots plant stuff without all the extra doodads.";
mPlantGrid::usage="Plots model results in a grid with four columns, for figure-making.";

Begin["`Private`"];

colors = {0 -> White, 1 -> Red, 2 -> Green, 3 -> Blue, 4 -> Yellow};

mPlotPlants = Function[{data},
	If[Depth[data] == 3,
		ArrayPlot[data, ColorRules -> colors, Mesh -> True, MeshStyle -> RGBColor[0.5, 0.5, 0.5, 0.2]],
		ListAnimate[Map[mPlotPlants[#] &, data], AnimationRunning -> False, Frame -> False]
	]
];

mPlotPlantsSimple = Function[{data},
    ArrayPlot[data, ColorRules -> colors]
];

mPlantGrid[results_, gens_] := 
    GraphicsGrid[Partition[mPlotPlantsSimple[results[[#]]] & /@ gens, 4, 4, 1, Nothing], ImageSize -> Large];

End[]
EndPackage[];



