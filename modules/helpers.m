(* ::Package:: *)

(* Wolfram Language package *)


Logistic [x_] := Module[{},
(*If[x < 1*10^-5, Return[0]];
If[x > 1*10^5, Return[1]];*)
Return[N[1/(1+Exp[-20(x-0.5)])]]
]


CropX[image_] := Module[{ppos, max, min, imgDim},
ppos = PixelValuePositions[image,Black,0.5];
max = Max[ppos[[1;;Length[ppos],1]]];
min = Min[ppos[[1;;Length[ppos],1]]];
imgDim = ImageDimensions[image];
ImageTake[image,{0, imgDim[[2]]},{min,max}]
]


ResultPlot[imgDataPixels_, imgDataPixelsMirrored_, phenotype_,imgDim_, showMarkers_] := Module[
{border = 10,
plotrange = Null,
aspectRatio =(imgDim[[2]]*2) / imgDim[[1]],
styleBG = Darker[LightGray],
styleLine = Blue,
styleDot = Red, bgPlotUpper, bgPlotLower, line, markers}, 

plotrange = {{-imgDim[[1]]/2 -border,imgDim[[1]]/2 + border},{-imgDim[[2]] - border,imgDim[[2]]+ border}};
bgPlotUpper = ListPlot[imgDataPixels,PlotRange->plotrange, PlotStyle->styleBG, AspectRatio->aspectRatio];
bgPlotLower = ListPlot[imgDataPixelsMirrored,PlotRange->plotrange, PlotStyle->styleBG, AspectRatio->aspectRatio];
line = ListLinePlot[phenotype,PlotRange->plotrange,  PlotStyle->styleLine];
markers =ListPlot[phenotype,PlotRange->plotrange,  PlotStyle->styleDot, PlotMarkers->{Automatic,Tiny}];
 If[showMarkers,
Show[bgPlotUpper,bgPlotLower,line, markers],
Show[bgPlotUpper,bgPlotLower,line]
]
];
