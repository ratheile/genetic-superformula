(* ::Package:: *)

(* Wolfram Language package *)


FFitFuncGen[image_, mode_] := Module[
{newFunc, imArr, imgDim, imgPix, imgRotated, imgArrGrey},
  imgRotated = ImageRotate[image, -Pi/2];
  imArr = 1 - ImageData[imgRotated];
  imgArrGrey = ImageData[ColorConvert[imgRotated, "Grayscale"]];
  imgDim = Dimensions[imArr];
  imgPix = imgDim[[1]] * imgDim[[2]];
  Print["Calculated imArr, should only happen once"];
  Switch[mode, 
  "fill",
  newFunc := Function[{model}, FFitOptimized[imArr, model,  imgPix]],
  "edges",
  newFunc := Function[{model}, FFitEdges[imArr, model,  imgDim, 2]]
  ];
  Return[newFunc]
  ]


FFitOptimized[imgArrayData_, pixelsToVerify_, imgPix_] := Module[
  {numPix = Length[pixelsToVerify], negSum, range={-1,1}},
  (*use With to distribute the value to the kernels*)
  With[{d = N[range[[2]]-range[[1]]], min = range[[1]]},
  negSum = Sum[
    (Total[imgArrayData[[pixelsToVerify[[i, 1]], pixelsToVerify[[i, 2]]]]]/3)-1
      , {i, 1, numPix} 
      ]];
  Return[numPix + negSum]
  ]


FFitEdges[imgArrayData_, pixelsToVerify_, imgDim_, patchsize_] := Module[
  {numPix = Length[pixelsToVerify], sum},
  With[{p = patchsize, d = imgDim, img = imgArrayData, pix = pixelsToVerify},
  sum = Sum[Module[{
     xmin = Max[{1, pix[[i,1]]- p}],
     ymin = Max[{1, pix[[i,2]]- p}],
     xmax = Min[{d[[1]], pix[[i,1]]+ p}],
     ymax = Min[{d[[2]], pix[[i,2]]+ p}],
     min, max, patch, delta},
     patch = img[[xmin ;; xmax,ymin ;; ymax]];
     delta = Max[patch] - Min[patch];
     If[delta >0.5, delta, 0]
      ], {i, 1, numPix} 
      ]];
  Return[sum]
  ]


GielisFFitGen[image_, gaussianparam_] := Module[
{rotatedImg, convImg, greyImg, imArr, newFunc},
 rotatedImg = ImageRotate[image, -Pi/2];
 convImg =ImageConvolve[rotatedImg,GaussianMatrix[gaussianparam]];
 greyImg =ColorConvert[convImg, "Grayscale"];
 imArr = 1 - ImageData[greyImg];
 newFunc := Function[{points}, FFitGielis[imArr, points]];
 Return[newFunc]
]


FFitGielis[imArr_, points_]:= Module[
{numPoints = Length[points], sum, rounded, imArrDim = Dimensions[imArr],m},
	rounded = Round[points];
	m = Round[imArrDim[[1]]/2];
	sum = Sum[Module[{p},
		p = rounded[[i]];
		If[
		   p[[1]]+m >= 1 && Abs[p[[2]]] >= 1 &&
		   p[[1]]+m <= imArrDim[[1]] && Abs[p[[2]]]<= imArrDim[[2]], 
		   If[Positive[p[[2]]],
		      imArr[[p[[1]] + m,p[[2]]]],
		      imArr[[p[[1]] + m,-p[[2]]]]
		      ],
		0 ]
	],{i,1,numPoints}];
	Return[sum]
];
