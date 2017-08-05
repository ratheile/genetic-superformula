(* ::Package:: *)

(* Wolfram Language package *)


ffit[img_, model_] := Fold[Plus,
  Map[Function[{x},
    Fold[#1 + #2 &,
      1 - PixelValue[img, x][[1 ;; 3]]
     ]
    ], model]
  ]



FFitFuncGen[image_] := Module[{newFunc, imArr},
  imArr = 1 - ImageData[image];
  Print["Calculated imArr, should only happen once"];
  newFunc := Function[{model}, FFitOptimized[imArr, model]];
  Return[newFunc]
  ]


FFitOptimized[imgArrayData_, pixelsToVerify_] := Module[
  {numPix = Length[pixelsToVerify], sum, range={-1,1}},
  (*use With to distribute the value to the kernels*)
  With[{d = N[range[[2]]-range[[1]]], min = range[[1]]},
  sum = ParallelSum[
    Total[imgArrayData[[pixelsToVerify[[i, 1]], pixelsToVerify[[i, 2]]]]]/3*d + min
      , {i, 1, numPix} 
      ]];
  Return[sum]
  ]
