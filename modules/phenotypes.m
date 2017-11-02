(* ::Package:: *)

BezierPhenotype[bin_, numCurves_, dimensions_, bezierOrder_, 
  resolution_ , imgSpace_] := 
 Module[
  {frac, curvesFrac, curvesImSpace},
  frac = Map[FromDigits[#, 2]/(2^resolution) &, 
    Partition[bin, resolution]];
  curvesFrac = Partition[Partition[frac, dimensions], bezierOrder];
  curvesImSpace = N[Map[Map[#*imgSpace &, #] &, curvesFrac]];
  Return[curvesImSpace]
  ](* Wolfram Language package *)
  
  
  


GielisPhenotype[bin_, resolution_, ranges_] :=  Module[{frac, params, scaledParams},
  frac = Map[FromDigits[#, 2]/(2^resolution) &, 
    Partition[bin, resolution]];
  scaledParams = N[Table[Rescale[frac[[i]], {0,1},ranges[[i]]],{i,1,7}]];
  Return[scaledParams]
]
