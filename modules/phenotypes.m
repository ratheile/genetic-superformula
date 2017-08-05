

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
  
  
  