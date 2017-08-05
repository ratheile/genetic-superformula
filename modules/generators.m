(* Wolfram Language package *)
(*
Generate a binary string population 

[numSamples_: number]	the number of samples
[strLength_: number]		the length of one sample

R[array<array<bit>>]		return the number of samples requested as binary arrays
*)
BinPopGenerator[numSamples_, strlength_] := 
  Table[Random[Integer, {0, 1}], {i, 1, numSamples}, {j, 1, 
    strlength}];