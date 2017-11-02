(* ::Package:: *)

(* Wolfram Language package *)


Gielis[p_,a_,b_,m1_,m2_,n1_, n2_, n3_] := Module[{rad},
rad =(Abs[Cos[m1*p/4]/a]^n2+Abs[Sin[m2*p/4]/b]^n3)^(-1/n1);
Return[{rad*Cos[p], rad*Sin[p]}]
]


GielisScaled[res_, boundaries_, param_]:= Module[{points, blow, bupper, mxx, mnx,dx, db,scaled, p}, 
 p = param/.x_/; x == 0 -> 0.00001;
points=Table[Gielis[(i*2*Pi/res),p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]]],{i,1,res}];
blow = boundaries[[1]];bupper =boundaries[[2]]; 
mxx =Max[points[[1;;res, 1]]];
mnx =Min[points[[1;;res, 1]]];
dx = mxx-mnx; db = bupper-blow;
scaled = Table[{((points[[i,1]]-mnx)*(db/dx))+ blow,points[[i,2]]* db/dx},
{i,1,res}];
Return[scaled]
]


GielisOffset[res_, param_]:= Module[{points,poffset,p}, 
 p = param/.x_/; x == 0 -> 0.00001;
points=Table[
	Gielis[-Pi+(i*2*Pi/res),
	p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]]],
{i,1,res}];
Return[points]
]


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)
