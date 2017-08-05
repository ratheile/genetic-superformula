(* Wolfram Language Test file *)

(*inverse is 0. or 1. because its float*)
Test[
	InvertBit[1],0.,
	TestID->"breed-20170804-T4G8K9"
]

(*With probability of 1, everything should mutate*)
mutation = Mutate[{1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0}, 16, 1];
Test[mutation,
	 {0., 1., 0., 1., 0., 1., 0., 1., 0., 1., 0., 1., 0., 1., 0., 1.},
	 TestID->"breed-20170804-H8F4X6"
]

(*Test for Crossover*)
testPop = BinPopGenerator[2, 2];
res = CrossOver[testPop[[1]], testPop[[2]], 1]
