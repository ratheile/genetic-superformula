(* ::Package:: *)

(* Wolfram Language package *)


InvertBit[in_] := Mod[2 (in/2 + 0.5), 2]


(*
[p1_: array<bit>]	array of bits that describe the first parent chromosome
[p2_: array<bit>]	array of bits that describe the second parent chromosome
[pos_: integer]		the position where the crossover cut happens

R[array<bit>]		one child chromosome as array<bit>
*)
CrossOver[p1_, p2_, pos_] :=
    Module[ {child},
        child = Join[Take[p1, pos], Drop[p2, pos]];
        Return[child]
    ]


(*
[chrom_: array<bit>] 		array of bits that describe the chromosome, 
[length_: integer] 		length of <chrom_>
[rate_: number] 		ratio of mutations added to the result

R[array<bit>]			Returns an array of bits representing the mutated chromosome
*)
Mutate[chrom_, length_, rate_] :=
    Module[ {tmp},  tmp  = chrom;
        Do[
         If[ Random [] < rate, 
              (*Print["Mutated at pos: ",i];*)
             tmp[[i]] = InvertBit[chrom[[i]]];
         ], {i, 1, length}];
        Return[tmp]
    ]



(*
[parent1_: array<bit>]												array of bits that describe the parent chromosome
[parent2_: array<bit>]												array of bits that describe the parent chromosome
[length_: integer] 													length of parent1 and parent2
[fpheno_: function(pop: Array<Array<bit>>) -> Array<Array>>] : 		a function that maps to the phenotype

R1[array<bit>]		child 1 from the parents 1 and 2
R2[array<bit>]		child 2 from the parents 1 and 2
*)
BreedSingleCross[parent1_, parent2_, length_, pcross_] :=
    Module[ {crossat = 0, child, parents},
        parents = {parent1, parent2};
        If[ Random[] < pcross,
        (*then do:*)
            crossat = RandomInteger[{1, length - 1}];
            child = CrossOver[parent1, parent2, crossat];,
         (*else*)
            child = parents[[RandomInteger[{1,2}]]];
        ];
        Return [child]
    ]


BreedDoubleCross[parent1_, parent2_, length_, pcross_] :=
    Module[ {crossat = 0, child, cross1, cross2, crossings, temp,
             parents, p1Ind, p2Ind},
        parents = {parent1, parent2};
        p1Ind = RandomInteger[{1,2}];
        p2Ind = If[p1Ind == 1, 2, 1];
        If[ Random[] < pcross,
        (*then do:*)
            cross1 = RandomInteger[{1, length - 1}];
            cross2 = RandomInteger[{1, length - 1}];
            If[cross1 != cross2, 
            (*then do:*)
               crossings= Sort[{cross1,cross2}];
               temp = CrossOver[parents[[p1Ind]], parents[[p2Ind]], crossings[[1]]];
               child = CrossOver[temp, parents[[p1Ind]], crossings[[2]]];,
            (*else:*)
               child = CrossOver[parents[[p1Ind]], parents[[p2Ind]], cross1];
            ];,
         (*else:*)
            child = parents[[p1Ind]];
        ];
        Return [child]
    ]


SelectParentByWheel[pop_, popFitness_] :=
    Module[ { totalFitness, wheel, elem, index, number},
        totalFitness = Fold[Plus, 0, popFitness]; (*Calculates sum(fitness(pheno(pop)))*)
    	(*Array of summed up fitness scores [f1, f1+f2, f1+f2+f3]*)
        wheel = FoldList[Plus, First[popFitness], Rest[popFitness]]; 
        (*WARNING: do not put this expression inside of the next statement, this results in an non-uniform distribution*)
        number = RandomReal[{0, totalFitness}];
        elem = First[Select[wheel, number <= #  &]];
        index = Flatten[Position[wheel, elem]];
        Return[First[index]]
    ]
