(* Wolfram Language Test file *)

img1px = {{{1, 1, 1}}};
img2x2px = {{{1, 1, 1}, {1, 1, 1}}, {{1, 1, 1}, {1, 1, 1}}};
img2x2px2 = {{{1, 1, 1}, {1, 1, 1}}, {{0, 0, 0}, {0, 0, 0}}};

Test[
    FFitOptimized[img1px, {{1, 1}}, 1]
    ,
    1.
    ,
    TestID->"fitfunctionsTest-20170805-U2H7D9"
]

Test[
    FFitOptimized[img2x2px, {{1, 1}, {1, 2}}, 4]
    ,
    0.5
    ,
    TestID->"fitfunctionsTest-20170805-V6F8T0"
]

Test[
    FFitOptimized[img2x2px, {{1, 1}, {1, 2}}, 4]
    ,
    1.
    ,
    TestID->"fitfunctionsTest-20170805-Q8F5D3"
]

Test[
    FFitOptimized[img2x2px, {}, 4]
    ,
    0.
    ,
    TestID->"fitfunctionsTest-20170805-K6W3X9"
]