BeginTestSection["RegularMaps"]


(* ===== Spherical: isomorphic to the Platonic graphs ===== *)

VerificationTest[
  IsomorphicGraphQ[ SchlafliTessellation[ { 3, 3 } ], GraphData[ "TetrahedralGraph" ] ],
  True,
  TestID -> "Schlafli-33-is-tetrahedron"
]

VerificationTest[
  IsomorphicGraphQ[ SchlafliTessellation[ { 4, 3 } ], GraphData[ "CubicalGraph" ] ],
  True,
  TestID -> "Schlafli-43-is-cube"
]

VerificationTest[
  IsomorphicGraphQ[ SchlafliTessellation[ { 3, 4 } ], GraphData[ "OctahedralGraph" ] ],
  True,
  TestID -> "Schlafli-34-is-octahedron"
]

VerificationTest[
  IsomorphicGraphQ[ SchlafliTessellation[ { 5, 3 } ], GraphData[ "DodecahedralGraph" ] ],
  True,
  TestID -> "Schlafli-53-is-dodecahedron"
]

VerificationTest[
  IsomorphicGraphQ[ SchlafliTessellation[ { 3, 5 } ], GraphData[ "IcosahedralGraph" ] ],
  True,
  TestID -> "Schlafli-35-is-icosahedron"
]


(* ===== Defining incidences: q-regular, and #p-cycles == F = |G|/p ===== *)

VerificationTest[
  Union @ VertexDegree @ SchlafliTessellation[ { 3, 5 } ],
  { 5 },
  TestID -> "Schlafli-is-q-regular"
]

VerificationTest[
  { Length @ FindCycle[ SchlafliTessellation[ { 4, 3 } ], { 4 }, All ],
    Length @ FindCycle[ SchlafliTessellation[ { 5, 3 } ], { 5 }, All ] },
  { 6, 12 },
  TestID -> "Schlafli-p-cycle-count-equals-faces"
]


(* ===== Local isomorphism: every 1-ball is the same induced graph ===== *)

VerificationTest[
  With[ { g = SchlafliTessellation[ { 3, 5 } ] },
    Length @ DeleteDuplicates[ CanonicalGraph[ NeighborhoodGraph[ g, #, 1 ] ] & /@ VertexList[ g ], IsomorphicGraphQ ]
  ],
  1,
  TestID -> "Schlafli-locally-isomorphic"
]


(* ===== Euclidean: forwards to the flat torus of size n ===== *)

VerificationTest[
  IsomorphicGraphQ[ SchlafliTessellation[ { 4, 4 }, 5 ], TorusTessellation[ { 5, 5 }, "Square" ] ],
  True,
  TestID -> "Schlafli-44-is-torus"
]

VerificationTest[
  IsomorphicGraphQ[ SchlafliTessellation[ { 6, 3 }, 4 ], TorusTessellation[ { 4, 4 }, "Hexagonal" ] ],
  True,
  TestID -> "Schlafli-63-is-hexagonal-torus"
]


(* ===== Hyperbolic: {3,7} is the Klein quartic (genus 3) ===== *)

VerificationTest[
  With[ { g = SchlafliTessellation[ { 3, 7 } ] },
    { VertexCount @ g, EdgeCount @ g, Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g }
  ],
  { 24, 84, { 7 }, True },
  TestID -> "Schlafli-37-Klein-quartic-skeleton"
]

VerificationTest[
  With[ { g = SchlafliTessellation[ { 3, 7 } ] },
    With[ { v = VertexCount @ g, e = EdgeCount @ g, f = Length @ FindCycle[ g, { 3 }, All ] },
      1 - ( v - e + f )/2 ]
  ],
  3,
  TestID -> "Schlafli-37-genus-3"
]


(* ===== RegularMapGenus: 0 spherical, 1 torus, >= 2 hyperbolic ===== *)

VerificationTest[
  { RegularMapGenus[ { 3, 5 }, SchlafliTessellation[ { 3, 5 } ] ],
    RegularMapGenus[ { 4, 4 }, SchlafliTessellation[ { 4, 4 }, 5 ] ],
    RegularMapGenus[ { 3, 7 }, SchlafliTessellation[ { 3, 7 } ] ] },
  { 0, 1, 3 },
  TestID -> "RegularMapGenus-sphere-torus-Klein"
]

VerificationTest[
  { RegularMapGenus[ SchlafliTessellation[ { 3, 5 } ] ],
    RegularMapGenus[ SchlafliTessellation[ { 4, 4 }, 5 ] ],
    RegularMapGenus[ SchlafliTessellation[ { 3, 7 } ] ] },
  { 0, 1, 3 },
  TestID -> "RegularMapGenus-from-graph-alone"
]


(* ===== RegularMapsAt: PSL(2,ell) needs a prime level ===== *)

VerificationTest[
  Head @ RegularMapsAt[ { 3, 7 }, 20 ],
  RegularMapsAt,
  TestID -> "RegularMapsAt-nonprime-level-inert"
]

VerificationTest[
  Length @ RegularMapsAt[ { 3, 7 }, 7 ],
  1,
  TestID -> "RegularMapsAt-Klein-unique-at-7"
]


(* ===== Hyperbolic map out of search range fails gracefully ===== *)

VerificationTest[
  SchlafliTessellation[ { 3, 7 }, 99 ],
  $Failed,
  TestID -> "Schlafli-hyperbolic-unreachable-is-Failed"
]


(* ===== Generality: the explicit-group primitive agrees ===== *)

VerificationTest[
  IsomorphicGraphQ[ RegularMap[ { 4, 3 }, SymmetricGroup[ 4 ] ], GraphData[ "CubicalGraph" ] ],
  True,
  TestID -> "RegularMap-explicit-group-cube"
]

EndTestSection[]
