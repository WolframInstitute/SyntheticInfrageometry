BeginTestSection["UniformMaps"]


(* ===== Conway operators on the cube give the Archimedean relatives ===== *)

VerificationTest[
  IsomorphicGraphQ[ RectifyMap @ PolyhedronData[ "Cube", "SkeletonGraph" ], PolyhedronData[ "Cuboctahedron", "SkeletonGraph" ] ],
  True,
  TestID -> "Rectify-cube-is-cuboctahedron"
]

VerificationTest[
  IsomorphicGraphQ[ TruncateMap @ PolyhedronData[ "Cube", "SkeletonGraph" ], PolyhedronData[ "TruncatedCube", "SkeletonGraph" ] ],
  True,
  TestID -> "Truncate-cube-is-truncated-cube"
]

VerificationTest[
  IsomorphicGraphQ[ DualMap @ PolyhedronData[ "Cube", "SkeletonGraph" ], PolyhedronData[ "Octahedron", "SkeletonGraph" ] ],
  True,
  TestID -> "Dual-cube-is-octahedron"
]

VerificationTest[
  IsomorphicGraphQ[ ExpandMap @ PolyhedronData[ "Cube", "SkeletonGraph" ], PolyhedronData[ "SmallRhombicuboctahedron", "SkeletonGraph" ] ],
  True,
  TestID -> "Expand-cube-is-small-rhombicuboctahedron"
]

VerificationTest[
  IsomorphicGraphQ[ BevelMap @ PolyhedronData[ "Cube", "SkeletonGraph" ], PolyhedronData[ "GreatRhombicuboctahedron", "SkeletonGraph" ] ],
  True,
  TestID -> "Bevel-cube-is-great-rhombicuboctahedron"
]


(* ===== Operators are curvature-agnostic: rectify preserves genus on the Klein quartic ===== *)

VerificationTest[
  With[ { r = RectifyMap @ SchlafliTessellation[ { 3, 7 } ] },
    { Union @ VertexDegree @ r, VertexTransitiveGraphQ @ r, MapGenus[ { 3, 7, 3, 7 }, r ] } ],
  { { 4 }, True, 3 },
  TestID -> "Rectify-Klein-is-genus-3-uniform"
]


(* ===== Spherical ArchimedeanTessellation: the 13 solids by vertex configuration ===== *)

VerificationTest[
  IsomorphicGraphQ[ ArchimedeanTessellation[ { 3, 4, 3, 4 } ], PolyhedronData[ "Cuboctahedron", "SkeletonGraph" ] ],
  True,
  TestID -> "Archimedean-3434-is-cuboctahedron"
]

VerificationTest[
  IsomorphicGraphQ[ ArchimedeanTessellation[ { 4, 6, 8 } ], PolyhedronData[ "GreatRhombicuboctahedron", "SkeletonGraph" ] ],
  True,
  TestID -> "Archimedean-468-is-great-rhombicuboctahedron"
]

VerificationTest[
  IsomorphicGraphQ[ ArchimedeanTessellation[ { 3, 3, 3, 3, 4 } ], PolyhedronData[ "SnubCube", "SkeletonGraph" ] ],
  True,
  TestID -> "Archimedean-snub-cube-via-PolyhedronData"
]


(* ===== Prisms and antiprisms ===== *)

VerificationTest[
  { VertexCount @ #, Union @ VertexDegree @ #, VertexTransitiveGraphQ @ # } &@ ArchimedeanTessellation[ { 4, 4, 5 } ],
  { 10, { 3 }, True },
  TestID -> "Archimedean-pentagonal-prism"
]

VerificationTest[
  IsomorphicGraphQ[ ArchimedeanTessellation[ { 3, 3, 3, 5 } ], PolyhedronData[ { "Antiprism", 5 }, "SkeletonGraph" ] ],
  True,
  TestID -> "Archimedean-pentagonal-antiprism"
]


(* ===== All-equal configuration forwards to SchlafliTessellation ===== *)

VerificationTest[
  IsomorphicGraphQ[ ArchimedeanTessellation[ { 4, 4, 4 } ], GraphData[ "CubicalGraph" ] ],
  True,
  TestID -> "Archimedean-444-forwards-to-cube"
]


(* ===== Euclidean uniform tilings on the torus: degree, transitivity, genus 1 ===== *)

VerificationTest[
  With[ { g = ArchimedeanTessellation[ { 3, 6, 3, 6 }, 4 ] },
    { Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g, MapGenus[ { 3, 6, 3, 6 }, g ] } ],
  { { 4 }, True, 1 },
  TestID -> "Archimedean-trihexagonal-torus"
]

VerificationTest[
  With[ { g = ArchimedeanTessellation[ { 4, 8, 8 }, 4 ] },
    { Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g, MapGenus[ { 4, 8, 8 }, g ] } ],
  { { 3 }, True, 1 },
  TestID -> "Archimedean-truncated-square-torus"
]

VerificationTest[
  With[ { g = ArchimedeanTessellation[ { 3, 12, 12 }, 4 ] },
    { Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g, MapGenus[ { 3, 12, 12 }, g ] } ],
  { { 3 }, True, 1 },
  TestID -> "Archimedean-truncated-hexagonal-torus"
]

VerificationTest[
  With[ { g = ArchimedeanTessellation[ { 3, 4, 6, 4 }, 4 ] },
    { Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g, MapGenus[ { 3, 4, 6, 4 }, g ] } ],
  { { 4 }, True, 1 },
  TestID -> "Archimedean-rhombitrihexagonal-torus"
]

VerificationTest[
  With[ { g = ArchimedeanTessellation[ { 4, 6, 12 }, 4 ] },
    { Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g, MapGenus[ { 4, 6, 12 }, g ] } ],
  { { 3 }, True, 1 },
  TestID -> "Archimedean-truncated-trihexagonal-torus"
]


(* ===== MapGenus: 0 spherical, 1 toroidal ===== *)

VerificationTest[
  { MapGenus[ { 3, 4, 3, 4 }, PolyhedronData[ "Cuboctahedron", "SkeletonGraph" ] ],
    MapGenus[ { 3, 6, 3, 6 }, ArchimedeanTessellation[ { 3, 6, 3, 6 }, 5 ] ] },
  { 0, 1 },
  TestID -> "MapGenus-sphere-and-torus"
]


(* ===== UniformMapQ: vertex-transitive 1-skeleton ===== *)

VerificationTest[
  { UniformMapQ @ PolyhedronData[ "Cuboctahedron", "SkeletonGraph" ], UniformMapQ @ PathGraph[ Range[ 4 ] ] },
  { True, False },
  TestID -> "UniformMapQ-cuboctahedron-yes-path-no"
]


(* ===== Deferred families fail gracefully ===== *)

VerificationTest[
  ArchimedeanTessellation[ { 3, 3, 3, 3, 6 }, 4 ],
  $Failed,
  { ArchimedeanTessellation::deferred },
  TestID -> "Archimedean-euclidean-snub-deferred"
]

EndTestSection[]
