(* InfraRevolution.wl tests *)


(* PathGraph cylinder, r = 0, Solid (default): result is the axis itself. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 7, axis = { 2, 3, 4, 5, 6 } },
    FindCylinder[ g, axis, 0 ][[ 1 ]] ],
  { 2, 3, 4, 5, 6 },
  TestID -> "FindCylinder-PathGraph-r0-default-Solid-is-axis"
]


(* PathGraph cylinder, r = 1, Solid: the +1 axis extension absorbs the
   immediate-neighbour bubble at each endpoint, so the cylinder reduces
   to the axis itself on a 1D substrate. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 9, axis = { 3, 4, 5, 6, 7 } },
    FindCylinder[ g, axis, 1, "Form" -> "Solid" ][[ 1 ]] ],
  { 3, 4, 5, 6, 7 },
  TestID -> "FindCylinder-PathGraph-r1-axis-only-via-extension"
]


(* Surface is a subset of Solid for the same axis and profile. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], axis = { 1, 2, 3, 4 } },
    With[ {
        surf = FindRevolution[ g, axis, 1, "Form" -> "Surface" ][[ 1 ]],
        sol  = FindRevolution[ g, axis, 1, "Form" -> "Solid"   ][[ 1 ]] },
      SubsetQ[ sol, surf ] ] ],
  True,
  TestID -> "FindRevolution-Surface-subset-Solid"
]


(* Solid equals the union of capped-profile Surfaces. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 }, prof = { 0, 1, 2, 1, 0 } },
    With[ {
        sol   = FindRevolution[ g, axis, prof, "Form" -> "Solid" ][[ 1 ]],
        union = Sort[ Union @@ Table[
          FindRevolution[ g, axis, Min[ #, k ] & /@ prof, "Form" -> "Surface" ][[ 1 ]],
          { k, 0, Max @ prof } ] ] },
      Sort @ sol === union ] ],
  True,
  TestID -> "FindRevolution-Solid-equals-union-of-Surfaces"
]


(* FindCone with slope = 1 matches an explicit linear profile on a grid. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 } },
    FindCone[ g, axis, 1, "Form" -> "Solid" ] ===
    FindRevolution[ g, axis, Range[ 0, 4 ], "Form" -> "Solid" ] ],
  True,
  TestID -> "FindCone-slope1-matches-linear-profile"
]


(* FindCone "Apex" -> Last reverses the profile. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 } },
    FindCone[ g, axis, 1, "Apex" -> Last, "Form" -> "Solid" ] ===
    FindRevolution[ g, axis, Range[ 4, 0, -1 ], "Form" -> "Solid" ] ],
  True,
  TestID -> "FindCone-Apex-Last-reverses-profile"
]


(* Profile as a List and as a callable agree. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 } },
    FindRevolution[ g, axis, Range[ 0, 4 ] ] ===
    FindRevolution[ g, axis, # - 1 & ] ],
  True,
  TestID -> "FindRevolution-list-and-function-agree"
]


(* Singleton axis with Surface degenerates to FindShell. *)

VerificationTest[
  With[ { g = PetersenGraph[] },
    FindRevolution[ g, { 1 }, 2, "Form" -> "Surface" ][[ 1 ]] ===
    Sort @ Select[ VertexList @ g, GraphDistance[ g, 1, # ] === 2 & ] ],
  True,
  TestID -> "FindRevolution-singleton-axis-Surface-equals-FindShell"
]


(* Profile larger than diameter:  Solid covers everything, Surface is empty. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 5 },
    FindRevolution[ g, { 3 }, 100, "Form" -> "Solid" ][[ 1 ]] === Sort @ VertexList @ g ],
  True,
  TestID -> "FindRevolution-large-radius-Solid-is-all"
]

VerificationTest[
  With[ { g = PathGraph @ Range @ 5 },
    FindRevolution[ g, { 3 }, 100, "Form" -> "Surface" ][[ 1 ]] ],
  { },
  TestID -> "FindRevolution-large-radius-Surface-is-empty"
]


(* Default "Form" is "Solid" and default Method is "Voronoi" with +1 axis
   extension; immediate-neighbour bubble at endpoints is absorbed. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 9, axis = { 3, 4, 5, 6, 7 } },
    FindCylinder[ g, axis, 1 ][[ 1 ]] ],
  { 3, 4, 5, 6, 7 },
  TestID -> "FindCylinder-default-is-Solid"
]


(* Method -> "PerpendicularBisector": on a path graph every position's
   bisector slab is just that position itself, so the cylinder degenerates
   to the axis. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 9, axis = { 3, 4, 5, 6, 7 } },
    FindCylinder[ g, axis, 1, Method -> "PerpendicularBisector" ][[ 1 ]] ],
  { 3, 4, 5, 6, 7 },
  TestID -> "FindCylinder-PerpendicularBisector-PathGraph"
]


(* RevolutionQ round-trip. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 }, prof = { 0, 1, 2, 1, 0 } },
    With[ { vs = FindRevolution[ g, axis, prof, "Form" -> "Solid" ][[ 1 ]] },
      RevolutionQ[ g, vs, axis, prof, "Form" -> "Solid" ] ] ],
  True,
  TestID -> "RevolutionQ-round-trip-Solid"
]

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 }, prof = { 0, 1, 2, 1, 0 } },
    With[ { vs = FindRevolution[ g, axis, prof, "Form" -> "Surface" ][[ 1 ]] },
      RevolutionQ[ g, vs, axis, prof, "Form" -> "Surface" ] ] ],
  True,
  TestID -> "RevolutionQ-round-trip-Surface"
]


(* Profile as an Association keyed by axis vertices. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], axis = { 1, 2, 3, 4 } },
    FindRevolution[ g, axis, <| 1 -> 0, 2 -> 1, 3 -> 1, 4 -> 0 |>, "Form" -> "Solid" ] ===
    FindRevolution[ g, axis, { 0, 1, 1, 0 }, "Form" -> "Solid" ] ],
  True,
  TestID -> "FindRevolution-Association-equals-List"
]


(* Multi-axis (two geodesics in CycleGraph[6] form a thick axis;
   profile { 0, 1, 0, 0 } picks up the off-axis position-2 vertex 6
   in addition to vertex 2, which the single-axis case misses). *)

VerificationTest[
  With[ { g = CycleGraph[ 6 ] },
    FindRevolution[ g,
      InfraSegment[ { { 1, 2, 3, 4 }, { 1, 6, 5, 4 } } ],
      { 0, 1, 0, 0 }, "Form" -> "Solid" ][[ 1 ]] ],
  { 1, 2, 3, 4, 5, 6 },
  TestID -> "FindRevolution-multi-axis-thick"
]


(* Single-axis variant of the same profile gives a strictly smaller set. *)

VerificationTest[
  With[ { g = CycleGraph[ 6 ] },
    FindRevolution[ g, { 1, 2, 3, 4 }, { 0, 1, 0, 0 }, "Form" -> "Solid" ][[ 1 ]] ],
  { 1, 2, 3, 4 },
  TestID -> "FindRevolution-single-axis-thinner"
]
