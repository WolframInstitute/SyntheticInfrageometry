(* InfraRevolution.wl tests *)


(* PathGraph cylinder, r = 0, Solid (default): result is the axis itself. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 7, axis = { 2, 3, 4, 5, 6 } },
    FindInfraCylinder[ g, axis, 0 ][[ 1 ]] ],
  { 2, 3, 4, 5, 6 },
  TestID -> "FindInfraCylinder-PathGraph-r0-default-Solid-is-axis"
]


(* PathGraph cylinder, r = 1, Solid (union-of-balls tube): the union of the
   closed radius-1 balls around each axis vertex, which on the path picks up
   the immediate neighbours 2 and 8 of the axis endpoints. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 9, axis = { 3, 4, 5, 6, 7 } },
    FindInfraCylinder[ g, axis, 1, "Form" -> "Solid" ][[ 1 ]] ],
  { 2, 3, 4, 5, 6, 7, 8 },
  TestID -> "FindInfraCylinder-PathGraph-r1-tube"
]


(* Surface is a subset of Solid for the same axis and profile. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], axis = { 1, 2, 3, 4 } },
    With[ {
        surf = FindInfraRevolution[ g, axis, 1, "Form" -> "Surface" ][[ 1 ]],
        sol  = FindInfraRevolution[ g, axis, 1, "Form" -> "Solid"   ][[ 1 ]] },
      SubsetQ[ sol, surf ] ] ],
  True,
  TestID -> "FindInfraRevolution-Surface-subset-Solid"
]


(* Solid equals the union of capped-profile Surfaces. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 }, prof = { 0, 1, 2, 1, 0 } },
    With[ {
        sol   = FindInfraRevolution[ g, axis, prof, "Form" -> "Solid" ][[ 1 ]],
        union = Sort[ Union @@ Table[
          FindInfraRevolution[ g, axis, Min[ #, k ] & /@ prof, "Form" -> "Surface" ][[ 1 ]],
          { k, 0, Max @ prof } ] ] },
      Sort @ sol === union ] ],
  True,
  TestID -> "FindInfraRevolution-Solid-equals-union-of-Surfaces"
]


(* FindInfraCone with slope = 1 matches an explicit linear profile on a grid. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 } },
    FindInfraCone[ g, axis, 1, "Form" -> "Solid" ] ===
    FindInfraRevolution[ g, axis, Range[ 0, 4 ], "Form" -> "Solid" ] ],
  True,
  TestID -> "FindInfraCone-slope1-matches-linear-profile"
]


(* FindInfraCone "Apex" -> Last reverses the profile. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 } },
    FindInfraCone[ g, axis, 1, "Apex" -> Last, "Form" -> "Solid" ] ===
    FindInfraRevolution[ g, axis, Range[ 4, 0, -1 ], "Form" -> "Solid" ] ],
  True,
  TestID -> "FindInfraCone-Apex-Last-reverses-profile"
]


(* Profile as a List and as a callable agree. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 } },
    FindInfraRevolution[ g, axis, Range[ 0, 4 ] ] ===
    FindInfraRevolution[ g, axis, # - 1 & ] ],
  True,
  TestID -> "FindInfraRevolution-list-and-function-agree"
]


(* Singleton axis with Surface degenerates to FindInfraShell. *)

VerificationTest[
  With[ { g = PetersenGraph[] },
    FindInfraRevolution[ g, { 1 }, 2, "Form" -> "Surface" ][[ 1 ]] ===
    Sort @ Select[ VertexList @ g, GraphDistance[ g, 1, # ] === 2 & ] ],
  True,
  TestID -> "FindInfraRevolution-singleton-axis-Surface-equals-FindInfraShell"
]


(* Profile larger than diameter:  Solid covers everything, Surface is empty. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 5 },
    FindInfraRevolution[ g, { 3 }, 100, "Form" -> "Solid" ][[ 1 ]] === Sort @ VertexList @ g ],
  True,
  TestID -> "FindInfraRevolution-large-radius-Solid-is-all"
]

VerificationTest[
  With[ { g = PathGraph @ Range @ 5 },
    FindInfraRevolution[ g, { 3 }, 100, "Form" -> "Surface" ][[ 1 ]] ],
  { },
  TestID -> "FindInfraRevolution-large-radius-Surface-is-empty"
]


(* Default "Form" is "Solid" and default Method is "Balls": the cylinder is the
   union of closed radius-1 balls around the axis (same tube as the explicit
   Solid form above). *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 9, axis = { 3, 4, 5, 6, 7 } },
    FindInfraCylinder[ g, axis, 1 ][[ 1 ]] ],
  { 2, 3, 4, 5, 6, 7, 8 },
  TestID -> "FindInfraCylinder-default-is-Solid"
]


(* Method -> "PerpendicularBisector": on a path graph every position's
   bisector slab is just that position itself, so the cylinder degenerates
   to the axis. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 9, axis = { 3, 4, 5, 6, 7 } },
    FindInfraCylinder[ g, axis, 1, Method -> "PerpendicularBisector" ][[ 1 ]] ],
  { 3, 4, 5, 6, 7 },
  TestID -> "FindInfraCylinder-PerpendicularBisector-PathGraph"
]


(* InfraRevolutionQ round-trip. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 }, prof = { 0, 1, 2, 1, 0 } },
    With[ { vs = FindInfraRevolution[ g, axis, prof, "Form" -> "Solid" ][[ 1 ]] },
      InfraRevolutionQ[ g, vs, axis, prof, "Form" -> "Solid" ] ] ],
  True,
  TestID -> "InfraRevolutionQ-round-trip-Solid"
]

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 }, prof = { 0, 1, 2, 1, 0 } },
    With[ { vs = FindInfraRevolution[ g, axis, prof, "Form" -> "Surface" ][[ 1 ]] },
      InfraRevolutionQ[ g, vs, axis, prof, "Form" -> "Surface" ] ] ],
  True,
  TestID -> "InfraRevolutionQ-round-trip-Surface"
]


(* Profile as an Association keyed by axis vertices. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], axis = { 1, 2, 3, 4 } },
    FindInfraRevolution[ g, axis, <| 1 -> 0, 2 -> 1, 3 -> 1, 4 -> 0 |>, "Form" -> "Solid" ] ===
    FindInfraRevolution[ g, axis, { 0, 1, 1, 0 }, "Form" -> "Solid" ] ],
  True,
  TestID -> "FindInfraRevolution-Association-equals-List"
]


(* Multi-axis (two geodesics in CycleGraph[6] form a thick axis;
   profile { 0, 1, 0, 0 } picks up the off-axis position-2 vertex 6
   in addition to vertex 2, which the single-axis case misses). *)

VerificationTest[
  With[ { g = CycleGraph[ 6 ] },
    FindInfraRevolution[ g,
      InfraSegment[ { { 1, 2, 3, 4 }, { 1, 6, 5, 4 } } ],
      { 0, 1, 0, 0 }, "Form" -> "Solid" ][[ 1 ]] ],
  { 1, 2, 3, 4, 5, 6 },
  TestID -> "FindInfraRevolution-multi-axis-thick"
]


(* Single-axis variant of the same profile gives a strictly smaller set. *)

VerificationTest[
  With[ { g = CycleGraph[ 6 ] },
    FindInfraRevolution[ g, { 1, 2, 3, 4 }, { 0, 1, 0, 0 }, "Form" -> "Solid" ][[ 1 ]] ],
  { 1, 2, 3, 4 },
  TestID -> "FindInfraRevolution-single-axis-thinner"
]
