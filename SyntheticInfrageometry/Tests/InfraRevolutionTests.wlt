(* InfraRevolution.wl tests *)


(* Auto-flatten on the wrapper head. *)

VerificationTest[
  InfraRevolution[ { InfraRevolution[ { { 1 }, { 2, 3 } } ], InfraRevolution[ { { 4 } } ] } ],
  InfraRevolution[ { { 1 }, { 2, 3 }, { 4 } } ],
  TestID -> "InfraRevolution-auto-flatten"
]


(* PathGraph cylinder, r = 0, Solid: result is the axis itself. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 7, axis = { 2, 3, 4, 5, 6 } },
    FindCylinder[ g, axis, 0, 1, "Form" -> "Solid" ][[ 1, 1, 1 ]] ],
  { 2, 3, 4, 5, 6 },
  TestID -> "FindCylinder-PathGraph-r0-Solid-is-axis"
]


(* PathGraph cylinder, r = 1, Solid: axis plus immediate neighbours. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 9, axis = { 3, 4, 5, 6, 7 } },
    FindCylinder[ g, axis, 1, 1, "Form" -> "Solid" ][[ 1, 1, 1 ]] ],
  { 2, 3, 4, 5, 6, 7, 8 },
  TestID -> "FindCylinder-PathGraph-r1-Solid"
]


(* Calling triple n / UpTo[n] / All on a multi-axis (CycleGraph[6] has two
   geodesics from 1 to 4).  Use radius 0 in Solid form so the two
   geodesics produce distinct vertex sets (the axes themselves). *)

VerificationTest[
  With[ { g = CycleGraph @ 6, seg = FindSegment[ CycleGraph @ 6, 1, 4, All ] },
    Length @ FindRevolution[ g, InfraSegment @ seg, 0, All, "Form" -> "Solid" ] ],
  2,
  TestID -> "FindRevolution-multi-axis-All"
]

VerificationTest[
  With[ { g = CycleGraph @ 6, seg = FindSegment[ CycleGraph @ 6, 1, 4, All ] },
    Length @ FindRevolution[ g, InfraSegment @ seg, 0, 1, "Form" -> "Solid" ] ],
  1,
  TestID -> "FindRevolution-multi-axis-n1"
]

VerificationTest[
  With[ { g = CycleGraph @ 6, seg = FindSegment[ CycleGraph @ 6, 1, 4, All ] },
    FindRevolution[ g, InfraSegment @ seg, 0, UpTo @ 10, "Form" -> "Solid" ] =!= $Failed ],
  True,
  TestID -> "FindRevolution-multi-axis-UpTo"
]

VerificationTest[
  With[ { g = CycleGraph @ 6, seg = FindSegment[ CycleGraph @ 6, 1, 4, All ] },
    FindRevolution[ g, InfraSegment @ seg, 0, 5, "Form" -> "Solid" ] ],
  $Failed,
  TestID -> "FindRevolution-multi-axis-n-shortfall"
]


(* Surface is a subset of Solid for the same axis and profile. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], axis = { 1, 2, 3, 4 } },
    With[ {
        surf = FindRevolution[ g, axis, 1                       ][[ 1, 1, 1 ]],
        sol  = FindRevolution[ g, axis, 1, 1, "Form" -> "Solid" ][[ 1, 1, 1 ]] },
      SubsetQ[ sol, surf ] ] ],
  True,
  TestID -> "FindRevolution-Surface-subset-Solid"
]


(* Solid equals the union of capped-profile Surfaces.  Small integer profile
   on a grid -- check by direct computation. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 }, prof = { 0, 1, 2, 1, 0 } },
    With[ {
        sol   = FindRevolution[ g, axis, prof, 1, "Form" -> "Solid" ][[ 1, 1, 1 ]],
        union = Sort[ Union @@ Table[
          FindRevolution[ g, axis, Min[ #, k ] & /@ prof, 1, "Form" -> "Surface" ][[ 1, 1, 1 ]],
          { k, 0, Max @ prof } ] ] },
      Sort @ sol === union ] ],
  True,
  TestID -> "FindRevolution-Solid-equals-union-of-Surfaces"
]


(* FindCone with slope = 1 matches an explicit linear profile on a grid. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 } },
    FindCone[ g, axis, 1, 1, "Form" -> "Solid" ] ===
    FindRevolution[ g, axis, Range[ 0, 4 ], 1, "Form" -> "Solid" ] ],
  True,
  TestID -> "FindCone-slope1-matches-linear-profile"
]


(* FindCone "Apex" -> Last reverses the profile. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 } },
    FindCone[ g, axis, 1, 1, "Apex" -> Last, "Form" -> "Solid" ] ===
    FindRevolution[ g, axis, Range[ 4, 0, -1 ], 1, "Form" -> "Solid" ] ],
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


(* Singleton axis degenerates to FindShell.  The non-axis Voronoi-closest
   test reduces to the unique axis vertex, the comparison becomes
   d(v, c) == r -- exactly the FindShell semantics. *)

VerificationTest[
  With[ { g = PetersenGraph[] },
    FindRevolution[ g, { 1 }, 2 ][[ 1, 1, 1 ]] ===
    Sort @ Select[ VertexList @ g, GraphDistance[ g, 1, # ] === 2 & ] ],
  True,
  TestID -> "FindRevolution-singleton-axis-equals-FindShell"
]


(* Profile larger than diameter:  Solid covers everything, Surface is empty. *)

VerificationTest[
  With[ { g = PathGraph @ Range @ 5 },
    FindRevolution[ g, { 3 }, 100, 1, "Form" -> "Solid" ][[ 1, 1, 1 ]] === Sort @ VertexList @ g ],
  True,
  TestID -> "FindRevolution-large-radius-Solid-is-all"
]

VerificationTest[
  With[ { g = PathGraph @ Range @ 5 },
    FindRevolution[ g, { 3 }, 100 ][[ 1, 1, 1 ]] ],
  { },
  TestID -> "FindRevolution-large-radius-Surface-is-empty"
]


(* RevolutionQ round-trip. *)

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 }, prof = { 0, 1, 2, 1, 0 } },
    With[ { vs = FindRevolution[ g, axis, prof, 1, "Form" -> "Solid" ][[ 1, 1, 1 ]] },
      RevolutionQ[ g, vs, axis, prof, "Form" -> "Solid" ] ] ],
  True,
  TestID -> "RevolutionQ-round-trip-Solid"
]

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ], axis = { 1, 2, 3, 4, 5 }, prof = { 0, 1, 2, 1, 0 } },
    With[ { vs = FindRevolution[ g, axis, prof, 1, "Form" -> "Surface" ][[ 1, 1, 1 ]] },
      RevolutionQ[ g, vs, axis, prof, "Form" -> "Surface" ] ] ],
  True,
  TestID -> "RevolutionQ-round-trip-Surface"
]


(* Profile as an Association keyed by axis vertices. *)

VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ], axis = { 1, 2, 3, 4 } },
    FindRevolution[ g, axis, <| 1 -> 0, 2 -> 1, 3 -> 1, 4 -> 0 |>, 1, "Form" -> "Solid" ] ===
    FindRevolution[ g, axis, { 0, 1, 1, 0 }, 1, "Form" -> "Solid" ] ],
  True,
  TestID -> "FindRevolution-Association-equals-List"
]
