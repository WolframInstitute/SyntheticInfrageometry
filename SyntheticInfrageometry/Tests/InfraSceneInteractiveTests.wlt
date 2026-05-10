(* Smoke tests: every Manipulate-based viewer constructs without throwing
   on a small grid graph. We do not exercise the interactive controls. *)

VerificationTest[
  FreeQ[ PointViewer[ GridGraph[ { 3, 3 } ] ], $Failed ],
  True,
  TestID -> "PointViewer-constructs"
]

VerificationTest[
  FreeQ[ SegmentViewer[ GridGraph[ { 3, 3 } ] ], $Failed ],
  True,
  TestID -> "SegmentViewer-constructs"
]

VerificationTest[
  FreeQ[ ShellViewer[ GridGraph[ { 3, 3 } ] ], $Failed ],
  True,
  TestID -> "ShellViewer-constructs"
]

VerificationTest[
  FreeQ[ CircleViewer[ GridGraph[ { 3, 3 } ] ], $Failed ],
  True,
  TestID -> "CircleViewer-constructs"
]

VerificationTest[
  With[ { g = PathGraph[ Range[ 5 ] ], scene = InfraScene[ { p }, { p == InfraPoint[] } ] },
    FreeQ[ InfraSceneViewer[ scene, g ], $Failed ]
  ],
  True,
  TestID -> "InfraSceneViewer-constructs"
]
