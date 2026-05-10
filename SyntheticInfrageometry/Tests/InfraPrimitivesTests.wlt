(* Wrapper-head behaviour defined in InfraObjects.wl: auto-flatten of nested
   wrappers, Part semantics, and ["Realizations"] / ["Length"] / ["Expand"]
   accessors. *)

VerificationTest[
  InfraPoint[ { InfraPoint[ { 1, 2 } ], InfraPoint[ { 3 } ], 4 } ],
  InfraPoint[ { 1, 2, 3, 4 } ],
  TestID -> "InfraPoint-auto-flatten"
]

VerificationTest[
  InfraSegment[ { InfraSegment[ { { 1, 2 }, { 1, 3 } } ], InfraSegment[ { { 2, 3 } } ] } ],
  InfraSegment[ { { 1, 2 }, { 1, 3 }, { 2, 3 } } ],
  TestID -> "InfraSegment-auto-flatten"
]

VerificationTest[
  InfraPencil[ { InfraPencil[ { InfraRay[ { { 1, 2 } } ] } ], InfraRay[ { { 1, 3 } } ] } ],
  InfraPencil[ { InfraRay[ { { 1, 2 } } ], InfraRay[ { { 1, 3 } } ] } ],
  TestID -> "InfraPencil-auto-flatten"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 }, { 1, 5, 3 } } ][[ 2 ]],
  InfraSegment[ { { 1, 4, 3 } } ],
  TestID -> "InfraSegment-Part-integer"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 }, { 1, 5, 3 } } ][[ 1 ;; 2 ]],
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ],
  TestID -> "InfraSegment-Part-span"
]

VerificationTest[
  InfraPencil[ { InfraRay[ { { 1, 2 } } ], InfraRay[ { { 1, 3 } } ] } ][[ 1 ]],
  InfraRay[ { { 1, 2 } } ],
  TestID -> "InfraPencil-Part-integer-returns-ray"
]

VerificationTest[
  InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ][ "Realizations" ],
  { { 1, 2, 3 }, { 1, 4, 3 } },
  TestID -> "InfraSegment-Realizations"
]

VerificationTest[
  InfraPoint[ { 1, 2, 3 } ][ "Length" ],
  3,
  TestID -> "InfraPoint-Length"
]

VerificationTest[
  InfraPoint[ { 1, 2, 3 } ][ "Expand" ],
  { InfraPoint[ { 1 } ], InfraPoint[ { 2 } ], InfraPoint[ { 3 } ] },
  TestID -> "InfraPoint-Expand"
]
