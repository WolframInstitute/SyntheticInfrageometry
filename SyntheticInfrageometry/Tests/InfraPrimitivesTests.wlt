(* Wrapper-head behaviour: only auto-flatten survives.  String accessors and
   Part upvalue rules were removed -- wrappers are raw data, callers use
   First / Length / Part on the inner list directly. *)

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

(* Round-trip: a List of unary wrappers wrapped under the same head collapses
   to the multi-realisation form (the canonical idiom for constructing multi
   from a Find* result). *)

VerificationTest[
  InfraPoint @ { InfraPoint[ { 1 } ], InfraPoint[ { 2 } ], InfraPoint[ { 3 } ] },
  InfraPoint[ { 1, 2, 3 } ],
  TestID -> "InfraPoint-unary-list-to-multi"
]

(* Default Part semantics: wrappers are raw data; Part returns inner elements. *)

VerificationTest[
  InfraPoint[ { 1, 2, 3 } ][[ 1 ]],
  { 1, 2, 3 },
  TestID -> "InfraPoint-Part-first-arg"
]

VerificationTest[
  First @ InfraSegment[ { { 1, 2, 3 }, { 1, 4, 3 } } ],
  { { 1, 2, 3 }, { 1, 4, 3 } },
  TestID -> "InfraSegment-First-inner-list"
]

VerificationTest[
  Length @ First @ InfraPoint[ { 1, 2, 3 } ],
  3,
  TestID -> "InfraPoint-Length-of-inner"
]
