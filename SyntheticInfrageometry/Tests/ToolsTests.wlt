BeginTestSection["Tools"]

(* the shape reader and the anchor rule are internal, so the tests reach them by
   their PackageScope context *)
toDensity           = WolframInstitute`SyntheticInfrageometry`PackageScope`toDensity;
pointQ              = WolframInstitute`SyntheticInfrageometry`PackageScope`pointQ;
multisetQ           = WolframInstitute`SyntheticInfrageometry`PackageScope`multisetQ;
walkQ               = WolframInstitute`SyntheticInfrageometry`PackageScope`walkQ;
geodesicGraph       = WolframInstitute`SyntheticInfrageometry`PackageScope`geodesicGraph;
infraSpread         = WolframInstitute`SyntheticInfrageometry`PackageScope`infraSpread;
infraVertexMultiset = WolframInstitute`SyntheticInfrageometry`PackageScope`infraVertexMultiset;
infraEdgeMultiset   = WolframInstitute`SyntheticInfrageometry`PackageScope`infraEdgeMultiset;
closedWalkGraph     = WolframInstitute`SyntheticInfrageometry`PackageScope`closedWalkGraph;

g33 = GridGraph[ { 3, 3 } ];

(* The distance metrics, centrality helpers, separating-cycle predicates, and
   path-selection routines in Tools.wl are now package-scope (internal). They
   are exercised indirectly through the public Find* functions and their
   "Select" option. Direct unit tests for them have been removed. *)

VerificationTest[
  True,
  True,
  TestID -> "Tools-placeholder"
]

(* ===================== InfraDensity ===================== *)

(* InfraDensity is the marginal of any shape to the vertex set with respect to the
   counting measure -- the anchor rule made public.  Raw masses, not a normalisation:
   Total, Merge, KeyMap and KeySelect are the density's own algebra, and dividing by
   Total is the caller's one-liner.  A family of two sets: the shared vertices carry 2 *)
VerificationTest[
  InfraDensity[ g33, { { 1, 2, 3 }, { 2, 3, 4 } } ],
  <| 1 -> 1, 2 -> 2, 3 -> 2, 4 -> 1 |>,
  TestID -> "InfraDensity-family-of-sets-sums-multiplicities"
]

(* one walk: every vertex visited exactly once maps to 1 *)
VerificationTest[
  InfraDensity[ g33, geodesicGraph @ { 1, 2, 3 } ],
  <| 1 -> 1, 2 -> 1, 3 -> 1 |>,
  TestID -> "InfraDensity-single-realisation-all-one"
]

(* a bundle: the total mass is the total length of its realisations, since every
   realisation contributes each of its vertices once *)
VerificationTest[
  With[ { reps = { { 1, 2, 3, 6, 9 }, { 1, 4, 7, 8, 9 }, { 1, 2, 5, 8, 9 } } },
    Total @ Values @ InfraDensity[ g33, geodesicGraph /@ reps ] === Total[ Length /@ reps ] ],
  True,
  TestID -> "InfraDensity-total-mass-is-total-length"
]

(* the empty class yields the empty density *)
VerificationTest[
  InfraDensity[ g33, { } ],
  <||>,
  TestID -> "InfraDensity-empty-class"
]

(* a density is already the marginal, so InfraDensity is the identity on it up to
   key order -- the guarantee that two built by different routes compare SameQ *)
VerificationTest[
  InfraDensity[ g33, <| 2 -> 3, 1 -> 1 |> ],
  <| 1 -> 1, 2 -> 3 |>,
  TestID -> "InfraDensity-density-is-key-sorted-identity"
]

(* Counts promotes a list, Keys demotes a density: the two steps InfraDensity sits
   between, and the reason there is no second coercion in the API *)
VerificationTest[
  With[ { ball = FindInfraBall[ g33, 5, 1 ] },
    Keys @ InfraDensity[ g33, ball ] === ball &&
      InfraDensity[ g33, ball ] === KeySort @ Counts @ ball ],
  True,
  TestID -> "InfraDensity-Counts-promotes-Keys-demotes"
]

(* a bare vertex is the unit mass, even where the label is itself a List: only the
   graph tells a list-labelled vertex from a two-element set *)
VerificationTest[
  With[ { g = GridGraph[ { 2, 2 }, VertexLabels -> None ] },
    { InfraDensity[ g33, 5 ],
      InfraDensity[ TessellationGraph[ { 4, 4 }, 1 ], { 1, 1 } ] } ],
  { <| 5 -> 1 |>, <| { 1, 1 } -> 1 |> },
  TestID -> "InfraDensity-vertex-is-unit-mass"
]

(* normalising is the caller's, and both readings the old measure head carried are
   one division away *)
VerificationTest[
  With[ { d = InfraDensity[ g33, <| 1 -> 3, 2 -> 1 |> ] },
    { Max @ Values[ d / Max @ Values @ d ], Total @ Values[ d / Total @ Values @ d ] } ],
  { 1, 1 },
  TestID -> "InfraDensity-normalisations-are-one-division-away"
]

(* edges are internal now: the renderer reads infraEdgeMultiset, keyed by the sorted
   vertex pair, and remaps it to UndirectedEdge itself *)
VerificationTest[
  Sort @ Keys @ infraEdgeMultiset[ PathGraph @ Range[ 4 ], geodesicGraph @ { 1, 2, 3, 4 } ],
  { { 1, 2 }, { 2, 3 }, { 3, 4 } },
  TestID -> "infraEdgeMultiset-keys-are-sorted-pairs"
]

(* a closed walk closes: the cycle 1-2-3 carries its three edges, the wrap included *)
VerificationTest[
  Sort @ Keys @ infraEdgeMultiset[ CycleGraph @ 3, closedWalkGraph @ { 1, 2, 3, 1 } ],
  { { 1, 2 }, { 1, 3 }, { 2, 3 } },
  TestID -> "infraEdgeMultiset-closed-walk-wraps"
]


(* ===== the shape reader ===== *)

(* the three rows of the ontology, on a graph whose vertices are integers *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], w = PathGraph[ { 1, 2, 3 }, DirectedEdges -> True ] },
    { pointQ[ g, 5 ], multisetQ[ g, 5 ], walkQ[ g, 5 ],
      pointQ[ g, { 1, 2 } ], multisetQ[ g, { 1, 2 } ], walkQ[ g, { 1, 2 } ],
      pointQ[ g, <| 1 -> 2 |> ], multisetQ[ g, <| 1 -> 2 |> ], walkQ[ g, <| 1 -> 2 |> ],
      pointQ[ g, w ], multisetQ[ g, w ], walkQ[ g, w ] } ],
  { True, False, False,
    False, True, False,
    False, True, False,
    False, False, True },
  TestID -> "shape-reader-three-rows"
]

(* the substrate is not decoration: on a graph whose vertex labels are themselves
   lists, only the graph separates a point from a two-element multiset *)
VerificationTest[
  With[ { t = Graph[ { { 1, 1 }, { 2, 1 } }, { { 1, 1 } <-> { 2, 1 } } ],
          g = GridGraph[ { 3, 3 } ] },
    { pointQ[ t, { 1, 1 } ], multisetQ[ t, { 1, 1 } ],
      pointQ[ g, { 1, 1 } ], multisetQ[ g, { 1, 1 } ] } ],
  { True, False, False, True },
  TestID -> "shape-reader-is-graph-relative"
]

(* a vertex a graph does not have is neither a point nor -- being an atom -- a multiset *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { pointQ[ g, 99 ], multisetQ[ g, 99 ], toDensity[ g, 99 ] } ],
  { False, False, <| 99 -> 1 |> },
  TestID -> "shape-reader-off-substrate-vertex"
]

(* a walk anchor reads as its vertex occupation, so a DAG contributes the geodesic
   count at each vertex rather than a unit mass *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    With[ { dag = FindInfraSegment[ g, 1, 9, All ] },
      toDensity[ g, dag ] === KeySort @ infraVertexMultiset @ dag ] ],
  True,
  TestID -> "walk-anchor-reads-as-occupation"
]


(* ===== a class is a set of realisations, and the anchor rule deduplicates ===== *)

(* a repeated anchor is one anchor: the mass is read, the support spread over *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { FindInfraSegment[ g, { 1, 1 }, 9, All ] === FindInfraSegment[ g, 1, 9, All ],
      FindInfraSegment[ g, <| 1 -> 5 |>, 9, All ] === FindInfraSegment[ g, 1, 9, All ] } ],
  { True, True },
  TestID -> "anchor-repetition-collapses"
]

(* the multiset layer is a headless <| atom -> weight |> Association: repetition in
   a list reads as mass, and Keys is the step down to the support *)
VerificationTest[
  With[ { g = PathGraph @ Range[ 4 ] },
    { Counts[ { 1, 1, 2 } ],
      toDensity[ g, { 1, 1, 2 } ],
      Keys @ toDensity[ g, { 1, 1, 2 } ] } ],
  { <| 1 -> 2, 2 -> 1 |>,
    <| 1 -> 2, 2 -> 1 |>,
    { 1, 2 } },
  TestID -> "multiset-layer-is-a-headless-association"
]

(* the ANCHOR RULE: a vertex, a vertex list, a density and a walk graph all coerce
   to one 0-d density on bare vertices, so every construction reads its anchors
   through a single step *)
VerificationTest[
  With[ { g = PathGraph @ Range[ 4 ] },
    { toDensity[ g, 1 ],
      toDensity[ g, <| 1 -> 1, 2 -> 1 |> ], toDensity[ g, { 1, 1, 2 } ],
      toDensity[ g, <| 1 -> 3 |> ], toDensity[ g, PathGraph[ { 1, 2, 3 }, DirectedEdges -> True ] ] } ],
  { <| 1 -> 1 |>,
    <| 1 -> 1, 2 -> 1 |>,
    <| 1 -> 2, 2 -> 1 |>,
    <| 1 -> 3 |>,
    <| 1 -> 1, 2 -> 1, 3 -> 1 |> },
  TestID -> "anchor-rule-coerces-everything-to-a-density"
]

(* the density algebra is the Association's own -- Keys, Values, Total, and division
   for either normalisation.  No engine call is involved, which is the point *)
VerificationTest[
  With[ { p = <| 1 -> 3, 2 -> 1 |> },
    { Keys @ p, Values @ p, Total @ p, p / Total @ p, p / Max @ Values @ p } ],
  { { 1, 2 }, { 3, 1 }, 4, <| 1 -> 3/4, 2 -> 1/4 |>, <| 1 -> 1, 2 -> 1/3 |> },
  TestID -> "density-algebra-is-the-Association-s-own"
]

(* the measure is CONSTRUCTED at a projection off a bundle, never carried by it:
   the endpoints are a set-level fact (every geodesic of a family shares them, and
   they are the degree-0 vertices of the DAG), the midpoint a genuine density *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ], s = FindInfraSegment[ GridGraph[ { 3, 3 } ], 1, 9, All ] },
    { Pick[ VertexList @ s, VertexInDegree @ s, 0 ],
      Pick[ VertexList @ s, VertexOutDegree @ s, 0 ],
      FindInfraMidpoint[ g, s ] } ],
  { { 1 }, { 9 }, <| 3 -> 1, 5 -> 4, 7 -> 1 |> },
  TestID -> "Measure-constructed-at-projection"
]

(* anchor masses do NOT propagate: a construction sees a density's support,
   so the family (and its measure) is the same weighted or not *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    InfraDensity[ g, FindInfraSegment[ g, <| 1 -> 2, 3 -> 1 |>, 9, All ] ] ===
    InfraDensity[ g, FindInfraSegment[ g, <| 1 -> 1, 3 -> 1 |>, 9, All ] ] ],
  True,
  TestID -> "Anchor-masses-do-not-propagate"
]

(* ===== the DAG carries the family it stands for ===== *)

(* the compact DAG and the enumerated family carry the same density *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    With[ { dag = FindInfraSegment[ g, 1, 9, All ] },
      InfraDensity[ g, dag ] === InfraDensity[ g, geodesicGraph /@ infraSpread @ dag ] ] ],
  True,
  TestID -> "DAG-equals-enumerated-density"
]

(* a multi-source family is the plain union of the per-source families: its raw
   occupation is the sum *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    KeySort @ infraVertexMultiset @ FindInfraSegment[ g, <| 1 -> 1, 3 -> 1 |>, 9, All ] ===
    KeySort @ Merge[ { infraVertexMultiset @ FindInfraSegment[ g, 1, 9, All ],
                       infraVertexMultiset @ FindInfraSegment[ g, 3, 9, All ] }, Total ] ],
  True,
  TestID -> "Multi-source-family-is-the-union"
]

(* DAG-native midpoint equals the enumerated midpoint, both parities *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    { With[ { dag = FindInfraSegment[ g, 1, 16, All ] },
        KeySort @ FindInfraMidpoint[ g, dag ] ===
          KeySort @ FindInfraMidpoint[ g, geodesicGraph /@ infraSpread @ dag ] ],
      With[ { dag = FindInfraSegment[ g, 1, 12, All ] },
        KeySort @ FindInfraMidpoint[ g, dag ] ===
          KeySort @ FindInfraMidpoint[ g, geodesicGraph /@ infraSpread @ dag ] ] } ],
  { True, True },
  TestID -> "DAG-midpoint-equals-enumeration"
]

(* a bounded count is a prefix of the whole class, and a strict count fails on
   under-supply rather than returning fewer *)
VerificationTest[
  With[ { g = GridGraph[ { 4, 4 } ] },
    { Length @ FindInfraSegment[ g, 1, 16, UpTo[ 2 ] ],
      SubsetQ[ infraSpread @ FindInfraSegment[ g, 1, 16, All ],
               infraSpread @ FindInfraSegment[ g, 1, 16, UpTo[ 2 ] ] ],
      FindInfraSegment[ g, 1, 16, 1000 ] } ],
  { 2, True, $Failed },
  TestID -> "bounded-count-is-a-prefix"
]

(* the count contract on FindInfraSegment: count-less is ONE path graph, a bounded
   count a List of them, All the interval DAG *)
VerificationTest[
  With[ { g = GridGraph[ { 3, 3 } ] },
    { GraphQ @ FindInfraSegment[ g, 1, 9 ],
      MatchQ[ FindInfraSegment[ g, 1, 9, 1 ], { _Graph } ],
      MatchQ[ FindInfraSegment[ g, 1, 9, UpTo[ 100 ] ], { __Graph } ],
      GraphQ @ FindInfraSegment[ g, 1, 9, All ],
      Length @ infraSpread @ FindInfraSegment[ g, 1, 9, All ] === 6,
      FindInfraSegment[ g, 1, 9, 7 ] === $Failed } ],
  { True, True, True, True, True, True },
  TestID -> "FindInfraSegment-count-contract"
]

EndTestSection[]
