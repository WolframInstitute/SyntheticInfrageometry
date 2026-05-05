Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== FindTarskiSegmentExtension ===================== *)

(* Tarski's segment-construction axiom A4 in finder form: given vertices
   a, b, c, d, return a vertex x with B(a, b, x) and bx == cd - i.e. a
   point on the geodesic continuation of a-b that lies at distance |cd|
   from b.  Generally multi-valued (if it exists at all - the axiom
   fails on most graphs because no such x is present in the substrate). *)

FindTarskiSegmentExtension[ graph_Graph, a_, b_, c_, d_, All ] :=
  With[ { target = GraphDistance[ graph, c, d ] },
    If[ target === Infinity, InfraPoint[ {} ],
      InfraPoint @ Select[ VertexList[ graph ],
        x |-> BetweennessQ[ graph, a, b, x ] && GraphDistance[ graph, b, x ] === target
      ]
    ]
  ]

FindTarskiSegmentExtension[ graph_Graph, a_, b_, c_, d_, UpTo[ n_Integer ] ] :=
  With[ { result = FindTarskiSegmentExtension[ graph, a, b, c, d, All ] },
    InfraPoint @ Take[ result[ "Realisations" ], UpTo[ n ] ]
  ]

FindTarskiSegmentExtension[ graph_Graph, a_, b_, c_, d_, n_Integer : 1 ] :=
  With[ { result = FindTarskiSegmentExtension[ graph, a, b, c, d, UpTo[ n ] ] },
    If[ result[ "Length" ] < n, $Failed, result ]
  ]


(* ===================== FindTarskiCounterexample ===================== *)

(* For a Tarski axiom predicate Tarski*Q passed as a head, return tuples
   of vertices that witness the axiom's failure on graph.  Universal
   axioms (forall ...) yield the offending tuple; the segment-construction
   axiom A4 yields (a, b, c, d) for which no extension exists.  The
   continuity axiom A11 has no finite witness; its counterexample finder
   issues ::nowitness and returns $Failed. *)

FindTarskiCounterexample::nowitness =
  "The continuity axiom has no finite vertex-tuple witness on a graph.";

FindTarskiCounterexample[ graph_Graph, predQ_Symbol, All ] :=
  Switch[ predQ,
    TarskiCongruenceReflexivityQ, { },
    TarskiCongruenceTransitivityQ, { },
    TarskiCongruenceIdentityQ, tarskiCongruenceIdentityCounter[ graph ],
    TarskiSegmentConstructionQ, tarskiSegmentConstructionCounter[ graph ],
    TarskiFiveSegmentsQ, tarskiFiveSegmentsCounter[ graph ],
    TarskiBetweennessIdentityQ, tarskiBetweennessIdentityCounter[ graph ],
    TarskiInnerPaschQ, tarskiInnerPaschCounter[ graph ],
    TarskiLowerDimensionQ, tarskiLowerDimensionCounter[ graph ],
    TarskiUpperDimensionQ, tarskiUpperDimensionCounter[ graph ],
    TarskiEuclidAxiomQ, { },
    TarskiContinuityQ,
      Message[ FindTarskiCounterexample::nowitness ];
      $Failed,
    _, $Failed
  ]

FindTarskiCounterexample[ graph_Graph, predQ_Symbol, UpTo[ n_Integer ] ] :=
  With[ { result = FindTarskiCounterexample[ graph, predQ, All ] },
    If[ ListQ[ result ], Take[ result, UpTo[ n ] ], result ]
  ]

FindTarskiCounterexample[ graph_Graph, predQ_Symbol, n_Integer : 1 ] :=
  With[ { result = FindTarskiCounterexample[ graph, predQ, UpTo[ n ] ] },
    Which[ ! ListQ[ result ], result, Length[ result ] < n, $Failed, True, result ]
  ]


(* ===================== Per-axiom counterexample searchers (PackageScope) ===================== *)

(* Return a List of vertex tuples that witness the failure of the named
   axiom on graph, or { } if the axiom holds.  Each searcher exhaustively
   enumerates the relevant tuple shape; the calling triple in
   FindTarskiCounterexample then trims via Take[#, UpTo[n]]. *)

PackageScope[tarskiCongruenceIdentityCounter]
PackageScope[tarskiSegmentConstructionCounter]
PackageScope[tarskiFiveSegmentsCounter]
PackageScope[tarskiBetweennessIdentityCounter]
PackageScope[tarskiInnerPaschCounter]
PackageScope[tarskiLowerDimensionCounter]
PackageScope[tarskiUpperDimensionCounter]


tarskiCongruenceIdentityCounter[ graph_Graph ] :=
  Cases[ Subsets[ VertexList[ graph ], { 2 } ],
    pair_ /; GraphDistance[ graph, pair[[ 1 ]], pair[[ 2 ]] ] === 0 :> pair
  ]


tarskiBetweennessIdentityCounter[ graph_Graph ] :=
  Module[ { vs = VertexList[ graph ] },
    Cases[ Tuples[ vs, 2 ],
      pair_ /;
        pair[[ 1 ]] =!= pair[[ 2 ]] &&
        BetweennessQ[ graph, pair[[ 1 ]], pair[[ 2 ]], pair[[ 1 ]] ] :> pair
    ]
  ]


(* A4: tuples (a, b, c, d) for which no extension x exists. *)

tarskiSegmentConstructionCounter[ graph_Graph ] :=
  Module[ { vs = VertexList[ graph ] },
    Select[ Tuples[ vs, 4 ],
      tuple |-> FindTarskiSegmentExtension[ graph,
        tuple[[ 1 ]], tuple[[ 2 ]], tuple[[ 3 ]], tuple[[ 4 ]], UpTo[ 1 ] ][ "Length" ] === 0
    ]
  ]


(* A5: 8-tuples (a,b,c,d,a',b',c',d') satisfying the four congruences and
   B(a,b,c), B(a',b',c'), with a != b, but cd != c'd'.  Brute O(n^8); use
   a moderate cap to keep tests responsive. *)

tarskiFiveSegmentsCounter[ graph_Graph ] :=
  Module[ { vs = VertexList[ graph ], cap = 200000, found = { }, count = 0, abort = False },
    Catch[
      Do[
        With[ {
          a = eight[[ 1 ]], b = eight[[ 2 ]], c = eight[[ 3 ]], d = eight[[ 4 ]],
          ap = eight[[ 5 ]], bp = eight[[ 6 ]], cp = eight[[ 7 ]], dp = eight[[ 8 ]]
        },
          count++;
          If[ count > cap, Throw[ Null ] ];
          If[ a =!= b &&
              GraphDistance[ graph, a, b ] === GraphDistance[ graph, ap, bp ] &&
              GraphDistance[ graph, b, c ] === GraphDistance[ graph, bp, cp ] &&
              GraphDistance[ graph, a, d ] === GraphDistance[ graph, ap, dp ] &&
              GraphDistance[ graph, b, d ] === GraphDistance[ graph, bp, dp ] &&
              BetweennessQ[ graph, a, b, c ] && BetweennessQ[ graph, ap, bp, cp ] &&
              GraphDistance[ graph, c, d ] =!= GraphDistance[ graph, cp, dp ],
            AppendTo[ found, eight ]
          ]
        ],
        { eight, Tuples[ vs, 8 ] }
      ]
    ];
    found
  ]


(* A7: 5-tuples (a, b, c, p, q) with B(a, p, c), B(b, q, c), but
   I(p, b) intersect I(q, a) is empty. *)

tarskiInnerPaschCounter[ graph_Graph ] :=
  Module[ { vs = VertexList[ graph ] },
    Select[ Tuples[ vs, 5 ],
      tuple |-> With[ {
        a = tuple[[ 1 ]], b = tuple[[ 2 ]], c = tuple[[ 3 ]],
        p = tuple[[ 4 ]], q = tuple[[ 5 ]]
      },
        BetweennessQ[ graph, a, p, c ] && BetweennessQ[ graph, b, q, c ] &&
        Intersection[
          MetricInterval[ graph, p, b ], MetricInterval[ graph, q, a ]
        ] === { }
      ]
    ]
  ]


(* A8: a "counterexample" is a graph in which all triples are collinear -
   we report the empty list when one exists, else { } meaning "axiom holds".
   FindTarskiCounterexample[g, TarskiLowerDimensionQ, _] returns the empty
   tuple { } as a witness of dimensional collapse - distinct from { } for
   "no counterexample found". *)

tarskiLowerDimensionCounter[ graph_Graph ] :=
  If[ AnyTrue[ Subsets[ VertexList[ graph ], { 3 } ],
        triple |-> ! CollinearQ[ graph, triple ] ],
    { },
    { { } }
  ]


(* A9: 5-tuples (p, q, a, b, c) with p != q, the three points a, b, c
   each equidistant from p and from q, but a, b, c not collinear. *)

tarskiUpperDimensionCounter[ graph_Graph ] :=
  Module[ { vs = VertexList[ graph ] },
    Select[ Tuples[ vs, 5 ],
      tuple |-> With[ {
        p = tuple[[ 1 ]], q = tuple[[ 2 ]],
        a = tuple[[ 3 ]], b = tuple[[ 4 ]], c = tuple[[ 5 ]]
      },
        p =!= q &&
        EquidistanceQ[ graph, a, p, a, q ] &&
        EquidistanceQ[ graph, b, p, b, q ] &&
        EquidistanceQ[ graph, c, p, c, q ] &&
        ! CollinearQ[ graph, { a, b, c } ]
      ]
    ]
  ]
