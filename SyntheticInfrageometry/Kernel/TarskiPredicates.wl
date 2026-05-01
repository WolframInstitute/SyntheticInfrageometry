Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== Messages ===================== *)

TarskiEuclidAxiomQ::nyi =
  "TarskiEuclidAxiomQ is not yet implemented; iteration 1 returns Indeterminate.";


(* ===================== Tarski primitives ===================== *)

(* Tarski's two primitive relations on a metric space, restated on graphs.
   B(u, w, v): w lies on a geodesic u-v, i.e. d(u, w) + d(w, v) = d(u, v).
   E(a, b, c, d): the unordered pairs are equidistant, d(a, b) = d(c, d).
   Together they generate first-order metric geometry on the graph. *)

BetweennessQ[ graph_Graph, u_, w_, v_ ] :=
  With[ { duw = GraphDistance[ graph, u, w ],
          dwv = GraphDistance[ graph, w, v ],
          duv = GraphDistance[ graph, u, v ] },
    duw =!= Infinity && dwv =!= Infinity && duv =!= Infinity && duw + dwv == duv
  ]

EquidistanceQ[ graph_Graph, a_, b_, c_, d_ ] :=
  GraphDistance[ graph, a, b ] === GraphDistance[ graph, c, d ]


(* ===================== TarskiStructure ===================== *)

(* The Tarski structure of a graph: a memoized Association bundling the
   hypermatrix substrate.  "Vertices" lists vertex names; "VertexIndex"
   maps name -> integer; "Distances" is the n x n distance matrix;
   "Betweenness" is the sparse rank-3 tensor B[i,j,k] = 1 iff
   D[i,k] = D[i,j] + D[j,k]; "Equidistance" partitions vertex pairs by
   distance value (each class is a list of unordered pairs); "Diameter"
   is the maximum finite distance. *)

TarskiStructure[ graph_Graph ] := TarskiStructure[ graph ] =
  Module[ { vs, n, dMat, finite, diam, bTensor, equidPart },
    vs = VertexList[ graph ];
    n = Length[ vs ];
    dMat = GraphDistanceMatrix[ graph ];
    finite = Cases[ Flatten @ dMat, _Integer ];
    diam = If[ finite === { }, 0, Max @ finite ];
    bTensor = SparseArray[
      Flatten[
        Table[
          If[ dMat[[ i, j ]] === Infinity ||
              dMat[[ j, k ]] === Infinity ||
              dMat[[ i, k ]] === Infinity, Nothing,
            If[ dMat[[ i, j ]] + dMat[[ j, k ]] == dMat[[ i, k ]],
              { i, j, k } -> 1, Nothing ]
          ],
          { i, n }, { j, n }, { k, n }
        ],
        2 ],
      { n, n, n }, 0
    ];
    equidPart = GatherBy[
      Subsets[ vs, { 2 } ],
      pair |-> GraphDistance[ graph, pair[[ 1 ]], pair[[ 2 ]] ]
    ];
    <|
      "Vertices" -> vs,
      "VertexIndex" -> AssociationThread[ vs, Range[ n ] ],
      "Distances" -> dMat,
      "Betweenness" -> bTensor,
      "Equidistance" -> equidPart,
      "Diameter" -> diam
    |>
  ]


(* TarskiBetweennessTensor projects out the rank-3 sparse tensor; the
   non-zero positions are exactly the (i, j, k) with B(v_i, v_j, v_k). *)

TarskiBetweennessTensor[ graph_Graph ] := TarskiStructure[ graph ][ "Betweenness" ]


(* TarskiEquidistanceClasses projects out the partition of unordered
   vertex pairs by their distance value. *)

TarskiEquidistanceClasses[ graph_Graph ] := TarskiStructure[ graph ][ "Equidistance" ]


(* ===================== Axiom predicates A1 - A11 ===================== *)

(* A1 (Reflexivity of Equidistance): forall a, b. ab == ba.
   Trivially true on undirected simple graphs - the distance is symmetric. *)

TarskiCongruenceReflexivityQ[ graph_Graph ] :=
  AllTrue[ Subsets[ VertexList[ graph ], { 2 } ],
    pair |-> EquidistanceQ[ graph,
      pair[[ 1 ]], pair[[ 2 ]], pair[[ 2 ]], pair[[ 1 ]] ]
  ]


(* A2 (Transitivity of Equidistance): ab == pq /\ ab == rs => pq == rs.
   A tautology of equality on distance values; we return True without
   the n^6 sweep. *)

TarskiCongruenceTransitivityQ[ _Graph ] := True


(* A3 (Identity of Equidistance): ab == cc => a == b.
   On a connected simple graph d(c, c) = 0, so the axiom reduces to
   "no two distinct vertices have distance 0", which holds. *)

TarskiCongruenceIdentityQ[ graph_Graph ] :=
  AllTrue[ Subsets[ VertexList[ graph ], { 2 } ],
    pair |-> GraphDistance[ graph, pair[[ 1 ]], pair[[ 2 ]] ] =!= 0
  ]


(* A4 (Segment Construction): forall a,b,c,d exists x. B(a,b,x) /\ bx == cd.
   Generally False on finite graphs - the substrate cannot freely extend
   segments to arbitrary lengths.  We test by asking
   FindTarskiSegmentExtension for at least one extension on every
   (a, b, c, d). *)

TarskiSegmentConstructionQ[ graph_Graph ] :=
  Module[ { vs = VertexList[ graph ] },
    AllTrue[ Tuples[ vs, 4 ],
      tuple |-> FindTarskiSegmentExtension[ graph,
        tuple[[ 1 ]], tuple[[ 2 ]], tuple[[ 3 ]], tuple[[ 4 ]], UpTo[ 1 ] ] =!= { }
    ]
  ]


(* A5 (Five Segments): if a != b, B(a,b,c), B(a',b',c') and the four
   congruences ab == a'b', bc == b'c', ad == a'd', bd == b'd' hold, then
   cd == c'd'.  Brute O(n^8); a moderate iteration cap keeps small-graph
   tests responsive.  Indeterminate if the cap is hit before exhaustion. *)

Options[ TarskiFiveSegmentsQ ] = { "MaxTuples" -> 200000 };

TarskiFiveSegmentsQ[ graph_Graph, OptionsPattern[] ] :=
  Module[ {
    vs = VertexList[ graph ],
    cap = OptionValue[ "MaxTuples" ],
    count = 0, ok = True, capHit = False
  },
    Catch[
      Do[
        count++;
        If[ count > cap, capHit = True; Throw[ Null ] ];
        With[ {
          a = eight[[ 1 ]], b = eight[[ 2 ]], c = eight[[ 3 ]], d = eight[[ 4 ]],
          ap = eight[[ 5 ]], bp = eight[[ 6 ]], cp = eight[[ 7 ]], dp = eight[[ 8 ]]
        },
          If[ a =!= b &&
              GraphDistance[ graph, a, b ] === GraphDistance[ graph, ap, bp ] &&
              GraphDistance[ graph, b, c ] === GraphDistance[ graph, bp, cp ] &&
              GraphDistance[ graph, a, d ] === GraphDistance[ graph, ap, dp ] &&
              GraphDistance[ graph, b, d ] === GraphDistance[ graph, bp, dp ] &&
              BetweennessQ[ graph, a, b, c ] && BetweennessQ[ graph, ap, bp, cp ] &&
              GraphDistance[ graph, c, d ] =!= GraphDistance[ graph, cp, dp ],
            ok = False; Throw[ Null ]
          ]
        ],
        { eight, Tuples[ vs, 8 ] }
      ]
    ];
    Which[ ! ok, False, capHit, Indeterminate, True, True ]
  ]


(* A6 (Identity of Betweenness): B(a, b, a) => a == b.
   On a connected simple graph B(a, b, a) requires d(a, a) = 2 d(a, b) = 0,
   forcing a = b.  Always True. *)

TarskiBetweennessIdentityQ[ graph_Graph ] :=
  Module[ { vs = VertexList[ graph ] },
    AllTrue[ Tuples[ vs, 2 ],
      pair |-> pair[[ 1 ]] === pair[[ 2 ]] ||
        ! BetweennessQ[ graph, pair[[ 1 ]], pair[[ 2 ]], pair[[ 1 ]] ]
    ]
  ]


(* A7 (Inner Pasch): if B(a, p, c) and B(b, q, c), then there is a vertex
   x with B(p, x, b) and B(q, x, a).  Equivalently x lies in
   I(p, b) intersect I(q, a).  Holds on median graphs (trees, hypercubes);
   fails on long cycles and on the Petersen graph. *)

TarskiInnerPaschQ[ graph_Graph ] :=
  Module[ { vs = VertexList[ graph ] },
    AllTrue[ Tuples[ vs, 5 ],
      tuple |-> With[ {
        a = tuple[[ 1 ]], b = tuple[[ 2 ]], c = tuple[[ 3 ]],
        p = tuple[[ 4 ]], q = tuple[[ 5 ]]
      },
        ! ( BetweennessQ[ graph, a, p, c ] && BetweennessQ[ graph, b, q, c ] ) ||
        Intersection[
          MetricInterval[ graph, p, b ], MetricInterval[ graph, q, a ]
        ] =!= { }
      ]
    ]
  ]


(* A8 (Lower Dimension): there exist three non-collinear points.  On
   graphs we use the projective CollinearQ (maximal-geodesic version);
   the axiom holds whenever the graph is not "thin" (path-like or
   complete) - i.e. some triple is not contained in a single canonical
   line. *)

TarskiLowerDimensionQ[ graph_Graph ] :=
  AnyTrue[ Subsets[ VertexList[ graph ], { 3 } ],
    triple |-> ! CollinearQ[ graph, triple ]
  ]


(* A9 (Upper Dimension): if three points are equidistant from two
   distinct points, they are collinear.  Equivalently the perpendicular
   bisector of any pair of distinct vertices is "1-dimensional" (its
   point set is collinear).  False on hypercubes and grids of effective
   dimension >= 3, True on path / cycle / planar substrates. *)

TarskiUpperDimensionQ[ graph_Graph ] :=
  Module[ { vs = VertexList[ graph ] },
    AllTrue[ Tuples[ vs, 5 ],
      tuple |-> With[ {
        p = tuple[[ 1 ]], q = tuple[[ 2 ]],
        a = tuple[[ 3 ]], b = tuple[[ 4 ]], c = tuple[[ 5 ]]
      },
        p === q ||
        ! ( EquidistanceQ[ graph, a, p, a, q ] &&
            EquidistanceQ[ graph, b, p, b, q ] &&
            EquidistanceQ[ graph, c, p, c, q ] ) ||
        CollinearQ[ graph, { a, b, c } ]
      ]
    ]
  ]


(* A10 (Euclid's parallel-axiom variant): iteration-1 stub.  A real
   implementation requires a Tarski-only perpendicular-foot construction
   (Gupta), deferred to a later iteration. *)

TarskiEuclidAxiomQ[ _Graph ] := (
  Message[ TarskiEuclidAxiomQ::nyi ];
  Indeterminate
)


(* A11 (Continuity): the first-order Dedekind schema cannot be satisfied
   by a finite discrete graph.  The failure has no single vertex-tuple
   witness; it is structural. *)

TarskiContinuityQ[ _Graph ] := False


(* ===================== TarskiAxiomQ dashboard ===================== *)

(* TarskiAxiomQ[g] is purely a convenience: it composes the eleven
   per-axiom predicates into an Association keyed by descriptive
   strings.  Pass any individual Tarski*Q symbol to FindTarskiCounterexample
   to obtain witnesses. *)

TarskiAxiomQ[ graph_Graph ] :=
  <|
    "EquidistanceReflexivity" -> TarskiCongruenceReflexivityQ[ graph ],
    "EquidistanceTransitivity" -> TarskiCongruenceTransitivityQ[ graph ],
    "EquidistanceIdentity" -> TarskiCongruenceIdentityQ[ graph ],
    "SegmentConstruction" -> TarskiSegmentConstructionQ[ graph ],
    "FiveSegments" -> TarskiFiveSegmentsQ[ graph ],
    "BetweennessIdentity" -> TarskiBetweennessIdentityQ[ graph ],
    "InnerPasch" -> TarskiInnerPaschQ[ graph ],
    "LowerDimension" -> TarskiLowerDimensionQ[ graph ],
    "UpperDimension" -> TarskiUpperDimensionQ[ graph ],
    "Euclid" -> Quiet @ TarskiEuclidAxiomQ[ graph ],
    "Continuity" -> TarskiContinuityQ[ graph ]
  |>
