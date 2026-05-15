Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[tarskiCongruenceIdentityCounter]
PackageScope[tarskiSegmentConstructionCounter]
PackageScope[tarskiFiveSegmentsCounter]
PackageScope[tarskiBetweennessIdentityCounter]
PackageScope[tarskiInnerPaschCounter]
PackageScope[tarskiLowerDimensionCounter]
PackageScope[tarskiUpperDimensionCounter]


(* ===================== BetweennessQ / EquidistanceQ ===================== *)

(* Tarski's two primitive relations restated on graphs.
   B(u, w, v): w lies on a u-v geodesic, i.e. d(u, w) + d(w, v) = d(u, v).
   E(a, b, c, d): the pairs are equidistant, d(a, b) = d(c, d). *)

BetweennessQ[ graph_Graph, u_, w_, v_ ] :=
  With[ { duw = GraphDistance[ graph, u, w ],
          dwv = GraphDistance[ graph, w, v ],
          duv = GraphDistance[ graph, u, v ] },
    duw =!= Infinity && dwv =!= Infinity && duv =!= Infinity && duw + dwv == duv
  ]

EquidistanceQ[ graph_Graph, a_, b_, c_, d_ ] :=
  GraphDistance[ graph, a, b ] === GraphDistance[ graph, c, d ]


(* ===================== TarskiStructure ===================== *)

(* Memoized Association bundling the hypermatrix substrate: vertex list,
   vertex-to-index map, distance matrix, betweenness rank-3 sparse tensor,
   equidistance partition of unordered pairs, diameter. *)

TarskiStructure[ graph_Graph ] := TarskiStructure[ graph ] =
  With[ { vs = VertexList[ graph ], dMat = GraphDistanceMatrix[ graph ] },
    With[ { n = Length[ vs ], finite = Cases[ Flatten @ dMat, _Integer ] },
      <|
        "Vertices"     -> vs,
        "VertexIndex"  -> AssociationThread[ vs, Range[ n ] ],
        "Distances"    -> dMat,
        "Betweenness"  -> SparseArray[
          Flatten[
            Table[
              If[ dMat[[ i, j ]] === Infinity ||
                  dMat[[ j, k ]] === Infinity ||
                  dMat[[ i, k ]] === Infinity, Nothing,
                If[ dMat[[ i, j ]] + dMat[[ j, k ]] == dMat[[ i, k ]],
                  { i, j, k } -> 1, Nothing ] ],
              { i, n }, { j, n }, { k, n } ],
            2 ],
          { n, n, n }, 0 ],
        "Equidistance" -> GatherBy[
          Subsets[ vs, { 2 } ],
          pair |-> GraphDistance[ graph, pair[[ 1 ]], pair[[ 2 ]] ] ],
        "Diameter"     -> If[ finite === { }, 0, Max @ finite ]
      |>
    ]
  ]


TarskiBetweennessTensor[ graph_Graph ] := TarskiStructure[ graph ][ "Betweenness" ]

TarskiEquidistanceClasses[ graph_Graph ] := TarskiStructure[ graph ][ "Equidistance" ]


(* ===================== Axiom predicates A1 - A11 ===================== *)

(* A1 (Reflexivity of Equidistance): forall a, b. ab == ba. *)

TarskiCongruenceReflexivityQ[ graph_Graph ] :=
  AllTrue[ Subsets[ VertexList[ graph ], { 2 } ],
    pair |-> EquidistanceQ[ graph, pair[[ 1 ]], pair[[ 2 ]], pair[[ 2 ]], pair[[ 1 ]] ] ]


(* A2 (Transitivity of Equidistance): tautology of equality. *)

TarskiCongruenceTransitivityQ[ _Graph ] := True


(* A3 (Identity of Equidistance): ab == cc  =>  a == b. *)

TarskiCongruenceIdentityQ[ graph_Graph ] :=
  AllTrue[ Subsets[ VertexList[ graph ], { 2 } ],
    pair |-> GraphDistance[ graph, pair[[ 1 ]], pair[[ 2 ]] ] =!= 0 ]


(* A4 (Segment Construction): forall a, b, c, d.  exists x. B(a, b, x) and bx == cd.
   Generally False on finite graphs; the 5-vertex ExtendInfraSegment form is the Find variant. *)

TarskiSegmentConstructionQ[ graph_Graph ] :=
  AllTrue[ Tuples[ VertexList[ graph ], 4 ],
    tuple |-> Length @ ExtendInfraSegment[ graph,
      tuple[[ 1 ]], tuple[[ 2 ]], tuple[[ 3 ]], tuple[[ 4 ]], UpTo[ 1 ] ] > 0 ]


(* A5 (Five Segments).  Brute O(n^8); "MaxTuples" cap keeps small-graph tests
   responsive.  Indeterminate if the cap is hit before exhaustion. *)

Options[ TarskiFiveSegmentsQ ] = { "MaxTuples" -> 200000 };

TarskiFiveSegmentsQ[ graph_Graph, OptionsPattern[] ] :=
  Module[ { count = 0, ok = True, capHit = False,
            vs = VertexList[ graph ], cap = OptionValue[ "MaxTuples" ] },
    Catch[
      Do[
        count++;
        If[ count > cap, capHit = True; Throw[ Null ] ];
        With[ {
          a = eight[[ 1 ]], b = eight[[ 2 ]], c = eight[[ 3 ]], d = eight[[ 4 ]],
          ap = eight[[ 5 ]], bp = eight[[ 6 ]], cp = eight[[ 7 ]], dp = eight[[ 8 ]] },
          If[ a =!= b &&
              GraphDistance[ graph, a, b ] === GraphDistance[ graph, ap, bp ] &&
              GraphDistance[ graph, b, c ] === GraphDistance[ graph, bp, cp ] &&
              GraphDistance[ graph, a, d ] === GraphDistance[ graph, ap, dp ] &&
              GraphDistance[ graph, b, d ] === GraphDistance[ graph, bp, dp ] &&
              BetweennessQ[ graph, a, b, c ] && BetweennessQ[ graph, ap, bp, cp ] &&
              GraphDistance[ graph, c, d ] =!= GraphDistance[ graph, cp, dp ],
            ok = False; Throw[ Null ] ]
        ],
        { eight, Tuples[ vs, 8 ] } ]
    ];
    Which[ ! ok, False, capHit, Indeterminate, True, True ]
  ]


(* A6 (Identity of Betweenness): B(a, b, a) => a == b. *)

TarskiBetweennessIdentityQ[ graph_Graph ] :=
  AllTrue[ Tuples[ VertexList[ graph ], 2 ],
    pair |-> pair[[ 1 ]] === pair[[ 2 ]] ||
      ! BetweennessQ[ graph, pair[[ 1 ]], pair[[ 2 ]], pair[[ 1 ]] ] ]


(* A7 (Inner Pasch): B(a, p, c) and B(b, q, c)  =>  exists x. B(p, x, b) and B(q, x, a),
   equivalently I(p, b) intersect I(q, a) is non-empty. *)

TarskiInnerPaschQ[ graph_Graph ] :=
  AllTrue[ Tuples[ VertexList[ graph ], 5 ],
    tuple |-> With[ {
        a = tuple[[ 1 ]], b = tuple[[ 2 ]], c = tuple[[ 3 ]],
        p = tuple[[ 4 ]], q = tuple[[ 5 ]] },
      ! ( BetweennessQ[ graph, a, p, c ] && BetweennessQ[ graph, b, q, c ] ) ||
      Intersection[ MetricInterval[ graph, p, b ], MetricInterval[ graph, q, a ] ] =!= { }
    ] ]


(* A8 (Lower Dimension): there exist three non-collinear points. *)

TarskiLowerDimensionQ[ graph_Graph ] :=
  AnyTrue[ Subsets[ VertexList[ graph ], { 3 } ],
    triple |-> ! CollinearQ[ graph, triple ] ]


(* A9 (Upper Dimension): three points equidistant from two distinct points are collinear. *)

TarskiUpperDimensionQ[ graph_Graph ] :=
  AllTrue[ Tuples[ VertexList[ graph ], 5 ],
    tuple |-> With[ {
        p = tuple[[ 1 ]], q = tuple[[ 2 ]],
        a = tuple[[ 3 ]], b = tuple[[ 4 ]], c = tuple[[ 5 ]] },
      p === q ||
      ! ( EquidistanceQ[ graph, a, p, a, q ] &&
          EquidistanceQ[ graph, b, p, b, q ] &&
          EquidistanceQ[ graph, c, p, c, q ] ) ||
      CollinearQ[ graph, { a, b, c } ]
    ] ]


(* A10 (Euclid's parallel-axiom variant): iteration-1 stub returning Indeterminate. *)

TarskiEuclidAxiomQ[ _Graph ] := Indeterminate


(* A11 (Continuity): the first-order Dedekind schema cannot be satisfied
   by a finite discrete graph. *)

TarskiContinuityQ[ _Graph ] := False


(* ===================== TarskiAxiomQ dashboard ===================== *)

(* Compose the eleven axiom predicates into a keyed Association.  Pass any
   individual Tarski*Q symbol to FindTarskiCounterexample for witnesses. *)

TarskiAxiomQ[ graph_Graph ] :=
  <|
    "EquidistanceReflexivity"  -> TarskiCongruenceReflexivityQ[ graph ],
    "EquidistanceTransitivity" -> TarskiCongruenceTransitivityQ[ graph ],
    "EquidistanceIdentity"     -> TarskiCongruenceIdentityQ[ graph ],
    "SegmentConstruction"      -> TarskiSegmentConstructionQ[ graph ],
    "FiveSegments"             -> TarskiFiveSegmentsQ[ graph ],
    "BetweennessIdentity"      -> TarskiBetweennessIdentityQ[ graph ],
    "InnerPasch"               -> TarskiInnerPaschQ[ graph ],
    "LowerDimension"           -> TarskiLowerDimensionQ[ graph ],
    "UpperDimension"           -> TarskiUpperDimensionQ[ graph ],
    "Euclid"                   -> TarskiEuclidAxiomQ[ graph ],
    "Continuity"               -> TarskiContinuityQ[ graph ]
  |>


(* ===================== FindTarskiCounterexample ===================== *)

(* Witnesses for the failure of a Tarski axiom predicate.  Universal axioms
   yield the offending tuple; A4 yields (a, b, c, d) for which no extension
   exists; A11 has no finite witness and returns $Failed. *)

FindTarskiCounterexample[ graph_Graph, predQ_Symbol, All ] :=
  Switch[ predQ,
    TarskiCongruenceReflexivityQ,   { },
    TarskiCongruenceTransitivityQ,  { },
    TarskiCongruenceIdentityQ,      tarskiCongruenceIdentityCounter[ graph ],
    TarskiSegmentConstructionQ,     tarskiSegmentConstructionCounter[ graph ],
    TarskiFiveSegmentsQ,            tarskiFiveSegmentsCounter[ graph ],
    TarskiBetweennessIdentityQ,     tarskiBetweennessIdentityCounter[ graph ],
    TarskiInnerPaschQ,              tarskiInnerPaschCounter[ graph ],
    TarskiLowerDimensionQ,          tarskiLowerDimensionCounter[ graph ],
    TarskiUpperDimensionQ,          tarskiUpperDimensionCounter[ graph ],
    TarskiEuclidAxiomQ,             { },
    TarskiContinuityQ,              $Failed
  ]

FindTarskiCounterexample[ graph_Graph, predQ_Symbol, UpTo[ n_Integer ] ] :=
  With[ { result = FindTarskiCounterexample[ graph, predQ, All ] },
    If[ ListQ[ result ], Take[ result, UpTo[ n ] ], result ]
  ]

FindTarskiCounterexample[ graph_Graph, predQ_Symbol, n_Integer : 1 ] :=
  With[ { result = FindTarskiCounterexample[ graph, predQ, UpTo[ n ] ] },
    Which[ ! ListQ[ result ], result, Length[ result ] < n, $Failed, True, result ]
  ]


(* ===================== Helpers: per-axiom counterexample searchers ===================== *)

tarskiCongruenceIdentityCounter[ graph_Graph ] :=
  Cases[ Subsets[ VertexList[ graph ], { 2 } ],
    pair_ /; GraphDistance[ graph, pair[[ 1 ]], pair[[ 2 ]] ] === 0 :> pair ]


tarskiBetweennessIdentityCounter[ graph_Graph ] :=
  Cases[ Tuples[ VertexList[ graph ], 2 ],
    pair_ /;
      pair[[ 1 ]] =!= pair[[ 2 ]] &&
      BetweennessQ[ graph, pair[[ 1 ]], pair[[ 2 ]], pair[[ 1 ]] ] :> pair ]


tarskiSegmentConstructionCounter[ graph_Graph ] :=
  Select[ Tuples[ VertexList[ graph ], 4 ],
    tuple |-> Length @ ExtendInfraSegment[ graph,
      tuple[[ 1 ]], tuple[[ 2 ]], tuple[[ 3 ]], tuple[[ 4 ]], UpTo[ 1 ] ] === 0 ]


tarskiFiveSegmentsCounter[ graph_Graph ] :=
  Module[ { found = { }, count = 0,
            vs = VertexList[ graph ], cap = 200000 },
    Catch[
      Do[
        With[ {
          a = eight[[ 1 ]], b = eight[[ 2 ]], c = eight[[ 3 ]], d = eight[[ 4 ]],
          ap = eight[[ 5 ]], bp = eight[[ 6 ]], cp = eight[[ 7 ]], dp = eight[[ 8 ]] },
          count++;
          If[ count > cap, Throw[ Null ] ];
          If[ a =!= b &&
              GraphDistance[ graph, a, b ] === GraphDistance[ graph, ap, bp ] &&
              GraphDistance[ graph, b, c ] === GraphDistance[ graph, bp, cp ] &&
              GraphDistance[ graph, a, d ] === GraphDistance[ graph, ap, dp ] &&
              GraphDistance[ graph, b, d ] === GraphDistance[ graph, bp, dp ] &&
              BetweennessQ[ graph, a, b, c ] && BetweennessQ[ graph, ap, bp, cp ] &&
              GraphDistance[ graph, c, d ] =!= GraphDistance[ graph, cp, dp ],
            AppendTo[ found, eight ] ]
        ],
        { eight, Tuples[ vs, 8 ] } ]
    ];
    found
  ]


tarskiInnerPaschCounter[ graph_Graph ] :=
  Select[ Tuples[ VertexList[ graph ], 5 ],
    tuple |-> With[ {
        a = tuple[[ 1 ]], b = tuple[[ 2 ]], c = tuple[[ 3 ]],
        p = tuple[[ 4 ]], q = tuple[[ 5 ]] },
      BetweennessQ[ graph, a, p, c ] && BetweennessQ[ graph, b, q, c ] &&
      Intersection[ MetricInterval[ graph, p, b ], MetricInterval[ graph, q, a ] ] === { }
    ] ]


tarskiLowerDimensionCounter[ graph_Graph ] :=
  If[ AnyTrue[ Subsets[ VertexList[ graph ], { 3 } ],
        triple |-> ! CollinearQ[ graph, triple ] ],
    { },
    { { } } ]


tarskiUpperDimensionCounter[ graph_Graph ] :=
  Select[ Tuples[ VertexList[ graph ], 5 ],
    tuple |-> With[ {
        p = tuple[[ 1 ]], q = tuple[[ 2 ]],
        a = tuple[[ 3 ]], b = tuple[[ 4 ]], c = tuple[[ 5 ]] },
      p =!= q &&
      EquidistanceQ[ graph, a, p, a, q ] &&
      EquidistanceQ[ graph, b, p, b, q ] &&
      EquidistanceQ[ graph, c, p, c, q ] &&
      ! CollinearQ[ graph, { a, b, c } ]
    ] ]
