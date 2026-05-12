Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findHomotopyCore]
PackageScope[resolveFaces]
PackageScope[faceMoves]
PackageScope[applyMove]
PackageScope[homotopyBFS]
PackageScope[chordlessCycleQ]


(* ===================== InfraHomotopy wrapper ===================== *)

(* Each realisation is the chain {p_0, p_1, ..., p_k} of intermediate walks
   produced by k elementary face moves from p_0 to p_k. *)

InfraHomotopy[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraHomotopy[ _List ] ] ] :=
  InfraHomotopy[ Flatten[ reps /. InfraHomotopy[ xs_List ] :> xs, 1 ] ]


(* ===================== FindHomotopy ===================== *)

(* A homotopy from p1 to p2: a finite sequence of elementary face moves, each
   replacing a contiguous sub-walk by an arc-equivalent walk from a face cycle.
   Returns InfraHomotopy wrappers holding the intermediate walks {p_0, ..., p_k}. *)

Options[ FindHomotopy ] = {
  "Faces"     -> Automatic,
  "MaxLength" -> Automatic,
  "MaxMoves"  -> Automatic,
  Method      -> "BFS"
};

FindHomotopy[ graph_Graph, p1_, p2_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ ] ] :=
  infraSpreadAndCartesian[ InfraHomotopy, count,
    findHomotopyCore[ graph, ##, opts ] &, p1, p2 ]


findHomotopyCore[ graph_Graph, p1_List, p2_List, opts : OptionsPattern[ FindHomotopy ] ] :=
  With[ { faces = resolveFaces[ graph, OptionValue[ FindHomotopy, { opts }, "Faces" ] ] },
    With[ { maxLen = Replace[ OptionValue[ FindHomotopy, { opts }, "MaxLength" ],
              Automatic :> Max[ Length /@ { p1, p2 } ] +
                Max[ If[ faces === { }, { 0 }, Length /@ faces ] ] + 2 ],
            maxMoves = Replace[ OptionValue[ FindHomotopy, { opts }, "MaxMoves" ],
              Automatic :> Infinity ] },
      Switch[ OptionValue[ FindHomotopy, { opts }, Method ],
        "BFS", homotopyBFS[ graph, p1, p2, faces, maxLen, maxMoves ]
      ]
    ]
  ]


(* ===================== HomotopicQ ===================== *)

(* True iff a face-move chain takes p1 to p2.  Multi-realisations spread via
   Cartesian product: every (p1_i, p2_j) pair must be homotopic. *)

Options[ HomotopicQ ] = Options[ FindHomotopy ];

HomotopicQ[ graph_Graph, p1_List, p2_List, opts : OptionsPattern[ ] ] :=
  findHomotopyCore[ graph, p1, p2, opts ] =!= { }

HomotopicQ[ graph_Graph, p1_, p2_, opts : OptionsPattern[ ] ] :=
  AllTrue[ Tuples[ infraSpread /@ { p1, p2 } ],
    pair |-> HomotopicQ[ graph, pair[[ 1 ]], pair[[ 2 ]], opts ] ]


(* ===================== NullHomotopicQ / FindNullHomotopy ===================== *)

(* A closed walk c = (v_0, ..., v_k, v_0) is null-homotopic iff it admits a
   face-move chain to the constant walk (v_0).  Open input (v_0, ..., v_k)
   with v_0 =!= v_k is auto-closed by appending v_0. *)

Options[ NullHomotopicQ ]   = Options[ FindHomotopy ];
Options[ FindNullHomotopy ] = Options[ FindHomotopy ];

NullHomotopicQ[ graph_Graph, cycle_List, opts : OptionsPattern[ ] ] :=
  With[ { closed = closeWalk[ cycle ] },
    HomotopicQ[ graph, closed, { First[ closed ] }, opts ]
  ]

NullHomotopicQ[ graph_Graph, InfraCircle[ reps_List ], opts : OptionsPattern[ ] ] :=
  AllTrue[ reps, NullHomotopicQ[ graph, #, opts ] & ]


FindNullHomotopy[ graph_Graph, cycle_List,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ ] ] :=
  With[ { closed = closeWalk[ cycle ] },
    FindHomotopy[ graph, closed, { First[ closed ] }, count, opts ]
  ]

FindNullHomotopy[ graph_Graph, InfraCircle[ reps_List ],
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1, opts : OptionsPattern[ ] ] :=
  With[ { result = FindNullHomotopy[ graph, #, count, opts ] & /@ reps },
    If[ MemberQ[ result, $Failed ], $Failed,
      With[ { capped = infraCap[
            DeleteDuplicates @ Flatten[ result /. InfraHomotopy[ rs_List ] :> rs, 1 ],
            count ] },
        If[ capped === $Failed, $Failed, InfraHomotopy[ { # } ] & /@ capped ]
      ]
    ]
  ]


(* ===================== ReducePath ===================== *)

(* Apply length-non-increasing face moves to a fixed point.  Default
   "Faces" -> {2} is pure backtrack reduction (spurs a-b-a collapsed);
   larger face alphabets allow triangle / k-cycle shortcuts that strictly
   shorten the walk.  Returns one canonical short representative. *)

Options[ ReducePath ] = { "Faces" -> { 2 } };

ReducePath[ graph_Graph, path_List, opts : OptionsPattern[ ] ] :=
  Module[ { current = path, next, shortened = True,
            moves = Select[
              Join @@ ( faceMoves /@ resolveFaces[ graph, OptionValue[ "Faces" ] ] ),
              Length[ #[[ 1 ]] ] > Length[ #[[ 2 ]] ] & ] },
    While[ shortened,
      shortened = False;
      next = SelectFirst[
        Catenate[ applyMove[ current, # ] & /@ moves ],
        Length[ # ] < Length[ current ] &,
        $Failed ];
      If[ next =!= $Failed, current = next; shortened = True ]
    ];
    current
  ]


(* ===================== Helpers ===================== *)

closeWalk[ cycle_List ] :=
  If[ First[ cycle ] === Last[ cycle ], cycle, Append[ cycle, First[ cycle ] ] ]


(* resolveFaces[g, spec] -> List of vertex sequences (faces), each a closed walk
   in g.  Length-2 faces {a, b} encode backtrack reduction a -> b -> a;
   length-k faces (k >= 3) come from FindCycle[g, {k}, All]. *)

resolveFaces[ graph_Graph, Automatic ] := resolveFaces[ graph, { 2, 3 } ]

resolveFaces[ graph_Graph, n_Integer ] /; n >= 2 := resolveFaces[ graph, Range[ 2, n ] ]

resolveFaces[ graph_Graph, lengths_List ] /; AllTrue[ lengths, IntegerQ[ # ] && # >= 2 & ] :=
  Join @@ ( facesOfLength[ graph, # ] & /@ lengths )

resolveFaces[ graph_Graph, All ] :=
  Join[
    facesOfLength[ graph, 2 ],
    Select[
      First /@ # & /@ FindCycle[ graph, Infinity, All ],
      chordlessCycleQ[ graph, # ] & ]
  ]

resolveFaces[ graph_Graph, "Fundamental" ] := First /@ # & /@ FindFundamentalCycles[ graph ]

resolveFaces[ graph_Graph, faces : { __List } ] := faces

resolveFaces[ _Graph, { } ] := { }


facesOfLength[ graph_Graph, 2 ] := { #[[ 1 ]], #[[ 2 ]] } & /@ EdgeList[ graph ]

facesOfLength[ graph_Graph, k_Integer ] /; k >= 3 := First /@ # & /@ FindCycle[ graph, { k }, All ]


(* A cycle is chordless iff no non-cyclically-adjacent pair of vertices is
   joined by a graph edge. *)

chordlessCycleQ[ graph_Graph, cycle_List ] :=
  With[ { k = Length[ cycle ] },
    AllTrue[
      Subsets[ Range[ k ], { 2 } ],
      { i, j } |->
        Abs[ i - j ] === 1 || Abs[ i - j ] === k - 1 ||
        ! EdgeQ[ graph, UndirectedEdge[ cycle[[ i ]], cycle[[ j ]] ] ]
    ]
  ]


(* faceMoves[face]: ordered (oldArc, newArc) pairs.  For closed walk
   (f_1, ..., f_k), each cut (s, L) yields oldArc = L edges from slot s+1,
   newArc = the complementary k-L edges (reversed to share endpoints).  Both
   orientations included so the rewrite is direction-symmetric. *)

faceMoves[ face_List ] /; Length[ face ] < 2 := { }

faceMoves[ face_List ] :=
  With[ { k = Length[ face ],
          doubled    = Join[ face, face ],
          revDoubled = Join[ Reverse @ face, Reverse @ face ] },
    DeleteDuplicates @ Select[
      Join[
        Flatten[ Table[
          { doubled[[ s + 1 ;; s + L + 1 ]],
            Reverse @ doubled[[ s + L + 1 ;; s + k + 1 ]] },
          { s, 0, k - 1 }, { L, 0, k } ], 1 ],
        Flatten[ Table[
          { revDoubled[[ s + 1 ;; s + L + 1 ]],
            Reverse @ revDoubled[[ s + L + 1 ;; s + k + 1 ]] },
          { s, 0, k - 1 }, { L, 0, k } ], 1 ]
      ],
      #[[ 1 ]] =!= #[[ 2 ]] &
    ]
  ]


(* Slide oldArc through path as a contiguous sub-walk; every match yields
   one rewritten path with oldArc replaced by newArc. *)

applyMove[ path_List, { oldArc_List, newArc_List } ] :=
  With[ { arcLen = Length[ oldArc ], pathLen = Length[ path ] },
    If[ arcLen > pathLen, { },
      Cases[
        Table[
          If[ path[[ i ;; i + arcLen - 1 ]] === oldArc,
            Join[ path[[ ;; i - 1 ]], newArc, path[[ i + arcLen ;; ]] ],
            Nothing ],
          { i, 1, pathLen - arcLen + 1 } ],
        _List ]
    ]
  ]


pathNeighbors[ path_List, moves_List ] :=
  DeleteDuplicates @ Flatten[ applyMove[ path, # ] & /@ moves, 1 ]


(* Breadth-first search over walks with the elementary face moves as the
   transition relation.  parent maps each visited walk to its predecessor;
   the chain p_0 -> ... -> p_k is reconstructed by walking parent backwards.
   Wolfram's BreadthFirstScan works over graph vertices, not arbitrary
   path-spaces -- hence the hand-rolled While loop. *)

homotopyBFS[ graph_Graph, p1_List, p2_List, faces_List, maxLen_, maxMoves_ ] :=
  Module[ { moves, parent, frontier, nextFrontier, found = False, layer = 0 },
    If[ First[ p1 ] =!= First[ p2 ] || Last[ p1 ] =!= Last[ p2 ], Return[ { } ] ];
    If[ p1 === p2, Return[ { { p1 } } ] ];
    moves = If[ faces === { }, { }, Join @@ ( faceMoves /@ faces ) ];
    parent = <| p1 -> None |>;
    frontier = { p1 };
    While[ ! found && frontier =!= { } && layer < maxMoves,
      nextFrontier = { };
      Scan[
        p |->
          Scan[
            q |->
              If[ ! KeyExistsQ[ parent, q ] && Length[ q ] <= maxLen,
                AssociateTo[ parent, q -> p ];
                If[ q === p2, found = True ];
                AppendTo[ nextFrontier, q ]
              ],
            pathNeighbors[ p, moves ]
          ],
        frontier
      ];
      frontier = nextFrontier;
      layer++
    ];
    If[ found, { reconstructChain[ parent, p2 ] }, { } ]
  ]


reconstructChain[ parent_Association, target_List ] :=
  Module[ { chain = { target }, current = target },
    While[ parent[ current ] =!= None,
      current = parent[ current ];
      PrependTo[ chain, current ] ];
    chain
  ]
