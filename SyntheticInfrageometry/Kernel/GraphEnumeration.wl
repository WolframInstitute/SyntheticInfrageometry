Package["WolframInstitute`SyntheticInfrageometry`"]


(* ===================== Graph enumeration ===================== *)

(* EnumerateGraphs[n, predQ] = { g in C_n : predQ[g] } where C_n is the
   canonical sequence of connected n-vertex graphs (GraphData["Connected",
   n], canonicalised).  Predicate-agnostic workhorse: any whole-graph
   predicate composes via &&.  "From" -> graphList overrides the default
   generator. *)

Options[ EnumerateGraphs ] = { "From" -> Automatic };

EnumerateGraphs[ n_, predQ_, All, opts : OptionsPattern[] ] :=
  Module[ { source },
    source = OptionValue[ "From" ];
    If[ source === Automatic,
      Select[
        SortBy[ GraphData /@ GraphData[ "Connected", n ], EdgeList @ CanonicalGraph @ # & ],
        predQ
      ],
      Select[ source, predQ ]
    ]
  ]

EnumerateGraphs[ n_, predQ_, UpTo[ k_Integer ], opts : OptionsPattern[] ] :=
  Take[ EnumerateGraphs[ n, predQ, All, opts ], UpTo[ k ] ]

EnumerateGraphs[ n_, predQ_, k_Integer, opts : OptionsPattern[] ] :=
  With[ { result = EnumerateGraphs[ n, predQ, UpTo[ k ], opts ] },
    If[ Length[ result ] < k, $Failed, result ]
  ]

EnumerateGraphs[ n_, predQ_, opts : OptionsPattern[] ] :=
  EnumerateGraphs[ n, predQ, All, opts ]
