Package["WolframInstitute`SyntheticInfrageometry`"]


(* methods loose -> strict: "Overlap" |A cap B| > 0, "Diffuse" |A cap B| > |A delta B|, "Set" equal vertex sets, "Multiset" equal measures *)

InfraEqualQ::badmethod = "Method `1` is not one of \"Overlap\", \"Diffuse\", \"Set\", \"Multiset\"."

Options[ InfraEqualQ ] = { Method -> "Diffuse" }

(* raw visit measures: every shape goes through the anchor rule, a walk graph or bundle contributing its occupation.  There is no cross-kind refusal: the method names what is compared, so a point and a set do overlap when the point lies in the set, and a walk graph equals the vertex list of its support under "Set".  The wrapper-era guard read Head, which under the shape ontology separates an integer-labelled vertex from a list-labelled one *)
InfraEqualQ[ graph_Graph, a_, b_, OptionsPattern[] ] :=
  With[ { ma = equalityMultiset[ graph, a ], mb = equalityMultiset[ graph, b ] },
    Switch[ OptionValue @ Method,
      "Overlap",  multisetCapMass[ ma, mb ] > 0,
      "Diffuse",  3 multisetCapMass[ ma, mb ] > Total @ ma + Total @ mb,
      "Set",      Sort @ Keys @ ma === Sort @ Keys @ mb,
      "Multiset", KeySort @ ma === KeySort @ mb,
      m_,         Message[ InfraEqualQ::badmethod, m ]; $Failed
    ]
  ]

equalityMultiset[ graph_Graph, obj_ ] := toDensity[ graph, obj ]

multisetCapMass[ a_Association, b_Association ] :=
  Total @ KeyValueMap[ { k, v } |-> Min[ v, Lookup[ b, k, 0 ] ], a ]
