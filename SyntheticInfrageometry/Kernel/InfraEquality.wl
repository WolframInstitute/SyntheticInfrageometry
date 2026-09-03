Package["WolframInstitute`SyntheticInfrageometry`"]


(* methods loose -> strict: "Overlap" |A cap B| > 0, "Diffuse" |A cap B| > |A delta B|, "Set" equal vertex sets, "Multiset" equal measures *)

InfraEqualQ::badmethod = "Method `1` is not one of \"Overlap\", \"Diffuse\", \"Set\", \"Multiset\"."

Options[ InfraEqualQ ] = { Method -> "Diffuse" }

InfraEqualQ[ _Graph, a_, b_, OptionsPattern[] ] /; Head[ a ] =!= Head[ b ] := False

(* raw visit measures: infraVertexMultiset == InfraMeasure[obj, "Normalize" -> False] *)
InfraEqualQ[ _Graph, a_, b_, OptionsPattern[] ] :=
  With[ { ma = infraVertexMultiset @ a, mb = infraVertexMultiset @ b },
    Switch[ OptionValue @ Method,
      "Overlap",  multisetCapMass[ ma, mb ] > 0,
      "Diffuse",  3 multisetCapMass[ ma, mb ] > Total @ ma + Total @ mb,
      "Set",      Sort @ Keys @ ma === Sort @ Keys @ mb,
      "Multiset", KeySort @ ma === KeySort @ mb,
      m_,         Message[ InfraEqualQ::badmethod, m ]; $Failed
    ]
  ]

multisetCapMass[ a_Association, b_Association ] :=
  Total @ KeyValueMap[ { k, v } |-> Min[ v, Lookup[ b, k, 0 ] ], a ]
