Package["WolframInstitute`SyntheticInfrageometry`"]

PackageScope[findRevolutionCore]


(* ===================== InfraRevolution wrapper ===================== *)

(* InfraRevolution[{set}] is the unary form (one rotational vertex set);
   InfraRevolution[{set1, ..., setk}] is the multi-realisation form.
   Auto-flatten on nested wrappers. *)

InfraRevolution[ reps_List ] /; AnyTrue[ reps, MatchQ[ InfraRevolution[ _List ] ] ] :=
  InfraRevolution[ Flatten[ reps /. InfraRevolution[ xs_List ] :> xs, 1 ] ]


(* ===================== FindRevolution ===================== *)

(* A rotational object around an axis [a_1, ..., a_{L+1}] with a per-axis-
   vertex radius profile [r_1, ..., r_{L+1}] is the vertex set carved out
   by the Voronoi-closest membership test:  a non-axis vertex v is in the
   object iff v is closest to some axis index i (possibly tied) and the
   form test holds on d(v, a_i) vs r_i (== for Surface, <= for Solid).
   Axis vertex a_i is in iff the form test holds on (0, r_i).            *)

FindRevolution::badprofile = "Profile `1` is not a valid profile specification for an axis of length `2`.";
FindRevolution::negradius  = "Profile contains negative radii: `1`.";
FindRevolution::badform    = "Form `1` is not \"Surface\" or \"Solid\".";

Options[ FindRevolution ] = { "Form" -> "Surface", Method -> "Metric" };

FindRevolution[ graph_Graph, axis_, profile_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1,
    opts : OptionsPattern[] ] :=
  infraSpreadAndCartesian[ InfraRevolution, count,
    findRevolutionCore[ graph, ##, opts ] &, axis, profile ]


findRevolutionCore[ graph_Graph, axis_, profile_,
    opts : OptionsPattern[ FindRevolution ] ] :=
  Module[ { axisList, radii, form },
    axisList = toVertexSet @ axis;
    radii = resolveProfile[ axisList, profile ];
    If[ radii === $Failed, Return[ $Failed ] ];
    If[ AnyTrue[ radii, NumericQ[ # ] && # < 0 & ],
      Message[ FindRevolution::negradius, Select[ radii, NumericQ[ # ] && # < 0 & ] ];
      Return[ $Failed ] ];
    form = OptionValue[ FindRevolution, { opts }, "Form" ];
    If[ form =!= "Surface" && form =!= "Solid",
      Message[ FindRevolution::badform, form ]; Return[ $Failed ] ];
    { revolutionVertexSet[ graph, axisList, radii, form ] }
  ]


(* Voronoi-closest membership.  For each non-axis vertex v compute its
   distance vector to the axis, take the minimum, and include v iff any
   tied closest axis index passes the form test against its profile radius.
   Axis vertices are tested directly: a_i is in iff cmp(0, r_i).         *)

revolutionVertexSet[ graph_Graph, axis_List, radii_List, form_String ] :=
  Module[ { cmp, dmat, vIdx, axisIdx, nonAxis, axisHits, nonAxisHits },
    cmp = If[ form === "Solid", LessEqual, Equal ];
    dmat = GraphDistanceMatrix[ graph ];
    vIdx = AssociationThread[ VertexList[ graph ], Range @ VertexCount @ graph ];
    axisIdx = vIdx /@ axis;
    nonAxis = Complement[ VertexList[ graph ], axis ];
    axisHits = Pick[ axis, cmp[ 0, # ] & /@ radii ];
    nonAxisHits = Select[ nonAxis,
      v |-> With[ { dists = dmat[[ vIdx[ v ], # ]] & /@ axisIdx },
        With[ { minD = Min @ dists },
          AnyTrue[ Range @ Length @ axis,
            i |-> dists[[ i ]] === minD && cmp[ dists[[ i ]], radii[[ i ]] ] ] ] ] ];
    Sort @ DeleteDuplicates @ Join[ axisHits, nonAxisHits ]
  ]


(* Profile resolution: returns a List of length Length[axis] or $Failed.
   Dispatch order matters -- NumericQ before List so a constant numeric
   profile does not fall through to the function clause.                  *)

resolveProfile[ axis_List, profile_ ] :=
  Which[
    NumericQ @ profile,
      ConstantArray[ profile, Length @ axis ],
    AssociationQ @ profile,
      With[ { vals = Lookup[ profile, axis, $Failed ] },
        If[ MemberQ[ vals, $Failed ],
          Message[ FindRevolution::badprofile, profile, Length @ axis ]; $Failed,
          vals ] ],
    ListQ @ profile,
      If[ Length @ profile === Length @ axis, profile,
        Message[ FindRevolution::badprofile, profile, Length @ axis ]; $Failed ],
    True,
      Quiet @ Check[ profile /@ Range @ Length @ axis,
        Message[ FindRevolution::badprofile, profile, Length @ axis ]; $Failed ]
  ]


(* ===================== FindCylinder ===================== *)

(* Constant-radius rotational set.  Delegates to FindRevolution with the
   numeric profile; multi-axis spread is handled there.                   *)

Options[ FindCylinder ] = Options[ FindRevolution ];

FindCylinder[ graph_Graph, axis_, radius_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1,
    opts : OptionsPattern[] ] :=
  FindRevolution[ graph, axis, radius, count, opts ]


(* ===================== FindCone ===================== *)

(* Linear-profile rotational set with apex at one end of the axis.  Radii
   are slope * Range[0, L] when Apex -> First (default) and slope * Range[L, 0, -1]
   when Apex -> Last (L = Length[axis] - 1).  Multi-axis is unpacked here
   because the radii list depends on the axis length.                     *)

Options[ FindCone ] = Join[ Options[ FindRevolution ], { "Apex" -> First } ];

FindCone[ graph_Graph, axis_, slope_,
    count : ( _Integer | UpTo[ _Integer ] | All ) : 1,
    opts : OptionsPattern[] ] :=
  Module[ { apex, revOpts, paths, allSets, capped },
    apex = OptionValue[ FindCone, { opts }, "Apex" ];
    revOpts = FilterRules[ { opts }, Options[ FindRevolution ] ];
    paths = Replace[ axis, {
      InfraSegment[ ps_List ] :> ps,
      v_ /; AtomQ[ v ] :> { { v } },
      l_List :> { l } } ];
    allSets = DeleteDuplicates @ Flatten[
      ( #[[ 1, 1 ]] & /@ FindRevolution[ graph, #, coneRadii[ Length @ #, slope, apex ],
                                         All, Sequence @@ revOpts ] ) & /@ paths,
      1 ];
    capped = infraCap[ allSets, count ];
    If[ capped === $Failed, $Failed, InfraRevolution[ { # } ] & /@ capped ]
  ]


coneRadii[ n_Integer, slope_, First ] := slope * Range[ 0, n - 1 ]
coneRadii[ n_Integer, slope_, Last  ] := slope * Range[ n - 1, 0, -1 ]


(* ===================== RevolutionQ ===================== *)

(* Decoder: vs is a rotational object around axis with profile iff
   Sort[vs] equals revolutionVertexSet[graph, axis, radii, form].         *)

RevolutionQ[ graph_Graph, vs_List, axis_, profile_,
    opts : OptionsPattern[ FindRevolution ] ] :=
  Module[ { axisList, radii, form, expected },
    axisList = Replace[ axis, { InfraSegment[ { p_List } ] :> p,
                                v_ /; AtomQ[ v ] :> { v } } ];
    radii = resolveProfile[ axisList, profile ];
    If[ radii === $Failed, Return[ False ] ];
    form = OptionValue[ FindRevolution, { opts }, "Form" ];
    If[ form =!= "Surface" && form =!= "Solid", Return[ False ] ];
    expected = revolutionVertexSet[ graph, axisList, radii, form ];
    Sort @ vs === expected
  ]
