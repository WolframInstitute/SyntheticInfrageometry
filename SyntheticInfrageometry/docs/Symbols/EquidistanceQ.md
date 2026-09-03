---
Template: Symbol
Name: EquidistanceQ
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/EquidistanceQ
Keywords: [equidistance, congruence, Tarski, four-place relation]
SeeAlso: [BetweennessQ, TarskiStructure, TarskiEquidistanceClasses, FindInfraBisectingHyperplane, TarskiAxiomQ]
RelatedGuides: [TarskiGeometryGuide]
---

## Usage

<code>[EquidistanceQ]()[*g*, *a*, *b*, *c*, *d*]</code> tests Tarski's equidistance $ab \equiv cd$: whether $d(a,b) = d(c,d)$ in *g*.

## Details & Options

Equidistance is the second primitive of Tarski's axiomatization, alongside betweenness ([BetweennessQ]()). It is a **four-place** relation on vertices — it compares two segments rather than describing one — and it is what does the work of congruence: Tarski has no separate congruence primitive, no motions, and no ruler.

On a graph it reduces to equality of graph distances, so it is cheap. What is not cheap, and not automatic, is that the axioms governing it hold: `TarskiCongruenceReflexivityQ`, `TarskiCongruenceTransitivityQ` and `TarskiCongruenceIdentityQ` test the three that concern equidistance alone, and `TarskiAxiomQ` runs the whole dashboard.

Because the graph metric takes only integer values, the equidistance classes are coarse: every pair at distance *k* is congruent to every other. `TarskiEquidistanceClasses` returns that partition. This is the sharpest way in which a graph is not a Euclidean plane — there are only $\mathrm{diam}(g)$ congruence classes of segments, so congruence carries far less information than it does classically.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Tarski | ab congruent to cd, primitive | One of the two primitive relations, together with betweenness. |
| Hilbert | Group III, III.1 to III.5 | Segment and angle congruence, taken as primitive. |
| Euclid | Common Notion 4 | Things coinciding with one another are equal — superposition, never postulated as a relation. |
| Birkhoff | Ruler postulate | Congruence is equality of ruler-coordinate differences. |

## Basic Examples

Equidistance is symmetric in each pair and reflexive, so a segment is congruent to itself read either way.

```wl
With[
  {g = InfraSubstrate["SquareTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 5 &]},
  {EquidistanceQ[g, a, b, a, b], EquidistanceQ[g, a, b, b, a]}]
```

The congruence classes are the distance classes, so their number is the diameter rather than a continuum — on each substrate every pair at a given distance is congruent to every other.

```wl
Association @ Table[
   name -> With[
     {g = InfraSubstrate[name, "Medium", "KeepCoordinates" -> True]},
     {c = First @ GraphCenter[g]},
     Length @ Union @ GraphDistance[g, c]],
   {name, {"SquareMeshGraph", "SquareTilingGraph", "HexagonalTilingGraph"}}]
```

Every vertex of the perpendicular bisector is, by definition, equidistant from the two points it bisects.

```wl
With[
  {g = InfraSubstrate["SquareTilingGraph", "Medium", "KeepCoordinates" -> True]},
  {a = First @ GraphCenter[g]},
  {b = First @ Sort @ Select[VertexList[g], GraphDistance[g, a, #] == 6 &]},
  AllTrue[First @ First @ FindInfraBisectingHyperplane[g, a, b], EquidistanceQ[g, a, #, b, #] &]]
```
