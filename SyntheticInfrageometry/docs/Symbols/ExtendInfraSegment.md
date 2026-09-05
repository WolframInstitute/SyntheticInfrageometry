---
Template: Symbol
Name: ExtendInfraSegment
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/ExtendInfraSegment
Keywords: [segment extension, geodesic extension, pool, distance matrix, Tarski A4, segment construction]
SeeAlso: [FindInfraSegment, InfraSegment, FindInfraLine, GeodesicExtensionGraph, InfraPoint, TarskiSegmentConstructionQ]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[ExtendInfraSegment]()[*g*, *seg*, *kspec*]</code> gives one geodesic containing *seg*, extended past its ends by at most *kspec* edges on each side and inextensible within that budget, as an [InfraSegment]() wrapper.

<code>[ExtendInfraSegment]()[*g*, *seg*, *kspec*, *n*]</code> gives exactly *n* extensions or `$Failed`; `UpTo[n]` gives up to *n*; `All` gives the whole class as a pool.

<code>[ExtendInfraSegment]()[*g*, *a*, *b*, *c*, *d*]</code> gives the vertices *x* with *b* between *a* and *x* and *d(b, x) = d(c, d)* — Tarski's segment construction, axiom A4 — as a list of [InfraPoint]() atoms.

## Details & Options

The seed *seg* is a vertex sequence, an [InfraSegment]() geodesic DAG, or any wrapper spreading to walks. A geodesic DAG extends **as one object**: the whole bundle from *p1* to *p2* gets the same ends.

Everything is read off the distance matrix. With the seed running from *p1* to *p2*, a pair of ends (*s*, *e*) is **admissible** when *d(s, e) = d(s, p1) + d(p1, p2) + d(p2, e)* — the concatenation is a geodesic whichever geodesics are used, so the condition reads only the ends — when the larger of *d(p1, s)* and *d(p2, e)* passes *kspec*, and when each free side is either at the budget or inextensible. The candidate ends are the vertices of the two extension graphs <code>[GeodesicExtensionGraph]()[*g*, {*p2*, *p1*}]</code> and <code>[GeodesicExtensionGraph]()[*g*, {*p1*, *p2*}]</code>, cut at the budget.

| *kspec* | Extension per side |
|---|---|
| *k* | at most *k* edges |
| {*k*} | exactly *k* edges |
| {*lo*, *hi*} | between *lo* and *hi* edges |
| `Infinity` (default) | unbounded — the lines through the seed |
| 0 | none — the seed itself |

`All` returns the **pool** <code>[InfraSegment]()[{*dag1*, …}]</code>, one geodesic DAG per admissible pair of ends; a single admissible pair collapses to <code>[InfraSegment]()[*dag*]</code>. `["Multiplicity"]`, `["Measure"]`, `["Start"]` and `["End"]` are read off the DAGs without enumeration and `["Length"]` is one number per DAG. A bounded count streams extensions off the admissible pairs.

<code>[ExtendInfraSegment]()[*g*, *seg*, Infinity, *n*]</code> is <code>[FindInfraLine]()[*g*, *seg*, *n*]</code> — the same pool.

The longest extensions are a selection on the result, `SelectInfraWalk[g, pool, All, "From" -> "MaxLength"]`.

| Option | Values | Meaning |
|---|---|---|
| `Method` | `Automatic` (default), `"Exhaustive"`, `"Greedy"`, `"RandomGreedy"` | `Automatic` resolves by the count: `All` to `"Exhaustive"`, the pool; a bounded count to `"RandomGreedy"`, admissible pairs and DAG branches in random order — put `SeedRandom` in front of a call whose output goes into a figure. The class is the same under every value. |
| `"Direction"` | `"BothSides"` (default), `"Forward"`, `"Backward"` | which ends may move: `"Forward"` keeps *p1* and extends past *p2* only, `"Backward"` the reverse. |
| `Properties` | `{}` | only the empty list; a rule on the extension is a local law and lives on `ExtendInfraGeodesic`. |

The six-argument form is Tarski's axiom A4: from *a* through *b*, lay off a segment congruent to *cd*. It returns every vertex *x* at distance *d(c, d)* beyond *b* on a geodesic from *a*; a trailing count *n*, `UpTo[n]` or `All` bounds the list.

## Basic Examples

One step per side beyond an interior edge of the grid: seven extensions.

```wl
ExtendInfraSegment[GridGraph[{4, 4}], {6, 7}, 1, All]["Realizations"]
```

A budget of 0 is the seed itself; no budget at all gives the lines through it.

```wl
With[
  {g = GridGraph[{4, 4}]},
  {ExtendInfraSegment[g, {6, 7}, 0, All]["Realizations"],
   ExtendInfraSegment[g, {6, 7}, Infinity, All]["Multiplicity"]}]
```

The count-less call is one extension.

```wl
SeedRandom[1]; ExtendInfraSegment[GridGraph[{4, 4}], {6, 7}, 2]
```

Tarski's segment construction on a path: from 2 through 3, lay off a segment as long as 5–7.

```wl
ExtendInfraSegment[PathGraph[Range[7]], 2, 3, 5, 7]
```

## Scope

`All` is the pool, and its ends are read off the DAGs.

```wl
With[
  {pool = ExtendInfraSegment[GridGraph[{4, 4}], {6, 7}, 2, All]},
  {pool["Multiplicity"], pool["Start"], pool["End"]}]
```

A whole geodesic bundle extends as one object: six geodesics from the corner, twelve lines through all of them.

```wl
With[
  {g = GridGraph[{4, 4}]},
  {seg = FindInfraSegment[g, 1, 11, All]},
  {seg["Multiplicity"], ExtendInfraSegment[g, seg, Infinity, All]["Multiplicity"]}]
```

On the 6-cycle a budget of 1 buys one extension of the edge 1–2, a budget of 2 the three lines through it.

```wl
{ExtendInfraSegment[CycleGraph[6], {1, 2}, 1, All]["Realizations"],
 ExtendInfraSegment[CycleGraph[6], {1, 2}, 2, All]["Realizations"]}
```

## Options

### "Direction"

`"Forward"` keeps the first vertex of the seed fixed.

```wl
ExtendInfraSegment[GridGraph[{4, 4}], {6, 7}, 1, All, "Direction" -> "Forward"]["Realizations"]
```

## Properties and Relations

With no budget the extension pool is the line pool.

```wl
With[
  {g = GridGraph[{4, 4}]},
  Sort @ ExtendInfraSegment[g, {6, 7}, Infinity, All]["Realizations"] ===
    Sort @ FindInfraLine[g, 6, 7, All]["Realizations"]]
```

Exactly one step per side gives the same seven as at most one, since every one-step extension is inextensible within the budget.

```wl
With[
  {g = GridGraph[{4, 4}]},
  Sort @ ExtendInfraSegment[g, {6, 7}, {1}, All]["Realizations"] ===
    Sort @ ExtendInfraSegment[g, {6, 7}, 1, All]["Realizations"]]
```
