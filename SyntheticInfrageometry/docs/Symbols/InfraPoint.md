---
Template: Symbol
Name: InfraPoint
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraPoint
Keywords: [point, atom, vertex, ontology, wrapper]
SeeAlso: [FindInfraPoint, InfraSet, InfraEffectivePoint, InfraMeasure, InfraSegment]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[InfraPoint]()[*v*]</code> is the point at vertex *v*.

## Details & Options

Definition: an infra-point is one vertex of the substrate, wrapped so that it carries geometry.

`InfraPoint` is the **atom** of the point ontology. A construction on a graph rarely has one answer, but the multiplicity does not live in this head: a family of candidate points is a `List` of atoms or an [InfraSet](), and a measure on vertices is an [InfraEffectivePoint]().

| you have | use |
|---|---|
| one vertex | `InfraPoint[v]` |
| several candidate points | `{InfraPoint[a], InfraPoint[b]}` — a plain list |
| a region of vertices | `InfraSet[{a, b, …}]` |
| a measure on vertices | `InfraEffectivePoint[<\|v -> m, …\|>]` |

The vertex label is carried verbatim, including a list label such as `{i, j}` on a grid or tessellation. This is the reason the atom exists: while one head served as both atom and container, `InfraPoint[{1, 1}]` was ambiguous on such a graph and silently produced a point supported on a vertex that was not in the graph.

Accessors:

| Accessor | Gives |
|---|---|
| `["Vertex"]` | the vertex label |
| `["First"]` | the same |
| `["Vertices"]` | the label in a one-element list |
| `["Mass"]` | `1` |

Graph-keyed invariants such as `["BallVolumes", g]` and `["Dimension", g]` return the bare numbers for that vertex; the same accessors on an [InfraSet]() return one row per vertex, which is the rectangular shape statistics compose over.

Inside an [InfraScene](), `InfraPoint[]` and `InfraPoint["Center"]` name a point to be solved for.

## Basic Examples

A point finder returns atoms — one, or a list.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {FindInfraPoint[g], FindInfraPoint[g, 3]}]
```

The accessor reads the vertex back out, and re-wrapping it is the identity.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {p = FindInfraPoint[g]},
  {p["Vertex"], InfraPoint[p["Vertex"]] === p}]
```

An atom answers invariants with bare numbers; a set of points answers with one row each.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {c = First @ GraphCenter[g]},
  {InfraPoint[c]["BallVolumes", g, {0, 3}],
   InfraSet[{c, First @ GraphPeriphery[g]}]["BallVolumes", g, {0, 3}]}]
```

## Properties and Relations

A list of atoms coerces to the region they span, or to the counting measure on them.

```wl
InfraSet[{InfraPoint[4], InfraPoint[9], InfraPoint[4]}]
```

```wl
InfraEffectivePoint[{InfraPoint[4], InfraPoint[9], InfraPoint[4]}]
```

A list-valued vertex label is just a label.

```wl
InfraPoint[{1, 1}]["Vertex"]
```
