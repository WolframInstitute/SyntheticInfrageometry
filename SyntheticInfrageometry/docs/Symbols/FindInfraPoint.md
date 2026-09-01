---
Template: Symbol
Name: FindInfraPoint
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraPoint
Keywords: [point, atom, candidate pool, centre, periphery]
SeeAlso: [InfraPoint, InfraSet, InfraEffectivePoint, SelectInfraPoint, FindInfraMidpoint, FindClosestInfraPoint]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraPoint]()[*g*]</code> gives one point of *g*, as an [InfraPoint]() atom.

<code>[FindInfraPoint]()[*g*, *n*]</code> gives *n* points, as a list of *n* atoms. Exactly *n* or `$Failed`; `UpTo[n]` gives up to *n*; `All` gives every vertex of the pool.

## Details & Options

Definition: a point of *g* drawn from the `"From"` candidate pool.

The count always means **how many points you get**; the `"Distance"` option constrains **which** ones. With a distance condition the returned list is therefore one mutually-constrained tuple — three points pairwise at maximum distance is a joint condition, and a plain list of atoms states it without ambiguity.

The whole pool as a *region* is a coercion, not a return value:

```wl
InfraSet @ FindInfraPoint[g, All]
```

Options:

| Option | Values |
|---|---|
| `"From"` | `"Random"` (default), `"Center"`, `"Periphery"`, or `anchor -> spec` |
| `"Distance"` | *d*, `{dMin, dMax}`, `"Max"`, `"Spread"` |
| `"MaxCliques"` | bound on the mutual-distance clique search |

`"Center"` and `"Periphery"` are meant in the graph-eccentricity sense. `"Distance"` imposes a mutual-distance condition on a tuple.

To narrow a bundle you already hold, use [SelectInfraPoint](). It takes the same `"From"` and `"Distance"` vocabulary on a supplied vertex set.

The draw is random: seed with `SeedRandom` before the first call if you need a reproducible figure.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Definition 1 | A point is that of which there is no part. |
| Hilbert | Group I | Points are one of the three primitive kinds of object. |
| Tarski | primitive | Variables range over points. There is no other sort. |
| Birkhoff | Ruler postulate | Points are what the reals coordinate. |

## Basic Examples

One point is an atom; the calling triple gives a list of them.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {FindInfraPoint[g], Length @ FindInfraPoint[g, All], VertexCount[g]}]
```

`"From"` selects the metrically special vertices. Here the centre is one vertex and the periphery is many.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  <|"centre" -> FindInfraPoint[g, All, "From" -> "Center"],
    "periphery count" -> Length @ FindInfraPoint[g, All, "From" -> "Periphery"]|>]
```

Centre and periphery drawn together. The periphery of a patch is its rim.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "Gray", "KeepCoordinates" -> True]},
  InfraSceneHighlight[g,
    {InfraSet @ FindInfraPoint[g, All, "From" -> "Periphery"] -> $InfraShellColor,
     InfraSet @ FindInfraPoint[g, All, "From" -> "Center"] -> $InfraPointColor},
    "PointSizeRange" -> 16,
    VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
    ImageSize -> 340]]
```

A tuple of three mutually most-distant points comes back as three atoms.

```wl
With[
  {g = InfraSubstrate["Square", "Medium", "KeepCoordinates" -> True]},
  {t = FindInfraPoint[g, 3, "Distance" -> "Max"]},
  <|"count" -> Length[t], "vertices" -> (#["Vertex"] & /@ t)|>]
```
