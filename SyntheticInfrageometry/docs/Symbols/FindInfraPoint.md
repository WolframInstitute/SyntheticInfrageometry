---
Template: Symbol
Name: FindInfraPoint
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraPoint
Keywords: [point, candidate pool, superposition, centre, periphery]
SeeAlso: [InfraPoint, SelectInfraPoint, FindInfraMidpoint, FindClosestInfraPoint, InfraSet]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

<code>[FindInfraPoint]()[*g*]</code> gives the canonical point of *g*: every vertex, as one [InfraPoint]().

<code>[FindInfraPoint]()[*g*, *n*]</code> gives *n* mutually constrained points, as a list of *n* wrappers. Exactly *n* or `$Failed`; `UpTo[n]` gives up to *n*; `All` gives every vertex.

## Details & Options

Definition: with no condition imposed, a point of *g* is the whole vertex set, held as one candidate family.

That is the honest answer to "give me a point". It is a starting object, not a placeholder. Constraints then narrow it.

The two forms return different shapes, on purpose.

- One point is **one wrapper** with many realisations.
- *n* points is a **list of n wrappers**.

The list is needed because the points constrain each other. Three points pairwise at maximum distance is a joint condition. One wrapper could not say which realisations go together.

Options:

| Option | Values |
|---|---|
| `"From"` | `"Random"` (default), `"Center"`, `"Periphery"`, or `anchor -> spec` |
| `"Distance"` | *d*, `{dMin, dMax}`, `"Max"`, `"Spread"` |
| `"MaxCliques"` | bound on the mutual-distance clique search |

`"Center"` and `"Periphery"` are meant in the graph-eccentricity sense. `"Distance"` imposes a mutual-distance condition on a tuple.

To narrow a bundle you already hold, use `SelectInfraPoint`. It takes the same `"From"` and `"Distance"` vocabulary on a supplied vertex set.

Corresponding notions in the classical axiom systems:

| System | Name | Statement |
|---|---|---|
| Euclid | Definition 1 | A point is that of which there is no part. |
| Hilbert | Group I | Points are one of the three primitive kinds of object. |
| Tarski | primitive | Variables range over points. There is no other sort. |
| Birkhoff | Ruler postulate | Points are what the reals coordinate. |

## Basic Examples

With no condition, the point is the whole vertex set.

```wl
With[
  {g = ExampleGraphData["Square", "Medium"]},
  {p = FindInfraPoint[g]},
  <|"realisations" -> Length @ p["Realizations"], "vertices in graph" -> VertexCount[g]|>]
```

`"From"` selects the metrically special vertices. Here the centre is one vertex and the periphery is many.

```wl
With[
  {g = ExampleGraphData["Square", "Medium"]},
  <|"centre" -> FindInfraPoint[g, "From" -> "Center"]["Realizations"],
    "periphery count" -> Length @ FindInfraPoint[g, "From" -> "Periphery"]["Realizations"]|>]
```

Centre and periphery drawn together. The periphery of a patch is its rim.

```wl
With[
  {g = Graph[ExampleGraphData["Square", "Medium"], Sequence @@ AmbientGraphStyle["Gray"]]},
  InfraSceneHighlight[g,
    {FindInfraPoint[g, "From" -> "Periphery"] -> $InfraShellColor,
     FindInfraPoint[g, "From" -> "Center"] -> $InfraPointColor},
    "PointSizeRange" -> 16,
    VertexShapeFunction -> ({AbsolutePointSize[2.2], Point[#]} &),
    ImageSize -> 340]]
```

A tuple of three mutually most-distant points comes back as three wrappers.

```wl
With[
  {g = ExampleGraphData["Square", "Medium"]},
  {t = FindInfraPoint[g, 3, "Distance" -> "Max"]},
  <|"shape" -> Head[t], "count" -> Length[t], "each" -> (First @ #["Realizations"] & /@ t)|>]
```
