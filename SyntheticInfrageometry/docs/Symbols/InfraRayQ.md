---
Template: Symbol
Name: InfraRayQ
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraRayQ
Keywords: [ray, half-line, geodesic, inextensible, predicate]
SeeAlso: [InfraRay, FindInfraRay, InfraSegmentQ, InfraLineQ, InfraPathQ]
RelatedGuides: [EuclideanGeometryGuide]
---

## Usage

`InfraRayQ[graph, ray]` tests whether ray is a pointed half-line: a geodesic from its own first vertex that cannot be prolonged past its last.

`InfraRayQ[graph, InfraRay[{ray1, ...}]]` tests every realisation of a bundle.

## Details & Options

A ray is the **pointed half of a maximal geodesic**. Two conditions, and the asymmetry between them is the whole definition:

| Condition | Why |
|---|---|
| the sequence is a geodesic | a ray is straight, so `InfraSegmentQ` must hold |
| no neighbour of the last vertex sits one step further from the first | the far end is inextensible |

Inextensibility is required **only at the far end**. The origin is an endpoint by fiat — that is exactly what distinguishes a ray from a line, and it means a ray may start anywhere, not only at a peripheral vertex.

Sequences shorter than two vertices are `False`: a single vertex has no direction.

The predicate is the companion of [FindInfraRay](), and every realisation that finder returns satisfies it.

## Basic Examples

On a path the whole graph is a ray from either end.

```wl
InfraRayQ[PathGraph[Range[5]], {1, 2, 3, 4, 5}]
```

A proper initial segment is not — it can still be prolonged.

```wl
InfraRayQ[PathGraph[Range[5]], {1, 2, 3}]
```

But a ray need not begin at the periphery. This one starts in the middle and runs to the leaf.

```wl
InfraRayQ[PathGraph[Range[5]], {3, 4, 5}]
```

## Properties and Relations

Every ray [FindInfraRay]() produces satisfies the predicate.

```wl
With[
  {g = GridGraph[{5, 5}]},
  {rays = FindInfraRay[g, 1, 13, All]},
  AllTrue[First @ rays, InfraRayQ[g, #] &]]
```

Dropping the far vertex breaks inextensibility, so the truncation is no longer a ray.

```wl
With[
  {g = GridGraph[{5, 5}]},
  {ray = First @ First @ FindInfraRay[g, 1, 13, All]},
  {InfraRayQ[g, ray], InfraRayQ[g, Most @ ray]}]
```

A ray is a geodesic that stops only because it must; a walk right round a cycle is not a geodesic at all.

```wl
InfraRayQ[CycleGraph[6], {1, 2, 3, 4, 5, 6}]
```
