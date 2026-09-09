---
Template: Symbol
Name: InfraDensity
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraDensity
---

## Usage

`InfraDensity[graph, x]` gives the marginal of the shape `x` to the vertex set of `graph`, as the Association <|v -> m|>, with respect to the counting measure.

## Details & Options

`InfraDensity` is the one public coercion in the API. `Keys` demotes a density back to its support; `Counts` promotes a vertex list to a density.

It reads every shape:

| `x` | `InfraDensity[graph, x]` |
|---|---|
| a vertex `v` | `<\|v -> 1\|>` |
| a vertex list | its `Counts` |
| a density `<\|v -> m\|>` | itself |
| a walk graph, or a bundle of them | the vertex occupation over the walks it carries |

The result is key-sorted, so two densities built by different routes compare with `SameQ`.

`InfraDensity` is the raw marginal: it takes no `Method` and no `"On"` option. The two normalisations are one division away -- `d / Max @ Values @ d` is the occupation in [0, 1], `d / Total @ d` the distribution summing to 1. Edge weights are internal to the renderer.

## Examples

```wolfram
graph = GridGraph[{3, 3}];
InfraDensity[graph, FindInfraSegment[graph, 1, 9, All]]
```
