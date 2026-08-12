---
Template: Symbol
Name: SelectInfraPath
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/SelectInfraPath
---

## Usage

`SelectInfraPath[graph, paths]` draws one path from the bundle treated as a finite metric space in path-space.

`SelectInfraPath[graph, paths, n]` returns exactly n or $Failed; UpTo[n] returns up to n; All returns the whole pool.

## Details & Options

Operator form SelectInfraPath[graph, n, opts][paths].

Options:

| Option | Values |
|---|---|
| `"From"` | All (default), "Center", "Periphery", "MostVisited", "Bottleneck", "MinLength", "MaxLength", anchor -> spec, {"Min" \| "Max", scoreFn} |
| `"Distance"` | -- |
| `"Metric"` | "Hausdorff" (default), "Frechet", "MeanFrechet" |
| `"MaxCliques"` | -- |
