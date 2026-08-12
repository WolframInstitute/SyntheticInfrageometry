---
Template: Symbol
Name: FindInfraEllipse
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/FindInfraEllipse
---

## Usage

`FindInfraEllipse[graph, {p1, p2}, c]` returns {InfraEllipse[{cycle}]} for the shortest separating cycle in the level-surface subgraph { v : cMin <= d(p1,v) + d(p2,v) <= cMax } (c scalar or {cMin, cMax}).

`FindInfraEllipse[graph, {p1, p2}, c, n]` returns exactly n realisations or $Failed; UpTo[n] returns up to n; All returns all.

## Details & Options

Options:

| Option | Values |
|---|---|
| `Properties` | default {"Separating", "Shortest"} |
