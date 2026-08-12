---
Template: Symbol
Name: GeodesicSprayGraph
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/GeodesicSprayGraph
---

## Usage

`GeodesicSprayGraph[graph, c]` returns the BFS DAG rooted at c: a directed graph with edge u -> v whenever d(c, v) = d(c, u) + 1 and u-v is an edge of graph.

`GeodesicSprayGraph[graph, InfraPoint[{c1, ..., ck}]`] uses multi-source BFS.

`GeodesicSprayGraph[graph, pairs]` returns the union of geodesics between the listed vertex pairs.

## Details & Options

Options: "AxisLength" (truncate the source-spray DAG at depth k; default All), "PathThickness" (per-pair Hausdorff threshold; 0 keeps one geodesic per pair, Infinity keeps all), "Directed".

Sinks are peripheral vertices reachable from c; directed paths c -> sink are exactly the maximal geodesics from c.
