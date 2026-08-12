---
Template: Symbol
Name: InfraSegment
Context: WolframInstitute`SyntheticInfrageometry`
ContextPath: [WolframInstitute`Infrageometry`]
Paclet: WolframInstitute/SyntheticInfrageometry
URI: WolframInstitute/SyntheticInfrageometry/ref/InfraSegment
---

## Usage

`InfraSegment[{path1, ..., pathk}]` is the multi-realisation geodesic bundle -- a set of alternative geodesics, carrying no masses; seg[[i]] is the measured InfraPoint of the i-th position across realisations (mass = multiplicity), which is where a measure gets constructed.

`InfraSegment[dag_Graph]` is the geodesic-DAG form with accessors ["Graph"], ["Vertices"], ["Length"], ["Multiplicity"], ["Start"] / ["End"], ["Measure"], and ["Realizations", n | UpTo[n] | All] to enumerate.

## Details & Options

Scene-language constructor InfraSegment[p, q] is used inside InfraScene.
