# Changelog

## Unreleased

- **Breaking:** `SelectPath` / `SelectCycle` renamed to `SelectInfraPath` / `SelectInfraCycle` for naming consistency with the `Infra*` wrapper family. New `SelectInfraPoint[g, vertices, n]` is the vertex-bundle analogue of `SelectInfraPath` — same calling triple, same `"From"` / `"Distance"` / `"MaxCliques"` options; no `"Metric"` (graph distance is canonical on vertex bundles). `EmbeddingClosestPaths` and `EmbeddingClosestCycles` collapsed into a single polymorphic `EmbeddingClosest[g, bundle, ref]`: reference shape `{p1, p2}` dispatches to segment-shape, `{center, radius_?NumericQ}` to circle-shape. Bundles preserve their wrappers (`InfraSegment`, `InfraLine`, `InfraPath`, `InfraRay`, `InfraCircle`). No deprecation aliases — direct rename.

- **Breaking:** `InfraExampleGraph` retired. Replaced by two primitives in `Kernel/ExampleGraphs.wl`: `PunchHole[g, r]` removes a closed `r`-ball around a random vertex (or `PunchHole[g, c -> r]` for an explicit center); multi-hole use is `Fold[PunchHole, g, list]`. `TorusTessellation[shape, {m, n}]` for `shape \[Element] {"Rectangular", "Triangular", "Hexagonal"}` — the three vertex-transitive flat-torus `{p, q}`-tessellations (`{4, 4}`, `{3, 6}`, `{6, 3}` respectively). The old registry was thin scaffolding over existing built-ins (`GridGraph`, `GraphData[{"Triangular", ...}]`, `PetersenGraph[]`, `CayleyGraph[FiniteGroupData[..., ...]]`, mesh discretisation); call those directly. The honeycomb `TorusTessellation["Hexagonal", {m, n}]` form implements the two-orbit Cayley graph on `Z_m × Z_n × Z_2` (previously documented only conceptually in `Wiki/Concepts/HomogeneousGraphs.md`). The earlier `HoleAdd[g, {{count, radius}, ...}]` / `GridGraphWithHoles[{m, n}, holes]` API has been replaced by `PunchHole`; recover the old behaviour with `Fold[PunchHole, GridGraph[{m, n}], Catenate[ConstantArray[Last @ #, First @ #] & /@ holes]]`.

- **Breaking:** `SelectPaths` / `SelectCycles` renamed to `SelectPath` / `SelectCycle` and redesigned as `FindPoint`-on-path-space. The bundle is now treated as a finite metric space (paths = points, distance = path-space metric); the API mirrors `FindPoint` exactly. Calling triple `SelectPath[g, paths, n_Integer | UpTo[n] | All]` with default `n = 1`. Options: `"From"` (pool selector: `All` (default), `"Center"`, `"Periphery"`, `"MostVisited"`, `anchor -> spec`, `InfraSegment[{...}] -> spec`; `SelectCycle` additionally accepts `"ShortestCircumference"` / `"LongestCircumference"`), `"Distance"` (mutual-distance constraint between returned paths: `None` (default), `"Max"`, numeric, range — k-clique in path-space), `"Metric"` (path-space metric: `"Hausdorff"` (default — well-defined on mixed-length bundles), `"Frechet"`, `"MeanFrechet"`), `"MaxCliques"`. Operator form: `SelectPath[g, n, opts][paths]`. The old `Method -> "Frechet" | ...` option becomes the quoted-string `"Metric"` option, and the default flipped from `"Frechet"` to `"Hausdorff"` to close the silent-failure mode of Frechet alignment on mixed-length bundles. The old criterion strings `"Central"` / `"Peripheral"` become `"From" -> "Center"` / `"Periphery"`; folded-list chaining of criteria is dropped — chain via `//` instead. To recover the previous behaviour: `SelectPaths[g, paths, "Central"]` → `SelectPath[g, paths, All, "From" -> "Center", "Metric" -> "Frechet"]`; `SelectCycles[g, cycles, "ShortestCircumference"]` → `SelectCycle[g, cycles, All, "From" -> "ShortestCircumference"]`.

- **Breaking:** `OrthogonalCoordinates` no longer auto-discovers a frame from a centre, and the `"Origin"` option is gone. The centre is now a required positional argument and the frame must be supplied explicitly. New canonical signatures: `OrthogonalCoordinates[graph, c, {a1, ..., an}, v]` and `OrthogonalCoordinates[graph, c, {a1, ..., an}]`. To recover the previous behaviour: `OrthogonalCoordinates[graph, c, FindOrthogonalFrame[graph, c]]`. The dropped overloads (`OrthogonalCoordinates[g, axes, v]`, `OrthogonalCoordinates[g, c, v]`, `OrthogonalCoordinates[g, c]`, `OrthogonalCoordinates[g, InfraPoint[...], v]`) and the `"Origin"` option no longer match a pattern, so calls fall through unevaluated.

## 0.8.3

- New public `InfraExampleGraph[name, params]` — paclet-wide example-graph registry for guides, tutorials, and symbol-page demonstrations. Twelve keys covering the curvature spectrum (`"Grid"`, `"RectangleMesh"`, `"DiskMesh"`, `"SphereMesh"`, `"TriangularLattice"`, `"HexagonalLattice"`, `"RegularTree"`, `"Cayley"`) plus small named gems (`"Petersen"`, `"Heawood"`, `"MobiusKantor"`, `"Tutte"`). Mesh keys forward `MaxCellMeasure` / `AccuracyGoal` to `DiscretizeRegion`.
- Retire `InfraMode`. The path/cycle cases collapse to `SelectPaths[g, infra, "MostVisited"]` / `SelectCycles[g, infra, "MostVisited"]`; `SelectPaths` extended to accept `InfraLine`, `InfraRay`, and `InfraPencil` (mapped over its rays).

## 0.8.2

- New public `InfraMode[graph, infra]` — picks the most-visited realisation(s) from any single-`_List`-arg `Infra*` wrapper (point, segment, line, shell, plane, circle, ray, pencil), the single-realisation readout of the diffuse measure that `InfraSceneHighlight` paints. Same engine exposed bundle-level as a new `"MostVisited"` criterion on `SelectPaths` / `SelectCycles`.

## 0.8.1

- Concise usage-message style: every `::usage` is one sentence per signature, no inline tutorials.
- Retire `Tessellations` from the kernel; the corresponding wiki entry is archived.
- Documentation: `Layer` -> `Geometry` rename across guide notebooks.

## 0.8.0

- Projective layer aligned with the `Find*` -> `Infra*` multi-object pattern used by the Euclidean and Tropical layers.
- New wrapper heads `InfraRay` (multi-realisation) and `InfraPencil` (multi-constituent).
- `FindRay` (formerly the roster's `FindRayClass`); `FindCommonLine` / `FindCommonPoint` accept `InfraPoint` / `InfraSegment` / `InfraRay` / `InfraPencil` anchors.
- New predicate `UniqueConcurrentQ`.

## 0.7.3

- Rename `Aggregation` -> `SelectCoordinate` in `OrthogonalCoordinates`; bare-symbol values (`First`, `Min`, `Median`, ...); add `All` for tied-list preservation.

## 0.7.2

- `Find*` wrapper pass: `FindPoint` / `FindSegment` / `FindLine` / `FindShell` / `FindCircle` return `Infra*` heads with consistent accessors.
- Tropical operations split into a dedicated `TropicalOperations.wl`.
- Option rename pass for consistency with Wolfram conventions.

## 0.6.0

- Euclidean API cleanup: `FindParallel` placeholder allow-list entries `"Spectral"` / `"Resistance"` removed (only `"Metric"` and `"Embedding"` remain).
- `InfraInstance` accessor overloads `InfraInstance[inst, sym]` / `InfraInstance[inst, {sym1, ...}]`.
- `Viewers.wl` split into `Highlights.wl` (diffuse-rendering primitive `InfraSceneHighlight`) + `Viewers.wl` (`Manipulate`-based interactive viewers).

## 0.5.x

See git log for the v0.5 series (curvature engine, Tarski layer, `PathSpace.wl`, `FindShell` / `FindCircle` split, `Curvatures.wl`).
