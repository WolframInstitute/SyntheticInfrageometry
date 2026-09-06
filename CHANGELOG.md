# Changelog

## Unreleased

- **Breaking:** `FindInfraCircle` joins the `Method` ladder. New option `Method -> Automatic | "Exhaustive" | {"Exhaustive", "Pruning" -> spec} | "Greedy" | "RandomGreedy"`, and the count default moves from `All` to one witness: `FindInfraCircle[g, c, r]` now returns one shortest separating circle, deterministic and streamed off the circle pool on the certified class; `FindInfraCircle[g, c, r, All]` is the pool as before. The class is the same under every `Method`: a bounded count streams circles off the pool's atoms in candidate (`"Greedy"`, `"Exhaustive"`) or random (`"RandomGreedy"`) order, and off the certified class every `Method` runs the same length sweep, where `"Pruning"` caps the cycles kept per length. Code that read the whole family without a count should pass `All`.

- **Breaking:** `FindInfraPolygon` and `FindInfraTriangle` take `Method -> Automatic` (was `"Exhaustive"`) and a count default of one witness (was `All`), like every ladder symbol. A bounded count streams that many geodesics per side and reads the first members of their product, instead of forming the whole product and discarding; `All` still forms the product. A bad `Method` raises `FindInfraPolygon::badmethod` / `FindInfraTriangle::badmethod` (was `FindInfraSegment::badmethod`). The count-less witness of a corner polygon may retrace a side; the class admits it.

- **Breaking:** `Method -> Automatic` on a bounded or absent count now resolves to `"Greedy"` on every `Find*` / `Extend*` ladder symbol (`All` still gives `"Exhaustive"`). The default witness is deterministic and reproducible without `SeedRandom`; the random-order descent is `Method -> "RandomGreedy"`, explicit only. Code that relied on the 2026-09-03 random default should say so. On the count-less two-point walk (`kspec Infinity`, no stopping condition, constraint-only rules) `"Greedy"` — and so the default — is the shortest path, the canonical witness.

- **Breaking:** `FindInfraParallel` returns one class under every `Method`: the inextensible geodesics through `p` inside the level set `{v : d(v, line) == d(p, line)}`, each in canonical orientation. Previously `"Exhaustive"` kept only the longest chain through each seed edge and `"Greedy"` emitted every inextensible chain in both orientations; the two now agree, and a `p` isolated in its level set gives `InfraLine[{}]` under every `Method` (the greedy used to return `{{p}}`). Built on a pair pool over the distance matrix, streamed lazily for bounded counts.

- `FindInfraShell`, `FindInfraBisectingHyperplane`, `FindInfraEllipticShell`: the lazy peel under `Method -> "Greedy"` / `"RandomGreedy"` visits each subset once. `Method -> "Greedy"` with `All` on a 5×5 grid went from minutes to under a second; the class is unchanged.

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
