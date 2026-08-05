# adac-edn-converter

Convert ADAC Flattened XSD (or LandXML-1.2) into SchemaCraft EDN (`{:schema :typedefs :elements}`).

See `doc/adac-importer-pack/` for the EDN contract and mapping notes.

# Instance XML ↔ SchemaCraft document EDN (authority workflow)

Import existing ADAC XML for editing in SchemaCraft against `target/adac-v600.edn`, then export Instance EDN back to XML for authority submission. SchemaCraft owns validation/highlighting; the converter is **lenient** on import and enforces XSD on the export gate.

```bash
# Ensure full schema exists (schema :id must match Composer)
lein run -- --full
# → target/adac-v600.edn

# Full authority round-trip (xml → instance-graph → xml → XSD validate)
lein run -- round-trip
# → target/adac-instance.edn + target/adac-roundtrip.xml

# Or step by step:
lein run -- xml-to-edn [schema.edn] [sample.xml] [out-instance.edn]
lein run -- edn-to-xml [schema.edn] [instance.edn] [out.xml]
lein run -- validate-xml [xsd-path] [document.xml]

# Optional: assembled ADAC body map only (no Document graph)
lein run -- xml-to-edn --assembled
```

**Fidelity gates (automated):** full Sample assembled deep-equality for XML↔EDN↔graph; Geometry Path/Ring vertices retained; Sample export validates against Flattened XSD.

**Lenient import:** known Element keys only; missing required nillable → `:schemacraft/nil`; bad types/enums → raw string; unknown tags/attrs dropped. Does not run XSD validation on import.

**EDN shape (default):** `:schemacraft/format :instance-graph` document with synthetic `:Document` root and `:ADAC` child — importable in SchemaCraft Composer. Schema `:id` is taken from `target/adac-v600.edn` (must match the schema loaded in Composer).

## Usage

```bash
# Milestone 1 slice (default): String_* / enums + Project + Sewerage MaintenanceHoles
lein run --
# → target/adac-v600-sewerage-mh.edn

lein run -- path/to/ADAC_V600_Flattened.xsd out.edn --slice sewerage-mh

# Full Flattened XSD (Milestone 2)
lein run -- --full
# → target/adac-v600.edn

# ADAC v5.0.1 modular pack (xs:include resolved by the parser)
lein run -- resources/adac/ADAC_v501_XSD/ADAC_V501.xsd target/adac-v501.edn --full
# → target/adac-v501.edn  (schema/version from XSD @version = 5.0.1)

# LandXML-1.2 (shared XSD core + LandXML front-end)
lein run -- --schema landxml
# → target/landxml-1.2.edn

lein run -- --schema bcib
# → target/bcib.edn  (from doc/bcib/bcib-schema.edn + behaviour-overlay.csv)
```

Default ADAC XSD: `resources/adac/ADAC_V600_Flattened.xsd`  
ADAC v5.0.1 pack: `resources/adac/ADAC_v501_XSD/ADAC_V501.xsd` (modular; parser follows `xs:include`)  
Default LandXML XSD: `resources/landxml/LandXML-1.2.xsd`

### LandXML refs (three different things)

| Mechanism | Example | EDN role |
|-----------|---------|----------|
| Element `ref=` | `<xs:element ref="Feature"/>` | Builds the content-model tree (`:child-refs`) |
| Soft-link attributes | `pntRef`, `mntRef`, `refStart` | Scalars with `:placement :parent-property` (instance name pointers) |
| Identity constraints | `xs:unique` / `xs:key` / `xs:keyref` + `xpath=` | Validation metadata only — **not** emitted; xpath does not invent children |

Recursive nesting (e.g. Feature inside Feature) uses `:collection` + `:collection/item-ref` so an Element never lists its own id in `:child-refs` (avoids SchemaCraft UI loops).

**GeomList-style ordered choices** (SDK `CoordGeom::GeomList`): nested `xs:choice maxOccurs="unbounded"` becomes `{Parent}_list` → `{Parent}_Fragment` (choice of Line/Curve/Spiral/…). Homogeneous multi-occurs particles become `{Name}_list` → item `{Name}` so lists and items stay distinct in the UI.

**Type forms (Alignment / Parcel / PlanFeature):** unbounded choice content is flattened into a sequence with visible `CoordGeom` (`CoordGeom_list` of Line/Curve/Spiral/…) and `Feature_list`. Segment types are CAD `:host-event`s with `:mode :self` (see `doc/landxml/schemacraft-host-event-v2.md`).

Samples / SDK (guidance only): `doc/landxml/IS185989_0.xml`, `doc/landxml/LandXMLSDK-1.2-08182008/`, fixtures under `test/fixtures/landxml-*.xml`.

## Tests

```bash
lein test
```

## Layout

| Path | Role |
|------|------|
| `src/adac_edn_converter/xsd/parse.clj` | XSD → IR |
| `src/adac_edn_converter/map/typedefs.clj` | TypeDefs |
| `src/adac_edn_converter/map/elements.clj` | Elements |
| `src/adac_edn_converter/convert.clj` | Orchestration + slice |
| `src/adac_edn_converter/edn/emit.clj` | Pretty-print EDN |
| `src/adac_edn_converter/instance/schema_index.clj` | Schema bundle lookup for instance I/O |
| `src/adac_edn_converter/instance/graph.clj` | Assembled map ↔ `:instance-graph` document |
| `src/adac_edn_converter/instance/xml/read.clj` | Instance XML → EDN |
| `src/adac_edn_converter/instance/xml/write.clj` | Instance EDN → XML |
| `src/adac_edn_converter/xsd/validate.clj` | JDK XSD validation |
| `src/adac_edn_converter/core.clj` | CLI |

## License

Eclipse Public License 2.0 (see LICENSE).
