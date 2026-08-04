# adac-edn-converter

Convert ADAC Flattened XSD (or LandXML-1.2) into SchemaCraft EDN (`{:schema :typedefs :elements}`).

See `doc/adac-importer-pack/` for the EDN contract and mapping notes.

## Usage

```bash
# Milestone 1 slice (default): String_* / enums + Project + Sewerage MaintenanceHoles
lein run --
# → target/adac-v600-sewerage-mh.edn

lein run -- path/to/ADAC_V600_Flattened.xsd out.edn --slice sewerage-mh

# Full Flattened XSD (Milestone 2)
lein run -- --full
# → target/adac-v600.edn

# LandXML-1.2 (shared XSD core + LandXML front-end)
lein run -- --schema landxml
# → target/landxml-1.2.edn

lein run -- --schema bcib
# → target/bcib.edn  (from doc/bcib/bcib-schema.edn + behaviour-overlay.csv)

# Instance XML ↔ EDN (requires target/adac-v600.edn)
lein run -- xml-to-edn [schema.edn] [sample.xml] [out-instance.edn]
lein run -- edn-to-xml [schema.edn] [instance.edn] [out.xml]
lein run -- validate-xml [xsd-path] [document.xml]
```

Default ADAC XSD: `resources/adac/ADAC_V600_Flattened.xsd`  
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
| `src/adac_edn_converter/instance/xml/read.clj` | Instance XML → EDN |
| `src/adac_edn_converter/instance/xml/write.clj` | Instance EDN → XML |
| `src/adac_edn_converter/xsd/validate.clj` | JDK XSD validation |
| `src/adac_edn_converter/core.clj` | CLI |

## License

Eclipse Public License 2.0 (see LICENSE).
