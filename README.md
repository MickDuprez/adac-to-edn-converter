# adac-edn-converter

Convert ADAC Flattened XSD into SchemaCraft EDN (`{:schema :typedefs :elements}`).

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

# Instance XML ↔ EDN (requires target/adac-v600.edn)
lein run -- xml-to-edn [schema.edn] [sample.xml] [out-instance.edn]
lein run -- edn-to-xml [schema.edn] [instance.edn] [out.xml]
lein run -- validate-xml [xsd-path] [document.xml]
```

Default XSD: `resources/adac/ADAC_V600_Flattened.xsd`

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
