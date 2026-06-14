# Tableau Workbook Parser (R6)

Initialize the parser from a `.twb` or `.twbx` path.

Return the TWBX manifest (if available).

Return TWBX extract entries.

Return TWBX image entries.

Extract files from the TWBX to disk.

Fields placed on visual shelves for one or all worksheets.

Detailed filter configuration for one or all worksheets.

Axis configuration for one or all worksheets.

Sort directives for one or all worksheets.

Worksheets embedded in one or all dashboards.

Full zone layout with container hierarchy.

Dashboard and workbook actions.

Calculated field complexity classifications.

Field usage matrix across worksheets.

Full replication brief for the workbook or a single dashboard.

Validate relationships; optionally stop on failure.

Print a concise summary of parsed content.

## Format

An R6 class generator.

## Arguments

- path:

  Path to a `.twb` or `.twbx` file.

- types:

  Optional vector of types (e.g., `"image"`, `"extract"`).

- pattern:

  Optional regex to match archive paths.

- files:

  Optional explicit archive paths to extract.

- exdir:

  Output directory (defaults to parser's twbx dir or tempdir()).

- sheet:

  Optional worksheet name.

- include_parameters:

  Logical; include parameter fields. Default `FALSE`.

- include_filters:

  Include filter appearances. Default `TRUE`.

- include_shelves:

  Include shelf appearances. Default `TRUE`.

- wide:

  Return wide format (one col per sheet). Default `FALSE`.

- dashboard:

  Optional dashboard name to scope the brief.

- include_sql:

  Include custom SQL blocks. Default `TRUE`.

- include_formulas:

  Add `formula_pretty` to calculated fields. Default `TRUE`.

- format:

  `"list"` (default) or `"text"`.

- error:

  If `TRUE`, [`stop()`](https://rdrr.io/r/base/stop.html) when
  validation fails.

## Details

Create a parser for Tableau `.twb` / `.twbx` files. On initialization,
the parser reads the XML and precomputes relationships, joins, fields,
calculated fields, inferred relationships, and datasource details. For
`.twbx`, it also extracts the largest `.twb` and records a manifest.

## Fields

- path:

  Path to the `.twb` or `.twbx` file on disk.

- xml_doc:

  Parsed `xml2` document of the workbook.

- twbx_path:

  Original `.twbx` path if the workbook was packaged.

- twbx_dir:

  Directory where the `.twbx` was extracted.

- twbx_manifest:

  Tibble of `.twbx` contents from
  [`twbx_list()`](https://prigasg.github.io/twbparser/reference/twbx_list.md).

- relations:

  Tibble of `<relation>` nodes from
  [`extract_relations()`](https://prigasg.github.io/twbparser/reference/extract_relations.md).

- joins:

  Tibble of join clauses from
  [`extract_joins()`](https://prigasg.github.io/twbparser/reference/extract_joins.md).

- relationships:

  Tibble of modern relationships from
  [`extract_relationships()`](https://prigasg.github.io/twbparser/reference/extract_relationships.md).

- inferred_relationships:

  Tibble of inferred relationship pairs by name and role.

- datasource_details:

  List containing `data_sources`, `parameters`, and `all_sources`.

- fields:

  Tibble of raw fields with table information.

- calculated_fields:

  Tibble of calculated fields.

- last_validation:

  Result from `validate()` as list with `ok` and `issues` elements.

## Methods

- new(path):

  Create a parser from `.twb` or `.twbx` file.

- get_twbx_manifest():

  Return `.twbx` manifest tibble.

- get_twbx_extracts():

  Return `.twbx` extract entries.

- get_twbx_images():

  Return `.twbx` image entries.

- extract_twbx_assets(types, pattern, files, exdir):

  Extract files from `.twbx` archive.

- get_relations():

  Return relations tibble.

- get_joins():

  Return joins tibble.

- get_relationships():

  Return modern relationships tibble.

- get_inferred_relationships():

  Return inferred relationship pairs.

- get_datasources():

  Return datasource details tibble.

- get_parameters():

  Return parameters tibble.

- get_datasources_all():

  Return all sources tibble.

- get_fields():

  Return raw fields tibble.

- get_calculated_fields(pretty = FALSE, strip_brackets = FALSE, wrap =
  100L):

  Return calculated fields tibble. When `pretty = TRUE`, includes a
  `formula_pretty` column with line breaks and indentation.

- validate(error = FALSE):

  Validate relationships. Stops execution if `error = TRUE`.

- summary():

  Print a brief summary to console.
