# Register NSE column names to silence R CMD check "no visible binding" notes.
utils::globalVariables(c(
  # columns from unzip() listing + our manifests
  "Name", "Length", "Date", "type", "size_bytes",
  # generic field/graph columns
  "name", "formula", "inputs", "from", "output", "to", "label", "field",
  # datasource/details joins
  "location", "location_named", "connection_type", "connection_class", "field_count",
  "datasource_name", "connection_caption", "datasource", "primary_table",
  "connection_id", "connection_target",
  # joins/relationships
  "left_field", "right_field", "operator", "left_table", "right_table",
  # inferred relationships
  "table_clean", "field_clean", "is_parameter", "table_use", "field_use",
  "semantic_role", "table_l", "table_r", "field_l", "field_r", "key",
  "field_lower",
  # validators
  "left_tok", "right_tok", "left_base", "right_base", "left_ok", "right_ok",
  # insights / page composition
  "value", "palette_name", "kind", "detail", "scope",
  "dashboard", "mark_types", "filters", "chart_types", "integer_", "page_type",
  # sheet_details
  "shelf", "field_ref", "field_instance", "aggregation",
  "filter_class", "include_mode", "members", "range_min", "range_max",
  "reversed", "include_zero", "scale_type",
  "sort_order", "sort_by",
  # dashboard_details
  "action_name", "action_type", "source_sheets", "target_sheet", "run_on",
  "url", "layout_type", "parent_zone_id", "component_type", "zone_id",
  "sheet"
))
