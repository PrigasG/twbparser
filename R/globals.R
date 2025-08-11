# Register NSE column names to silence R CMD check "no visible binding" notes.
utils::globalVariables(c(
  # columns from unzip() listing + our manifests
  "Name","Length","Date","type","size_bytes",
  # generic field/graph columns
  "name","formula","inputs","from","output","to","label","field",
  # datasource/details joins
  "location","location_named","connection_type","connection_class","field_count",
  "datasource_name","connection_caption","datasource","primary_table",
  "connection_id","connection_target",
  # joins/relationships
  "left_field","right_field","operator","left_table","right_table",
  "left_source","right_source",
  # inferred relationships
  "table_clean","field_clean","is_parameter","table_use","field_use","semantic_role",
  "table_l","table_r","field_l","field_r","key",
  # validators
  "left_tok","right_tok","left_base","right_base","left_ok","right_ok"
))
