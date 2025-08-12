pkgname <- "twbparser"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "twbparser-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('twbparser')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("build_dependency_graph")
### * build_dependency_graph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: build_dependency_graph
### Title: Build a field dependency graph from calculated fields
### Aliases: build_dependency_graph

### ** Examples

## Not run: 
##D fields <- tibble::tibble(
##D   name = c("X_plus_Y", "Z"),
##D   formula = c("[X] + [Y]", "[X_plus_Y] * 2")
##D )
##D g <- build_dependency_graph(fields)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("build_dependency_graph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_calculated_fields")
### * extract_calculated_fields

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_calculated_fields
### Title: Extract calculated fields from a TWB
### Aliases: extract_calculated_fields

### ** Examples

## Not run: 
##D xml <- xml2::read_xml("inst/extdata/sample.twb")
##D calc <- extract_calculated_fields(xml)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_calculated_fields", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_columns_with_table_source")
### * extract_columns_with_table_source

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_columns_with_table_source
### Title: Extract columns with their source tables from a TWB
### Aliases: extract_columns_with_table_source

### ** Examples

## Not run: 
##D xml <- xml2::read_xml("inst/extdata/sample.twb")
##D fields <- extract_columns_with_table_source(xml)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_columns_with_table_source", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_datasource_details")
### * extract_datasource_details

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_datasource_details
### Title: Extract datasource details from a Tableau TWB
### Aliases: extract_datasource_details

### ** Examples

## Not run: 
##D xml <- xml2::read_xml("inst/extdata/sample.twb")
##D out <- extract_datasource_details(xml)
##D out$data_sources
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_datasource_details", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_joins")
### * extract_joins

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_joins
### Title: Extract Tableau join clauses from <relation type="join"> nodes
### Aliases: extract_joins

### ** Examples

## Not run: 
##D xml <- xml2::read_xml("inst/extdata/sample.twb")
##D extract_joins(xml)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_joins", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_named_connections")
### * extract_named_connections

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_named_connections
### Title: Extract <named-connection> entries from a TWB
### Aliases: extract_named_connections

### ** Examples

## Not run: 
##D xml <- xml2::read_xml("inst/extdata/sample.twb")
##D extract_named_connections(xml)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_named_connections", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_parameters")
### * extract_parameters

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_parameters
### Title: Extract parameter fields from a TWB
### Aliases: extract_parameters

### ** Examples

## Not run: 
##D xml <- xml2::read_xml("inst/extdata/sample.twb")
##D params <- extract_parameters(xml)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_parameters", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_raw_fields")
### * extract_raw_fields

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_raw_fields
### Title: Extract non-calculated, non-parameter fields from a TWB
### Aliases: extract_raw_fields

### ** Examples

## Not run: 
##D xml <- xml2::read_xml("inst/extdata/sample.twb")
##D raw_fields <- extract_raw_fields(xml)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_raw_fields", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_relations")
### * extract_relations

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_relations
### Title: Extract all <relation> tags from a TWB
### Aliases: extract_relations

### ** Examples

## Not run: 
##D xml <- xml2::read_xml("inst/extdata/sample.twb")
##D extract_relations(xml)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_relations", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_relationships")
### * extract_relationships

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_relationships
### Title: Extract modern relationships from a Tableau TWB
### Aliases: extract_relationships

### ** Examples

## Not run: 
##D xml <- xml2::read_xml("inst/extdata/sample.twb")
##D extract_relationships(xml)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_relationships", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("infer_implicit_relationships")
### * infer_implicit_relationships

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: infer_implicit_relationships
### Title: Infer implicit relationships between tables from field metadata
### Aliases: infer_implicit_relationships

### ** Examples

## Not run: 
##D fields <- extract_columns_with_table_source(xml)
##D infer_implicit_relationships(fields)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("infer_implicit_relationships", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_dependency_graph")
### * plot_dependency_graph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_dependency_graph
### Title: Plot a field dependency graph
### Aliases: plot_dependency_graph

### ** Examples

## Not run: 
##D g <- build_dependency_graph(fields)
##D plot_dependency_graph(g, fields)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_dependency_graph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("twbx_list")
### * twbx_list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: twbx_list
### Title: List contents of a Tableau .twbx
### Aliases: twbx_list

### ** Examples

## Not run: 
##D twbx_list(system.file("extdata", "example.twbx", package = "twbparser"))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("twbx_list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("validate_relationships")
### * validate_relationships

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: validate_relationships
### Title: Validate relationships against available datasources and fields
### Aliases: validate_relationships

### ** Examples

## Not run: 
##D res <- validate_relationships(parser)
##D if (!res$ok) print(res$issues)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("validate_relationships", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
