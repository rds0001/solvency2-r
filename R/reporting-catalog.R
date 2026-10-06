# Explicit archived technical releases are independent of regulatory Contexts.
.s2_reporting_versions <- c("2.8.2" = "2_8_2", "2.8.2-hotfix" = "2_8_2_hotfix", "2.10.0" = "2_10_0")

#' Archived reporting definitions for an explicit technical release
#' @param technical_version One of `"2.8.2"`, `"2.8.2-hotfix"`, `"2.10.0"`.
#' @return Catalogue of modules, templates, tables and source-bound resources.
#' @details These are definition lookups, not filing obligations, validation
#'   results or automatic taxonomy selection. Baseline and hotfix may share URIs.
#' @examples
#' names(reporting_catalog("2.10.0")$tables)[1:3]
#' @export
reporting_catalog <- function(technical_version) {
  .s2_require(.s2_string(technical_version) && technical_version %in% names(.s2_reporting_versions),
    "technical_version", "choose 2.8.2, 2.8.2-hotfix or 2.10.0 explicitly", "UNSUPPORTED_PARAMETER")
  data <- .s2_load(paste0("reporting_", .s2_reporting_versions[[technical_version]], "_catalog.json"))
  .s2_require(identical(data$technical_version, technical_version), "technical_version", "catalog version mismatch", "RESOURCE_INTEGRITY")
  data
}

.s2_reporting_detail <- function(technical_version, category, descriptor) {
  name <- paste0("reporting_", .s2_reporting_versions[[technical_version]], "_", category, ".json.gz")
  manifest <- .s2_load("manifest.json")$sha256
  .s2_require(.s2_mapping(descriptor) && identical(descriptor$resource, name) &&
    name %in% names(manifest) && identical(manifest[[name]], descriptor$sha256),
    "technical_version", "archive resource binding mismatch", "RESOURCE_INTEGRITY")
  data <- .s2_load(name)
  .s2_require(identical(data$technical_version, technical_version), "technical_version", "detail version mismatch", "RESOURCE_INTEGRITY")
  data
}

#' Load one reporting definition category
#' @param category `"structure"`, `"cells"`, `"dictionary"` or `"rules"`.
#' @param technical_version Explicit archived technical release.
#' @return Hash-verified data preserving raw DPM field names and flags.
#' @details `IsEnabled` and `IncludeInXBRL` are archived definitions, not current
#'   supervisory activation decisions. Only the selected category is decompressed.
#' @examples
#' names(reporting_data("structure", "2.10.0")$tables)
#' @export
reporting_data <- function(category, technical_version) {
  .s2_require(.s2_string(category) && category %in% c("structure", "cells", "dictionary", "rules"),
    "category", "choose structure, cells, dictionary or rules", "UNSUPPORTED_PARAMETER")
  catalog <- reporting_catalog(technical_version)
  descriptor <- catalog$detail_resources[[category]]
  data <- .s2_reporting_detail(technical_version, category, descriptor)
  .s2_require(identical(data$source, catalog$source), "category", "detail source mismatch", "RESOURCE_INTEGRITY")
  .s2_require(identical(sort(names(data$tables)), unlist(descriptor$tables, use.names = FALSE)),
    "category", "detail table inventory mismatch", "RESOURCE_INTEGRITY")
  data
}

#' Definitions and axes for one reporting table
#' @param table_code Exact table code in the selected catalogue.
#' @param technical_version Explicit archived technical release.
#' @return Table, cells, axes, ordinates, positions, categorisations and open-axis
#'   restrictions. Open axes are not expanded and shaded cells are not zero inputs.
#' @examples
#' code <- names(reporting_catalog("2.10.0")$tables)[[1]]
#' reporting_table(code, "2.10.0")$status
#' @export
reporting_table <- function(table_code, technical_version) {
  catalog <- reporting_catalog(technical_version)
  .s2_require(.s2_string(table_code) && table_code %in% names(catalog$tables), "table_code", "unknown table in selected technical version", "UNSUPPORTED_PARAMETER")
  structure <- reporting_data("structure", technical_version)$tables
  definitions <- Filter(function(row) identical(row$TableCode, table_code), structure$mTable)
  .s2_require(length(definitions) == 1L, "table_code", "ambiguous table definition", "RESOURCE_INTEGRITY")
  definition <- definitions[[1L]]
  detail <- reporting_data("cells", technical_version)$tables
  cells <- Filter(function(row) identical(row$TableID, definition$TableID), detail$mTableCell)
  axes <- Filter(function(row) identical(row$TableID, definition$TableID), detail$mTableAxis)
  ids <- function(rows, key) unlist(lapply(rows, `[[`, key), use.names = FALSE)
  cell_ids <- ids(cells, "CellID"); axis_ids <- ids(axes, "AxisID")
  ordinates <- Filter(function(row) row$AxisID %in% axis_ids, detail$mAxisOrdinate)
  ordinate_ids <- ids(ordinates, "OrdinateID")
  positions <- Filter(function(row) row$CellID %in% cell_ids, detail$mCellPosition)
  .s2_require(all(vapply(positions, function(row) row$OrdinateID %in% ordinate_ids, logical(1))),
    "table_code", "cell position outside selected axes", "RESOURCE_INTEGRITY")
  list(technical_version = technical_version, source = catalog$source,
    status = "REFERENCE_ONLY", legal_applicability_verified = FALSE,
    validation_rules_executed = FALSE, open_axes_expanded = FALSE,
    table = definition, cells = cells, table_axes = axes,
    axes = Filter(function(row) row$AxisID %in% axis_ids, detail$mAxis),
    ordinates = ordinates, cell_positions = positions,
    categorisations = Filter(function(row) row$OrdinateID %in% ordinate_ids, detail$mOrdinateCategorisation),
    open_axis_restrictions = Filter(function(row) row$AxisID %in% axis_ids, detail$mOpenAxisValueRestriction))
}

.s2_reporting_metadata <- function(technical_version, kind) {
  catalog <- reporting_catalog(technical_version)
  descriptor <- catalog[[paste0(kind, "_resource")]]
  data <- .s2_reporting_detail(technical_version, kind, descriptor)
  keys <- if (kind == "validation_status") c("source", "snapshot_date", "sheet") else c("source_id", "archive_sha256")
  .s2_require(all(vapply(keys, function(key) identical(data[[key]], descriptor[[key]]), logical(1))),
    "technical_version", "archive evidence mismatch", "RESOURCE_INTEGRITY")
  data
}
