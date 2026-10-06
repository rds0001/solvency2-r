.s2_module_rule_codes <- function(metadata, validation_metadata, module) {
  codes <- character()
  for (record in metadata$modules[[module]]$preconditions) {
    fragment <- metadata$assertions[[record$assertion_key]]$fragment
    if (!.s2_string(fragment) || !startsWith(fragment, "s2md_") || nchar(fragment) <= 5L) stop("Module assertion lacks an unambiguous source rule code")
    codes <- c(codes, substring(fragment, 6L))
  }
  for (entry in validation_metadata$rows) {
    row <- entry$values; code <- row[["Rule code"]]
    if (!.s2_string(code) || !nzchar(code)) stop("Workbook row lacks an explicit rule code")
    relevant <- tryCatch(module %in% names(.s2_reporting_severities(row[["Severity and modules"]])), error = function(error) TRUE)
    if (relevant) codes <- c(codes, code)
  }
  as.list(sort(unique(codes)))
}
.s2_instance_rules <- function(codes, facts, reconciliation, entity, instant, indicators, module, technical_version,
  qualification_reference, rules, rule_source, metadata) {
  context <- list(entity = paste0(entity$scheme, ":", entity$identifier), period = instant, unit = "EXPLICIT_PER_FACT_CHECKED_AFTER_SELECTION")
  records <- lapply(facts, function(fact) list(cell = fact$business_code, context = context, fact = fact$atom))
  unit_by_cell <- .s2_object()
  for (row in reconciliation) unit_by_cell[as.character(row$cell_id)] <- list(facts[[row$input_index + 1L]]$unit)
  by_code <- .s2_object()
  for (rule in rules) by_code[[rule$ValidationCode]] <- c(by_code[[rule$ValidationCode]], list(rule))
  tables <- .s2_object()
  lookup <- function(code) {
    if (!code %in% names(tables)) tables[[code]] <<- reporting_table(code, technical_version)
    tables[[code]]
  }
  output <- list()
  for (code in codes) {
    item <- list(validation_code = code, status = "REVIEW_REQUIRED", full_report_validated = FALSE)
    item <- tryCatch({
      activation <- reporting_validation_status(code, technical_version, module); activation$input_hash <- NULL
      item$activation <- activation
      if (identical(activation$active_in_snapshot, FALSE)) item$status <- "NOT_ACTIVE_IN_SNAPSHOT" else
        if (is.null(activation$active_in_snapshot)) item$reason <- activation$reason else {
          precondition <- reporting_rule_precondition(code, indicators, technical_version, module); precondition$input_hash <- NULL
          item$precondition <- precondition
          if (!precondition$precondition_met) item$status <- "FILING_PRECONDITION_FALSE" else {
            selected <- by_code[[code]]
            if (length(selected) != 1L) stop("No unique archived DPM rule")
            rule <- selected[[1L]]
            if (.s2_truth(rule$Scope)) {
              result <- .s2_reporting_check_scoped(rule, lookup, records, context, technical_version,
                qualification_reference, rule_source, function() metadata$dictionary_namespaces, unit_by_cell)
              item$scope_checks <- result$scope_checks
            } else {
              prepared <- .s2_reporting_bind_selectors(rule, lookup, records, context)
              selected_cells <- unique(unlist(lapply(prepared$evidence, `[[`, "cell_ids")))
              units <- Filter(Negate(is.null), lapply(selected_cells, function(id) unit_by_cell[[as.character(id)]]))
              if (length(unique(vapply(units, .s2_fingerprint, character(1)))) > 1L) stop("Cross-unit expression requires qualified dimensional evaluation")
              result <- .s2_reporting_check_bound(rule, prepared$bindings, technical_version, qualification_reference, rule_source, function() metadata$dictionary_namespaces)
              item$cell_bindings <- prepared$evidence
            }
            item$status <- if (result$value) "PASS" else "FAIL"; item$expression <- result; item$severity <- activation$severity
          }
        }
      item
    }, error = function(error) {
      if (inherits(error, "s2_error") && identical(error$code, "RESOURCE_INTEGRITY")) stop(error)
      item$reason <- conditionMessage(error)
      if (inherits(error, "s2_error")) item$error_code <- error$code
      item
    })
    output[[length(output) + 1L]] <- item
  }
  output
}

#' Assemble an offline XBRL draft from explicit DPM cell facts
#' @param facts Ordered list of records containing `business_code`, `atom`, and
#'   optionally `open_dimensions` and `unit`. Full archived cell codes are required.
#' @param technical_version Explicit archived technical release.
#' @param module Exact reporting module code.
#' @param entity Named list with explicit `scheme` and `identifier`.
#' @param instant Canonical ISO reporting instant, `YYYY-MM-DD`.
#' @param indicators Named list of explicit Boolean filing indicators.
#' @param qualification_reference External reporting-scope qualification reference.
#' @param validation_codes Optional ordered rule-code list, or `"ALL_MODULE"` to
#'   attempt all archived module/workbook candidates. Unsupported checks stay visible.
#' @param filing_overrides List of explicit [NumericOverride()] entries for
#'   archived filing precision parameters, with original values and reasons audited.
#' @return XML text and SHA-256, cell reconciliation, selected rule checks, currency
#'   and precision findings, override audit and outstanding validation obligations.
#' @details This returns `DRAFT_XBRL_INSTANCE`, never submission approval. It makes
#'   no network request, writes no files, invents no facts, and performs no rounding.
#'   Native R serialization is deterministic; XML hashes bind the returned bytes,
#'   not another language's serializer. XML schema validation remains separate.
#' @examples
#' names(reference_dataset("reference_reporting_portfolios.json"))
#' @export
reporting_instance <- function(facts, technical_version, module, entity, instant, indicators,
  qualification_reference, validation_codes = NULL, filing_overrides = list()) {
  .s2_require(.s2_string(qualification_reference) && nzchar(trimws(qualification_reference)),
    "qualification_reference", "external reporting-scope qualification required", "MISSING_INPUT")
  .s2_require(is.list(facts) && is.null(names(facts)) && length(facts) > 0L, "facts", "nonempty explicit fact list required")
  all_module <- identical(validation_codes, "ALL_MODULE")
  codes <- if (is.null(validation_codes) || all_module) list() else validation_codes
  .s2_require(is.list(codes) && is.null(names(codes)) && all(vapply(codes, function(code) .s2_string(code) && nzchar(code), logical(1))) && !anyDuplicated(unlist(codes)),
    "validation_codes", "unique explicit rule codes required")
  catalog <- reporting_catalog(technical_version)
  .s2_require(.s2_string(module) && module %in% names(catalog$modules), "module", "unknown module in selected technical version", "UNSUPPORTED_PARAMETER")
  selected <- catalog$modules[[module]]; templates <- .s2_object()
  for (row in selected$templates) {
    if (!.s2_string(row$code) || !grepl("^[A-Z]+\\.[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}$", row$code)) .s2_fail("INVALID_INPUT", "reporting_instance", "unsupported archived template variant code")
    family <- sub("\\.[^.]+$", "", row$code)
    templates[[family]] <- unique(c(templates[[family]], unlist(row$table_codes, use.names = FALSE)))
  }
  .s2_require(.s2_mapping(indicators) && length(indicators) > 0L && all(names(indicators) %in% names(templates)) &&
    all(vapply(indicators, function(value) is.logical(value) && length(value) == 1L && !is.na(value), logical(1))), "indicators", "explicit Boolean indicators from selected module required")
  structure <- reporting_data("structure", technical_version)$tables
  cells <- reporting_data("cells", technical_version)$tables$mTableCell
  dictionary <- reporting_data("dictionary", technical_version)$tables
  metadata <- .s2_reporting_metadata(technical_version, "xbrl_preconditions")
  if (all_module) codes <- tryCatch(.s2_module_rule_codes(metadata, .s2_reporting_metadata(technical_version, "validation_status"), module),
    error = function(error) .s2_fail("REVIEW_REQUIRED", "validation_codes", conditionMessage(error)))
  .s2_require(length(metadata$dictionary_elements) > 0L, "resources", "original dictionary declarations required", "RESOURCE_INTEGRITY")
  table_codes <- stats::setNames(lapply(structure$mTable, `[[`, "TableCode"), vapply(structure$mTable, function(row) as.character(row$TableID), character(1)))
  module_tables <- unique(unlist(templates, use.names = FALSE)); by_code <- .s2_object()
  for (cell in cells) if (table_codes[[as.character(cell$TableID)]] %in% module_tables)
    by_code[[cell$BusinessCode]] <- c(by_code[[cell$BusinessCode]], list(cell))
  bound <- mapped <- reconciliation <- list(); represented <- character()
  xml <- tryCatch({
    for (i in seq_along(facts)) {
      fact <- facts[[i]]
      if (!.s2_mapping(fact) || !all(c("business_code", "atom") %in% names(fact)) || length(setdiff(names(fact), c("business_code", "atom", "open_dimensions", "unit"))))
        stop("Explicit business_code/atom and supported fact fields required")
      code <- fact$business_code
      if (!.s2_string(code) || length(by_code[[code]]) != 1L) stop("Unknown or ambiguous cell in selected module")
      cell <- by_code[[code]][[1L]]; table_code <- table_codes[[as.character(cell$TableID)]]
      owners <- names(templates)[vapply(templates, function(codes) table_code %in% codes, logical(1))]
      positive <- owners[vapply(owners, function(key) identical(indicators[[key]], TRUE), logical(1))]
      if (!length(positive)) stop("Fact is not covered by a positive filing indicator")
      represented <- unique(c(represented, positive))
      dimensions <- if ("open_dimensions" %in% names(fact)) fact$open_dimensions else .s2_object()
      row <- .s2_reporting_bind_cell(cell, fact$atom, dimensions, dictionary, metadata$dictionary_namespaces, metadata$dictionary_elements, fact$unit)
      bound[[i]] <- row$fact; mapped[[i]] <- row
      reconciliation[[i]] <- list(input_index = i - 1L, business_code = code, cell_id = cell$CellID, table_code = table_code,
        positive_templates = as.list(sort(positive)), metric = row$fact$metric, omitted_explicit_defaults = row$omitted_explicit_defaults,
        schema_member = row$schema_declaration$member, schema_sha256 = row$schema_declaration$sha256)
    }
    if (!setequal(represented, names(indicators)[vapply(indicators, isTRUE, logical(1))])) stop("Positive filing indicator without facts")
    namespaces <- metadata$dictionary_namespaces; namespaces$iso4217 <- "http://www.xbrl.org/2003/iso4217"
    .s2_write_instance(selected$schema_uri, entity, instant, indicators, bound, namespaces)
  }, error = function(error) {
    if (inherits(error, "s2_error")) stop(error)
    .s2_fail("INVALID_INPUT", "reporting_instance", conditionMessage(error))
  })
  currency_checks <- .s2_instance_currency(mapped, dictionary)
  precision <- tryCatch({
    filing <- .s2_filing_policy(catalog, filing_overrides)
    list(filing = filing, checks = .s2_instance_precision(mapped, filing$policy))
  }, error = function(error) .s2_fail("INVALID_INPUT", "filing_overrides", conditionMessage(error)))
  checks <- list()
  if (length(codes)) {
    rules <- reporting_data("rules", technical_version)
    checks <- .s2_instance_rules(codes, facts, reconciliation, entity, instant, indicators, module, technical_version,
      qualification_reference, rules$tables$vValidationRuleExpressions, rules$source, metadata)
  }
  mode <- if (all_module) "ALL_MODULE" else "EXPLICIT"
  list(status = "DRAFT_XBRL_INSTANCE", technical_version = technical_version, module = module,
    xml = xml, xml_sha256 = digest::digest(enc2utf8(xml), algo = "sha256", serialize = FALSE), source = catalog$source,
    xbrl_source_id = metadata$source_id, xbrl_archive_sha256 = metadata$archive_sha256, reconciliation = reconciliation,
    qualification_reference = qualification_reference,
    input_hash = .s2_fingerprint(list(facts = facts, technical_version = technical_version, module = module, entity = entity,
      instant = instant, indicators = indicators, qualification_reference = qualification_reference, validation_codes = codes,
      validation_selection = mode, filing_policy_hash = precision$filing$hash)),
    selected_rule_checks = checks, all_archived_rules_executed = FALSE,
    rule_selection = list(mode = mode, codes = codes, all_module_candidates_attempted = all_module,
      attempted_count = length(checks), unresolved_codes = lapply(Filter(function(r) r$status == "REVIEW_REQUIRED", checks), `[[`, "validation_code")),
    currency_checks = currency_checks, precision_checks = precision$checks, filing_policy_hash = precision$filing$hash,
    filing_parameter_overrides = precision$filing$audit,
    filing_policy_status = if (length(precision$filing$audit)) "ANALYST_OVERRIDE_NOT_APPROVED" else "ARCHIVED_SOURCE_PARAMETERS",
    xml_well_formed = TRUE, cell_metric_datatypes_checked = TRUE, full_report_validated = FALSE, submission_ready = FALSE,
    pending_checks = as.list(c("LEGAL_AND_TEMPLATE_COMPLETENESS", "ENTITY_IDENTIFIER_QUALIFICATION", "REPORTING_CURRENCY_AND_PRECISION",
      "ROW_KEYS_AND_OPEN_AXIS_HIERARCHIES", "XML_SCHEMA_AND_XBRL_DIMENSIONS", "BUSINESS_RULES_AND_DEACTIVATIONS", "NATIONAL_FILING_REQUIREMENTS")))
}
