.s2_selector_fields <- function(selector) {
  body <- substr(selector, 2L, nchar(selector) - 1L)
  chars <- strsplit(body, "", fixed = TRUE)[[1L]]
  parts <- character(); start <- position <- 1L; depth <- 0L; quote <- NULL
  while (position <= length(chars)) {
    char <- chars[[position]]
    if (!is.null(quote)) {
      if (char == quote) {
        if (position < length(chars) && chars[[position + 1L]] == quote) { position <- position + 2L; next }
        quote <- NULL
      }
    } else if (char %in% c("'", '"')) quote <- char else
      if (char %in% c("(", "[", "{")) depth <- depth + 1L else
      if (char %in% c(")", "]", "}")) depth <- depth - 1L else
      if (char == "," && depth == 0L) { parts <- c(parts, substr(body, start, position - 1L)); start <- position + 1L }
    position <- position + 1L
  }
  parts <- c(parts, substring(body, start)); output <- .s2_object()
  for (part in parts) {
    separator <- regexpr(":", part, fixed = TRUE)[[1L]]
    key <- trimws(substr(part, 1L, separator - 1L)); value <- trimws(substring(part, separator + 1L))
    .s2_require(separator > 0L && nzchar(key) && nzchar(value) && !key %in% names(output),
      "selector", "ambiguous selector attributes", "UNSUPPORTED_METHOD")
    output[[key]] <- value
  }
  .s2_require(depth == 0L && is.null(quote), "selector", "unbalanced selector", "UNSUPPORTED_METHOD")
  output
}
.s2_reporting_bound_fact <- function(record) {
  if (!.s2_mapping(record) || !setequal(names(record), c("fact", "dimensions"))) return(list(.s2_reporting_fact(record), NULL))
  value <- .s2_reporting_fact(record$fact); declared <- record$dimensions
  .s2_require(is.list(declared) && is.null(names(declared)) && length(declared) <= 1000L, "dimensions", "explicit bounded dimension declarations required")
  dimensions <- .s2_object()
  for (item in declared) {
    .s2_require(.s2_mapping(item) && setequal(names(item), c("dimension", "value")), "dimensions", "dimension name and tagged value required")
    name <- item$dimension
    .s2_require(.s2_mapping(name) && setequal(names(name), c("namespace", "local_name")), "dimensions", "expanded dimension QName required")
    name <- tryCatch(do.call(.s2_reporting_qname, name), error = function(error) .s2_fail("INVALID_INPUT", "dimensions", conditionMessage(error)))
    key <- .s2_qname_key(name)
    .s2_require(nzchar(name$namespace) && !key %in% names(dimensions), "dimensions", "nonempty namespace and unique dimension required")
    atom <- .s2_reporting_fact(item$value)
    .s2_require(!inherits(atom, "ReportingNumber") || atom$tolerance == 0, "dimensions", "numeric dimension values require exact precision")
    dimensions[key] <- list(atom)
  }
  list(value, dimensions)
}
.s2_reporting_filter <- function(expression, candidate, namespaces) {
  if (is.null(expression) || !nzchar(expression)) return(TRUE)
  .s2_require(!is.null(candidate[[1L]]), "filter", "an absent scalar is not a candidate fact", "UNSUPPORTED_METHOD")
  result <- tryCatch(.s2_expression_evaluate(expression,
    function(selector) .s2_expression_error(paste("MISSING_BINDING:", selector)), namespaces, current_fact = candidate),
    s2_expression_error = function(error) .s2_fail("REVIEW_REQUIRED", "filter", conditionMessage(error)))
  .s2_require(is.logical(result) && length(result) == 1L && !is.na(result), "filter", "local filter must return Boolean", "UNSUPPORTED_METHOD")
  result
}
.s2_reporting_check_bound <- function(rule, bindings, technical_version, binding_reference, source, namespace_loader) {
  validation_code <- rule$ValidationCode
  .s2_require(.s2_string(binding_reference) && nzchar(trimws(binding_reference)), "binding_reference", "external qualification reference required", "MISSING_INPUT")
  .s2_require(.s2_mapping(bindings), "bindings", "source-variable-labelled mapping required")
  .s2_require(isTRUE(rule$IsEnabled == 1), "validation_code", "archived rule disabled", "UNSUPPORTED_METHOD")
  .s2_require(!any(vapply(c("Filter", "Join", "Scope", "Precondition", "Prerequisites"), function(key) .s2_truth(rule[[key]]), logical(1))),
    "validation_code", "rule filter/join/scope/precondition not supported", "UNSUPPORTED_METHOD")
  tree <- tryCatch(.s2_expression_parse(rule$Rule), s2_expression_error = function(error) .s2_fail("UNSUPPORTED_METHOD", "validation_code", conditionMessage(error)))
  selectors <- attributes <- .s2_object(); filter_qnames <- character(); needs_namespaces <- FALSE
  walk <- function(node) {
    if (node$kind == "qname") needs_namespaces <<- TRUE
    if (node$kind == "selector") {
      fields <- .s2_selector_fields(node$value); id <- fields$id
      .s2_require(.s2_string(id) && grepl("^[A-Za-z_][A-Za-z_0-9]*$", id), "selector", "source variable ID required", "UNSUPPORTED_METHOD")
      .s2_require(.s2_string(fields$seq) && fields$seq %in% c("True", "False"), "selector", "implicit sequence mode not supported", "UNSUPPORTED_METHOD")
      if (!is.null(fields$filter) && nzchar(fields$filter)) {
        needs_namespaces <<- TRUE
        filter_tree <- tryCatch(.s2_expression_parse(fields$filter), s2_expression_error = function(error) .s2_fail("UNSUPPORTED_METHOD", "filter", conditionMessage(error)))
        filter_walk <- function(item) {
          .s2_require(item$kind != "selector", "filter", "cross-fact filters unsupported", "UNSUPPORTED_METHOD")
          if (item$kind == "qname") filter_qnames <<- unique(c(filter_qnames, item$value))
          for (child in item$children) filter_walk(child)
        }
        filter_walk(filter_tree)
      }
      .s2_require(!id %in% names(selectors) || identical(selectors[[id]], node$value), "selector", "conflicting source definitions for variable", "UNSUPPORTED_METHOD")
      selectors[[id]] <<- node$value; attributes[[id]] <<- fields
    }
    # Match the source's stack traversal order for reproducible audit metadata.
    for (child in rev(node$children)) walk(child)
  }
  walk(tree)
  .s2_require(setequal(names(bindings), names(selectors)), "bindings", "exactly the declared source variables are required", "MISSING_INPUT")
  namespaces <- if (needs_namespaces) namespace_loader() else NULL
  for (lexical in filter_qnames) tryCatch(.s2_reporting_expand(lexical, namespaces),
    error = function(error) .s2_fail("UNSUPPORTED_METHOD", "filter", conditionMessage(error)))
  resolved <- dimension_bindings <- .s2_object(); applied <- character()
  for (id in names(selectors)) {
    selector <- selectors[[id]]; record <- bindings[[id]]; fields <- attributes[[id]]
    if (fields$seq == "True") {
      .s2_require(.s2_mapping(record) && identical(names(record), "values") && is.list(record$values) && is.null(names(record$values)),
        "bindings", "sequence variable requires an explicit values list")
      candidates <- lapply(record$values, .s2_reporting_bound_fact)
      .s2_require(all(vapply(candidates, function(candidate) !is.null(candidate[[1L]]), logical(1))),
        "bindings", "absence is not an item in a declared sequence; use an explicit empty values list")
      selected <- Filter(function(candidate) .s2_reporting_filter(fields$filter, candidate, namespaces), candidates)
      resolved[selector] <- list(lapply(selected, `[[`, 1L))
    } else {
      candidate <- .s2_reporting_bound_fact(record)
      accepted <- .s2_reporting_filter(fields$filter, candidate, namespaces)
      resolved[selector] <- list(if (accepted) candidate[[1L]] else NULL)
      dimension_bindings[selector] <- list(candidate[[2L]])
    }
    if (!is.null(fields$filter) && nzchar(fields$filter)) applied <- c(applied, id)
  }
  value <- tryCatch(.s2_expression_evaluate(rule$Rule, function(selector) {
    if (!selector %in% names(resolved)) .s2_expression_error(paste("MISSING_BINDING:", selector))
    resolved[[selector]]
  }, namespaces, dimensions = dimension_bindings), s2_expression_error = function(error) .s2_fail("REVIEW_REQUIRED", "expression", conditionMessage(error)))
  .s2_require(is.logical(value) && length(value) == 1L && !is.na(value), "expression", "rule expression did not return boolean", "UNSUPPORTED_METHOD")
  list(value = value, status = "REFERENCE_EXPRESSION_COMPONENT", validation_code = validation_code,
    technical_version = technical_version, source = source, rule_hash = .s2_fingerprint(rule),
    input_hash = .s2_fingerprint(list(bindings = bindings, binding_reference = binding_reference, technical_version = technical_version, validation_code = validation_code)),
    selectors = selectors, binding_reference = binding_reference, local_filters_applied = as.list(sort(applied)),
    fact_dimension_admissibility_verified = FALSE, binding_qualification_verified = FALSE,
    legal_applicability_verified = FALSE, fact_schema_validated = FALSE, fact_domain_membership_verified = FALSE,
    current_deactivations_checked = FALSE, full_report_validated = FALSE,
    archived_is_enabled = rule$IsEnabled, archived_include_in_xbrl = rule$IncludeInXBRL)
}

#' Check an archived rule on explicitly bound facts
#' @param validation_code Exact archived validation rule code.
#' @param bindings Named list keyed by source variable IDs. Supply tagged scalar
#'   facts, or `list(values = list(...))` for an explicitly declared sequence.
#' @param technical_version Explicit archived technical release.
#' @param binding_reference Nonempty external fact-selection/entity/period/unit
#'   qualification reference. It is recorded, not approved by this function.
#' @return Boolean component result with selector, source and input provenance.
#' @details Missing facts are not zero. Every syntactic variable must be supplied,
#'   including inactive branches. Supported local filters are applied; unsupported
#'   global filters, joins and scopes fail explicitly. No full report is certified.
#' @examples
#' bindings <- list(v1 = list(value = 1000, decimals = 0),
#'                  v2 = list(values = list(list(value = 1000, decimals = 0))))
#' reporting_bound_rule_check("BV1053-1", bindings, "2.10.0", "synthetic-example")$value
#' @export
reporting_bound_rule_check <- function(validation_code, bindings, technical_version, binding_reference) {
  .s2_require(.s2_string(validation_code) && nzchar(validation_code), "validation_code", "exact source rule code required")
  .s2_require(.s2_string(binding_reference) && nzchar(trimws(binding_reference)), "binding_reference", "external fact qualification reference required", "MISSING_INPUT")
  .s2_require(.s2_mapping(bindings), "bindings", "source-variable-labelled fact mapping required")
  resource <- reporting_data("rules", technical_version)
  selected <- Filter(function(row) identical(row$ValidationCode, validation_code), resource$tables$vValidationRuleExpressions)
  .s2_require(length(selected) == 1L, "validation_code", "unknown or ambiguous rule", "UNSUPPORTED_PARAMETER")
  .s2_reporting_check_bound(selected[[1L]], bindings, technical_version, binding_reference, resource$source,
    function() reporting_catalog(technical_version)$xbrl_namespace_bindings)
}
