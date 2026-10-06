# Archived activation evidence is never inferred from today's date.
.s2_reporting_activation <- function(row, snapshot) {
  flag <- row$Deactivated; full <- row[["Full or partial deactivation"]]
  off <- row[["Deactivated on"]]; on <- row[["Reactivated on"]]
  result <- function(active, reason) list(active = active, reason = reason)
  if (is.null(flag) || !flag %in% c("yes", "no")) return(result(NULL, "UNKNOWN_DEACTIVATION_FLAG"))
  if (!is.null(full) && !full %in% c("", "Full")) return(result(NULL, "PARTIAL_OR_UNKNOWN_DEACTIVATION_SCOPE"))
  valid <- tryCatch({
    if (!is.null(off)) .s2_iso_date(off, "date")
    if (!is.null(on)) .s2_iso_date(on, "date")
    TRUE
  }, error = function(error) FALSE)
  if (!valid) return(result(NULL, "INVALID_STATUS_DATE"))
  if ((!is.null(off) && off > snapshot) || (!is.null(on) && on > snapshot)) return(result(NULL, "STATUS_DATE_AFTER_SNAPSHOT"))
  if (flag == "yes") {
    if (!identical(full, "Full") || is.null(off) || !is.null(on)) return(result(NULL, "INCONSISTENT_DEACTIVATION_EVIDENCE"))
    return(result(FALSE, "FULLY_DEACTIVATED_IN_SNAPSHOT"))
  }
  if (!is.null(on)) {
    if (is.null(off) || on <= off) return(result(NULL, "INCONSISTENT_REACTIVATION_EVIDENCE"))
    return(result(TRUE, "REACTIVATED_IN_SNAPSHOT"))
  }
  if (!is.null(off) || (!is.null(full) && full != "")) return(result(NULL, "INCONSISTENT_ACTIVE_EVIDENCE"))
  result(TRUE, "ACTIVE_IN_SNAPSHOT")
}

.s2_reporting_severities <- function(text) {
  if (!.s2_string(text) || !nzchar(trimws(text))) stop("Missing module/severity declaration")
  parts <- strsplit(text, "|", fixed = TRUE)[[1L]]
  if (length(parts) && !nzchar(trimws(tail(parts, 1)))) parts <- head(parts, -1L)
  output <- .s2_object()
  for (part in parts) {
    match <- regmatches(part, regexec("^\\s*(ERROR|WARNING)\\s*-\\s*([a-z][a-z0-9]*)\\s*$", part, perl = TRUE))[[1L]]
    if (length(match) != 3L || match[[3L]] %in% names(output)) stop("Unknown or duplicate module/severity declaration")
    output[[match[[3L]]]] <- match[[2L]]
  }
  output
}

#' Archived module-specific validation status
#' @param validation_code Exact archived rule code.
#' @param technical_version Explicit archived technical release.
#' @param module Exact archived module code, for example `"ars"`.
#' @return Source rows, snapshot date, severity, implementation channel and
#'   activation evidence. Ambiguous or conflicting evidence is `REVIEW_REQUIRED`.
#' @details This is not a current supervisory clearance or an activation history.
#'   Non-XBRL checks are not automatically inactive. No online lookup occurs.
#' @examples
#' reporting_validation_status("BV1053-1", "2.8.2", "ars")$reason
#' @export
reporting_validation_status <- function(validation_code, technical_version, module) {
  metadata <- .s2_reporting_metadata(technical_version, "validation_status")
  .s2_require(.s2_string(validation_code) && nzchar(validation_code), "validation_status", "exact rule code required")
  .s2_require(.s2_string(module) && grepl("^[a-z][a-z0-9]*$", module), "validation_status", "exact archived module code required")
  .s2_iso_date(metadata$snapshot_date, "validation_status")
  entries <- relevant <- list(); malformed <- FALSE
  for (entry in metadata$rows) {
    row <- entry$values
    if (!identical(row[["Rule code"]], validation_code)) next
    severities <- tryCatch(.s2_reporting_severities(row[["Severity and modules"]]), error = identity)
    if (inherits(severities, "error")) { severities <- .s2_object(); malformed <- TRUE }
    activation <- .s2_reporting_activation(row, metadata$snapshot_date)
    item <- c(entry, list(module_severities = severities, active_in_snapshot = activation$active, activation_reason = activation$reason))
    entries[[length(entries) + 1L]] <- item
    if (module %in% names(severities)) relevant[[length(relevant) + 1L]] <- item
  }
  active <- severity <- implementation <- NULL
  if (malformed) reason <- "UNQUALIFIED_MODULE_DECLARATION" else if (!length(entries)) reason <- "RULE_NOT_LISTED" else
    if (!length(relevant)) reason <- "RULE_NOT_LISTED_FOR_MODULE" else if (length(relevant) != 1L) reason <- "AMBIGUOUS_WORKBOOK_ROWS" else {
      selected <- relevant[[1L]]; severity <- selected$module_severities[[module]]
      included <- selected$values[["Include in XBRL"]]
      if (is.null(included) || !included %in% c("yes", "no")) reason <- "UNKNOWN_IMPLEMENTATION_CHANNEL" else {
        implementation <- if (included == "yes") "XBRL_ASSERTION" else "OTHER_REQUIRED_CHECK"
        active <- selected$active_in_snapshot; reason <- selected$activation_reason
      }
    }
  comparison <- metadata$related_snapshot_comparison
  conflicts <- Filter(function(change) identical(change$validation_code, validation_code), comparison$changes)
  if (is.null(conflicts)) conflicts <- list()
  if (length(conflicts)) { active <- NULL; reason <- "RELATED_SNAPSHOT_CONFLICT" }
  list(status = if (is.null(active)) "REVIEW_REQUIRED" else "ARCHIVED_RULE_STATUS_COMPONENT",
    technical_version = technical_version, validation_code = validation_code, module = module,
    snapshot_date = metadata$snapshot_date, source = metadata$source, sheet = metadata$sheet,
    active_in_snapshot = active, reason = reason, severity = severity, implementation_channel = implementation,
    entries = entries, related_snapshot_conflicts = conflicts, related_snapshot_source = comparison$source,
    current_deactivations_checked = FALSE, legal_applicability_verified = FALSE,
    activation_history_reconstructed = FALSE, full_report_validated = FALSE,
    input_hash = .s2_fingerprint(list(validation_code = validation_code, module = module, technical_version = technical_version)))
}

# Restricted Boolean grammar, not an R evaluator: parse both sides explicitly.
.s2_reporting_precondition <- function(expression, indicators) {
  fail <- function(message) .s2_fail("UNSUPPORTED_METHOD", "precondition", message)
  if (!.s2_string(expression) || !nzchar(trimws(expression)) || nchar(expression) > 100000) fail("Unsupported filing precondition")
  if (!.s2_mapping(indicators) || !all(vapply(indicators, function(value) is.logical(value) && length(value) == 1L && !is.na(value), logical(1))))
    fail("Explicit Boolean filing indicators required")
  rest <- trimws(expression); tokens <- character()
  while (nzchar(rest)) {
    match <- regmatches(rest, regexec("^\\s*(\\$find:t[A-Z][A-Z0-9]*(?:\\.[0-9]+)+|and\\b|or\\b|[()])", rest, perl = TRUE))[[1L]]
    if (!length(match)) fail("Unsupported filing precondition syntax")
    tokens <- c(tokens, match[[2L]]); rest <- substring(rest, nchar(match[[1L]]) + 1L)
    if (length(tokens) > 20000L) fail("Filing precondition too large")
  }
  cursor <- 1L
  atom <- function(depth) {
    if (depth > 100L || cursor > length(tokens)) fail("Invalid filing precondition nesting")
    token <- tokens[[cursor]]; cursor <<- cursor + 1L
    if (token == "(") {
      value <- disjunction(depth + 1L)
      if (cursor > length(tokens) || tokens[[cursor]] != ")") fail("Unclosed filing precondition")
      cursor <<- cursor + 1L
      return(value)
    }
    if (!startsWith(token, "$find:t")) fail("Expected filing indicator")
    code <- substring(token, 8L)
    if (!code %in% names(indicators)) fail(paste("Missing filing indicator:", code))
    indicators[[code]]
  }
  conjunction <- function(depth) {
    result <- atom(depth)
    while (cursor <= length(tokens) && tokens[[cursor]] == "and") {
      cursor <<- cursor + 1L; right <- atom(depth); result <- result && right
    }
    result
  }
  disjunction <- function(depth) {
    result <- conjunction(depth)
    while (cursor <= length(tokens) && tokens[[cursor]] == "or") {
      cursor <<- cursor + 1L; right <- conjunction(depth); result <- result || right
    }
    result
  }
  result <- disjunction(0L)
  if (cursor <= length(tokens)) fail("Unexpected filing precondition suffix")
  result
}

#' Evaluate an archived filing-indicator precondition
#' @param validation_code Exact archived rule code.
#' @param indicators Named list of explicit logical declarations for all referenced
#'   template indicators. Missing declarations do not mean `FALSE`.
#' @param technical_version Explicit archived technical release.
#' @param module Exact archived module code.
#' @return Boolean precondition result plus archived source hashes and limits.
#' @details This evaluates only the archived Boolean indicator condition, not
#'   legal applicability, current activation or the reported numeric facts.
#' @examples
#' # Use explicit declarations from a qualified reporting input set.
#' names(reporting_catalog("2.8.2")$modules)[1:3]
#' @export
reporting_rule_precondition <- function(validation_code, indicators, technical_version, module) {
  metadata <- .s2_reporting_metadata(technical_version, "xbrl_preconditions")
  fail <- function(message) .s2_fail("UNSUPPORTED_METHOD", "precondition", message)
  if (!.s2_string(module) || !module %in% names(metadata$modules)) fail("Unknown archived reporting module")
  if (!.s2_string(validation_code) || !nzchar(validation_code)) fail("Exact validation code required")
  definition <- metadata$modules[[module]]
  selected <- Filter(function(record) identical(metadata$assertions[[record$assertion_key]]$fragment, paste0("s2md_", validation_code)), definition$preconditions)
  if (length(selected) != 1L) fail("No unique archived module precondition")
  record <- selected[[1L]]; assertion <- metadata$assertions[[record$assertion_key]]
  if (!identical(assertion$kind, "{http://xbrl.org/2008/assertion/value}valueAssertion")) fail("Existence assertion preconditions require separate handling")
  if (any(sub("^.*}", "", names(assertion$attributes)) %in% c("fromDate", "toDate"))) fail("Dated assertion requires explicit applicability resolver")
  list(precondition_met = .s2_reporting_precondition(record$expression, indicators),
    technical_version = technical_version, module = module, validation_code = validation_code,
    expression = record$expression, archive_sha256 = metadata$archive_sha256,
    module_preconditions_sha256 = definition$sha256, assertion_sha256 = assertion$sha256,
    status = "ARCHIVED_FILING_PRECONDITION_ONLY", legal_applicability_verified = FALSE,
    current_deactivations_checked = FALSE, full_report_validated = FALSE,
    input_hash = .s2_fingerprint(list(validation_code = validation_code, indicators = indicators,
      technical_version = technical_version, module = module)))
}
