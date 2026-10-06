.s2_filing_policy <- function(catalog, overrides) {
  policy <- catalog$filing_parameters
  resource <- paste0("reporting_", gsub("[.-]", "_", catalog$technical_version), "_catalog.json")
  if (!is.list(overrides) || !is.null(names(overrides))) stop("Explicit NumericOverride list required")
  audit <- list(); seen <- character()
  for (override in overrides) {
    path <- override$path
    if (!inherits(override, "NumericOverride") || !identical(override$resource, resource) ||
      length(path) != 4L || !identical(unlist(path[1:2], use.names = FALSE), c("filing_parameters", "parameters")) ||
      !identical(path[[4L]], "value") || !.s2_string(path[[3L]]) || !path[[3L]] %in% names(policy$parameters) ||
      !.s2_string(override$reason) || !nzchar(trimws(override$reason))) stop("Known filing parameter path and explicit override reason required")
    key <- path[[3L]]
    if (key %in% seen) stop("Duplicate filing parameter override")
    seen <- c(seen, key)
    if (!is.numeric(override$value) || length(override$value) != 1L || !is.finite(override$value)) stop("Finite JSON numeric override required")
    value <- .s2_reporting_rational(override$value); definition <- policy$parameters[[key]]; kind <- definition$kind
    if (grepl("integer", kind, fixed = TRUE) && gmp::denominator(value) != 1) stop("Integral filing parameter required")
    if (grepl("nonnegative", kind, fixed = TRUE) && value < 0) stop("Nonnegative filing parameter required")
    audit[[length(audit) + 1L]] <- list(resource = resource, path = path, original_value = definition$value,
      value = override$value, reason = override$reason, rule = definition$rule, source_id = policy$source_id)
    policy$parameters[[key]]$value <- override$value
  }
  p <- lapply(policy$parameters, function(row) .s2_reporting_rational(row$value))
  if (!(p$monetary_low_threshold > 0 && p$monetary_low_threshold < p$monetary_medium_threshold && p$monetary_medium_threshold < p$monetary_high_threshold))
    stop("Strictly increasing positive monetary bands required")
  list(policy = policy, audit = audit, hash = .s2_fingerprint(list(policy = policy, overrides = audit)))
}
.s2_instance_precision <- function(mapped, policy) {
  p <- lapply(policy$parameters, function(row) .s2_reporting_rational(row$value)); findings <- list()
  for (row in mapped) {
    dtype <- row$metric_check$data_type; atom <- row$fact$atom
    base <- list(business_code = row$business_code, source_id = policy$source_id)
    if (dtype == "String") {
      text <- atom$text
      if (nchar(text) > p$string_maximum_length) findings[[length(findings) + 1L]] <- c(base,
        list(status = "WARNING", rule = "S.2.22", reason = "TEXT_LENGTH_EXCEEDS_SHOULD_LIMIT"))
      if (text != trimws(text, whitespace = "[\\h\\v]")) findings[[length(findings) + 1L]] <- c(base,
        list(status = "WARNING", rule = "S.2.21", reason = "QUALIFY_MEANINGFUL_LEADING_OR_TRAILING_WHITESPACE"))
      next
    }
    if (!dtype %in% c("Monetary", "Decimal", "Integer", "Percent")) next
    decimals <- atom$decimals; result <- c(base, list(status = "PASS", declared_decimals = decimals))
    if (dtype == "Decimal") {
      result$rule <- "S.2.18(e)"; result$status <- "REVIEW_REQUIRED"; result$reason <- "APPROPRIATE_DECIMAL_PRECISION_EXTERNAL"
    } else if (dtype == "Integer") {
      result$rule <- "S.2.18(d)"; result$required_decimals <- as.numeric(p$integer_decimals)
      if (!identical(decimals, "INF") && decimals != p$integer_decimals) {
        result$status <- "FAIL"; result$reason <- "INTEGER_DECIMALS_NOT_ALLOWED"
      }
    } else {
      if (dtype == "Percent") { minimum <- p$percent_minimum_decimals; rule <- "S.2.18(e)" } else {
        table <- strsplit(sub("^[{]", "", row$business_code), ",", fixed = TRUE)[[1L]][[1L]]
        family <- paste(head(strsplit(table, ".", fixed = TRUE)[[1L]], 3L), collapse = ".")
        rule <- "S.2.18(c)"
        if (family %in% unlist(policy$precision_templates)) minimum <- p$monetary_special_decimals else {
          magnitude <- abs(.s2_reporting_rational(atom$value)); minimum <- p$monetary_small_decimals
          for (band in c("high", "medium", "low")) if (magnitude >= p[[paste0("monetary_", band, "_threshold")]]) {
            minimum <- p[[paste0("monetary_", band, "_decimals")]]; break
          }
        }
      }
      result$rule <- rule; result$minimum_decimals <- as.numeric(minimum)
      if (!identical(decimals, "INF") && decimals < minimum) { result$status <- "FAIL"; result$reason <- "DECLARED_PRECISION_BELOW_MINIMUM" }
    }
    findings[[length(findings) + 1L]] <- result
  }
  list(findings = findings, values_rounded_or_scaled = FALSE, original_measurement_accuracy_verified = FALSE, full_report_validated = FALSE)
}

.s2_derivative_key <- function(fact) {
  dimensions <- stats::setNames(fact$dimensions, vapply(fact$dimensions, function(d) .s2_qname_key(d$dimension), character(1)))
  root <- "http://eiopa.europa.eu/xbrl/s2c/dict/dim\n"
  ui <- dimensions[[paste0(root, "UI")]]; scope <- dimensions[[paste0(root, "SU")]]
  if (is.null(ui) || !"domain" %in% names(ui) || !.s2_string(ui$text) || !nzchar(trimws(ui$text)) || is.null(scope) || !"member" %in% names(scope)) return(NULL)
  list(ui$domain$namespace, ui$domain$local_name, ui$text, scope$member$namespace, scope$member$local_name)
}
.s2_instance_currency <- function(mapped, tables) {
  iso <- "http://www.xbrl.org/2003/iso4217"; instance <- "http://www.xbrl.org/2003/instance"
  dim <- "http://eiopa.europa.eu/xbrl/s2c/dict/dim"; cu <- "http://eiopa.europa.eu/xbrl/s2c/dict/dom/CU"
  ca <- "http://eiopa.europa.eu/xbrl/s2c/dict/dom/CA"
  currency_domains <- unlist(lapply(Filter(function(row) identical(row$DomainCode, "CU"), tables$mDomain), `[[`, "DomainID"))
  currency_codes <- unlist(lapply(Filter(function(row) row$DomainID %in% currency_domains && grepl("^[A-Z]{3}$", row$MemberCode), tables$mMember), `[[`, "MemberCode"))
  labels <- .s2_object()
  for (row in tables$mMember) labels[row$MemberCode] <- list(row$MemberLabel)
  declarations <- lapply(Filter(function(row) identical(row$fact$metric$local_name, "ei1930"), mapped), function(row) row$fact$atom$qname)
  currency <- if (length(declarations) == 1L) declarations[[1L]] else NULL
  valid_currency <- .s2_mapping(currency) && identical(currency$namespace, cu) && currency$local_name %in% currency_codes
  derivative_index <- .s2_object()
  parts_for <- function(row) strsplit(gsub("^[{]|[}]$", "", row$business_code), ",", fixed = TRUE)[[1L]]
  for (row in mapped) {
    parts <- parts_for(row)
    if (row$fact$metric$local_name != "ei1024" || !"C0370" %in% parts || !grepl("^S\\.08\\.01\\.(01|04)\\.02$", parts[[1L]])) next
    key <- .s2_derivative_key(row$fact)
    if (!is.null(key)) {
      id <- .s2_fingerprint(list(parts[[1L]], key))
      derivative_index[[id]] <- c(derivative_index[[id]], list(row))
    }
  }
  open_pair <- function(row) {
    parts <- parts_for(row); key <- .s2_derivative_key(row$fact)
    match <- regmatches(parts[[1L]], regexec("^S\\.08\\.01\\.(01|04)\\.01$", parts[[1L]]))[[1L]]
    if (row$fact$metric$local_name != "mi2822" || !length(match) || !"C0131" %in% parts || is.null(key) ||
      !identical(unlist(tail(key, 2L)), c("http://eiopa.europa.eu/xbrl/s2c/dict/dom/MC", "x169")))
      return(list(status = "REVIEW_REQUIRED", reason = "OPEN_MD_CURRENCY_PAIRING_ASSERTION_REQUIRED"))
    candidates <- derivative_index[[.s2_fingerprint(list(sub("01$", "02", parts[[1L]]), key))]]
    if (length(candidates) != 1L) return(list(status = "REVIEW_REQUIRED", reason = "NO_UNIQUE_OPEN_CURRENCY_COUNTERPART"))
    counterpart <- candidates[[1L]]; declared <- counterpart$fact$atom$qname
    agrees <- .s2_mapping(declared) && identical(declared$namespace, cu) && identical(declared$local_name, row$fact$unit$local_name)
    list(status = if (agrees) "PASS" else "FAIL", reason = if (agrees) "OPEN_CURRENCY_PAIR_MATCH" else "OPEN_CURRENCY_PAIR_MISMATCH",
      currency_cell = counterpart$business_code, join_dimension = "s2c_dim:UI", source_comparison = if (match[[2L]] == "01") "TV1002_1" else "TV1002_2",
      full_xbrl_assertion_executed = FALSE)
  }
  results <- list()
  for (row in mapped) {
    dtype <- row$metric_check$data_type
    if (!dtype %in% c("Monetary", "Decimal", "Percent", "Integer")) next
    fact <- row$fact; unit <- fact$unit
    result <- list(business_code = row$business_code, status = "PASS", rule = "III.8")
    finding <- function(status, reason) { result$status <<- status; result$reason <<- reason }
    if (dtype != "Monetary") {
      if (!.s2_equal(unit, list(namespace = instance, local_name = "pure"))) finding("FAIL", "NONMONETARY_UNIT_MUST_BE_XBRLI_PURE")
    } else if (!identical(unit$namespace, iso) || !unit$local_name %in% currency_codes) finding("FAIL", "MONETARY_UNIT_MUST_BE_ARCHIVED_ISO_CURRENCY") else {
      dimensions <- .s2_object()
      for (d in fact$dimensions) dimensions[.s2_qname_key(d$dimension)] <- list(d$member)
      approach <- dimensions[[paste0(dim, "\nAF")]]; original <- dimensions[[paste0(dim, "\nOC")]]
      if (.s2_equal(approach, list(namespace = ca, local_name = "x1"))) {
        if (!.s2_mapping(original) || !identical(original$namespace, cu) || !identical(original$local_name, unit$local_name)) finding("FAIL", "ORIGINAL_CURRENCY_DIMENSION_UNIT_MISMATCH") else
          if (!valid_currency) finding("REVIEW_REQUIRED", "NO_UNIQUE_REPORTING_CURRENCY_DECLARATION")
      } else if (!is.null(labels[[fact$metric$local_name]]) && grepl("|AF/Expressed in currency of denomination (not converted to reporting currency)", labels[[fact$metric$local_name]], fixed = TRUE)) {
        result <- .s2_merge(result, open_pair(row))
        if (result$status == "PASS" && !valid_currency) finding("REVIEW_REQUIRED", "NO_UNIQUE_REPORTING_CURRENCY_DECLARATION")
      } else if (!valid_currency) finding("REVIEW_REQUIRED", "NO_UNIQUE_REPORTING_CURRENCY_DECLARATION") else
        if (.s2_equal(original, currency)) finding("FAIL", "SAME_ORIGINAL_CURRENCY_REQUIRES_UNCONVERTED_APPROACH") else
        if (!identical(unit$local_name, currency$local_name)) finding("FAIL", "MONETARY_UNIT_REPORTING_CURRENCY_MISMATCH")
    }
    results[[length(results) + 1L]] <- result
  }
  list(rule_source_sections = list("III.8 / 3.1", "III.8 / 3.2(a)"), reporting_currency = if (valid_currency) currency else NULL,
    findings = results, full_currency_validation = FALSE, open_md_pairing_and_required_conversion_approach_verified = FALSE)
}
