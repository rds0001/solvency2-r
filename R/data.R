# Local-input adapters deliberately never download, interpolate or infer units.
.s2_iso_date <- function(value, field) {
  tryCatch(.s2_date(value), s2_type_error = function(e) .s2_fail("INVALID_INPUT", field, "canonical ISO date required"),
           s2_value_error = function(e) .s2_fail("INVALID_INPUT", field, "canonical ISO date required"))
}
.s2_local_path <- function(path) {
  .s2_require(.s2_string(path) && nzchar(path), "path", "explicit local path required")
  drive <- grepl("^[A-Za-z]:[/\\\\]", path)
  .s2_require(!grepl("^[A-Za-z][A-Za-z0-9+.-]*:", path) || drive,
              "path", "URLs are not supported; obtain the file yourself", "UNSUPPORTED_INPUT")
  path
}
.s2_read_json <- function(path) {
  path <- .s2_local_path(path)
  text <- tryCatch(readLines(path, encoding = "UTF-8", warn = FALSE),
    error = function(e) .s2_fail("INPUT_IO_ERROR", "path", "cannot read local UTF-8 JSON file"))
  payload <- tryCatch(jsonlite::fromJSON(paste(text, collapse = "\n"), simplifyVector = FALSE),
    error = function(e) .s2_fail("INVALID_INPUT", "json", "invalid JSON document"))
  check <- function(value) {
    if (is.list(value)) {
      .s2_require(!anyDuplicated(names(value)), "json", "duplicate object key")
      for (child in value) check(child)
    }
  }
  check(payload)
  payload
}

#' Explicit local discount curve
#' @param curve_id Nonempty identifier supplied by the user.
#' @param as_of Canonical valuation date, `YYYY-MM-DD`.
#' @param currency Uppercase three-letter currency.
#' @param curve_kind One of `base`, `va`, `ma`, `stress`, `custom`.
#' @param source_reference User-owned provenance, not an instruction to download.
#' @param points Ordered list of two-element lists containing payment date and
#'   discount factor. Factors must be positive; values above one are permitted.
#' @return A `DiscountCurve` list. Use [discount_curve_factor()] for exact lookup
#'   and [discount_curve_to_dict()] for the version-1 JSON-compatible schema.
#' @details Points are sorted by date and duplicate dates rejected. No
#'   interpolation, day-count convention, currency conversion or approval is
#'   inferred. A copy can be edited, but adapters revalidate it before use.
#' @examples
#' curve <- DiscountCurve("example", "2025-01-17", "EUR", "custom",
#'   "synthetic example", list(list("2026-01-17", 0.97)))
#' discount_curve_factor(curve, "2026-01-17")
#' @export
DiscountCurve <- function(curve_id, as_of, currency, curve_kind, source_reference, points) {
  for (field in c("curve_id", "source_reference")) {
    value <- get(field)
    .s2_require(.s2_string(value) && nzchar(trimws(value)), field, "nonempty reference required")
  }
  valuation <- .s2_iso_date(as_of, "as_of")
  .s2_currency(currency, "currency")
  .s2_require(.s2_string(curve_kind) && curve_kind %in% c("base", "va", "ma", "stress", "custom"), "curve_kind", "explicit curve kind required")
  .s2_require(is.list(points) && length(points) > 0L, "points", "nonempty points required")
  dates <- character()
  normalized <- lapply(points, function(point) {
    .s2_require(is.list(point) && length(point) == 2L, "points", "date and discount factor required")
    label <- point[[1L]]
    date <- .s2_iso_date(label, "points.date")
    .s2_require(date >= valuation, "points.date", "point precedes curve valuation date")
    factor <- .s2_number(point[[2L]], "points.discount_factor")
    .s2_require(factor > 0, "points.discount_factor", "strictly positive discount factor required")
    .s2_require(date != valuation || factor == 1, "points.discount_factor", "valuation-date discount factor must be one")
    list(label, factor)
  })
  dates <- vapply(normalized, `[[`, character(1), 1L)
  .s2_require(!anyDuplicated(dates), "points.date", "duplicate payment date")
  structure(list(curve_id = curve_id, as_of = as_of, currency = currency, curve_kind = curve_kind,
    source_reference = source_reference, points = normalized[order(dates, method = "radix")]),
    class = c("DiscountCurve", "list"))
}
.s2_validate_curve <- function(curve) {
  .s2_require(inherits(curve, "DiscountCurve"), "curve", "DiscountCurve required")
  do.call(DiscountCurve, unclass(curve))
}

#' @rdname DiscountCurve
#' @param curve A [DiscountCurve()].
#' @param payment_date Exact canonical payment date.
#' @export
discount_curve_factor <- function(curve, payment_date) {
  curve <- .s2_validate_curve(curve)
  .s2_iso_date(payment_date, "payment_date")
  labels <- vapply(curve$points, `[[`, character(1), 1L)
  i <- match(payment_date, labels)
  .s2_require(!is.na(i), "payment_date", "explicit discount factor missing", "MISSING_INPUT")
  curve$points[[i]][[2L]]
}

#' @rdname DiscountCurve
#' @export
discount_curve_to_dict <- function(curve) {
  curve <- .s2_validate_curve(curve)
  list(schema_version = 1L, curve_id = curve$curve_id, as_of = curve$as_of,
    currency = curve$currency, curve_kind = curve$curve_kind, source_reference = curve$source_reference,
    value_type = "discount_factor", unit = "dimensionless",
    points = lapply(curve$points, function(row) list(date = row[[1L]], discount_factor = row[[2L]])))
}

#' @rdname DiscountCurve
#' @param payload Exact version-1 schema with metadata and `date`/`discount_factor` rows.
#' @export
discount_curve_from_dict <- function(payload) {
  .s2_keys(payload, c("schema_version", "curve_id", "as_of", "currency", "curve_kind", "source_reference", "value_type", "unit", "points"), "curve")
  .s2_require(is.numeric(payload$schema_version) && identical(as.numeric(payload$schema_version), 1), "schema_version", "only version 1 is supported", "UNSUPPORTED_INPUT")
  .s2_require(identical(payload$value_type, "discount_factor") && identical(payload$unit, "dimensionless"), "value_type/unit", "explicit dimensionless discount factors required")
  .s2_require(is.list(payload$points) && is.null(names(payload$points)), "points", "point list required")
  points <- lapply(payload$points, function(row) {
    .s2_keys(row, c("date", "discount_factor"), "points")
    list(row$date, row$discount_factor)
  })
  do.call(DiscountCurve, c(payload[c("curve_id", "as_of", "currency", "curve_kind", "source_reference")], list(points = points)))
}

#' Import a user-supplied local discount curve
#' @param path Local file path; URLs, including `file://`, are rejected.
#' @param curve_id,as_of,currency,curve_kind,source_reference Explicit CSV metadata;
#'   see [DiscountCurve()]. JSON carries these in its version-1 schema.
#' @return A validated [DiscountCurve()].
#' @details JSON must be UTF-8 with unique object keys. CSV must have exactly
#'   `date,discount_factor` columns, comma separators and decimal points.
#'   Neither adapter accesses the Internet or guesses units.
#' @examples
#' path <- tempfile(fileext = ".csv")
#' writeLines(c("date,discount_factor", "2026-01-17,0.97"), path)
#' curve <- load_discount_curve_csv(path, "example", "2025-01-17", "EUR",
#'                                  "custom", "synthetic example")
#' discount_curve_factor(curve, "2026-01-17")
#' unlink(path)
#' @export
load_discount_curve_json <- function(path) discount_curve_from_dict(.s2_read_json(path))

#' @rdname load_discount_curve_json
#' @export
load_discount_curve_csv <- function(path, curve_id, as_of, currency, curve_kind, source_reference) {
  path <- .s2_local_path(path)
  .s2_require(file.exists(path) && !dir.exists(path), "path", "cannot read local curve file", "INPUT_IO_ERROR")
  rows <- tryCatch(utils::read.csv(path, colClasses = "character", check.names = FALSE,
    stringsAsFactors = FALSE, fileEncoding = "UTF-8", na.strings = character(), fill = FALSE),
    error = function(e) .s2_fail("INVALID_INPUT", "csv", "invalid CSV curve document"))
  .s2_require(length(names(rows)) == 2L && setequal(names(rows), c("date", "discount_factor")), "csv.header", "exact columns date,discount_factor required")
  points <- lapply(seq_len(nrow(rows)), function(i) {
    factor <- suppressWarnings(as.numeric(rows$discount_factor[[i]]))
    .s2_require(!is.na(factor), "discount_factor", "numeric CSV value required")
    list(rows$date[[i]], factor)
  })
  DiscountCurve(curve_id, as_of, currency, curve_kind, source_reference, points)
}

#' Build explicit discount factors from externally supplied spot rates
#' @param points Named list keyed by payment date, each with
#'   `annual_effective_rate` (decimal) and `time_years` (explicit year fraction).
#' @param curve_id,curve_kind,source_reference Curve identity; see [DiscountCurve()].
#' @param time_basis_reference Nonempty external year-fraction convention reference.
#' @param context Explicit [Context()].
#' @return A [DiscountCurve()] with raw inputs and convention fingerprinted in provenance.
#' @details No interpolation, extrapolation or calendar convention is inferred.
#' @examples
#' ctx <- Context("2025-01-17", "2026-09-23", "EUR", "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' curve <- discount_curve_from_spot_rates(
#'   list("2026-01-17" = list(annual_effective_rate = 0.03, time_years = 1)),
#'   "example", "custom", "synthetic rates", "explicit one year", ctx)
#' discount_curve_factor(curve, "2026-01-17")
#' @export
discount_curve_from_spot_rates <- function(points, curve_id, curve_kind, source_reference, time_basis_reference, context) {
  .s2_validate_context(context)
  .s2_require(.s2_mapping(points) && length(points) > 0L, "points", "nonempty mapping required")
  for (field in c("source_reference", "time_basis_reference")) {
    value <- get(field)
    .s2_require(.s2_string(value) && nzchar(trimws(value)), field, "nonempty reference required")
  }
  valuation <- .s2_iso_date(context$as_of, "as_of")
  normalized <- lapply(names(points), function(label) {
    date <- .s2_iso_date(label, "points.date")
    .s2_require(date >= valuation, "points.date", "point precedes valuation date")
    row <- points[[label]]
    .s2_keys(row, c("annual_effective_rate", "time_years"), "points.row")
    rate <- .s2_number(row$annual_effective_rate, "annual_effective_rate")
    time <- .s2_number(row$time_years, "time_years", 0)
    .s2_require((time == 0) == (date == valuation), "time_years", "date/time zero mismatch")
    list(annual_effective_rate = rate, time_years = time)
  })
  names(normalized) <- names(points)
  normalized <- normalized[order(names(normalized), method = "radix")]
  times <- vapply(normalized, function(row) row$time_years, numeric(1))
  .s2_require(all(diff(times) > 0), "time_years", "times must increase with dates")
  hash <- .s2_fingerprint(list(points = normalized, time_basis_reference = time_basis_reference, compounding = "annual_effective"))
  reference <- paste0(source_reference, "; annual_effective; time_basis:", time_basis_reference, "; sha256:", hash)
  factors <- lapply(names(normalized), function(label) {
    row <- normalized[[label]]
    list(label, rfr_spot_discount_factor(row$annual_effective_rate, row$time_years, reference, context)$value)
  })
  DiscountCurve(curve_id, context$as_of, context$currency, curve_kind, reference, factors)
}

#' Bind a local discount curve to externally projected cashflows
#' @param cashflows Named list keyed by payment date with `expected_inflows`
#'   and `expected_outflows` in context currency.
#' @param curve Validated [DiscountCurve()] with an explicit point for every payment date.
#' @param runoff_complete Explicit completeness declaration.
#' @param cashflow_reference External cashflow provenance.
#' @param context Explicit [Context()] matching the curve date and currency.
#' @return A `Result`, including the user curve's metadata and content hash.
#' @examples
#' ctx <- Context("2025-01-17", "2026-09-23", "EUR", "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' curve <- DiscountCurve("example", ctx$as_of, "EUR", "custom",
#'   "synthetic example", list(list("2026-01-17", 0.97)))
#' best_estimate_with_curve(list("2026-01-17" = list(expected_inflows = 0,
#'   expected_outflows = 100)), curve, TRUE, "synthetic cashflows", ctx)$value
#' @export
best_estimate_with_curve <- function(cashflows, curve, runoff_complete, cashflow_reference, context) {
  .s2_validate_context(context)
  curve <- .s2_validate_curve(curve)
  .s2_require(identical(curve$as_of, context$as_of), "curve.as_of", "curve/context dates differ")
  .s2_require(identical(curve$currency, context$currency), "curve.currency", "curve/context currencies differ")
  .s2_require(.s2_mapping(cashflows) && length(cashflows) > 0L, "cashflows", "nonempty mapping required")
  bound <- lapply(names(cashflows), function(label) {
    row <- cashflows[[label]]
    .s2_keys(row, c("expected_inflows", "expected_outflows"), "cashflows.row")
    c(row, list(discount_factor = discount_curve_factor(curve, label)))
  })
  names(bound) <- names(cashflows)
  payload <- discount_curve_to_dict(curve)
  hash <- .s2_fingerprint(payload)
  reference <- paste0(curve$curve_id, "; sha256:", hash, "; ", curve$source_reference)
  result <- best_estimate_cashflows(bound, runoff_complete, cashflow_reference, reference, context)
  payload$points <- NULL
  result$details$user_curve <- c(payload, list(input_hash = hash, qualification_status = "EXTERNAL_UNVERIFIED"))
  result
}

#' Load an explicit analyst-override batch
#' @param payload Named list with `schema_version = 1` and an ordered `overrides`
#'   list of `resource`, `path`, `value`, `reason` entries.
#' @param path Local UTF-8 JSON file; URLs are rejected.
#' @return A list of [NumericOverride()] entries for [Context()].
#' @details Paths retain zero-based JSON array indices. Parsing does not change
#'   installed data; the selected context validates parameter applicability.
#' @examples
#' numeric_overrides_from_dict(list(schema_version = 1L, overrides = list()))
#' @export
numeric_overrides_from_dict <- function(payload) {
  .s2_keys(payload, c("schema_version", "overrides"), "scenario")
  .s2_require(is.numeric(payload$schema_version) && identical(as.numeric(payload$schema_version), 1), "schema_version", "only version 1 is supported", "UNSUPPORTED_INPUT")
  .s2_require(is.list(payload$overrides) && is.null(names(payload$overrides)), "overrides", "list required")
  lapply(payload$overrides, function(row) {
    .s2_keys(row, c("resource", "path", "value", "reason"), "override")
    .s2_require(is.list(row$path) && length(row$path) > 0L && all(vapply(row$path,
      function(p) .s2_string(p) || (is.numeric(p) && length(p) == 1L && is.finite(p) && p == trunc(p)), logical(1))),
      "path", "nonempty list of string keys/integer indices required")
    NumericOverride(row$resource, row$path, .s2_number(row$value, "value"), row$reason)
  })
}

#' @rdname numeric_overrides_from_dict
#' @export
load_numeric_overrides_json <- function(path) numeric_overrides_from_dict(.s2_read_json(path))
