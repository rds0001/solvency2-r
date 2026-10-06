# Shared local-market boundary. The bundled archive is deliberately synthetic.
.s2_native_rfr_monthly___archived_dataset <- function(context, market_data) {
  if (is.null(market_data)) data <- .s2_effective("rfr_monthly.json", context) else {
    .s2_require(.s2_mapping(market_data), "market_data", "JSON object required")
    .s2_require(!any(vapply(context$overrides, function(o) identical(o$resource, "rfr_monthly.json"), logical(1))),
      "market_data", "edit supplied data explicitly; bundled overrides cannot accompany external tables")
    data <- market_data
  }
  .s2_require(.s2_equal(data$schema_version, 1), "market_data", "schema_version 1 required")
  .s2_require(.s2_string(data$status) && data$status %in% c("SYNTHETIC_DEMO", "USER_SUPPLIED"),
    "market_data", "explicit SYNTHETIC_DEMO or USER_SUPPLIED status required")
  .s2_require(.s2_mapping(data$datasets), "market_data", "datasets mapping required")
  as_of <- context$as_of
  .s2_require(as_of %in% names(data$datasets), "as_of", "exact archived month unavailable; no carry-forward", "MISSING_INPUT")
  dataset <- data$datasets[[as_of]]
  .s2_require(.s2_mapping(dataset) && identical(dataset$data_kind, data$status), "market_data", "dataset kind must match top-level status")
  for (key in c("source_id", "retrieved_at", "archive_sha256"))
    .s2_require(.s2_string(dataset[[key]]) && nzchar(trimws(dataset[[key]])), "market_data", paste("nonempty", key, "required"))
  .s2_fingerprint(dataset)
  .s2_require(.s2_iso_date(context$known_at, "known_at") >= .s2_iso_date(dataset$retrieved_at, "market_data"),
    "known_at", "availability before archive retrieval unverified", "REVIEW_REQUIRED")
  if (dataset$data_kind == "SYNTHETIC_DEMO") warning(
    "SYNTHETIC_DEMO: invented market inputs, not for production valuation.", call. = FALSE)
  list(as_of, dataset)
}
