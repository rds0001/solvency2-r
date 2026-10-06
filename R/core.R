# Native R context, input guards, resource access and calculation provenance.
# JSON paths deliberately retain the reference format's zero-based indices.
.s2_cache <- new.env(parent = emptyenv())

.s2_fail <- function(code, field, message) {
  stop(CalculationError(code, field, message))
}

.s2_require <- function(ok, field, message, code = "INVALID_INPUT") {
  if (!isTRUE(ok)) .s2_fail(code, field, message)
  invisible(NULL)
}

.s2_number <- function(value, field, minimum = NULL, maximum = NULL) {
  if (is.null(value)) .s2_fail("MISSING_INPUT", field, "explicit value required")
  .s2_require(is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.finite(value), field, "finite numeric scalar required")
  .s2_require(is.null(minimum) || value >= minimum, field, "value below minimum")
  .s2_require(is.null(maximum) || value <= maximum, field, "value above maximum")
  as.numeric(value)
}

.s2_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
.s2_mapping <- function(x) is.list(x) && !is.null(names(x)) && !anyDuplicated(names(x))
.s2_object <- function() stats::setNames(list(), character())
.s2_keys <- function(x, keys, field) {
  keys <- unlist(keys, use.names = FALSE)
  .s2_require(.s2_mapping(x), field, "named list required")
  .s2_require(all(keys %in% names(x)), field, "missing keys", "MISSING_INPUT")
  .s2_require(length(x) == length(keys) && setequal(names(x), keys), field, "unexpected or duplicate keys")
}

.s2_load <- function(name) {
  if (exists(name, envir = .s2_cache, inherits = FALSE)) return(.s2_cache[[name]])
  root <- system.file("extdata", package = "solvency2", mustWork = TRUE)
  if (!exists("manifest", envir = .s2_cache, inherits = FALSE))
    .s2_cache$manifest <- jsonlite::fromJSON(file.path(root, "resource_manifest.json"), simplifyVector = FALSE)
  if (identical(name, "manifest.json")) {
    # Golden-Source hashes are provenance, not hashes of recompressed R files.
    original <- lapply(.s2_cache$manifest, function(entry) entry$source_sha256)
    return(list(sha256 = original[!startsWith(names(original), "library/")]))
  }
  entry <- .s2_cache$manifest[[name]]
  .s2_require(!is.null(entry), "resource", "unknown resource")
  path <- file.path(root, entry$file)
  .s2_require(identical(digest::digest(file = path, algo = "sha256"), entry$sha256),
              name, "resource hash mismatch", "RESOURCE_INTEGRITY")
  connection <- xzfile(path, "rb")
  on.exit(close(connection))
  raw <- readBin(connection, "raw", n = entry$uncompressed_bytes)
  .s2_require(identical(digest::digest(raw, algo = "sha256", serialize = FALSE), entry$json_sha256),
              name, "JSON content hash mismatch", "RESOURCE_INTEGRITY")
  value <- jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
  .s2_cache[[name]] <- value
  value
}

.s2_contracts <- function() {
  if (!exists("contracts", envir = .s2_cache, inherits = FALSE)) {
    path <- system.file("extdata", "calculation_contracts.json", package = "solvency2", mustWork = TRUE)
    .s2_cache$contracts <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  }
  .s2_cache$contracts
}

.s2_sort_object <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) x <- x[order(names(x), method = "radix")]
    return(lapply(x, .s2_sort_object))
  }
  x
}

.s2_fingerprint <- function(x) {
  .s2_json_input(x)
  text <- as.character(jsonlite::toJSON(.s2_sort_object(unclass(x)),
    auto_unbox = TRUE, digits = NA, null = "null", na = "null", force = TRUE))
  digest::digest(text, algo = "sha256", serialize = FALSE)
}

.s2_json_input <- function(x, field = "inputs") {
  if (is.null(x)) return(invisible(NULL))
  if (is.list(x) && !isS4(x)) {
    .s2_require(is.null(names(x)) || (!anyNA(names(x)) && !anyDuplicated(names(x))),
      field, "unique JSON object keys required")
    for (value in x) .s2_json_input(value, field)
  } else {
    .s2_require((is.numeric(x) || is.character(x) || is.logical(x)) && !is.object(x) &&
      !anyNA(x) && (!is.numeric(x) || all(is.finite(x))), field, "finite JSON-compatible inputs required")
  }
  invisible(NULL)
}

#' Explicit calculation context
#'
#' Select a dated rule profile and optional numeric analyst overrides. No current
#' date, currency, rule profile or external approval is inferred.
#' @param as_of,known_at ISO dates for valuation and information availability.
#' @param currency Three-letter uppercase currency identifier.
#' @param profile_id Explicit profile identifier; inspect [rule_profiles()].
#' @param mode Must be `"REFERENCE_ONLY"`; a result is not supervisory approval.
#' @param overrides List of [NumericOverride()] entries.
#' @return An immutable-by-convention S3 `Context` list. Functions validate it
#'   before calculation; modifying a copy does not affect other contexts.
#' @seealso [parameter()], [rule_profiles()]
#' @examples
#' context <- Context("2025-01-17", "2026-09-23", "EUR",
#'                    "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' intangible_risk(100, context = context)$value
#' @export
Context <- function(as_of, known_at, currency, profile_id,
                    mode = "REFERENCE_ONLY", overrides = list()) {
  result <- structure(list(as_of = as_of, known_at = known_at, currency = currency,
    profile_id = profile_id, mode = mode, overrides = overrides), class = c("Context", "list"))
  .s2_validate_context(result)
  result
}

.s2_descriptor <- function(profile_id) {
  profiles <- .s2_load("rule_profiles.json")$profiles
  .s2_require(.s2_string(profile_id) && profile_id %in% names(profiles),
              "profile_id", "unknown rule profile", "UNSUPPORTED_RULESET")
  profiles[[profile_id]]
}

.s2_resource_allowed <- function(name, descriptor) {
  allowed <- descriptor$supported_resources
  .s2_require(identical(allowed, "BASELINE_RESOURCES") || name %in% unlist(allowed),
              "resource", "resource not qualified for selected profile", "UNSUPPORTED_RULESET")
}

.s2_validate_context <- function(context) {
  .s2_require(inherits(context, "Context"), "context", "Context required", "MISSING_INPUT")
  for (name in c("as_of", "known_at")) {
    value <- context[[name]]
    valid <- .s2_string(value) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)
    converted <- if (valid) suppressWarnings(as.Date(value, format = "%Y-%m-%d")) else NA
    .s2_require(valid && !is.na(converted) && identical(format(converted, "%Y-%m-%d"), value),
                "dates", "ISO dates required")
  }
  .s2_require(.s2_string(context$currency) && grepl("^[A-Z]{3}$", context$currency),
              "currency", "explicit three-letter uppercase currency required")
  descriptor <- .s2_descriptor(context$profile_id)
  profile <- .s2_load(descriptor$resource)
  .s2_require(identical(context$profile_id, profile$profile_id), "profile_id", "profile resource mismatch", "RESOURCE_INTEGRITY")
  .s2_require(identical(context$mode, "REFERENCE_ONLY"), "mode", "reference mode required", "REVIEW_REQUIRED")
  .s2_require(context$as_of >= descriptor$reference_from &&
    (is.null(descriptor$reference_until_exclusive) || context$as_of < descriptor$reference_until_exclusive),
    "as_of", "outside this document snapshot's reference range", "UNSUPPORTED_RULESET")
  .s2_require(context$known_at >= descriptor$knowledge_not_before,
              "known_at", "documents not available at requested knowledge date", "UNSUPPORTED_RULESET")
  .s2_require(is.list(context$overrides), "overrides", "list of NumericOverride entries required")
  if (length(context$overrides)) .s2_overlay(context)
  profile
}

#' A traceable numeric analyst override
#' @param resource Logical resource name, for example `"profile.json"`.
#' @param path List of JSON keys and zero-based integer array indices.
#' @param value Finite numeric replacement.
#' @param reason Nonempty explanation recorded with the result.
#' @return A `NumericOverride` list, to include in [Context()]'s overrides.
#' @details Symmetric matrix entries are mirrored; supply only one side.
#'   Overrides never remove applicability or unsupported-source guards.
#' @seealso [Context()], [parameter()]
#' @examples
#' change <- NumericOverride("profile.json",
#'   list("scalars", "intangible", "value"), 0.7, "Synthetic sensitivity")
#' context <- Context("2025-01-17", "2026-09-23", "EUR",
#'   "EU-S2-DOCUMENTS-2024-2025-v0.1", overrides = list(change))
#' intangible_risk(100, context = context)$value
#' @export
NumericOverride <- function(resource, path, value, reason) {
  .s2_require(.s2_string(resource) && is.list(path) && length(path) > 0L,
              "overrides", "resource and JSON path list required")
  .s2_require(.s2_string(reason) && nzchar(trimws(reason)), "override.reason", "nonempty explanation required")
  value <- .s2_number(value, "override.value")
  structure(list(resource = resource, path = path, value = value, reason = reason),
            class = c("NumericOverride", "list"))
}

.s2_override_kind <- function(resource, path) {
  n <- length(path)
  at <- function(i, value) n >= i && identical(path[[i]], value)
  if (resource == "profile.json") {
    if (n == 3L && at(1, "scalars") && at(3, "value")) return("number")
    if (n == 5L && at(1, "matrices") && at(3, "values")) return("matrix")
    if (n == 5L && at(1, "tables") && at(3, "rows")) return("number")
  }
  if (resource == "catastrophe_matrices.json" && n == 5L && at(1, "matrices") && at(3, "values")) return("matrix")
  if (resource == "catastrophe_regions.json" && at(1, "perils")) {
    if (n == 4L && at(3, "factors")) return("nonnegative")
    if (n == 6L && at(3, "matrix") && at(4, "values")) return("matrix")
  }
  if (resource == "catastrophe_weights.json") {
    if (n == 5L && at(1, "tables") && at(3, "weights")) return("nonnegative")
    if (n == 3L && at(1, "special_factors") && at(3, "value")) return("nonnegative")
  }
  if (resource == "rfr_monthly.json" && at(1, "datasets")) {
    if (n == 8L && at(3, "series") && at(5, "variants") && at(7, "rates")) return("number")
    if (n == 9L && at(3, "government_spreads") && at(4, "tables") && at(6, "rows") && at(8, "values_percent")) return("number")
    if (n == 12L && at(3, "corporate_credit") && at(4, "series") && at(6, "sectors") && at(8, "tables") && at(10, "values")) return("number")
    if (at(3, "supplemental_ltas") && at(4, "tables") && at(6, "rows") &&
        ((n == 8L && at(8, "value_percent")) || (n == 9L && at(8, "values_percent")))) return("number")
  }
  .s2_fail("UNSUPPORTED_PARAMETER", "override.path", "numeric rule cell not supported")
}

.s2_path_index <- function(node, part) {
  if (!is.null(names(node))) {
    .s2_require(.s2_string(part) && part %in% names(node), "override.path", "unknown key", "UNSUPPORTED_PARAMETER")
    return(part)
  }
  .s2_require(is.list(node) && is.numeric(part) && length(part) == 1L &&
    is.finite(part) && part == floor(part) && part >= 0 && part < length(node),
    "override.path", "unknown array index", "UNSUPPORTED_PARAMETER")
  as.integer(part + 1L)
}

.s2_cell <- function(data, path) {
  for (part in path) data <- data[[.s2_path_index(data, part)]]
  .s2_require(is.numeric(data) && length(data) == 1L, "override.path", "existing numeric cell required", "UNSUPPORTED_PARAMETER")
  as.numeric(data)
}

.s2_set_cell <- function(data, path, value) {
  index <- .s2_path_index(data, path[[1L]])
  if (length(path) == 1L) data[[index]] <- value else data[[index]] <- .s2_set_cell(data[[index]], path[-1L], value)
  data
}

.s2_overlay <- function(context) {
  descriptor <- .s2_descriptor(context$profile_id)
  result <- list(data = list(), audit = list())
  seen <- character()
  for (entry in context$overrides) {
    .s2_require(inherits(entry, "NumericOverride"), "overrides", "NumericOverride entries required")
    .s2_resource_allowed(entry$resource, descriptor)
    .s2_require(.s2_string(entry$reason) && nzchar(trimws(entry$reason)), "override.reason", "nonempty explanation required")
    kind <- .s2_override_kind(entry$resource, entry$path)
    value <- .s2_number(entry$value, "override.value", if (kind == "nonnegative") 0 else NULL,
                        if (kind == "matrix") 1 else NULL)
    identifier_path <- entry$path
    n <- length(identifier_path)
    if (kind == "matrix") {
      indices <- unlist(entry$path[(n - 1L):n], use.names = FALSE)
      .s2_require(is.numeric(indices) && length(indices) == 2L && all(is.finite(indices)) &&
        all(indices == floor(indices)), "override.path", "integer matrix indices required")
      .s2_require(value >= -1 && (indices[[1L]] != indices[[2L]] || value == 1),
                  "override.value", "correlation bounds or unit diagonal violated")
      identifier_path[(n - 1L):n] <- as.list(sort(indices))
    }
    identifier <- .s2_fingerprint(list(entry$resource, identifier_path))
    .s2_require(!(identifier %in% seen), "override.path", "duplicate numeric override")
    seen <- c(seen, identifier)
    if (is.null(result$data[[entry$resource]])) {
      physical <- if (entry$resource == "profile.json") descriptor$resource else descriptor$resource_aliases[[entry$resource]]
      if (is.null(physical)) physical <- entry$resource
      result$data[[entry$resource]] <- .s2_load(physical)
    }
    data <- result$data[[entry$resource]]
    original <- .s2_cell(data, entry$path)
    data <- .s2_set_cell(data, entry$path, value)
    if (kind == "matrix" && indices[[1L]] != indices[[2L]]) {
      mirror <- entry$path
      mirror[(n - 1L):n] <- rev(mirror[(n - 1L):n])
      mirror_original <- .s2_cell(data, mirror)
      .s2_require(mirror_original == original || mirror_original == value,
                  "override.path", "matrix conflicts with earlier override")
      data <- .s2_set_cell(data, mirror, value)
    }
    result$data[[entry$resource]] <- data
    result$audit[[length(result$audit) + 1L]] <- list(resource = entry$resource,
      path = entry$path, original = original, value = value, reason = entry$reason)
  }
  result
}

.s2_effective <- function(name, context) {
  descriptor <- .s2_descriptor(context$profile_id)
  .s2_resource_allowed(name, descriptor)
  if (length(context$overrides)) {
    data <- .s2_overlay(context)$data[[name]]
    if (!is.null(data)) return(data)
  }
  physical <- if (name == "profile.json") descriptor$resource else descriptor$resource_aliases[[name]]
  .s2_load(if (is.null(physical)) name else physical)
}

.s2_scalar <- function(key, context) .s2_effective("profile.json", context)$scalars[[key]]$value

# Installed profiles are hash-checked on first read and immutable. Hash them
# once; scenarios still include their own audit and never reuse the base hash.
.s2_profile_hash <- function(profile, context, audit = list()) {
  if (length(audit)) return(.s2_fingerprint(list(profile = profile, overrides = audit)))
  key <- paste0("profile_hash/", context$profile_id)
  if (!exists(key, envir = .s2_cache, inherits = FALSE))
    .s2_cache[[key]] <- .s2_fingerprint(profile)
  .s2_cache[[key]]
}

.s2_calculate <- function(name, inputs, context, implementation) {
  inputs <- tryCatch(inputs, error = function(e) .s2_fail("INVALID_INPUT", name, conditionMessage(e)))
  profile <- .s2_validate_context(context)
  descriptor <- .s2_descriptor(context$profile_id)
  .s2_require((identical(descriptor$supported_calculations, "ALL_IMPLEMENTED_BASELINE") ||
    name %in% unlist(descriptor$supported_calculations)) && !(name %in% unlist(descriptor$excluded_calculations)),
    name, "calculation not implemented for selected profile", "UNSUPPORTED_RULESET")
  contract <- .s2_contracts()[[name]]
  .s2_require(!is.null(contract), name, "unknown calculation", "UNSUPPORTED_METHOD")
  result <- tryCatch(implementation(inputs, context),
    error = function(e) {
      if (inherits(e, c("s2_overflow", "s2_zero_division")))
        .s2_fail("NUMERICAL_ERROR", name, conditionMessage(e))
      if (startsWith(name, "rfr_archived_") &&
        (!inherits(e, "s2_error") || identical(e$field, "key") || identical(e$field, "index")))
        .s2_fail("INVALID_INPUT", "market_data", paste("invalid table schema:", conditionMessage(e)))
      stop(e)
    })
  if (inherits(result$value, "bigz")) result$value <- as.numeric(result$value)
  result$value <- .s2_number(result$value, "result")
  if (is.null(result$details)) result$details <- .s2_object()
  sources <- unique(c(unlist(contract$sources), unlist(result$details[["_sources"]])))
  result$details[["_sources"]] <- NULL
  audit <- if (length(context$overrides)) .s2_overlay(context)$audit else list()
  status <- if (length(audit)) "ANALYST_SCENARIO" else "REFERENCE_COMPONENT"
  if (length(audit)) {
    result$details$analyst_overrides <- audit
    sources <- c(sources, "ANALYST_OVERRIDE")
  }
  if (identical(result$details$market_data_kind, "SYNTHETIC_DEMO")) {
    status <- "SYNTHETIC_DEMO"
    sources <- c(sources, "SYNTHETIC_DEMO")
  }
  structure(list(value = result$value, unit = if (contract$unit == "currency") context$currency else contract$unit,
    formula_id = name, sources = as.list(sources), context = context,
    input_hash = .s2_fingerprint(list(formula = name, inputs = inputs, context = context)),
    profile_hash = .s2_profile_hash(profile, context, audit),
    details = result$details, status = status), class = c("Result", "list"))
}

#' @rdname Result
#' @param x A result to print.
#' @param ... Additional printing arguments (currently unused).
#' @export
print.Result <- function(x, ...) {
  cat(x$formula_id, ": ", format(x$value, digits = 12), " ", x$unit,
      " [", x$status, "]\n", sep = "")
  invisible(x)
}

#' Structured calculation failure
#' @param code Machine-readable error code.
#' @param field Offending input or resource field.
#' @param message Human-readable explanation.
#' @return An R condition inheriting from `CalculationError`, `s2_error`, `error`
#'   and `condition`. Constructing it does not signal it; use `stop()` to signal.
#' @examples
#' error <- CalculationError("INVALID_INPUT", "amount", "finite amount required")
#' error$code
#' @export
CalculationError <- function(code, field, message) structure(
  list(message = paste(code, field, message, sep = ": "), call = NULL, code = code, field = field),
  class = c("CalculationError", "s2_error", "error", "condition"))

#' Structured calculation result and provenance
#' @param value Calculated numeric value.
#' @param unit Explicit value unit.
#' @param formula_id Formula identifier.
#' @param sources Source-reference list.
#' @param context Explicit [Context()].
#' @param input_hash,profile_hash Reproducible R input and profile fingerprints.
#' @param details Formula-specific trace and qualification metadata.
#' @param status Component/scenario status; default `"REFERENCE_COMPONENT"`.
#' @return An ordinary R list with S3 class `Result`. Construction itself does not
#'   validate or approve an external calculation. Use the formula APIs to calculate.
#' @examples
#' ctx <- Context("2025-01-17", "2026-09-23", "EUR",
#'                "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' intangible_risk(100, context = ctx)
#' @export
Result <- function(value, unit, formula_id, sources, context, input_hash, profile_hash,
  details, status = "REFERENCE_COMPONENT") structure(list(value = value, unit = unit,
    formula_id = formula_id, sources = sources, context = context, input_hash = input_hash,
    profile_hash = profile_hash, details = details, status = status), class = c("Result", "list"))
