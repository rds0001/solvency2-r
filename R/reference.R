#' Available dated rule profiles
#' @return Named list of profile intervals, supported methods and resource aliases.
#' @details Profiles and their applicability guards are inherited from the
#'   published Python Golden Source. No latest profile is selected implicitly.
#' @seealso [Context()], [parameter()]
#' @examples
#' names(rule_profiles()$profiles)
#' @export
rule_profiles <- function() .s2_load("rule_profiles.json")

#' Independent synthetic calculation references
#' @return A list with the fixed `cases`, including inputs, independently
#'   specified expected values or error codes, and derivation notes.
#' @details Regulatory constants are not synthetic; example portfolios and market
#'   inputs are. Accessing references does not run calculations.
#' @seealso [reference_portfolios()], [reference_case()]
#' @examples
#' # Select by function when looking for an example:
#' cases <- reference_cases()$cases
#' head(vapply(cases, function(x) x$id, character(1)))
#' @export
reference_cases <- function() .s2_load("reference_cases.json")

#' A single reference case by identifier
#' @param id Exact reference-case identifier from [reference_cases()].
#' @return The selected case, including its inputs, context changes and expected
#'   outcome. An unknown identifier raises a structured `MISSING_INPUT` condition.
#' @seealso [reference_cases()]
#' @examples
#' id <- reference_cases()$cases[[1]]$id
#' identical(reference_case(id)$id, id)
#' @export
reference_case <- function(id) {
  .s2_require(.s2_string(id), "id", "string identifier required")
  cases <- reference_cases()$cases
  if (!exists("case_ids", envir = .s2_cache, inherits = FALSE))
    .s2_cache$case_ids <- vapply(cases, function(x) x$id, character(1))
  index <- match(id, .s2_cache$case_ids)
  .s2_require(!is.na(index), "id", "unknown reference case", "MISSING_INPUT")
  cases[[index]]
}

#' Synthetic integration portfolios
#' @return A list containing named dependency nodes, explicit contexts and
#'   independently specified expected values for the reference portfolios.
#' @seealso [reference_cases()], [rule_profiles()]
#' @examples
#' length(reference_portfolios()$portfolios)
#' @export
reference_portfolios <- function() .s2_load("reference_portfolios.json")

#' Synthetic market-data template
#' @return A fresh-by-value list with an explicit `SYNTHETIC_DEMO` status, recipe
#'   and illustrative dated tables. These are not production market inputs.
#' @seealso [reference_cases()]
#' @examples
#' rfr_monthly_data()$status
#' @export
rfr_monthly_data <- function() .s2_load("rfr_monthly.json")

#' Parameter value and its regulatory provenance
#' @param parameter_id A slash-separated identifier, for example
#'   `"scalars/intangible"` or `"matrices/health_nslt"`.
#' @param context An explicit [Context()].
#' @return Named list with profile identifier/hash, value or table, regulatory
#'   sources and applicable numeric override audit.
#' @details `scalars`, `matrices` and `tables` use the selected profile.
#'   Catastrophe categories also expose matrices, regions, weights, links,
#'   postcode groups, raw geography and administrative mappings. All
#'   parameters are returned by value; edits do not change the installed package.
#' @seealso [NumericOverride()], [Context()]
#' @examples
#' ctx <- Context("2025-01-17", "2026-09-23", "EUR",
#'                "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' parameter("scalars/intangible", context = ctx)$value
#' parameter("matrices/health_nslt", context = ctx)$labels
#' @export
parameter <- function(parameter_id, context) {
  profile <- .s2_validate_context(context)
  .s2_require(.s2_string(parameter_id), "parameter_id", "string identifier required")
  slash <- regexpr("/", parameter_id, fixed = TRUE)[[1L]]
  .s2_require(slash > 1L, "parameter_id", "category/key required")
  category <- substr(parameter_id, 1L, slash - 1L)
  key <- substring(parameter_id, slash + 1L)
  .s2_require(category %in% c("scalars", "matrices", "tables", "catastrophe", "catastrophe_region",
    "catastrophe_weights", "catastrophe_links", "catastrophe_geography", "catastrophe_geography_raw",
    "catastrophe_administrative"), "parameter_id", "unsupported parameter category")
  effective <- .s2_effective("profile.json", context)
  audit <- if (length(context$overrides)) .s2_overlay(context)$audit else list()
  metadata <- list(parameter_id = parameter_id, profile_id = context$profile_id,
    profile_hash = .s2_profile_hash(profile, context, audit))
  available <- function(ok) .s2_require(ok, "parameter_id", "parameter not implemented", "UNSUPPORTED_PARAMETER")
  if (category %in% c("scalars", "matrices", "tables")) {
    available(key %in% names(effective[[category]]))
    content <- effective[[category]][[key]]
  } else {
    resource <- switch(category, catastrophe = "catastrophe_matrices.json",
      catastrophe_region = "catastrophe_regions.json", catastrophe_weights = "catastrophe_weights.json",
      catastrophe_links = "catastrophe_zone_links.json", catastrophe_geography = "catastrophe_postcode_groups.json",
      catastrophe_geography_raw = "catastrophe_geography_raw.json", catastrophe_administrative = "catastrophe_administrative.json")
    data <- .s2_effective(resource, context)
    if (category != "catastrophe_links") metadata$source_sha256 <- data$source$pdf_sha256
    if (category == "catastrophe") {
      .s2_require(is.null(data$blocked_matrices[[key]]), "parameter_id", "source matrix requires review", "REVIEW_REQUIRED")
      available(key %in% names(data$matrices)); content <- data$matrices[[key]]
    } else if (category == "catastrophe_region") {
      available(key %in% names(data$perils)); content <- data$perils[[key]]
    } else if (category == "catastrophe_links") {
      available(key %in% names(data$links)); content <- data$links[[key]]
    } else if (category == "catastrophe_geography_raw") {
      available(key %in% names(data$regions)); content <- data$regions[[key]]
    } else {
      split <- regexpr("/", key, fixed = TRUE)[[1L]]
      available(split > 0L)
      region <- substr(key, 1L, split - 1L); ordinal <- substring(key, split + 1L)
      if (category == "catastrophe_weights") {
        available(region %in% names(data$tables) && ordinal %in% names(data$tables[[region]]$weights))
        table <- data$tables[[region]]; weights <- table$weights[[ordinal]]
        blocked <- .s2_object()
        for (code in names(weights)) {
          cell <- paste(region, code, ordinal, sep = "/")
          if (cell %in% names(data$blocked_cells)) blocked[[code]] <- data$blocked_cells[[cell]]
        }
        content <- list(weights = weights, evidence = table$evidence[[ordinal]], source = table$source, blocked_regions = blocked)
      } else if (category == "catastrophe_geography") {
        available(region %in% names(data$regions) && ordinal %in% names(data$regions[[region]]$zones))
        group <- data$regions[[region]]
        content <- c(list(region = region, zone_ordinal = ordinal, prefix_length = group$prefix_length,
          source = group$source), group$zones[[ordinal]])
      } else {
        available(region %in% names(data$mapping) && ordinal %in% names(data$mapping[[region]]))
        content <- c(list(region = region, zone_ordinal = ordinal), data$mapping[[region]][[ordinal]])
      }
    }
  }
  value <- c(metadata, content)
  if (length(audit)) value$analyst_overrides <- audit
  value
}
