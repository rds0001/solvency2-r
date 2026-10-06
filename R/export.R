#' Export local reference resources or an explicit analyst scenario
#' @param destination Path to a new directory. Existing paths are never overwritten.
#' @param context Optional [Context()]. If supplied, only resources qualified for
#'   that profile and matching synthetic references are exported.
#' @return Exported filenames and SHA-256 hashes of their actual JSON bytes.
#' @details This explicitly requested local write does not download anything.
#'   Analyst overrides are marked as scenarios, not regulatory approval. Exported
#'   JSON formatting and its hashes are R-specific; source hashes remain provenance.
#' @examples
#' # Creates a new temporary directory only when explicitly called:
#' destination <- tempfile("solvency2-export-")
#' result <- export_resources(destination)
#' length(result$files)
#' # Remove only the directory just created by this example.
#' unlink(destination, recursive = TRUE)
#' @export
export_resources <- function(destination, context = NULL) {
  .s2_local_path(destination)
  .s2_require(!file.exists(destination) && !dir.exists(destination), "destination", "must be a new directory")
  if (!is.null(context)) .s2_validate_context(context)
  names <- c("profile.json", "reference_cases.json", "reference_portfolios.json", "coverage.json", "rfr_monthly.json",
    "catastrophe_matrices.json", "catastrophe_regions.json", "catastrophe_weights.json",
    "catastrophe_zone_map.json", "catastrophe_postcode_groups.json", "catastrophe_geography_raw.json",
    "catastrophe_administrative.json", "catastrophe_zone_links.json")
  descriptor <- if (is.null(context)) NULL else .s2_descriptor(context$profile_id)
  if (!is.null(descriptor) && !identical(descriptor$supported_resources, "BASELINE_RESOURCES"))
    names <- names[names %in% unlist(descriptor$supported_resources)]
  contents <- stats::setNames(lapply(names, function(name) if (is.null(context)) .s2_load(name) else .s2_effective(name, context)), names)
  contents[["rule_profiles.json"]] <- rule_profiles()
  if (is.null(context)) {
    for (scope in contents[["rule_profiles.json"]]$profiles) {
      for (physical in c(scope$resource, unlist(scope$resource_aliases, use.names = FALSE)))
        if (!physical %in% names(contents)) contents[[physical]] <- .s2_load(physical)
    }
  } else {
    descriptor$resource <- "profile.json"
    if ("resource_aliases" %in% names(descriptor)) descriptor$resource_aliases <- .s2_object()
    contents[["rule_profiles.json"]]$profiles <- stats::setNames(list(descriptor), context$profile_id)
    baseline_id <- .s2_load("profile.json")$profile_id
    cases <- reference_cases()
    cases$cases <- Filter(function(case) {
      id <- case$context$profile_id
      identical(if (is.null(id)) baseline_id else id, context$profile_id)
    }, cases$cases)
    contents[["reference_cases.json"]] <- cases
    portfolios <- reference_portfolios()
    portfolios$portfolios <- Filter(function(portfolio) identical(portfolio$context$profile_id, context$profile_id), portfolios$portfolios)
    contents[["reference_portfolios.json"]] <- portfolios
    contents[["export_context.json"]] <- list(context = unclass(context), profile_scope = descriptor, status = "PARTIAL_REFERENCE_ONLY")
    if (length(context$overrides)) contents[["analyst_overrides.json"]] <- list(
      status = "ANALYST_SCENARIO_NOT_REGULATORY_APPROVAL", context = unclass(context),
      overrides = .s2_overlay(context)$audit, reference_cases_status = "BASELINE_ONLY", reference_portfolios_status = "BASELINE_ONLY")
  }
  .s2_require(dir.create(destination, recursive = TRUE, showWarnings = FALSE), "destination", "cannot create export directory")
  hashes <- .s2_object()
  for (name in names(contents)) {
    path <- file.path(destination, name)
    jsonlite::write_json(contents[[name]], path, auto_unbox = TRUE, digits = NA, null = "null", pretty = FALSE)
    hashes[[name]] <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  }
  jsonlite::write_json(list(sha256 = hashes), file.path(destination, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  list(files = as.list(names(hashes)), sha256 = hashes)
}
