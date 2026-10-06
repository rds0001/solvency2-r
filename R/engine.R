#' Execute a dependency graph of granular calculations
#'
#' @param nodes Named list of nodes, each containing `function` (an API name)
#'   and `inputs` (a named list). `list("$ref" = "node_id")` passes a dependency's
#'   numeric value into an input; provenance stays in the node results.
#' @param context Explicit [Context()] shared by all nodes.
#' @return `Engine()` returns an object with a `run(nodes, context)` function
#'   and an instance-local cache. `calculate()` returns a list containing
#'   results, execution order, cache hits, coverage and profile scope.
#' @details Cycles and missing dependencies raise structured conditions. Cached
#'   numeric results do not suppress synthetic-data provenance. Outputs retain
#'   `PARTIAL` status: combining reference components does not approve an SCR.
#' @examples
#' ctx <- Context("2025-01-17", "2026-09-23", "EUR",
#'                "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' nodes <- list(charge = list("function" = "intangible_risk",
#'                            inputs = list(value = 100)))
#' # Use a source reference's exact input names:
#' case <- Filter(function(x) x[["function"]] == "intangible_risk" &&
#'                  is.null(x$expected_error), reference_cases()$cases)[[1]]
#' nodes$charge$inputs <- case$kwargs
#' calculate(nodes, ctx)$results$charge$value
#' @export
Engine <- function() {
  cache <- new.env(parent = emptyenv())
  run <- function(nodes, context) {
    .s2_validate_context(context)
    .s2_require(.s2_mapping(nodes), "nodes", "mapping required")
    .s2_require(all(nzchar(names(nodes))), "nodes", "nonempty string IDs required")
    outputs <- .s2_object(); visiting <- character(); order <- hits <- list()
    references <- function(value) {
      if (!is.list(value)) return(character())
      if (.s2_mapping(value) && "$ref" %in% names(value)) return(value[["$ref"]])
      unlist(lapply(value, references), use.names = FALSE)
    }
    resolve <- function(value) {
      if (!is.list(value)) return(value)
      if (.s2_mapping(value) && "$ref" %in% names(value)) {
        .s2_require(identical(names(value), "$ref") && .s2_string(value[["$ref"]]), "$ref", "node ID required")
        return(evaluate(value[["$ref"]])$value)
      }
      lapply(value, resolve)
    }
    evaluate <- function(id) {
      if (id %in% names(outputs)) return(outputs[[id]])
      .s2_require(id %in% names(nodes), id, "missing dependency", "MISSING_INPUT")
      .s2_require(!id %in% visiting, id, "cyclic dependency", "DEPENDENCY_CYCLE")
      visiting <<- c(visiting, id)
      node <- nodes[[id]]
      .s2_require(.s2_mapping(node) && setequal(names(node), c("function", "inputs")), id, "function and inputs required")
      name <- node[["function"]]
      .s2_require(.s2_string(name) && name %in% names(.s2_implementations), id, "unknown formula", "UNSUPPORTED_METHOD")
      .s2_require(.s2_mapping(node$inputs) && !"context" %in% names(node$inputs), id,
        "input mapping without context override required")
      inputs <- resolve(node$inputs)
      # Resources and native code are immutable within this package instance;
      # Context includes the complete explicit analyst scenario.
      .s2_json_input(inputs, id)
      key <- .s2_fingerprint(list(name = name, inputs = inputs, context = context))
      if (exists(key, cache, inherits = FALSE)) {
        result <- cache[[key]]; hits[[length(hits) + 1L]] <<- id
      } else {
        result <- do.call(get(name, envir = environment(.s2_calculate)), c(inputs, list(context = context)))
        cache[[key]] <- result
      }
      dependencies <- unique(references(node$inputs))
      demo <- sort(Filter(function(ref) identical(outputs[[ref]]$status, "SYNTHETIC_DEMO"), dependencies))
      if (length(demo)) {
        result$status <- "SYNTHETIC_DEMO"
        result$details$synthetic_demo_dependencies <- as.list(demo)
        result$sources <- as.list(unique(c(unlist(result$sources), "SYNTHETIC_DEMO")))
      }
      outputs[[id]] <<- result
      visiting <<- setdiff(visiting, id)
      order[[length(order) + 1L]] <<- id
      result
    }
    for (id in names(nodes)) evaluate(id)
    list(status = "PARTIAL", results = outputs, execution_order = order,
      cache_hits = hits, regulatory_total_released = FALSE, coverage = coverage(),
      context = unclass(context), profile_scope = .s2_descriptor(context$profile_id))
  }
  structure(list(run = run), class = "Engine")
}

#' @rdname Engine
#' @export
calculate <- function(nodes, context) Engine()$run(nodes, context)

#' Declared regulatory coverage and limitations
#' @return Named list inherited from the Golden Source. This describes regulatory
#'   scope, not certification or approval of a company's inputs or capital result.
#' @examples
#' names(coverage())
#' @export
coverage <- function() .s2_load("coverage.json")

#' Catalogue of Golden Source APIs
#' @return Named list of functions, domains, source signatures and contracts.
#'   Source signatures use Python notation; the R help pages give R signatures.
#' @examples
#' names(api_catalogue())
#' @export
api_catalogue <- function() .s2_load("library/api_catalogue.json")

#' Additional synthetic reporting reference datasets
#' @param name Exact dataset filename from `reference_datasets()`.
#' @return `reference_datasets()` returns dataset names. `reference_dataset()`
#'   returns the selected hash-verified dataset as a list.
#' @examples
#' reference_datasets()
#' names(reference_dataset(reference_datasets()[[1]]))
#' @export
reference_datasets <- function() {
  .s2_load("library/api_catalogue.json")
  names <- names(.s2_cache$manifest)
  sort(sub("^library/", "", names[startsWith(names, "library/reference_reporting")]))
}

#' @rdname reference_datasets
#' @export
reference_dataset <- function(name) {
  .s2_require(.s2_string(name) && name %in% reference_datasets(), "name", "unknown reference dataset")
  .s2_load(paste0("library/", name))
}
