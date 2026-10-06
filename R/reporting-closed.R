.s2_reporting_context <- function(context) {
  if (!.s2_mapping(context) || !setequal(names(context), c("entity", "period", "unit")) ||
    !all(vapply(context, function(value) .s2_string(value) && nzchar(trimws(value)), logical(1)))) stop("Explicit entity, period and unit required")
}
.s2_reporting_products <- function(axes) {
  rows <- list(list())
  for (axis in axes) {
    following <- list()
    for (row in rows) for (value in axis) following[[length(following) + 1L]] <- c(row, list(value))
    rows <- following
  }
  rows
}
.s2_reporting_bind_closed <- function(fields, table, facts, context) {
  required <- c("t", "r", "c", "id", "seq", "f", "fv")
  if (!.s2_mapping(fields) || !all(required %in% names(fields)) || length(setdiff(names(fields), c(required, "dv", "z"))))
    stop("Only explicit closed-cell selector attributes supported")
  if (fields$f != "solvency" || fields$fv != "solvency2" || !fields$seq %in% c("True", "False")) stop("Unsupported framework or sequence mode")
  if (!identical(fields$t, table$table$TableCode)) stop("Wrong physical table")
  if (!length(table$axes) || any(vapply(table$axes, function(axis) !isTRUE(axis$IsOpenAxis == 0), logical(1)))) stop("Open or unqualified table axes not supported")
  .s2_reporting_context(context)
  coordinates <- list(); count <- 1L
  for (key in c("r", "c", "z")) {
    if (key == "z" && !key %in% names(fields)) {
      if (!length(table$cells) || any(vapply(table$axes, function(axis) identical(axis$AxisOrientation, "Z"), logical(1))) ||
        any(vapply(table$cells, function(cell) length(strsplit(gsub("^[{]|[}]$", "", cell$BusinessCode), ",", fixed = TRUE)[[1L]]) != 3L, logical(1))))
        stop("Missing sheet coordinate requires a source table without sheet axis")
      coordinates[[length(coordinates) + 1L]] <- list(NULL)
      next
    }
    value <- fields[[key]]
    if (!.s2_string(value)) stop("Explicit coordinate list required")
    items <- trimws(strsplit(value, ";", fixed = TRUE)[[1L]])
    if (!length(items) || endsWith(value, ";") || anyDuplicated(items) || any(!grepl(paste0("^", toupper(key), "[0-9]{4}$"), items))) stop("Unsupported or repeated coordinate")
    coordinates[[length(coordinates) + 1L]] <- as.list(items); count <- count * length(items)
  }
  if (count > 10000L || (fields$seq == "False" && count != 1L)) stop("Unsupported cell selection cardinality")
  indexed <- .s2_object()
  for (cell in table$cells) {
    code <- cell$BusinessCode
    if (code %in% names(indexed)) stop("Ambiguous physical cell")
    indexed[[code]] <- cell
  }
  if (!is.list(facts) || !is.null(names(facts))) stop("Explicit fact list required")
  selected <- .s2_object()
  for (fact in facts) {
    if (!.s2_mapping(fact) || !(setequal(names(fact), c("cell", "context", "value", "decimals")) || setequal(names(fact), c("cell", "context", "fact"))))
      stop("Explicit cell/context and either value/decimals or one tagged fact required")
    if (!.s2_string(fact$cell) || !.s2_mapping(fact$context)) stop("Invalid fact cell or context")
    if (.s2_equal(fact$context, context)) {
      if (fact$cell %in% names(selected)) stop("Duplicate fact for cell/context")
      selected[[fact$cell]] <- fact
    }
  }
  values <- cell_ids <- signatures <- xbrl_signatures <- list()
  for (coordinates_row in .s2_reporting_products(coordinates)) {
    code <- paste0("{", paste(c(fields$t, unlist(coordinates_row)), collapse = ","), "}")
    cell <- indexed[[code]]
    if (is.null(cell) || !isTRUE(cell$IsShaded == 0)) stop("Missing or shaded source cell")
    signature <- cell$DatapointSignature
    if (!.s2_string(signature) || !nzchar(signature) || any(vapply(c("*", "[", "]", "?"), grepl, logical(1), signature, fixed = TRUE))) stop("Unresolved data-point signature not supported")
    if (!code %in% names(selected)) stop("Missing explicitly qualified fact; no implicit fallback")
    fact <- selected[[code]]
    atom <- if ("fact" %in% names(fact)) fact$fact else list(value = fact$value, decimals = fact$decimals)
    validated <- .s2_reporting_fact(atom)
    if (is.null(validated) && fields$seq == "True") stop("Absence is not a sequence item; no implicit fallback")
    i <- length(values) + 1L
    values[[i]] <- atom; cell_ids[[i]] <- cell$CellID; signatures[[i]] <- cell$DatapointSignature; xbrl_signatures[i] <- list(cell$DPS)
  }
  list(binding = if (fields$seq == "True") list(values = values) else values[[1L]],
    cell_ids = cell_ids, datapoint_signatures = signatures, xbrl_signatures = xbrl_signatures,
    context = context, context_qualification_verified = FALSE, fallbacks_applied = FALSE)
}
.s2_reporting_complete_coordinates <- function(fields, table, coordinates) {
  if (!length(table$axes) || any(vapply(table$axes, function(axis) !isTRUE(axis$IsOpenAxis == 0), logical(1)))) stop("Open-table scope binding not supported")
  inherited <- singleton <- .s2_object(); absent <- list()
  for (axis in c("r", "c", "z")) if (!axis %in% names(fields) && axis %in% names(coordinates)) {
    fields[[axis]] <- coordinates[[axis]]; inherited[[axis]] <- coordinates[[axis]]
  }
  for (i in 1:3) {
    axis <- c("r", "c", "z")[[i]]
    if (axis %in% names(fields)) next
    members <- list()
    for (cell in table$cells) {
      code <- strsplit(gsub("^[{]|[}]$", "", cell$BusinessCode), ",", fixed = TRUE)[[1L]]
      if (!length(code) %in% c(3L, 4L) || code[[1L]] != fields$t) stop("Ambiguous closed-table business coordinate")
      member <- if (length(code) >= i + 1L) code[[i + 1L]] else NULL
      if (!any(vapply(members, identical, logical(1), member))) members[length(members) + 1L] <- list(member)
    }
    if (length(members) != 1L) stop("Missing coordinate is not a unique closed-table axis")
    if (is.null(members[[1L]]) && axis == "z") {
      if (any(vapply(table$axes, function(a) identical(a$AxisOrientation, "Z"), logical(1)))) stop("Sheet axis conflicts with source business cells")
      absent[[length(absent) + 1L]] <- axis; next
    }
    fields[[axis]] <- members[[1L]]; singleton[[axis]] <- members[[1L]]
  }
  list(fields = fields, audit = list(inherited_scope_coordinates = inherited,
    source_singleton_coordinates = singleton, source_absent_axes = absent))
}
.s2_reporting_bind_selectors <- function(rule, table_lookup, facts, context, coordinate_scope = NULL) {
  .s2_reporting_context(context)
  if (!is.list(facts) || !is.null(names(facts))) stop("Explicit fact list required")
  if (any(vapply(c("Filter", "Join", "Scope", "Precondition", "Prerequisites"), function(key) .s2_truth(rule[[key]]), logical(1)))) stop("Filtered/joined/scoped rule not supported")
  selectors <- bindings <- evidence <- tables <- .s2_object(); used <- character()
  walk <- function(node) {
    if (node$kind == "selector") {
      fields <- .s2_selector_fields(node$value); id <- fields$id
      if (!.s2_string(id) || !grepl("^[A-Za-z_][A-Za-z_0-9]*$", id)) stop("Explicit source variable identifier required")
      if (id %in% names(selectors)) {
        if (!identical(selectors[[id]], node$value)) stop("Conflicting selectors for one variable")
        return(invisible(NULL))
      }
      code <- fields$t
      if (!.s2_string(code) || !nzchar(code)) stop("Explicit physical table required")
      if (!code %in% names(tables)) tables[[code]] <<- table_lookup(code)
      audit <- .s2_object()
      if (!is.null(coordinate_scope)) {
        completed <- .s2_reporting_complete_coordinates(fields, tables[[code]], coordinate_scope[[code]])
        fields <- completed$fields; audit <- completed$audit
        if (length(audit$inherited_scope_coordinates)) used <<- unique(c(used, code))
      }
      result <- .s2_reporting_bind_closed(fields, tables[[code]], facts, context)
      selectors[[id]] <<- node$value; bindings[[id]] <<- result$binding
      evidence[[id]] <<- c(result[setdiff(names(result), "binding")], audit)
    }
    for (child in rev(node$children)) walk(child)
  }
  walk(.s2_expression_parse(rule$Rule))
  if (!is.null(coordinate_scope) && !setequal(used, names(coordinate_scope))) stop("Scope table has no inherited selector coordinates")
  list(bindings = bindings, selectors = selectors, evidence = evidence, status = "CLOSED_TABLE_BINDING_COMPONENT", full_report_validated = FALSE)
}
.s2_reporting_scope_coordinates <- function(expression) {
  tokens <- .s2_expression_tokens(expression)
  if (length(tokens) < 4L || !identical(tokens[1:2], c("scope", "(")) || tail(tokens, 1L) != ")") stop("Explicit scope(selector,...) required")
  body <- tokens[3:(length(tokens) - 1L)]
  if (length(body) %% 2L != 1L || any(body[seq_along(body) %% 2L == 0L] != ",")) stop("Explicit scope selector list required")
  tables <- .s2_object(); axes <- list()
  for (token in body[seq_along(body) %% 2L == 1L]) {
    if (!startsWith(token, "{") || !endsWith(token, "}")) stop("Scope requires only table selectors")
    fields <- .s2_selector_fields(token)
    if (!all(c("t", "f", "fv") %in% names(fields)) || length(setdiff(names(fields), c("t", "r", "c", "z", "f", "fv")))) stop("Only explicit table/row/column/sheet scopes supported")
    if (fields$f != "solvency" || fields$fv != "solvency2" || fields$t %in% names(tables)) stop("Unknown scope framework or repeated table")
    dimensions <- fields[intersect(c("r", "c", "z"), names(fields))]
    if (!length(dimensions)) stop("Scope must declare coordinates")
    tables[[fields$t]] <- dimensions
    for (axis in names(dimensions)) {
      value <- dimensions[[axis]]; members <- trimws(strsplit(value, ";", fixed = TRUE)[[1L]])
      if (!length(members) || endsWith(value, ";") || anyDuplicated(members) || any(!grepl(paste0("^", toupper(axis), "[0-9]{4}$"), members))) stop("Only distinct explicit scope coordinates supported")
      axes[[length(axes) + 1L]] <- list(table = fields$t, axis = axis, members = as.list(members))
    }
  }
  if (length(tables) > 1L && any(vapply(axes, function(axis) length(axis$members) != 1L, logical(1)))) stop("Cross-table range pairing requires separate qualification")
  if (prod(vapply(axes, function(axis) length(axis$members), integer(1))) > 10000L) stop("Technical scope expansion limit")
  lapply(.s2_reporting_products(lapply(axes, `[[`, "members")), function(values) {
    selected <- stats::setNames(rep(list(.s2_object()), length(tables)), names(tables))
    for (i in seq_along(axes)) selected[[axes[[i]]$table]][[axes[[i]]$axis]] <- values[[i]]
    selected
  })
}
.s2_reporting_check_scoped <- function(rule, table_lookup, facts, context, technical_version, binding_reference, source, namespace_loader, unit_by_cell = NULL) {
  evaluation_rule <- rule; evaluation_rule["Scope"] <- list(NULL)
  checks <- list(); tables <- .s2_object()
  lookup <- function(code) {
    if (!code %in% names(tables)) tables[[code]] <<- table_lookup(code)
    tables[[code]]
  }
  for (coordinates in .s2_reporting_scope_coordinates(rule$Scope)) {
    prepared <- .s2_reporting_bind_selectors(evaluation_rule, lookup, facts, context, coordinates)
    if (!is.null(unit_by_cell)) {
      selected <- unique(unlist(lapply(prepared$evidence, `[[`, "cell_ids")))
      units <- Filter(Negate(is.null), lapply(selected, function(cell) unit_by_cell[[as.character(cell)]]))
      if (length(unique(vapply(units, .s2_fingerprint, character(1)))) > 1L) stop("Cross-unit expression requires qualified dimensional evaluation")
    }
    result <- .s2_reporting_check_bound(evaluation_rule, prepared$bindings, technical_version, binding_reference, source, namespace_loader)
    result$evaluation_rule_hash <- result$rule_hash; result$rule_hash <- .s2_fingerprint(rule)
    checks[[length(checks) + 1L]] <- list(coordinates = coordinates, expression = result, cell_bindings = prepared$evidence)
  }
  list(value = all(vapply(checks, function(check) check$expression$value, logical(1))),
    status = "SCOPED_CLOSED_TABLE_EXPRESSION_COMPONENT", scope_checks = checks,
    validation_code = rule$ValidationCode, technical_version = technical_version, source = source,
    rule_hash = .s2_fingerprint(rule), original_scope = rule$Scope, binding_reference = binding_reference,
    closed_cell_selection_verified = TRUE, binding_qualification_verified = FALSE, full_report_validated = FALSE,
    legal_applicability_verified = FALSE, fact_schema_validated = FALSE, fact_domain_membership_verified = FALSE,
    current_deactivations_checked = FALSE, fact_selection_hash = .s2_fingerprint(list(facts = facts, fact_context = context)))
}

#' Check a rule against explicit closed-table cell facts
#' @param validation_code Exact archived validation code.
#' @param facts Ordered list of explicit cell/context and tagged fact records.
#' @param technical_version Explicit archived technical release.
#' @param fact_context Named list containing explicit `entity`, `period`, `unit`.
#' @param binding_reference External qualification reference.
#' @return Component check with cell bindings and source provenance; scoped rules
#'   include the individual coordinate checks. Missing cells never default to zero.
#' @details No XBRL import, automatic filing obligation, open-table expansion or
#'   full-report approval is performed.
#' @examples
#' cases <- reference_dataset("reference_reporting_scope_cases.json")$cases
#' case <- cases[[1]]
#' reporting_closed_rule_check(case$validation_code, case$facts,
#'   case$technical_version, case$fact_context, case$binding_reference)$value
#' @export
reporting_closed_rule_check <- function(validation_code, facts, technical_version, fact_context, binding_reference) {
  .s2_require(.s2_string(validation_code) && nzchar(validation_code), "validation_code", "exact source code required")
  resource <- reporting_data("rules", technical_version)
  selected <- Filter(function(rule) identical(rule$ValidationCode, validation_code), resource$tables$vValidationRuleExpressions)
  .s2_require(length(selected) == 1L, "validation_code", "unknown or ambiguous archived rule", "UNSUPPORTED_PARAMETER")
  rule <- selected[[1L]]; lookup <- function(code) reporting_table(code, technical_version)
  tryCatch({
    if (.s2_truth(rule$Scope)) return(.s2_reporting_check_scoped(rule, lookup, facts, fact_context,
      technical_version, binding_reference, resource$source, function() reporting_catalog(technical_version)$xbrl_namespace_bindings))
    prepared <- .s2_reporting_bind_selectors(rule, lookup, facts, fact_context)
    result <- .s2_reporting_check_bound(rule, prepared$bindings, technical_version, binding_reference,
      resource$source, function() reporting_catalog(technical_version)$xbrl_namespace_bindings)
    result$status <- "CLOSED_TABLE_EXPRESSION_COMPONENT"; result$cell_bindings <- prepared$evidence
    result$closed_cell_selection_verified <- TRUE
    result$fact_selection_hash <- .s2_fingerprint(list(facts = facts, fact_context = fact_context))
    result
  }, error = function(error) {
    if (inherits(error, "s2_error")) stop(error)
    .s2_fail("UNSUPPORTED_METHOD", "closed_binding", conditionMessage(error))
  })
}
