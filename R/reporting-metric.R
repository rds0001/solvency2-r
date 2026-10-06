.s2_reporting_index <- function(rows, key) {
  ids <- vapply(rows, function(row) as.character(row[[key]]), character(1))
  if (anyDuplicated(ids)) stop(paste("Ambiguous DPM identifier:", key))
  stats::setNames(rows, ids)
}
.s2_reporting_enumeration <- function(tables, metric, members, namespaces) {
  domains <- .s2_reporting_index(tables$mDomain, "DomainID")
  hierarchies <- .s2_reporting_index(tables$mHierarchy, "HierarchyID")
  domain <- as.character(metric$ReferencedDomainID); hierarchy <- as.character(metric$ReferencedHierarchyID)
  if (length(domain) != 1L || length(hierarchy) != 1L || !domain %in% names(domains) ||
    !identical(domains[[domain]]$IsTypedDomain, 0L) && !identical(domains[[domain]]$IsTypedDomain, 0) ||
    !hierarchy %in% names(hierarchies) || as.character(hierarchies[[hierarchy]]$DomainID) != domain)
    stop("Explicit consistent enumeration domain/hierarchy required")
  nodes <- .s2_reporting_index(Filter(function(row) as.character(row$HierarchyID) == hierarchy, tables$mHierarchyNode), "MemberID")
  if (!length(nodes)) stop("Missing enumeration hierarchy nodes")
  children <- stats::setNames(rep(list(character()), length(nodes)), names(nodes)); roots <- character()
  for (id in names(nodes)) {
    node <- nodes[[id]]
    if (!id %in% names(members) || as.character(members[[id]]$DomainID) != domain) stop("Hierarchy member/domain mismatch")
    if (!is.numeric(node$IsAbstract) || length(node$IsAbstract) != 1L || !node$IsAbstract %in% c(0, 1)) stop("Explicit abstract-member flag required")
    parent <- as.character(node$ParentMemberID)
    if (!length(parent)) roots <- c(roots, id) else if (parent %in% names(nodes)) children[[parent]] <- c(children[[parent]], id) else stop("Hierarchy parent missing")
  }
  pending <- roots; visited <- character()
  while (length(pending)) {
    id <- tail(pending, 1L); pending <- head(pending, -1L)
    if (id %in% visited) stop("Cyclic or ambiguous member hierarchy")
    visited <- c(visited, id); pending <- c(pending, children[[id]])
  }
  if (!setequal(visited, names(nodes))) stop("Cyclic member hierarchy")
  start <- as.character(metric$HierarchyStartingMemberID)
  if (!length(start)) selected <- names(nodes) else {
    included <- metric$IsStartingMemberIncluded
    if (!start %in% names(nodes) || !is.numeric(included) || length(included) != 1L || !included %in% c(0, 1)) stop("Unqualified hierarchy starting member")
    selected <- if (included == 1) start else character(); pending <- children[[start]]
    while (length(pending)) {
      id <- tail(pending, 1L); pending <- head(pending, -1L)
      selected <- c(selected, id); pending <- c(pending, children[[id]])
    }
  }
  selected <- selected[vapply(selected, function(id) nodes[[id]]$IsAbstract == 0, logical(1))]
  selected <- selected[order(as.numeric(selected))]
  qnames <- lapply(selected, function(id) .s2_reporting_expand(members[[id]]$MemberXBRLCode, namespaces))
  keys <- vapply(qnames, function(q) paste(q$namespace, q$local_name, sep = "\n"), character(1))
  if (anyDuplicated(keys)) stop("Ambiguous enumeration QName identity")
  list(ids = as.list(as.numeric(selected)), qnames = qnames)
}
.s2_reporting_metric <- function(tables, namespaces, metric_code, fact) {
  if (!.s2_string(metric_code) || !grepl("^[A-Za-z_][A-Za-z_0-9.-]*$", metric_code)) stop("Exact DPM metric MemberCode required")
  members <- .s2_reporting_index(tables$mMember, "MemberID")
  candidates <- Filter(function(metric) {
    id <- as.character(metric$CorrespondingMemberID)
    length(id) == 1L && id %in% names(members) && identical(members[[id]]$MemberCode, metric_code)
  }, tables$mMetric)
  if (length(candidates) != 1L) stop("Unknown or ambiguous DPM metric code")
  metric <- candidates[[1L]]
  if (!is.null(metric$CustomDataTypeID) || isTRUE(metric$IsAbstract == 1)) stop("Custom or abstract metric not supported")
  atom <- .s2_reporting_fact(fact)
  if (is.null(atom)) stop("Absence/nil admissibility is not a datatype decision")
  dtype <- metric$DataType; allowed <- list(); member_matches <- NULL
  if (identical(dtype, "Enumeration/Code")) {
    enumeration <- .s2_reporting_enumeration(tables, metric, members, namespaces)
    allowed <- enumeration$ids
    type_matches <- inherits(atom, "ReportingQName")
    member_matches <- type_matches && any(vapply(enumeration$qnames, identical, logical(1), atom))
    value <- member_matches
  } else {
    if (any(vapply(c("ReferencedDomainID", "ReferencedHierarchyID", "HierarchyStartingMemberID"), function(key) !is.null(metric[[key]]), logical(1))))
      stop("Additional non-enumeration domain constraints not supported")
    if (dtype %in% c("Monetary", "Decimal", "Percent", "Integer")) {
      type_matches <- inherits(atom, "ReportingNumber")
      value <- type_matches && (dtype != "Integer" || gmp::denominator(atom$value) == 1)
    } else if (dtype == "Boolean") value <- type_matches <- is.logical(atom) else
      if (dtype == "String") value <- type_matches <- is.character(atom) else
      if (dtype == "Date") value <- type_matches <- inherits(atom, "ReportingDate") else stop(paste("Unsupported metric datatype:", dtype))
  }
  list(value = value, metric_code = metric_code, metric_id = metric$MetricID,
    metric_xbrl_code = members[[as.character(metric$CorrespondingMemberID)]]$MemberXBRLCode,
    data_type = dtype, atomic_type_matches = type_matches, enumeration_member_matches = member_matches,
    allowed_member_ids = allowed, metric_definition = metric, status = "DPM_METRIC_VALUE_COMPONENT",
    xml_schema_validated = FALSE, presence_or_nil_admissibility_verified = FALSE,
    context_or_unit_verified = FALSE, business_rules_executed = FALSE,
    legal_applicability_verified = FALSE, full_report_validated = FALSE)
}

#' Check an explicit fact against an archived DPM metric
#' @param metric_code Exact DPM metric MemberCode, preserving case.
#' @param fact Tagged atom: `list(value = ..., decimals = ...)`, `list(text = ...)`,
#'   `list(boolean = ...)`, `list(date = "YYYY-MM-DD")`, or an expanded `qname`.
#' @param technical_version Explicit archived technical release.
#' @return Datatype and, where prescribed, enumeration-membership result with
#'   metric definition and provenance. This is not XML or full-report validation.
#' @examples
#' cases <- reference_dataset("reference_reporting_metric_cases.json")$cases
#' names(cases[[1]])
#' @export
reporting_metric_value_check <- function(metric_code, fact, technical_version) {
  data <- reporting_data("dictionary", technical_version)
  namespaces <- reporting_catalog(technical_version)$xbrl_namespace_bindings
  result <- tryCatch(.s2_reporting_metric(data$tables, namespaces, metric_code, fact),
    error = function(error) {
      if (inherits(error, "s2_error")) stop(error)
      .s2_fail("UNSUPPORTED_METHOD", "metric_value", conditionMessage(error))
    })
  c(result, list(technical_version = technical_version, source = data$source,
    input_hash = .s2_fingerprint(list(metric_code = metric_code, fact = fact, technical_version = technical_version))))
}
