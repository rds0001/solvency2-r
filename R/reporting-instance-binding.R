.s2_reporting_bind_cell <- function(cell, atom, open_dimensions, tables, namespaces, declarations, unit = NULL) {
  if (!isTRUE(cell$IsShaded == 0) || !.s2_truth(cell$DPS)) stop("Unshaded cell with explicit DPS required")
  if (!.s2_mapping(open_dimensions)) stop("Explicit open dimension mapping required")
  declared <- function(lexical, reference = FALSE) {
    q <- .s2_reporting_expand(lexical, namespaces); key <- paste0("{", q$namespace, "}", q$local_name)
    if (!key %in% names(declarations)) stop(paste("QName not declared in archived dictionary:", lexical))
    declaration <- declarations[[key]]
    if (!identical(declaration$namespace, q$namespace) || !identical(declaration$local_name, q$local_name)) stop("Dictionary declaration identity mismatch")
    abstract <- declaration$attributes$abstract
    if (!reference && !is.null(abstract) && !abstract %in% c("false", "0")) stop("Abstract dictionary element cannot be emitted")
    list(qname = unclass(q), declaration = declaration)
  }
  parts <- lapply(strsplit(cell$DPS, "|", fixed = TRUE)[[1L]], function(part) {
    match <- regmatches(part, regexec("^([^()]+)\\(([^()]*)\\)$", part, perl = TRUE))[[1L]]
    if (length(match) != 3L) stop("Unsupported archived datapoint signature")
    match[2:3]
  })
  if (!length(parts) || parts[[1L]][[1L]] != "MET" || any(vapply(parts[-1L], function(part) part[[1L]] == "MET", logical(1)))) stop("Exactly one leading DPS metric required")
  selected <- declared(parts[[1L]][[2L]]); metric <- selected$qname; declaration <- selected$declaration
  if (!identical(declaration$attributes[["{http://www.xbrl.org/2003/instance}periodType"]], "instant")) stop("Only declared instant metrics supported")
  checked <- .s2_reporting_metric(tables, namespaces, metric$local_name, atom)
  if (!checked$value) stop("Fact violates archived DPM metric datatype/domain")
  # Source lookups retain the last archived row. Null lexical codes cannot be
  # supplied as QName strings and are not valid lookup candidates.
  lookup <- function(rows, key) {
    result <- .s2_object()
    for (row in rows) if (!is.null(row[[key]])) result[[as.character(row[[key]])]] <- row
    result
  }
  dimensions <- lookup(tables$mDimension, "DimensionXBRLCode")
  domains <- lookup(tables$mDomain, "DomainID")
  members <- lookup(tables$mMember, "MemberXBRLCode")
  result <- omitted <- list(); consumed <- seen <- character()
  for (part in parts[-1L]) {
    name <- part[[1L]]; member <- part[[2L]]
    if (name %in% seen || !name %in% names(dimensions)) stop("Duplicate or unknown DPS dimension")
    seen <- c(seen, name); dimension <- dimensions[[name]]
    domain <- domains[[as.character(dimension$DomainID)]]
    if (is.null(domain)) stop("Missing dimension domain")
    dimension_qname <- declared(name, TRUE)$qname
    restriction <- regmatches(member, regexec("^\\*(\\?)?\\[([^;\\[\\]]+);([^;\\[\\]]+);([01])\\]$", member, perl = TRUE))[[1L]]
    if (startsWith(member, "*") && member != "*" && !length(restriction)) stop("Unsupported open dimension restriction")
    if (member == "*" || length(restriction)) {
      if (!name %in% names(open_dimensions)) stop(paste("Missing explicit open dimension:", name))
      member <- open_dimensions[[name]]; consumed <- c(consumed, name)
    } else if (name %in% names(open_dimensions)) stop("Fixed DPS dimension cannot be overridden")
    if (isTRUE(dimension$IsTypedDimension == 1)) {
      if (length(restriction)) stop("Explicit hierarchy restriction on typed dimension")
      if (!isTRUE(domain$IsTypedDomain == 1) || !identical(domain$DataType, "String")) stop("Unsupported typed dimension domain")
      if (is.null(member)) {
        if (!isTRUE(domain$IsNillable == 1)) stop("Typed nil forbidden by DPM domain")
      } else if (!.s2_string(member)) stop("Typed string dimension requires text")
      domain_qname <- declared(domain$DomainXBRLCode)$qname
      result[[length(result) + 1L]] <- list(dimension = dimension_qname, domain = domain_qname, text = member)
    } else if (isTRUE(dimension$IsTypedDimension == 0)) {
      if (!.s2_string(member) || !member %in% names(members)) stop("Unknown explicit dimension member")
      if (!identical(members[[member]]$DomainID, domain$DomainID)) stop("Explicit member outside dimension domain")
      member_qname <- declared(member, TRUE)$qname
      if (length(restriction)) {
        hierarchies <- Filter(function(h) identical(h$DomainID, domain$DomainID) && identical(h$HierarchyCode, restriction[[3L]]), tables$mHierarchy)
        starts <- Filter(function(m) identical(m$DomainID, domain$DomainID) && identical(m$MemberCode, restriction[[4L]]), tables$mMember)
        if (length(hierarchies) != 1L || length(starts) != 1L) stop("Ambiguous open-axis hierarchy or starting member")
        selection <- list(ReferencedDomainID = domain$DomainID, ReferencedHierarchyID = hierarchies[[1L]]$HierarchyID,
          HierarchyStartingMemberID = starts[[1L]]$MemberID, IsStartingMemberIncluded = as.integer(restriction[[5L]]))
        allowed <- .s2_reporting_enumeration(tables, selection, .s2_reporting_index(tables$mMember, "MemberID"), namespaces)$ids
        if (!members[[member]]$MemberID %in% unlist(allowed)) stop("Member outside allowed open-axis hierarchy")
      }
      default_id <- dimension$DefaultMemberID
      is_default <- if (!is.null(default_id)) identical(members[[member]]$MemberID, default_id) else isTRUE(members[[member]]$IsDefaultMember == 1)
      if (is_default) {
        if (!length(restriction) || restriction[[2L]] != "?") stop("Default omission not qualified by archived DPS")
        omitted[[length(omitted) + 1L]] <- list(dimension = dimension_qname, member = member_qname, dps_restriction = restriction[[1L]])
        next
      }
      result[[length(result) + 1L]] <- list(dimension = dimension_qname, member = member_qname)
    } else stop("Explicit dimension kind required")
  }
  if (!setequal(consumed, names(open_dimensions))) stop("Unused or undeclared open dimension inputs")
  numeric <- checked$data_type %in% c("Monetary", "Decimal", "Percent", "Integer")
  if (numeric != !is.null(unit)) stop("Unit required exactly for numeric facts")
  fact <- list(metric = metric, atom = atom, dimensions = result)
  if (numeric) fact$unit <- unit
  list(fact = fact, metric_check = checked, cell_id = cell$CellID, business_code = cell$BusinessCode,
    schema_declaration = declaration, omitted_explicit_defaults = omitted, dps_hierarchy_restrictions_verified = TRUE,
    open_axis_hierarchies_verified = FALSE, row_key_constraints_verified = FALSE,
    dimensional_relationships_verified = FALSE, full_report_validated = FALSE)
}
