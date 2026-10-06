.s2_xml_text <- function(value) {
  if (!.s2_string(value)) stop("XML 1.0 text required")
  codes <- utf8ToInt(value)
  if (any(!(codes %in% c(9, 10, 13) | codes >= 32 & codes <= 55295 |
    codes >= 57344 & codes <= 65533 | codes >= 65536 & codes <= 1114111))) stop("XML 1.0 text required")
  value
}
.s2_xml_escape <- function(value, attribute = FALSE) {
  value <- .s2_xml_text(value)
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  if (attribute) {
    value <- gsub('"', "&quot;", value, fixed = TRUE)
    value <- gsub("\n", "&#10;", value, fixed = TRUE)
    value <- gsub("\r", "&#13;", value, fixed = TRUE)
    value <- gsub("\t", "&#9;", value, fixed = TRUE)
  }
  value
}
.s2_xml_element <- function(name, attributes = .s2_object(), text = NULL, children = NULL) {
  attrs <- paste0(vapply(names(attributes), function(key) paste0(" ", key, '="', .s2_xml_escape(attributes[[key]], TRUE), '"'), character(1)), collapse = "")
  if (is.null(text) && !length(children)) return(paste0("<", name, attrs, " />"))
  paste0("<", name, attrs, ">", if (!is.null(text)) .s2_xml_escape(text) else paste0(children, collapse = ""), "</", name, ">")
}
.s2_xml_qname <- function(value) {
  if (!.s2_mapping(value) || !setequal(names(value), c("namespace", "local_name"))) stop("Expanded QName required")
  q <- do.call(.s2_reporting_qname, value)
  if (!nzchar(q$namespace)) stop("Nonempty QName namespace required")
  .s2_xml_text(q$namespace)
  list(q$namespace, q$local_name)
}
.s2_xml_decimal <- function(value) {
  denominator <- gmp::denominator(value); twos <- fives <- 0L
  while (denominator %% 2 == 0) { denominator <- denominator %/% 2; twos <- twos + 1L }
  while (denominator %% 5 == 0) { denominator <- denominator %/% 5; fives <- fives + 1L }
  if (denominator != 1) stop("Nonterminating decimal cannot be emitted without rounding")
  scale <- max(twos, fives)
  if (scale > 4000L) stop("TECHNICAL_LIMIT: XML decimal scale")
  numerator <- abs(gmp::numerator(value)) * gmp::as.bigz(2)^(scale - twos) * gmp::as.bigz(5)^(scale - fives)
  digits <- as.character(numerator)
  if (nchar(digits) < scale + 1L) digits <- paste0(strrep("0", scale + 1L - nchar(digits)), digits)
  lexical <- if (!scale) digits else paste0(substr(digits, 1, nchar(digits) - scale), ".", substring(digits, nchar(digits) - scale + 1L))
  paste0(if (value < 0) "-" else "", lexical)
}
.s2_write_instance <- function(schema_uri, entity, instant, indicators, facts, namespaces) {
  .s2_xml_text(schema_uri)
  if (!grepl("^https?://[^/?#]+[^#]*$", schema_uri)) stop("Absolute module schema URI required")
  if (!.s2_mapping(entity) || !setequal(names(entity), c("scheme", "identifier")) ||
    !all(vapply(entity, function(value) nzchar(trimws(.s2_xml_text(value))), logical(1)))) stop("Explicit nonempty entity scheme and identifier required")
  .s2_iso_date(instant, "instant")
  if (!.s2_mapping(indicators) || !length(indicators) || any(!nzchar(trimws(names(indicators)))) ||
    !all(vapply(indicators, function(value) is.logical(value) && length(value) == 1L && !is.na(value), logical(1))))
    stop("Explicit unique Boolean filing indicators required")
  if (!.s2_mapping(namespaces)) stop("Explicit namespace bindings required")
  bindings <- list(xbrli = "http://www.xbrl.org/2003/instance", link = "http://www.xbrl.org/2003/linkbase",
    xlink = "http://www.w3.org/1999/xlink", xbrldi = "http://xbrl.org/2006/xbrldi",
    xsi = "http://www.w3.org/2001/XMLSchema-instance", find = "http://www.eurofiling.info/xbrl/ext/filing-indicators")
  for (prefix in names(namespaces)) {
    uri <- namespaces[[prefix]]
    if (!grepl("^[A-Za-z_][A-Za-z_0-9.-]*$", prefix) || startsWith(tolower(prefix), "xml") ||
      !.s2_string(uri) || !nzchar(uri) || grepl("[[:space:]]", uri) || (prefix %in% names(bindings) && !identical(bindings[[prefix]], uri)))
      stop("Invalid or conflicting namespace binding")
    bindings[[prefix]] <- .s2_xml_text(uri)
  }
  if (anyDuplicated(unlist(bindings))) stop("One explicit canonical prefix per namespace required")
  reverse <- stats::setNames(as.list(names(bindings)), unlist(bindings)); used <- c("xbrli", "link", "xlink", "find")
  lexical <- function(qname) {
    if (!qname[[1L]] %in% names(reverse)) stop("Missing source-bound namespace prefix")
    prefix <- reverse[[qname[[1L]]]]; used <<- unique(c(used, prefix))
    paste0(prefix, ":", qname[[2L]])
  }
  if (!is.list(facts) || !is.null(names(facts)) || !length(facts)) stop("Nonempty bound fact list required")
  prepared <- list(); identities <- character(); contexts <- stats::setNames(list(list()), .s2_fingerprint(list())); units <- .s2_object()
  for (fact in facts) {
    if (!.s2_mapping(fact) || !all(c("metric", "atom") %in% names(fact)) || length(setdiff(names(fact), c("metric", "atom", "dimensions", "unit")))) stop("Explicit supported bound fact fields required")
    metric <- .s2_xml_qname(fact$metric); lexical(metric)
    dimensions <- list(); dimension_ids <- character()
    declared <- fact$dimensions
    if (is.null(declared) && !"dimensions" %in% names(fact)) declared <- list()
    if (!is.list(declared) || !is.null(names(declared))) stop("Dimension list required")
    for (dim in declared) {
      if (!.s2_mapping(dim) || !(setequal(names(dim), c("dimension", "member")) || setequal(names(dim), c("dimension", "domain", "text")))) stop("Explicit or typed dimension required")
      name <- .s2_xml_qname(dim$dimension); lexical(name); key <- .s2_fingerprint(name)
      if (key %in% dimension_ids) stop("Duplicate dimension")
      dimension_ids <- c(dimension_ids, key)
      if ("member" %in% names(dim)) {
        member <- .s2_xml_qname(dim$member); lexical(member)
        dimensions[[length(dimensions) + 1L]] <- list(name, "explicit", member, "")
      } else {
        domain <- .s2_xml_qname(dim$domain); lexical(domain); text <- dim$text
        if (!is.null(text)) .s2_xml_text(text) else used <- unique(c(used, "xsi"))
        dimensions[[length(dimensions) + 1L]] <- list(name, if (is.null(text)) "typed_nil" else "typed", domain, if (is.null(text)) "" else text)
      }
    }
    dimensions <- .s2_sorted(dimensions)
    if (length(dimensions)) used <- unique(c(used, "xbrldi"))
    context_key <- .s2_fingerprint(dimensions); contexts[context_key] <- list(dimensions)
    atom <- .s2_reporting_fact(fact$atom); unit <- decimals <- NULL
    if (inherits(atom, "ReportingNumber")) {
      unit <- .s2_xml_qname(fact$unit); lexical(unit); units[.s2_fingerprint(unit)] <- list(unit)
      decimals <- as.character(fact$atom$decimals); value <- .s2_xml_decimal(atom$value)
    } else {
      if ("unit" %in% names(fact)) stop("Nonnumeric facts must not have units")
      if (is.null(atom)) stop("Nil/absent business facts cannot be emitted")
      value <- if (inherits(atom, "ReportingQName")) lexical(list(atom$namespace, atom$local_name)) else
        if (inherits(atom, "ReportingDate")) atom$lexical else if (is.logical(atom)) if (atom) "true" else "false" else .s2_xml_text(atom)
    }
    identity <- .s2_fingerprint(list(metric, dimensions, unit))
    if (identity %in% identities) stop("Duplicate business fact aspects")
    identities <- c(identities, identity)
    prepared[[length(prepared) + 1L]] <- list(metric = metric, dimensions = dimensions, unit = unit, decimals = decimals, value = value)
  }
  contexts <- .s2_sorted(unname(contexts)); units <- .s2_sorted(unname(units))
  context_ids <- stats::setNames(as.list(paste0("c", seq_along(contexts) - 1L)), vapply(contexts, .s2_fingerprint, character(1)))
  unit_ids <- stats::setNames(as.list(paste0("u", seq_along(units) - 1L)), vapply(units, .s2_fingerprint, character(1)))
  el <- .s2_xml_element
  children <- list(el("link:schemaRef", list("xlink:type" = "simple", "xlink:href" = schema_uri)))
  for (dimensions in contexts) {
    content <- c(el("xbrli:entity", children = el("xbrli:identifier", list(scheme = entity$scheme), text = entity$identifier)),
      el("xbrli:period", children = el("xbrli:instant", text = instant)))
    if (length(dimensions)) {
      scenario <- lapply(dimensions, function(dim) {
        if (dim[[2L]] == "explicit") el("xbrldi:explicitMember", list(dimension = lexical(dim[[1L]])), text = lexical(dim[[3L]])) else
          el("xbrldi:typedMember", list(dimension = lexical(dim[[1L]])), children =
            if (dim[[2L]] == "typed_nil") el(lexical(dim[[3L]]), list("xsi:nil" = "true")) else el(lexical(dim[[3L]]), text = dim[[4L]]))
      })
      content <- c(content, el("xbrli:scenario", children = scenario))
    }
    children[[length(children) + 1L]] <- el("xbrli:context", list(id = context_ids[[.s2_fingerprint(dimensions)]]), children = content)
  }
  for (unit in units) children[[length(children) + 1L]] <- el("xbrli:unit", list(id = unit_ids[[.s2_fingerprint(unit)]]), children = el("xbrli:measure", text = lexical(unit)))
  filing <- lapply(sort(names(indicators)), function(template) el("find:filingIndicator",
    list(contextRef = context_ids[[.s2_fingerprint(list())]], "find:filed" = if (indicators[[template]]) "true" else "false"), text = template))
  children[[length(children) + 1L]] <- el("find:fIndicators", children = filing)
  # Stable native ordering; XML semantics, not serializer-specific bytes, form
  # the language-neutral contract. xml_sha256 binds these actual UTF-8 bytes.
  order <- order(vapply(prepared, function(row) paste(c(unlist(row$metric), .s2_fingerprint(row$dimensions), unlist(row$unit)), collapse = "\n"), character(1)), method = "radix")
  for (row in prepared[order]) {
    attributes <- list(contextRef = context_ids[[.s2_fingerprint(row$dimensions)]])
    if (!is.null(row$unit)) attributes <- c(attributes, list(unitRef = unit_ids[[.s2_fingerprint(row$unit)]], decimals = row$decimals))
    children[[length(children) + 1L]] <- el(lexical(row$metric), attributes, text = row$value)
  }
  used <- sort(used); attributes <- stats::setNames(bindings[used], paste0("xmlns:", used))
  output <- paste0("<?xml version='1.0' encoding='utf-8'?>\n", el("xbrli:xbrl", attributes, children = children))
  xml2::read_xml(charToRaw(enc2utf8(output)), options = "NONET")
  output
}
