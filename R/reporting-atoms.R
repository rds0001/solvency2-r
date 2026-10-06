# Exact rational reporting atoms. These are deliberately internal: users supply
# transparent tagged JSON-compatible facts, never native arithmetic objects.
.s2_reporting_rational <- function(value) {
  if (inherits(value, "bigq")) {
    if (max(nchar(as.character(gmp::numerator(value))), nchar(as.character(gmp::denominator(value)))) > 30104L)
      stop("TECHNICAL_LIMIT: reporting rational size")
    return(value)
  }
  if (is.logical(value) || length(value) != 1L || !(is.character(value) || is.numeric(value)) || is.na(value)) stop("Finite numeric value required")
  if (is.numeric(value) && !is.finite(value)) stop("Finite numeric value required")
  text <- .s2_stringify(value)
  if (nchar(text) > 10000L) stop("TECHNICAL_LIMIT: reporting numeric text length")
  text <- trimws(text)
  if (!grepl("^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$", text, perl = TRUE)) stop("Finite numeric value required")
  parts <- strsplit(tolower(text), "e", fixed = TRUE)[[1L]]
  mantissa <- sub("^[+-]", "", parts[[1L]])
  decimal_places <- if (grepl(".", mantissa, fixed = TRUE)) nchar(sub("^[^.]*\\.", "", mantissa)) else 0L
  exponent <- (if (length(parts) == 2L) as.numeric(parts[[2L]]) else 0) - decimal_places
  digits <- sub("^0+", "", gsub(".", "", mantissa, fixed = TRUE))
  if (nchar(digits) > 4000L || !is.finite(exponent) || abs(exponent) > 4000L) stop("TECHNICAL_LIMIT: reporting decimal digits/exponent")
  .s2_rational(text)
}
.s2_reporting_number <- function(value, tolerance = 0) {
  value <- .s2_reporting_rational(value); tolerance <- .s2_reporting_rational(tolerance)
  if (tolerance < 0) stop("Tolerance must be nonnegative")
  structure(list(value = value, tolerance = tolerance), class = "ReportingNumber")
}
.s2_reporting_reported <- function(value, decimals) {
  if (identical(decimals, "INF")) return(.s2_reporting_number(value))
  if (!is.numeric(decimals) || length(decimals) != 1L || !is.finite(decimals) || decimals != trunc(decimals)) stop("Explicit integer decimals or INF required")
  if (abs(decimals) > 1000) stop("Decimals outside technical support range [-1000,1000]")
  .s2_reporting_number(value, gmp::as.bigq(10)^(-decimals) / 2)
}
.s2_reporting_qname <- function(namespace, local_name) {
  if (!.s2_string(namespace) || nchar(namespace) > 10000L || grepl("[[:space:]]", namespace)) stop("Explicit namespace URI required")
  if (!.s2_string(local_name) || nchar(local_name) > 1000L || !grepl("^[A-Za-z_][A-Za-z_0-9.-]*$", local_name)) stop("Supported explicit QName local name required")
  structure(list(namespace = namespace, local_name = local_name), class = "ReportingQName")
}
.s2_reporting_expand <- function(lexical, namespaces) {
  if (identical(lexical, "Default")) return(.s2_reporting_qname("", "Default"))
  if (!.s2_string(lexical) || !.s2_mapping(namespaces) || !grepl("^[^:]+:[^:]+$", lexical)) stop("Explicit prefixed QName and namespace bindings required")
  parts <- strsplit(lexical, ":", fixed = TRUE)[[1L]]
  if (!grepl("^[A-Za-z_][A-Za-z_0-9.-]*$", parts[[1L]]) || !parts[[1L]] %in% names(namespaces) || !nzchar(namespaces[[parts[[1L]]]])) stop("Unknown QName prefix")
  .s2_reporting_qname(namespaces[[parts[[1L]]]], parts[[2L]])
}
.s2_reporting_fact <- function(record) {
  tagged <- function(keys) .s2_mapping(record) && setequal(names(record), keys)
  if (tagged("date")) {
    .s2_iso_date(record$date, "bindings")
    return(structure(list(lexical = record$date), class = "ReportingDate"))
  }
  if (tagged("text")) {
    .s2_require(.s2_string(record$text) && nchar(record$text) <= 100000L, "bindings", "explicit supported text value required")
    return(record$text)
  }
  if (tagged("boolean")) {
    .s2_require(is.logical(record$boolean) && length(record$boolean) == 1L && !is.na(record$boolean), "bindings", "explicit Boolean value required")
    return(record$boolean)
  }
  if (tagged("absent")) {
    .s2_require(identical(record$absent, TRUE), "bindings", "absence must be explicitly true")
    return(NULL)
  }
  if (tagged("qname")) {
    .s2_require(.s2_mapping(record$qname) && setequal(names(record$qname), c("namespace", "local_name")), "bindings", "expanded QName requires namespace and local_name")
    return(tryCatch(do.call(.s2_reporting_qname, record$qname), error = function(error) .s2_fail("INVALID_INPUT", "bindings", conditionMessage(error))))
  }
  .s2_require(tagged(c("value", "decimals")), "bindings", "each numeric fact requires exactly value and decimals")
  tryCatch(.s2_reporting_reported(record$value, record$decimals), error = function(error) .s2_fail("INVALID_INPUT", "bindings", conditionMessage(error)))
}

.s2_reporting_binary <- function(operation, left, right) {
  if (!inherits(left, "ReportingNumber") || !inherits(right, "ReportingNumber")) stop("Explicit ReportingNumber operands required")
  a <- left$value; b <- right$value; ta <- left$tolerance; tb <- right$tolerance
  switch(operation,
    add = .s2_reporting_number(a + b, ta + tb),
    subtract = .s2_reporting_number(a - b, ta + tb),
    multiply = .s2_reporting_number(a * b, abs(a) * tb + abs(b) * ta + ta * tb),
    divide = {
      if (b - tb <= 0 && b + tb >= 0) stop("REVIEW_REQUIRED: denominator interval contains zero")
      central <- a / b
      endpoints <- list((a - ta) / (b - tb), (a - ta) / (b + tb), (a + ta) / (b - tb), (a + ta) / (b + tb))
      tolerance <- Reduce(max, lapply(endpoints, function(point) abs(central - point)))
      .s2_reporting_number(central, tolerance)
    }, equal = abs(a - b) <= ta + tb, less = a - b < ta + tb,
    less_equal = a - b <= ta + tb, greater = b - a < ta + tb,
    greater_equal = b - a <= ta + tb, stop("Unsupported arithmetic operation"))
}
.s2_reporting_aggregate <- function(operation, operands) {
  if (!is.list(operands) || !all(vapply(operands, inherits, logical(1), "ReportingNumber"))) stop("Explicit sequence of ReportingNumber operands required")
  if (!operation %in% c("sum", "min", "max", "multiply")) stop("Unsupported aggregate operation")
  if (!length(operands)) return(.s2_reporting_number(0))
  if (operation == "sum") return(Reduce(function(a, b) .s2_reporting_binary("add", a, b), operands, init = .s2_reporting_number(0)))
  if (operation == "multiply") return(Reduce(function(a, b) .s2_reporting_binary("multiply", a, b), operands))
  Reduce(function(a, b) if (if (operation == "min") b$value < a$value else b$value > a$value) b else a, operands)
}
