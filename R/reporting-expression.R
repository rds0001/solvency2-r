.s2_expression_boolean <- function(value) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) .s2_expression_error("TYPE_ERROR: boolean required")
  value
}
.s2_expression_number <- function(value, interval = TRUE) {
  if (!inherits(value, "ReportingNumber")) .s2_expression_error("TYPE_ERROR: explicit numeric binding required")
  if (interval) value else .s2_reporting_number(value$value)
}
.s2_expression_sequence <- function(value) is.list(value) && !inherits(value, c("ReportingNumber", "ReportingQName", "ReportingDate"))
.s2_qname_key <- function(qname) paste(qname$namespace, qname$local_name, sep = "\n")
.s2_reporting_atomic_compare <- function(operation, left, right) {
  if (inherits(left, "ReportingDate") && inherits(right, "ReportingDate")) {
    a <- left$lexical; b <- right$lexical
    return(switch(operation, `=` = a == b, `!=` = a != b, `<` = a < b, `<=` = a <= b, `>` = a > b, `>=` = a >= b,
      stop("Unsupported calendar date operation")))
  }
  type <- function(value) if (inherits(value, "ReportingQName")) "qname" else if (.s2_string(value)) "str" else
    if (is.logical(value) && length(value) == 1L && !is.na(value)) "bool" else "unsupported"
  if (!operation %in% c("=", "!=") || type(left) != type(right) || type(left) == "unsupported") stop("Unsupported typed atomic comparison")
  equal <- identical(left, right)
  if (operation == "=") equal else !equal
}
.s2_reporting_lei <- function(value) {
  if (!.s2_string(value)) stop("TYPE_ERROR: LEI functions require explicit text")
  if (!grepl("^[A-Z0-9]{18}[0-9]{2}$", value)) return(TRUE)
  remainder <- 0
  for (code in utf8ToInt(value)) {
    number <- code - if (code <= 57) 48 else 55
    remainder <- (remainder * (if (number < 10) 10 else 100) + number) %% 97
  }
  remainder == 1
}
.s2_reporting_power <- function(base, numerator, denominator) {
  if (numerator != 1 || denominator != 2) stop("UNSUPPORTED_ARGUMENT: only archived exponent (1,2) supported")
  value <- base$value
  if (value < 0 || value >= gmp::as.bigz(10)^24) stop("UNSUPPORTED_RANGE: source root requires 0 <= base < 10^24")
  scaled <- value * 10^8
  integer <- gmp::numerator(scaled) %/% gmp::denominator(scaled)
  # GMP integer square root is exact, unlike a double approximation near a step.
  root <- gmp::as.bigz(0)
  if (integer > 0) {
    root <- integer; next_root <- (root + 1) %/% 2
    while (next_root < root) { root <- next_root; next_root <- (root + integer %/% root) %/% 2 }
  }
  .s2_reporting_number(gmp::as.bigq(root, gmp::as.bigz(10000)))
}
.s2_expression_evaluate <- function(text, resolver, namespaces = NULL, current_fact = NULL, dimensions = NULL) {
  tree <- .s2_expression_parse(text)
  tryCatch({
    if (!is.function(resolver)) .s2_expression_error("INVALID_INPUT: explicit resolver required")
    qnames <- .s2_object()
    literals <- function(node) {
      if (node$kind == "qname") qnames[[node$value]] <<- .s2_reporting_expand(node$value, namespaces)
      for (child in node$children) literals(child)
    }
    literals(tree)
    if (!is.null(dimensions) && !.s2_mapping(dimensions)) .s2_expression_error("TYPE_ERROR: explicit dimension bindings required")
    if (!is.null(current_fact) && (!is.list(current_fact) || length(current_fact) != 2L)) .s2_expression_error("TYPE_ERROR: explicit current fact context required")
    run <- function(node) {
      kind <- node$kind; name <- node$value
      if (kind == "number") return(.s2_reporting_number(name))
      if (kind == "string") return(name)
      if (kind == "qname") return(qnames[[name]])
      if (kind == "selector") return(resolver(name))
      if (kind == "if") return(run(node$children[[if (.s2_expression_boolean(run(node$children[[1L]]))) 2L else 3L]]))
      if (kind == "unary") {
        operand <- .s2_expression_number(run(node$children[[1L]]), startsWith(name, "i"))
        return(if (endsWith(name, "-")) .s2_reporting_number(-operand$value, operand$tolerance) else operand)
      }
      if (kind == "binary") {
        left <- run(node$children[[1L]])
        if (name == "and") return(.s2_expression_boolean(left) && .s2_expression_boolean(run(node$children[[2L]])))
        if (name == "or") return(.s2_expression_boolean(left) || .s2_expression_boolean(run(node$children[[2L]])))
        right <- run(node$children[[2L]]); interval <- startsWith(name, "i")
        operator <- if (interval) substring(name, 2L) else name
        if (!interval && operator %in% c("=", "!=", "<", "<=", ">", ">=") &&
          !(inherits(left, "ReportingNumber") && inherits(right, "ReportingNumber")))
          return(.s2_reporting_atomic_compare(operator, left, right))
        operation <- c("+" = "add", "-" = "subtract", "*" = "multiply", "/" = "divide", "=" = "equal", "!=" = "equal",
          "<" = "less", "<=" = "less_equal", ">" = "greater", ">=" = "greater_equal")[[operator]]
        result <- .s2_reporting_binary(operation, .s2_expression_number(left, interval), .s2_expression_number(right, interval))
        return(if (operator == "!=") !result else result)
      }
      if (name == "this") {
        if (is.null(current_fact)) .s2_expression_error("MISSING_FACT_CONTEXT: this() requires a filter candidate")
        return(current_fact[[1L]])
      }
      args <- lapply(node$children, run)
      if (name == "dim") {
        target <- node$children[[1L]]
        if (is.null(args[[1L]]) || .s2_expression_sequence(args[[1L]]) || !inherits(args[[2L]], "ReportingQName"))
          .s2_expression_error("UNSUPPORTED_ARGUMENT: scalar fact and expanded dimension name required")
        if (target$kind == "selector") declared <- dimensions[[target$value]] else
          if (target$kind == "call" && target$value == "this") declared <- current_fact[[2L]] else
            .s2_expression_error("UNSUPPORTED_ARGUMENT: dim requires a fact selector or this()")
        key <- .s2_qname_key(args[[2L]])
        if (!.s2_mapping(declared) || !key %in% names(declared)) .s2_expression_error("MISSING_DIMENSION_BINDING: no implicit dimension defaults")
        return(declared[[key]])
      }
      if (name == "true") return(TRUE)
      if (name == "false") return(FALSE)
      if (name == "emptySequence") return(list())
      if (name == "not") return(!.s2_expression_boolean(args[[1L]]))
      if (name %in% c("isNull", "isnull")) return(is.null(args[[1L]]) || (.s2_expression_sequence(args[[1L]]) && !length(args[[1L]])))
      if (name == "count") {
        values <- if (is.null(args[[1L]])) list() else if (.s2_expression_sequence(args[[1L]])) args[[1L]] else args[1L]
        if (!all(vapply(values, function(value) inherits(value, c("ReportingNumber", "ReportingQName", "ReportingDate")) || .s2_string(value) || is.logical(value), logical(1))))
          .s2_expression_error("TYPE_ERROR: explicit atomic sequence required")
        return(.s2_reporting_number(length(values)))
      }
      if (name == "leiChecksum") return(.s2_reporting_lei(args[[1L]]))
      if (name == "matches") return(.s2_reporting_matches(args[[1L]], args[[2L]]))
      if (name %in% c("substring", "exp")) {
        positions <- lapply(args[-1L], function(arg) .s2_expression_number(arg, FALSE)$value)
        if (any(vapply(positions, function(value) gmp::denominator(value) != 1, logical(1))))
          .s2_expression_error("UNSUPPORTED_ARGUMENT: integer positions or exponents required")
        if (name == "exp") return(.s2_reporting_power(.s2_expression_number(args[[1L]], FALSE), positions[[1L]], positions[[2L]]))
        return(do.call(.s2_reporting_substring, c(args[1L], lapply(positions, as.numeric))))
      }
      if (name %in% c("localName", "LocalName")) {
        if (!inherits(args[[1L]], "ReportingQName")) .s2_expression_error("TYPE_ERROR: explicit QName required")
        return(args[[1L]]$local_name)
      }
      if (name %in% names(.s2_expression_margins)) {
        values <- lapply(args, function(arg) .s2_expression_number(arg)$value)
        return(.s2_reporting_binary(.s2_expression_margins[[name]], .s2_reporting_number(values[[1L]], values[[3L]]), .s2_reporting_number(values[[2L]])))
      }
      interval <- startsWith(name, "i"); operation <- if (interval) substring(name, 2L) else name
      if (operation == "abs") {
        operand <- .s2_expression_number(args[[1L]], interval)
        return(.s2_reporting_number(abs(operand$value), operand$tolerance))
      }
      values <- list()
      for (arg in args) for (item in if (.s2_expression_sequence(arg)) arg else list(arg))
        values[[length(values) + 1L]] <- .s2_expression_number(item, interval)
      .s2_reporting_aggregate(operation, values)
    }
    run(tree)
  }, error = function(error) {
    if (inherits(error, "s2_expression_error")) stop(error)
    .s2_expression_error(paste("NUMERIC_OR_TYPE_ERROR:", conditionMessage(error)))
  })
}
