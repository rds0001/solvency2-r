# A bounded parser for the archived DPM expression language, never eval/parse
# of host R code. Selectors remain exact source text for explicit fact binding.
.s2_expression_error <- function(message) stop(structure(list(message = message, call = NULL),
  class = c("s2_expression_error", "error", "condition")))
.s2_expression_margins <- c(customMarginEqual = "equal", customMarginLessThan = "less",
  customMarginLessThanOrEqual = "less_equal", customMarginGreaterThan = "greater", customMarginGreaterThanOrEqual = "greater_equal")
.s2_expression_arity <- c(list(true = c(0, 0), false = c(0, 0), emptySequence = c(0, 0),
  not = c(1, 1), isNull = c(1, 1), isnull = c(1, 1), count = c(1, 1), sum = c(1, 1), isum = c(1, 1),
  abs = c(1, 1), iabs = c(1, 1), min = c(1, 100), max = c(1, 100), imin = c(1, 100), imax = c(1, 100),
  localName = c(1, 1), LocalName = c(1, 1), leiChecksum = c(1, 1), matches = c(2, 2),
  substring = c(2, 3), this = c(0, 0), dim = c(2, 2), exp = c(3, 3)),
  stats::setNames(rep(list(c(3, 3)), length(.s2_expression_margins)), names(.s2_expression_margins)))
.s2_expression_precedence <- c(or = 10, and = 20,
  stats::setNames(rep(30, 12), c("=", "!=", "<", "<=", ">", ">=", "i=", "i!=", "i<", "i<=", "i>", "i>=")),
  stats::setNames(rep(40, 4), c("+", "-", "i+", "i-")), stats::setNames(rep(50, 4), c("*", "/", "i*", "i/")))
.s2_expression_tokens <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1L]]; n <- length(chars)
  position <- 1L; result <- character()
  while (position <= n) {
    start <- position; char <- chars[[position]]
    if (char == "{") {
      depth <- 0L; quote <- NULL
      while (position <= n) {
        char <- chars[[position]]
        if (!is.null(quote)) {
          if (char == quote) {
            if (position < n && chars[[position + 1L]] == quote) { position <- position + 2L; next }
            quote <- NULL
          }
        } else if (char %in% c("'", '"')) quote <- char else if (char == "{") depth <- depth + 1L else if (char == "}") depth <- depth - 1L
        position <- position + 1L
        if (depth == 0L) break
      }
      if (depth != 0L || !is.null(quote)) .s2_expression_error("UNSUPPORTED_SYNTAX: unclosed selector")
    } else if (char %in% c("'", '"')) {
      quote <- char; closed <- FALSE; position <- position + 1L
      while (position <= n) {
        if (chars[[position]] == quote) {
          if (position < n && chars[[position + 1L]] == quote) { position <- position + 2L; next }
          position <- position + 1L; closed <- TRUE; break
        }
        position <- position + 1L
      }
      if (!closed) .s2_expression_error("UNSUPPORTED_SYNTAX: unclosed string")
    } else if (char == "[") {
      while (position <= n && chars[[position]] != "]") position <- position + 1L
      if (position > n) .s2_expression_error("UNSUPPORTED_SYNTAX: unclosed QName")
      position <- position + 1L
    } else {
      rest <- substring(text, position)
      match <- regmatches(rest, regexpr("^(?:\\s+|i(?:<=|>=|!=|=|<|>|\\+|-|\\*|/)|<=|>=|!=|[=<>+*/(),-]|(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)|[A-Za-z_][A-Za-z_0-9]*)", rest, perl = TRUE))
      if (!length(match) || !nzchar(match)) .s2_expression_error(paste("UNSUPPORTED_SYNTAX at character", position - 1L))
      position <- position + nchar(match)
      if (!nzchar(trimws(match))) next
    }
    result <- c(result, substr(text, start, position - 1L))
    if (length(result) > 20000L) .s2_expression_error("TECHNICAL_LIMIT: token count")
  }
  result
}
.s2_expression_parse <- function(text) {
  if (!.s2_string(text) || !nzchar(trimws(text)) || nchar(text) > 100000L)
    .s2_expression_error("TECHNICAL_LIMIT_OR_INVALID_INPUT: expression text")
  tokens <- .s2_expression_tokens(text); position <- 1L; depth <- 0L
  peek <- function() if (position <= length(tokens)) tokens[[position]] else ""
  take <- function(expected = NULL) {
    token <- peek()
    if (!nzchar(token) || (!is.null(expected) && token != expected)) .s2_expression_error("UNSUPPORTED_SYNTAX: unexpected token")
    position <<- position + 1L; token
  }
  node <- function(kind, value, children = list()) list(kind = kind, value = value, children = children)
  expression <- function(minimum = 0) {
    depth <<- depth + 1L
    on.exit(depth <<- depth - 1L)
    if (depth > 100L) .s2_expression_error("TECHNICAL_LIMIT: expression nesting")
    token <- take()
    if (token == "if") {
      condition <- expression(); take("then"); yes <- expression(); take("else")
      left <- node("if", "", list(condition, yes, expression()))
    } else if (token %in% c("+", "-", "i-", "i+")) left <- node("unary", token, list(expression(60))) else
      if (token == "(") { left <- expression(); take(")") } else
      if (startsWith(token, "{")) left <- node("selector", token) else
      if (substring(token, 1, 1) %in% c("'", '"')) {
        quote <- substring(token, 1, 1)
        left <- node("string", gsub(paste0(quote, quote), quote, substr(token, 2, nchar(token) - 1L), fixed = TRUE))
      } else if (grepl("^\\[(?:[A-Za-z_][A-Za-z_0-9.-]*:[A-Za-z_][A-Za-z_0-9.-]*|Default)\\]$", token, perl = TRUE))
        left <- node("qname", substr(token, 2, nchar(token) - 1L)) else
      if (grepl("^(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)$", token, perl = TRUE)) left <- node("number", token) else
      if (token %in% names(.s2_expression_arity)) {
        take("("); args <- list()
        if (peek() != ")") {
          args[[1L]] <- expression()
          while (peek() == ",") { take(","); args[[length(args) + 1L]] <- expression() }
        }
        take(")"); bounds <- .s2_expression_arity[[token]]
        if (length(args) < bounds[[1L]] || length(args) > bounds[[2L]]) .s2_expression_error(paste("UNSUPPORTED_ARITY:", token))
        left <- node("call", token, args)
      } else .s2_expression_error(paste("UNSUPPORTED_SYNTAX_OR_FUNCTION:", token))
    compared <- FALSE
    while (peek() %in% names(.s2_expression_precedence) && .s2_expression_precedence[[peek()]] >= minimum) {
      operator <- take(); precedence <- .s2_expression_precedence[[operator]]
      if (precedence == 30 && compared) .s2_expression_error("UNSUPPORTED_SYNTAX: chained comparison")
      if (precedence == 30) compared <- TRUE
      left <- node("binary", operator, list(left, expression(precedence + 1)))
    }
    left
  }
  result <- expression()
  if (nzchar(peek())) .s2_expression_error("UNSUPPORTED_SYNTAX: trailing token")
  walk <- function(node, level) {
    if (level > 100L) .s2_expression_error("TECHNICAL_LIMIT: evaluation nesting")
    if (node$kind == "call" && node$value == "matches" && node$children[[2L]]$kind == "string")
      tryCatch(.s2_pattern_compile(node$children[[2L]]$value), error = function(error) .s2_expression_error(conditionMessage(error)))
    for (child in node$children) walk(child, level + 1L)
  }
  walk(result, 1L)
  result
}
