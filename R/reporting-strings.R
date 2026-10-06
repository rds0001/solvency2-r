# Source-compatible bounded regex NFA. User expressions never reach a host
# backtracking regex engine; only fixed Unicode-category classification uses PCRE.
.s2_reporting_text <- function(value) {
  if (is.null(value) || (is.list(value) && !length(value))) return("")
  if (!.s2_string(value)) stop("TYPE_ERROR: explicit single text or empty sequence required")
  if (nchar(value) > 100000L) stop("TECHNICAL_LIMIT: text length")
  codes <- utf8ToInt(value)
  if (any(!(codes %in% c(9, 10, 13) | codes >= 32 & codes <= 55295 |
    codes >= 57344 & codes <= 65533 | codes >= 65536 & codes <= 1114111))) stop("UNSUPPORTED_INPUT: non-XML character")
  value
}
.s2_reporting_substring <- function(value, start, length = NULL) {
  value <- .s2_reporting_text(value)
  if (abs(start) > 2^53 || (!is.null(length) && abs(length) > 2^53)) stop("TECHNICAL_LIMIT: exactly representable double integer required")
  begin <- max(0, min(nchar(value), start - 1))
  end <- if (is.null(length)) nchar(value) else max(0, min(nchar(value), start + length - 1))
  if (end <= begin) "" else substr(value, begin + 1, end)
}
.s2_pattern_compile <- function(pattern) {
  if (!.s2_string(pattern) || nchar(pattern) > 10000L) stop("TECHNICAL_LIMIT_OR_TYPE_ERROR: regex pattern")
  .s2_reporting_text(pattern)
  if (any(startsWith(pattern, c("one of options as per ", "ISO 8601 ", "yyyy-mm-dd pattern"))))
    stop("UNSUPPORTED_PATTERN: descriptive standard reference is not a regex")
  chars <- strsplit(pattern, "", fixed = TRUE)[[1L]]; position <- 1L; depth <- 0L
  peek <- function(offset = 0L) if (position + offset <= length(chars)) chars[[position + offset]] else ""
  take <- function() {
    value <- peek()
    if (!nzchar(value)) stop("INVALID_PATTERN: unexpected end")
    position <<- position + 1L; value
  }
  escaped <- function() {
    char <- take()
    if (char %in% c("n", "r", "t")) return(list("literal", switch(char, n = "\n", r = "\r", t = "\t")))
    if (char %in% strsplit("\\|.?*+(){}-[]^$", "", fixed = TRUE)[[1L]]) return(list("literal", char))
    if (char %in% c("s", "S", "d", "D")) return(list("escape", char))
    stop("UNSUPPORTED_PATTERN: escape or backreference")
  }
  class_item <- function() {
    char <- take()
    if (char == "\\") return(escaped())
    if (char %in% c("[", "]")) stop("UNSUPPORTED_PATTERN: nested or unclosed character class")
    list("literal", char)
  }
  character_class <- function() {
    negative <- peek() == "^"
    if (negative) take()
    items <- list()
    while (peek() != "]") {
      is_escaped <- peek() == "\\"; item <- class_item()
      if (!is_escaped && identical(item, list("literal", "-")) && length(items) && peek() != "]") stop("INVALID_PATTERN: interior hyphen")
      if (peek() == "-" && peek(1L) != "]") {
        take(); end <- class_item()
        if (item[[1L]] != "literal" || end[[1L]] != "literal" || utf8ToInt(item[[2L]]) > utf8ToInt(end[[2L]])) stop("INVALID_PATTERN: character range")
        item <- list("range", item[[2L]], end[[2L]])
      }
      items[[length(items) + 1L]] <- item
    }
    take()
    if (!length(items)) stop("INVALID_PATTERN: empty character class")
    list("character", list("class", negative, items))
  }
  atom <- function() {
    char <- take()
    if (char == "(") {
      result <- expression()
      if (take() != ")") stop("INVALID_PATTERN: unclosed group")
      return(result)
    }
    if (char == "[") return(character_class())
    if (char == "\\") return(list("character", escaped()))
    if (char %in% c("^", "$")) return(list("anchor", char))
    if (char == ".") return(list("character", list("dot")))
    if (char %in% c("?", "*", "+", "{", "}", "]", ")")) stop("INVALID_PATTERN: unexpected metacharacter")
    list("character", list("literal", char))
  }
  quantity <- function() {
    digits <- ""
    while (peek() %in% as.character(0:9)) digits <- paste0(digits, take())
    if (!nzchar(digits)) stop("INVALID_PATTERN: missing repetition quantity")
    if (nchar(digits) > 4L || as.numeric(digits) > 1000) stop("TECHNICAL_LIMIT: regex repetition")
    as.integer(digits)
  }
  expression <- function() {
    depth <<- depth + 1L
    on.exit(depth <<- depth - 1L)
    if (depth > 50L) stop("TECHNICAL_LIMIT: regex nesting")
    branches <- list()
    repeat {
      pieces <- list()
      while (nzchar(peek()) && !peek() %in% c("|", ")")) {
        item <- atom(); token <- peek()
        if (token %in% c("?", "*", "+", "{")) {
          take()
          if (token == "{") {
            lower <- upper <- quantity()
            if (peek() == ",") { take(); upper <- if (peek() == "}") NULL else quantity() }
            if (take() != "}" || (!is.null(upper) && upper < lower)) stop("INVALID_PATTERN: repetition range")
          } else {
            lower <- if (token == "+") 1L else 0L
            upper <- if (token == "?") 1L else NULL
          }
          if (peek() == "?") take()
          item <- list("repeat", item, lower, upper)
        }
        pieces[[length(pieces) + 1L]] <- item
      }
      branches[[length(branches) + 1L]] <- list("sequence", pieces)
      if (peek() != "|") break
      take()
    }
    list("choice", branches)
  }
  tree <- expression()
  if (nzchar(peek())) stop("INVALID_PATTERN: trailing input")
  states <- list()
  state <- function() {
    if (length(states) >= 4096L) stop("TECHNICAL_LIMIT: regex states")
    states[[length(states) + 1L]] <<- list()
    length(states)
  }
  edge <- function(start, end, kind = "empty", value = NULL) {
    states[[start]][[length(states[[start]]) + 1L]] <<- list(kind, value, end)
  }
  build <- function(node, start, end) {
    kind <- node[[1L]]
    if (kind %in% c("character", "anchor")) edge(start, end, kind, node[[2L]]) else
      if (kind == "choice") {
        for (child in node[[2L]]) {
          branch_start <- state(); branch_end <- state()
          edge(start, branch_start); build(child, branch_start, branch_end); edge(branch_end, end)
        }
      } else if (kind == "sequence") {
        current <- start
        for (child in node[[2L]]) { following <- state(); build(child, current, following); current <- following }
        edge(current, end)
      } else if (kind == "repeat") {
        child <- node[[2L]]; lower <- node[[3L]]; upper <- node[[4L]]; current <- start
        for (i in seq_len(lower)) { following <- state(); build(child, current, following); current <- following }
        if (is.null(upper)) {
          edge(current, end); following <- state(); build(child, current, following); edge(following, current)
        } else {
          for (i in seq_len(upper - lower)) { edge(current, end); following <- state(); build(child, current, following); current <- following }
          edge(current, end)
        }
      } else stop("UNSUPPORTED_PATTERN: node")
  }
  start <- state(); end <- state(); build(tree, start, end)
  list(states = states, start = start, end = end)
}
.s2_pattern_accepts <- function(predicate, char) {
  kind <- predicate[[1L]]
  if (kind == "literal") return(char == predicate[[2L]])
  if (kind == "range") return(utf8ToInt(char) >= utf8ToInt(predicate[[2L]]) && utf8ToInt(char) <= utf8ToInt(predicate[[3L]]))
  if (kind == "dot") return(char != "\n")
  if (kind == "escape") {
    escape <- predicate[[2L]]
    found <- if (tolower(escape) == "s") char %in% c(" ", "\t", "\n", "\r") else grepl("^\\p{Nd}$", char, perl = TRUE)
    return(if (escape %in% c("S", "D")) !found else found)
  }
  if (kind == "class") {
    found <- any(vapply(predicate[[3L]], .s2_pattern_accepts, logical(1), char))
    return(if (predicate[[2L]]) !found else found)
  }
  stop("UNSUPPORTED_PATTERN: predicate")
}
.s2_reporting_matches <- function(value, pattern) {
  value <- .s2_reporting_text(value); compiled <- .s2_pattern_compile(pattern)
  chars <- strsplit(value, "", fixed = TRUE)[[1L]]; states <- compiled$states
  active <- integer(); steps <- 0L
  for (position in 0:length(chars)) {
    pending <- unique(c(active, compiled$start)); closure <- integer()
    while (length(pending)) {
      index <- tail(pending, 1L); pending <- head(pending, -1L)
      if (index %in% closure) next
      closure <- c(closure, index)
      for (edge in states[[index]]) {
        kind <- edge[[1L]]; predicate <- edge[[2L]]; target <- edge[[3L]]
        steps <- steps + 1L
        if (steps > 2000000L) stop("TECHNICAL_LIMIT: regex transitions")
        if (kind == "empty" || (kind == "anchor" && ((predicate == "^" && position == 0L) || (predicate == "$" && position == length(chars)))))
          pending <- c(pending, target)
      }
    }
    if (compiled$end %in% closure) return(TRUE)
    active <- integer()
    if (position < length(chars)) for (index in closure) for (edge in states[[index]]) {
      kind <- edge[[1L]]; predicate <- edge[[2L]]; target <- edge[[3L]]
      steps <- steps + 1L + if (kind == "character" && predicate[[1L]] == "class") length(predicate[[3L]]) else 0L
      if (steps > 2000000L) stop("TECHNICAL_LIMIT: regex transitions")
      if (kind == "character" && .s2_pattern_accepts(predicate, chars[[position + 1L]])) active <- unique(c(active, target))
    }
  }
  FALSE
}
