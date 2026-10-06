# Small, native-R value operations used by mechanically translated formulas.
# These preserve explicit mappings, null entries and zero-based source indices.
# There is no source evaluator, Python bridge or runtime code generation.
.s2_truth <- function(x) {
  if (is.null(x) || !length(x)) return(FALSE)
  if (inherits(x, "mpfr")) return(isTRUE(as.logical(x != 0)))
  if (is.list(x)) return(TRUE)
  if (is.character(x)) return(length(x) > 1L || nzchar(x))
  if (length(x) > 1L) return(TRUE)
  isTRUE(as.logical(x != 0))
}
.s2_and <- function(a, b) if (.s2_truth(a)) b else a
.s2_or <- function(a, b) if (.s2_truth(a)) a else b
.s2_native_require <- function(ok, field, message, code = "INVALID_INPUT")
  .s2_require(.s2_truth(ok), field, message, code)
.s2_erfc <- function(x) 2 * stats::pnorm(-x * sqrt(2))
.s2_length <- function(x) if (.s2_string(x)) nchar(x, type = "chars") else length(x)
.s2_iter <- function(x) {
  if (.s2_mapping(x)) return(lapply(names(x), function(key) {
    original <- attr(x, "s2_key_values")[[key]]
    if (is.null(original)) key else original
  }))
  if (.s2_string(x)) return(as.list(strsplit(x, "", fixed = TRUE)[[1L]]))
  if (inherits(x, c("bigq", "mpfr"))) return(lapply(seq_len(length(x)), function(i) x[i]))
  as.list(x)
}
.s2_isinstance <- function(x, types) {
  any(vapply(types, function(type) switch(type,
    str = .s2_string(x), bool = is.logical(x) && length(x) == 1L && !is.na(x),
    int = (is.numeric(x) || is.logical(x)) && length(x) == 1L && is.finite(x) && x == trunc(x),
    float = is.numeric(x) && length(x) == 1L,
    list = (is.list(x) && is.null(names(x))) || (is.atomic(x) && !is.character(x) && is.null(names(x))),
    tuple = (is.list(x) && is.null(names(x))) || (is.atomic(x) && !is.character(x) && is.null(names(x))),
    dict = .s2_mapping(x),
    Mapping = .s2_mapping(x),
    Sequence = (is.list(x) && is.null(names(x))) || is.atomic(x),
    bytes = is.raw(x),
    Fraction = inherits(x, "bigq"), FALSE), logical(1)))
}
.s2_at <- function(x, key) {
  if (inherits(x, "bigq") && identical(key, "numerator")) return(gmp::numerator(x))
  if (inherits(x, "bigq") && identical(key, "denominator")) return(gmp::denominator(x))
  if (inherits(x, "Date") && key %in% c("year", "month", "day"))
    return(as.integer(format(x, switch(key, year = "%Y", month = "%m", day = "%d"))))
  if (inherits(x, "difftime") && identical(key, "days")) return(as.numeric(x, units = "days"))
  if (.s2_mapping(x)) key <- .s2_key(key)
  if (is.character(key)) {
    .s2_require(length(key) == 1L && !is.na(key) && key %in% names(x), "key", "missing mapping key", "MISSING_INPUT")
    return(x[[key]])
  }
  .s2_require(is.numeric(key) && length(key) == 1L && is.finite(key) && key == trunc(key), "index", "integer index required")
  n <- .s2_length(x)
  index <- if (key < 0) n + key + 1L else key + 1L
  .s2_require(index > 0 && index <= n, "index", "index outside supplied sequence")
  if (.s2_string(x)) return(substr(x, index, index))
  if (inherits(x, "bigq")) return(x[index])
  x[[index]]
}
.s2_put <- function(x, key, value) {
  # Python evaluates an assignment's RHS before its target. This matters when
  # the RHS pops a key from the same mapping; force it before reading x.
  force(value)
  original <- key
  if (.s2_mapping(x)) key <- .s2_key(key)
  if (is.character(key)) {
    if (!length(x)) x <- .s2_object()
    x[key] <- list(value)
    if (!is.character(original)) {
      keys <- attr(x, "s2_key_values")
      if (is.null(keys)) keys <- list()
      keys[key] <- list(original)
      attr(x, "s2_key_values") <- keys
    }
    return(x)
  }
  index <- if (key < 0) length(x) + key + 1L else key + 1L
  .s2_require(index >= 1L && index <= length(x), "index", "index outside supplied sequence")
  x[index] <- list(value)
  x
}
.s2_slice <- function(x, start = NULL, stop = NULL, step = NULL) {
  n <- .s2_length(x)
  if (is.null(step)) step <- 1
  .s2_require(step != 0, "slice", "zero step")
  if (step > 0) {
    if (is.null(start)) start <- 0 else if (start < 0) start <- n + start
    if (is.null(stop)) stop <- n else if (stop < 0) stop <- n + stop
    start <- min(n, max(0, start)); stop <- min(n, max(0, stop))
  } else {
    if (is.null(start)) start <- n - 1 else if (start < 0) start <- n + start
    if (is.null(stop)) stop <- -1 else if (stop < 0) stop <- n + stop
    start <- min(n - 1, max(-1, start)); stop <- min(n - 1, max(-1, stop))
  }
  indices <- if ((stop - start) * step <= 0) numeric() else seq(start, stop - sign(step), by = step)
  if (.s2_string(x)) return(paste0(unlist(.s2_iter(x)[indices + 1]), collapse = ""))
  x[indices + 1]
}
.s2_dict <- function(keys, values) {
  result <- .s2_object()
  for (i in seq_along(keys)) result <- .s2_put(result, keys[[i]], values[[i]])
  result
}
.s2_merge <- function(...) {
  result <- .s2_object()
  for (part in list(...)) for (key in .s2_iter(part)) result <- .s2_put(result, key, .s2_at(part, key))
  result
}
.s2_dict_from <- function(x = list(), ...) {
  if (.s2_mapping(x)) return(.s2_merge(x, list(...)))
  result <- .s2_object()
  for (row in .s2_iter(x)) {
    .s2_require(length(row) == 2L, "mapping", "key/value pair required")
    result <- .s2_put(result, row[[1L]], row[[2L]])
  }
  .s2_merge(result, list(...))
}
.s2_equal <- function(a, b) {
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  if (inherits(a, "mpfr") || inherits(b, "mpfr")) return(length(a) == length(b) && isTRUE(all(a == b)))
  if (inherits(a, "s2_set") || inherits(b, "s2_set"))
    return(length(a) == length(b) && all(vapply(a, function(v) .s2_contains(b, v), logical(1))))
  if (inherits(a, "bigq") || inherits(b, "bigq")) return(length(a) == length(b) && all(a == b))
  if (.s2_mapping(a) || .s2_mapping(b)) {
    if (!setequal(names(a), names(b)) || length(a) != length(b)) return(FALSE)
    return(all(vapply(names(a), function(k) .s2_equal(a[[k]], b[[k]]), logical(1))))
  }
  if (is.list(a) || is.list(b)) {
    if (length(a) != length(b)) return(FALSE)
    return(all(vapply(seq_along(a), function(i) .s2_equal(a[[i]], b[[i]]), logical(1))))
  }
  if (is.character(a) != is.character(b)) return(FALSE)
  length(a) == length(b) && isTRUE(all(a == b))
}
.s2_contains <- function(container, value) {
  if (.s2_mapping(container)) return(.s2_key(value) %in% names(container))
  if (.s2_string(container) && .s2_string(value)) return(grepl(value, container, fixed = TRUE))
  any(vapply(.s2_iter(container), function(x) .s2_equal(x, value), logical(1)))
}
.s2_set <- function(x = list()) {
  result <- list()
  for (value in .s2_iter(x)) if (!.s2_contains(result, value)) result[length(result) + 1L] <- list(value)
  structure(result, class = c("s2_set", "list"))
}
.s2_add <- function(a, b) {
  if (inherits(a, "mpfr") || inherits(b, "mpfr")) return(a + b)
  if (is.list(a) && is.list(b)) return(c(a, b))
  if (is.character(a) && is.character(b)) return(paste0(a, b))
  a + b
}
.s2_multiply <- function(a, b) {
  if (inherits(a, "mpfr") || inherits(b, "mpfr")) return(a * b)
  if (is.list(a) && is.numeric(b) && length(b) == 1L) return(rep(a, max(0, b)))
  if (is.numeric(a) && length(a) == 1L && is.list(b)) return(rep(b, max(0, a)))
  if (.s2_string(a) && is.numeric(b)) return(paste0(rep(a, max(0, b)), collapse = ""))
  a * b
}
.s2_fromkeys <- function(keys, value = NULL) {
  keys <- .s2_iter(keys)
  .s2_dict(keys, rep(list(value), length(keys)))
}
.s2_subtract <- function(a, b) {
  if (inherits(a, "s2_set") && inherits(b, "s2_set"))
    return(.s2_set(Filter(function(value) !.s2_contains(b, value), a)))
  a - b
}
.s2_remove <- function(x, value) {
  matches <- which(vapply(.s2_iter(x), function(item) .s2_equal(item, value), logical(1)))
  .s2_require(length(matches) > 0L, "value", "value absent from collection")
  x[-matches[[1L]]]
}
.s2_stringify <- function(x) {
  if (.s2_string(x)) return(x)
  if (is.null(x)) return("None")
  if (identical(x, TRUE)) return("True")
  if (identical(x, FALSE)) return("False")
  if (inherits(x, "bigq")) return(as.character(x))
  if (is.numeric(x) && length(x) == 1L && is.finite(x)) {
    # Shortest decimal that round-trips to this input; preserves decimal-based
    # exact rational thresholds rather than approximating them with epsilon.
    for (digits in seq_len(17L)) {
      candidate <- format(x, digits = digits, scientific = TRUE, trim = TRUE)
      if (identical(as.numeric(candidate), as.numeric(x))) {
        if (x == trunc(x) && abs(x) < 1e16) return(format(x, scientific = FALSE, trim = TRUE))
        return(candidate)
      }
    }
  }
  as.character(x)
}
.s2_rational <- function(numerator = 0, denominator = NULL) {
  if (inherits(numerator, "bigq") && is.null(denominator)) return(numerator)
  if (!is.null(denominator)) return(.s2_rational(numerator) / .s2_rational(denominator))
  # Numeric arguments in emitted formulas are integer constants; decimal
  # quantities explicitly arrive through the reference's str(value) operation.
  if (is.numeric(numerator) && numerator == trunc(numerator)) return(gmp::as.bigq(numerator))
  if (is.numeric(numerator)) return(gmp::as.bigq(numerator))
  parts <- strsplit(tolower(numerator), "e", fixed = TRUE)[[1L]]
  exponent <- if (length(parts) == 2L) as.integer(parts[[2L]]) else 0L
  decimal <- strsplit(parts[[1L]], ".", fixed = TRUE)[[1L]]
  scale <- if (length(decimal) == 2L) nchar(decimal[[2L]]) else 0L
  # Decimal lexical inputs may start with zero; GMP's autodetection would
  # interpret strings such as "025" as octal rather than decimal 25.
  lexical <- paste0(decimal, collapse = "")
  negative <- startsWith(lexical, "-")
  lexical <- sub("^0+", "", sub("^[+-]", "", lexical))
  if (!nzchar(lexical)) lexical <- "0"
  integer <- gmp::as.bigz(paste0(if (negative) "-" else "", lexical))
  power <- exponent - scale
  if (power >= 0L) gmp::as.bigq(integer * gmp::as.bigz(10)^power) else gmp::as.bigq(integer, gmp::as.bigz(10)^(-power))
}
.s2_float <- function(x) {
  value <- as.numeric(x)
  if (length(value) == 1L && is.infinite(value)) stop(structure(
    list(message = "numeric overflow", call = NULL), class = c("s2_overflow", "error", "condition")))
  value
}
.s2_int <- function(x) trunc(as.numeric(x))
.s2_list <- function(x = list()) .s2_iter(x)
.s2_all <- function(x) all(vapply(.s2_iter(x), .s2_truth, logical(1)))
.s2_any <- function(x) any(vapply(.s2_iter(x), .s2_truth, logical(1)))
.s2_sum <- function(x, start = 0) {
  values <- .s2_iter(x)
  if (inherits(start, c("bigq", "mpfr")) || any(vapply(values, inherits, logical(1), c("bigq", "mpfr"))))
    return(Reduce(`+`, values, init = start))
  sum(unlist(values, use.names = FALSE), start)
}
.s2_extreme <- function(values, largest, key = identity, default = NULL) {
  if (!length(values) && !is.null(default)) return(default)
  .s2_require(length(values) > 0L, "sequence", "nonempty sequence required")
  result <- values[[1L]]
  for (value in values[-1L]) if (isTRUE(if (largest) key(value) > key(result) else key(value) < key(result))) result <- value
  result
}
.s2_max <- function(..., key = identity, default = NULL) {
  args <- list(...)
  .s2_extreme(if (length(args) == 1L) .s2_iter(args[[1L]]) else args, TRUE, key, default)
}
.s2_min <- function(..., key = identity, default = NULL) {
  args <- list(...)
  .s2_extreme(if (length(args) == 1L) .s2_iter(args[[1L]]) else args, FALSE, key, default)
}
.s2_sorted <- function(x, reverse = FALSE, key = identity) {
  values <- .s2_iter(x)
  if (!length(values)) return(list())
  keys <- lapply(values, key)
  if (all(vapply(keys, function(k) is.atomic(k) && length(k) == 1L, logical(1))))
    return(values[order(unlist(keys), decreasing = reverse, method = "radix")])
  # Stable insertion order for lexicographic tuple keys; do not flatten pairs.
  indices <- integer()
  for (i in seq_along(keys)) {
    pos <- which(vapply(indices, function(j) .s2_compare(keys[[i]], keys[[j]], if (reverse) ">" else "<"), logical(1)))
    indices <- append(indices, i, after = if (length(pos)) pos[[1L]] - 1L else length(indices))
  }
  values[indices]
}
.s2_enumerate <- function(x, start = 0) {
  values <- .s2_iter(x)
  lapply(seq_along(values), function(i) list(i - 1L + start, values[[i]]))
}
.s2_zip <- function(...) {
  values <- lapply(list(...), .s2_iter)
  if (!length(values)) return(list())
  lapply(seq_len(min(lengths(values))), function(i) lapply(values, `[[`, i))
}
.s2_range <- function(start, stop = NULL, step = 1) {
  if (is.null(stop)) { stop <- start; start <- 0 }
  .s2_require(step != 0, "range", "zero step")
  if ((stop - start) * step <= 0) return(list())
  as.list(seq(start, stop - sign(step), by = step))
}
.s2_hypot <- function(...) {
  x <- unlist(list(...), use.names = FALSE)
  if (!length(x)) return(0)
  scale <- max(abs(x))
  if (!scale) return(0)
  scale * sqrt(sum((x / scale)^2))
}
.s2_power <- function(a, b) {
  result <- a^b
  if (length(result) == 1L && is.infinite(result) && is.finite(a) && is.finite(b))
    stop(structure(list(message = "numeric overflow", call = NULL), class = c("s2_overflow", "s2_arithmetic_error", "error", "condition")))
  result
}
.s2_divide <- function(a, b) {
  if (length(b) == 1L && isTRUE(b == 0)) stop(structure(
    list(message = "division by zero", call = NULL), class = c("s2_zero_division", "s2_arithmetic_error", "error", "condition")))
  a / b
}
.s2_decimal <- function(x = 0) Rmpfr::mpfr(x, precBits = 320L)
.s2_identity_key <- function(x) digest::digest(x, algo = "sha256")
.s2_bisect_left <- function(values, value) sum(unlist(values) < value)
.s2_arithmetic_error <- function(message = "arithmetic error") structure(
  list(message = message, call = NULL), class = c("s2_arithmetic_error", "error", "condition"))
.s2_key <- function(key) {
  if (is.list(key)) return(paste0("@tuple:", as.character(jsonlite::toJSON(key, auto_unbox = TRUE))))
  .s2_stringify(key)
}
.s2_compare <- function(a, b, op) {
  if (inherits(a, "mpfr") || inherits(b, "mpfr"))
    return(isTRUE(as.logical(switch(op, `<` = a < b, `<=` = a <= b, `>` = a > b, `>=` = a >= b))))
  if (inherits(a, "s2_set") || inherits(b, "s2_set")) {
    subset <- all(vapply(.s2_iter(a), function(v) .s2_contains(b, v), logical(1)))
    superset <- all(vapply(.s2_iter(b), function(v) .s2_contains(a, v), logical(1)))
    return(switch(op, `<` = subset && length(a) < length(b), `<=` = subset,
      `>` = superset && length(a) > length(b), `>=` = superset))
  }
  if (is.list(a) && is.list(b)) {
    for (i in seq_len(min(length(a), length(b)))) {
      if (!.s2_equal(a[[i]], b[[i]])) return(.s2_compare(a[[i]], b[[i]], op))
    }
    return(.s2_compare(length(a), length(b), op))
  }
  isTRUE(switch(op, `<` = a < b, `<=` = a <= b, `>` = a > b, `>=` = a >= b))
}
.s2_floor <- function(x) {
  if (inherits(x, "bigq")) return(gmp::numerator(x) %/% gmp::denominator(x))
  floor(x)
}
.s2_ceiling <- function(x) {
  if (inherits(x, "bigq")) return(-.s2_floor(-x))
  ceiling(x)
}
.s2_isclose <- function(a, b, rel_tol = 1e-9, abs_tol = 0) {
  if (identical(a, b)) return(TRUE)
  is.finite(a) && is.finite(b) && abs(a - b) <= max(abs_tol, rel_tol * max(abs(a), abs(b)))
}
.s2_nextafter <- function(x, y) {
  # Required by the equivalent-rate solver: nearest representable rate > -1.
  if (identical(as.numeric(x), -1) && y == 0) return(-1 + .Machine$double.eps / 2)
  .s2_fail("INVALID_INPUT", "nextafter", "unsupported internal nextafter arguments")
}
.s2_map <- function(fun, ...) {
  rows <- .s2_zip(...)
  lapply(rows, function(row) do.call(fun, row))
}
.s2_next <- function(x, default = NULL) if (length(x)) x[[1L]] else default
.s2_exact_type <- function(x, type) {
  if (type == "int") return(!is.logical(x) && .s2_isinstance(x, "int"))
  .s2_isinstance(x, type)
}
.s2_union <- function(a, b) .s2_set(c(.s2_iter(a), .s2_iter(b)))
.s2_intersection <- function(a, b) .s2_set(Filter(function(v) .s2_contains(b, v), .s2_iter(a)))
.s2_update <- function(a, b) if (inherits(a, "s2_set")) .s2_union(a, b) else .s2_merge(a, b)
.s2_method <- function(x, method, ...) {
  args <- list(...)
  switch(method,
    get = if (.s2_key(args[[1L]]) %in% names(x)) x[[.s2_key(args[[1L]])]] else if (length(args) > 1L) args[[2L]] else NULL,
    items = lapply(.s2_iter(x), function(key) list(key, .s2_at(x, key))),
    values = unname(x), keys = .s2_iter(x), copy = x, to_dict = unclass(x),
    intersection = .s2_intersection(x, args[[1L]]), union = .s2_union(x, args[[1L]]),
    difference = .s2_subtract(.s2_set(x), .s2_set(args[[1L]])),
    issubset = all(vapply(.s2_iter(x), function(v) .s2_contains(args[[1L]], v), logical(1))),
    isdisjoint = !length(.s2_intersection(x, args[[1L]])),
    isoformat = format(x, "%Y-%m-%d"),
    sqrt = sqrt(x),
    split = .s2_split(x, if (length(args)) args[[1L]] else NULL, if (length(args) > 1L) args[[2L]] else -1),
    join = paste0(unlist(.s2_iter(args[[1L]])), collapse = x),
    replace = if (inherits(x, "Date")) .s2_date(sprintf("%04d-%s", args$year, format(x, "%m-%d"))) else gsub(args[[1L]], args[[2L]], x, fixed = TRUE),
    count = sum(vapply(.s2_iter(x), function(v) .s2_equal(v, args[[1L]]), logical(1))),
    index = { matches <- which(vapply(.s2_iter(x), function(v) .s2_equal(v, args[[1L]]), logical(1))); if (!length(matches)) stop(.s2_value_error("value absent")); matches[[1L]] - 1L },
    isdecimal = grepl("^[0-9]+$", x),
    strip = trimws(x), lower = tolower(x), upper = toupper(x),
    is_integer = is.finite(x) && x == trunc(x),
    startswith = startsWith(x, args[[1L]]), endswith = endsWith(x, args[[1L]]),
    isascii = !grepl("[^\x01-\x7F]", x, perl = TRUE),
    isalpha = grepl("^[[:alpha:]]+$", x),
    isupper = identical(toupper(x), x) && grepl("[[:upper:]]", x),
    .s2_fail("INVALID_INPUT", "method", "unknown native value operation"))
}
.s2_split <- function(x, separator = NULL, maximum = -1) {
  if (is.null(separator)) return(as.list(strsplit(trimws(x), "[[:space:]]+")[[1L]]))
  if (!nzchar(separator)) stop(.s2_value_error("empty separator"))
  positions <- gregexpr(separator, x, fixed = TRUE)[[1L]]
  positions <- positions[positions > 0L]
  if (maximum >= 0) positions <- head(positions, maximum)
  starts <- c(1L, positions + nchar(separator))
  ends <- c(positions - 1L, nchar(x))
  lapply(seq_along(starts), function(i) if (ends[[i]] < starts[[i]]) "" else substr(x, starts[[i]], ends[[i]]))
}
.s2_delete <- function(x, key) {
  if (is.character(key)) return(x[setdiff(names(x), key)])
  index <- if (key < 0) length(x) + key + 1L else key + 1L
  x[-index]
}
.s2_value_error <- function(message = "invalid value") structure(
  list(message = message, call = NULL), class = c("s2_value_error", "error", "condition"))
.s2_type_error <- function(message = "invalid type") structure(
  list(message = message, call = NULL), class = c("s2_type_error", "error", "condition"))
.s2_assert_error <- function(message = "invalid internal state") structure(
  list(message = message, call = NULL), class = c("s2_assert_error", "error", "condition"))
.s2_date <- function(value) {
  if (!.s2_string(value)) stop(.s2_type_error("date must be text"))
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) stop(.s2_value_error("invalid ISO date"))
  result <- suppressWarnings(as.Date(value, format = "%Y-%m-%d"))
  if (is.na(result) || !identical(format(result, "%Y-%m-%d"), value)) stop(.s2_value_error("invalid ISO date"))
  result
}
