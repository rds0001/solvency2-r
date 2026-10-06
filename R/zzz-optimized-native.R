# Equivalent native vectorized arithmetic for shared hot paths. Do not run
# interpreted per-cell loops over the large regulatory catastrophe matrices.
.s2_native_aggregation__quadratic <- function(components, matrix) {
  result <- .s2_quadratic(components, matrix)
  list(result$value, result$details)
}
