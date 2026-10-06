# Direct native implementations of the first coherent capital/aggregation block.
# API wrappers and source anchors are emitted from the frozen public contract.
.s2_quadratic <- function(components, matrix) {
  labels <- unlist(matrix$labels, use.names = FALSE)
  .s2_keys(components, labels, "components")
  x <- vapply(labels, function(k) .s2_number(components[[k]], k, 0), numeric(1))
  correlations <- do.call(rbind, lapply(matrix$values, unlist, use.names = FALSE))
  products <- as.vector(correlations %*% x)
  squared <- sum(x * products)
  .s2_require(squared >= 0, "matrix", "negative quadratic form", "NUMERICAL_ERROR")
  value <- sqrt(squared)
  euler <- if (value == 0) rep(0, length(x)) else x * products / value
  list(value = value, details = list(quadratic_form = squared, labels = as.list(labels),
    euler_contributions = as.list(stats::setNames(euler, labels)), `_sources` = list(matrix$source)))
}

.s2_implementations <- list(
  intangible_risk = function(x, context) {
    list(value = .s2_number(x$recognised_value, "recognised_value", 0) * .s2_scalar("intangible", context))
  },
  aggregate_risk = function(x, context) {
    matrices <- .s2_effective("profile.json", context)$matrices
    .s2_require(.s2_string(x$matrix_id) && x$matrix_id %in% names(matrices),
      "matrix_id", "unknown matrix", "UNSUPPORTED_PARAMETER")
    result <- .s2_quadratic(x$components, matrices[[x$matrix_id]])
    result$details$matrix_id <- x$matrix_id
    result$details$scope <- "aggregation_only"
    result
  },
  basic_scr = function(x, context) {
    result <- .s2_quadratic(x$components, .s2_effective("profile.json", context)$matrices$bscr)
    intangible <- .s2_number(x$intangible_charge, "intangible_charge", 0)
    result$value <- result$value + intangible
    result$details$intangible_charge <- intangible
    result$details$scope <- "aggregation_only"
    result
  },
  risk_margin = function(x, context) {
    .s2_require(identical(x$runoff_complete, TRUE), "runoff_complete", "complete runoff required", "MISSING_INPUT")
    .s2_require((is.list(x$projected_scr) || is.numeric(x$projected_scr)) &&
      (is.list(x$basic_rates) || is.numeric(x$basic_rates)), "projection", "ordered vectors required")
    .s2_require(length(x$projected_scr) > 0L && length(x$projected_scr) == length(x$basic_rates),
                "projection", "equal nonzero lengths required")
    profile <- .s2_effective("profile.json", context)
    method <- profile$methods$risk_margin
    .s2_require(method %in% c("UNDAMPED_BASELINE", "TIME_ADJUSTED_2027"),
                "risk_margin_method", "unimplemented method", "UNSUPPORTED_METHOD")
    decay <- time_floor <- 1
    if (method == "TIME_ADJUSTED_2027") {
      decay <- .s2_number(.s2_scalar("risk_margin_time_decay", context), "time_decay", 0, 1)
      time_floor <- .s2_number(.s2_scalar("risk_margin_time_floor", context), "time_floor", 0, 1)
    }
    discounted <- factors <- vector("list", length(x$projected_scr))
    for (i in seq_along(x$projected_scr)) {
      scr <- .s2_number(x$projected_scr[[i]], paste0("scr[", i - 1L, "]"), 0)
      rate <- .s2_number(x$basic_rates[[i]], paste0("rate[", i, "]"))
      .s2_require(rate > -1, paste0("rate[", i, "]"), "annual rate must exceed -1")
      factors[[i]] <- max(decay^(i - 1L), time_floor)
      discounted[[i]] <- factors[[i]] * scr / (1 + rate)^i
    }
    cost <- .s2_number(.s2_scalar("cost_of_capital", context), "cost_of_capital", 0)
    list(value = cost * sum(unlist(discounted)), details = list(discounted_scr = discounted,
      cost_of_capital = cost, risk_margin_method = method, time_adjustment_factors = factors,
      projection_support = "EXTERNAL_INPUT_ONLY", source_profile = context$profile_id,
      `_sources` = list(paste0(context$profile_id, ":", profile$sources$DR$sha256, ":articles-37-39"))))
  },
  operational_risk = function(x, context) {
    for (name in c("bscr", "expenses_ul", "earned_life", "earned_life_ul", "earned_nonlife",
                    "previous_life", "previous_life_ul", "previous_nonlife"))
      x[[name]] <- .s2_number(x[[name]], name, 0)
    for (name in c("tp_life", "tp_life_ul", "tp_nonlife")) x[[name]] <- .s2_number(x[[name]], name)
    .s2_require(x$earned_life_ul <= x$earned_life && x$previous_life_ul <= x$previous_life,
                "unit_linked_premiums", "subset cannot exceed total premiums")
    s <- function(key) .s2_scalar(key, context)
    growth <- s("op_growth_threshold")
    prem <- s("op_life_premium") * (x$earned_life - x$earned_life_ul) + s("op_nonlife_premium") * x$earned_nonlife +
      max(0, s("op_life_premium") * ((x$earned_life - x$earned_life_ul) - growth * (x$previous_life - x$previous_life_ul))) +
      max(0, s("op_nonlife_premium") * (x$earned_nonlife - growth * x$previous_nonlife))
    prov <- s("op_life_tp") * max(0, x$tp_life - x$tp_life_ul) + s("op_nonlife_tp") * max(0, x$tp_nonlife)
    base <- max(prem, prov)
    list(value = min(s("op_cap") * x$bscr, base) + s("op_ul_expense") * x$expenses_ul,
      details = list(premium_charge = prem, provision_charge = prov, basic_charge = base,
                     cap = s("op_cap") * x$bscr, unit_linked_addition = s("op_ul_expense") * x$expenses_ul))
  },
  lac_tp = function(x, context) {
    gross <- .s2_number(x$bscr, "bscr", 0)
    net <- .s2_number(x$net_bscr, "net_bscr", 0)
    fdb <- .s2_number(x$future_discretionary_benefits, "fdb", 0)
    list(value = -max(min(gross - net, fdb), 0), details = list(unbounded_difference = gross - net, fdb_cap = fdb))
  },
  lac_dt_stress_amount = function(x, context) {
    gross <- .s2_number(x$bscr, "bscr", 0)
    op <- .s2_number(x$operational, "operational", 0)
    tp <- .s2_number(x$adjustment_tp, "adjustment_tp", -gross, 0)
    list(value = sum(c(gross, op, tp)), details = list(bscr = gross, operational = op,
      adjustment_tp = tp, tax_revaluation = "EXTERNAL_INPUT_ONLY"))
  },
  mcr_combined = function(x, context) {
    linear <- .s2_number(x$linear, "linear")
    scr <- .s2_number(x$scr, "scr", 0)
    floor <- .s2_number(x$absolute_floor, "absolute_floor", 0)
    lower <- .s2_number(.s2_scalar("mcr_floor", context), "floor_fraction", 0, 1)
    upper <- .s2_number(.s2_scalar("mcr_cap", context), "cap_fraction", 0, 1)
    .s2_require(lower <= upper, "bounds", "floor fraction must not exceed cap fraction")
    combined <- min(max(linear, lower * scr), upper * scr)
    list(value = max(combined, floor), details = list(combined_before_absolute_floor = combined,
      absolute_floor_support = "EXTERNAL_INPUT_ONLY"))
  },
  mcr_linear = function(x, context) {
    nonlife <- .s2_number(x$nonlife, "nonlife", 0)
    life <- .s2_number(x$life, "life")
    list(value = sum(c(nonlife, life)), details = list(nonlife_component = nonlife,
      life_component = life, component_derivation_required = "DR:articles-250-251"))
  },
  mcr_life = function(x, context) {
    rows <- .s2_effective("profile.json", context)$tables$mcr_life$rows
    keys <- vapply(rows, function(row) row[[1L]], character(1))
    .s2_keys(x$components, keys, "components")
    terms <- stats::setNames(lapply(rows, function(row) max(0, .s2_number(x$components[[row[[1L]]]], row[[1L]])) * row[[2L]]), keys)
    list(value = sum(unlist(terms)), details = list(terms = terms))
  },
  mcr_nonlife = function(x, context) {
    rows <- .s2_effective("profile.json", context)$tables$mcr_nonlife$rows
    keys <- vapply(rows, function(row) row[[1L]], character(1))
    .s2_keys(x$segments, keys, "segments")
    terms <- stats::setNames(lapply(rows, function(row) {
      key <- row[[1L]]
      segment <- x$segments[[key]]
      .s2_keys(segment, c("technical_provisions", "written_premiums"), key)
      tp <- max(.s2_number(segment$technical_provisions, paste0(key, ".tp")), 0)
      premium <- max(.s2_number(segment$written_premiums, paste0(key, ".premiums")), 0)
      row[[2L]] * tp + row[[3L]] * premium
    }), keys)
    list(value = sum(unlist(terms)), details = list(terms = terms))
  }
)
