# Topological look-through uses exact intermediate multipliers; no NAV model.
.s2_implementations <- c(.s2_implementations, list(
  lookthrough_exposure_amount = function(x, context) {
    .s2_reference(x$basis_reference, "basis_reference")
    complete <- .s2_known_flag(x$complete_scope, "complete_scope")
    .s2_require(complete, "complete_scope", "complete chosen scope must be qualified", "REVIEW_REQUIRED")
    values <- function(input, field) {
      .s2_require(.s2_mapping(input), field, "labelled numeric mapping required")
      result <- .s2_object()
      for (key in names(input)) {
        .s2_identifier(key, paste0(field, ".id"))
        result[[key]] <- .s2_number(input[[key]], paste0(field, ".", key))
      }
      result
    }
    finite <- function(value, field) {
      result <- as.numeric(value)
      .s2_require(is.finite(result), field, "look-through amount/factor exceeds finite range", "REVIEW_REQUIRED")
      .s2_require(result != 0 || value == 0, field, "nonzero look-through amount/factor underflows", "REVIEW_REQUIRED")
      result
    }
    fraction <- function(value) .s2_rational(.s2_stringify(value))
    roots <- values(x$holdings, "holdings")
    .s2_require(length(roots) > 0L, "holdings", "nonempty root holdings required", "MISSING_INPUT")
    .s2_require(.s2_mapping(x$funds) && length(x$funds) > 0L, "funds", "nonempty fund mapping required", "MISSING_INPUT")
    rows <- .s2_object(); exposures <- character()
    for (label in names(x$funds)) {
      .s2_identifier(label, "fund_id")
      row <- x$funds[[label]]
      .s2_keys(row, c("positions", "fund_links"), label)
      positions <- values(row$positions, paste0(label, ".positions"))
      links <- values(row$fund_links, paste0(label, ".fund_links"))
      .s2_require(length(positions) > 0L || length(links) > 0L, label, "empty fund is not complete exposure data", "MISSING_INPUT")
      rows[[label]] <- list(positions = positions, fund_links = links)
      exposures <- union(exposures, names(positions))
    }
    .s2_require(all(names(roots) %in% names(rows)), "holdings", "unknown root fund", "MISSING_INPUT")
    for (label in names(rows)) .s2_require(all(names(rows[[label]]$fund_links) %in% names(rows)),
      paste0(label, ".fund_links"), "unknown underlying fund", "MISSING_INPUT")
    reachable <- character(); pending <- names(roots)
    while (length(pending)) {
      label <- pending[[length(pending)]]; pending <- pending[-length(pending)]
      if (!(label %in% reachable)) {
        reachable <- c(reachable, label)
        pending <- c(pending, names(rows[[label]]$fund_links))
      }
    }
    .s2_require(setequal(reachable, names(rows)), "funds", "unreachable supplied fund outside chosen scope")
    .s2_require(.s2_string(x$exposure_id) && x$exposure_id %in% exposures, "exposure_id", "known underlying exposure ID required", "UNSUPPORTED_PARAMETER")
    indegree <- stats::setNames(rep(0L, length(rows)), names(rows))
    factors <- stats::setNames(lapply(rows, function(row) .s2_rational(0)), names(rows))
    for (label in names(roots)) factors[[label]] <- factors[[label]] + fraction(roots[[label]])
    for (row in rows) for (child in names(row$fund_links)) indegree[[child]] <- indegree[[child]] + 1L
    ready <- sort(names(indegree)[indegree == 0L], method = "radix")
    totals <- stats::setNames(lapply(exposures, function(key) .s2_rational(0)), exposures)
    contributions <- .s2_object(); order <- list()
    while (length(ready)) {
      label <- ready[[1L]]; ready <- ready[-1L]
      order <- c(order, list(label)); row <- rows[[label]]
      contributions[[label]] <- .s2_object()
      for (key in names(row$positions)) {
        contribution <- factors[[label]] * fraction(row$positions[[key]])
        contributions[[label]][[key]] <- contribution
        totals[[key]] <- totals[[key]] + contribution
      }
      for (child in names(row$fund_links)) {
        factors[[child]] <- factors[[child]] + factors[[label]] * fraction(row$fund_links[[child]])
        indegree[[child]] <- indegree[[child]] - 1L
        if (indegree[[child]] == 0L) ready <- sort(c(ready, child), method = "radix")
      }
    }
    .s2_require(length(order) == length(rows), "fund_links", "cyclic structures need separate qualification/solution", "UNSUPPORTED_METHOD")
    amounts <- lapply(sort(names(totals), method = "radix"), function(key) finite(totals[[key]], paste0("underlying.", key)))
    names(amounts) <- sort(names(totals), method = "radix")
    audit <- .s2_object()
    for (label in sort(names(rows), method = "radix")) {
      allocated <- lapply(contributions[[label]], function(value) finite(value, paste0(label, ".allocated")))
      audit[[label]] <- list(positions = .s2_ordered(rows[[label]]$positions), fund_links = .s2_ordered(rows[[label]]$fund_links),
        effective_factor = finite(factors[[label]], paste0("effective_factor.", label)), allocated_positions = .s2_ordered(allocated))
    }
    list(value = amounts[[x$exposure_id]], details = list(exposure_id = x$exposure_id, underlying_amounts = amounts,
      funds = audit, holdings = .s2_ordered(roots), topological_order = order, complete_scope_declared = complete,
      basis_reference = x$basis_reference, qualification_status = "EXTERNAL_UNVERIFIED", completeness_verified = FALSE,
      factors_normalised = FALSE, nav_or_ownership_computed = FALSE, asset_or_risk_classification_performed = FALSE,
      capital_charge_computed = FALSE, scope = "acyclic_qualified_linear_underlying_amounts_not_full_lookthrough_SCR"))
  }
))
