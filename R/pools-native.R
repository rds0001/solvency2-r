# Native collection algorithms use local copies, not Python mutable aliases.
.s2_identifier <- function(value, field, nullable = FALSE) {
  .s2_require((nullable && is.null(value)) || (.s2_string(value) && nzchar(trimws(value))),
              field, "nonempty string ID or explicitly qualified absence required")
  value
}
.s2_ordered <- function(x) x[order(names(x), method = "radix")]
.s2_implementations <- c(.s2_implementations, list(
  default_pool_solvency_ratio = function(x, context) {
    .s2_reference(x$membership_reference, "membership_reference")
    .s2_require(.s2_mapping(x$in_scope_members) && .s2_mapping(x$out_of_scope_members), "members", "two explicit member mappings required")
    .s2_require(length(x$in_scope_members) > 0L || length(x$out_of_scope_members) > 0L, "members", "pool is empty", "MISSING_INPUT")
    .s2_require(!length(intersect(names(x$in_scope_members), names(x$out_of_scope_members))), "members", "duplicate member across groups")
    validate <- function(members, first) {
      result <- .s2_object()
      for (label in names(members)) {
        .s2_identifier(label, "members")
        row <- members[[label]]
        .s2_require(.s2_mapping(row) && setequal(names(row), c(first, "solvency_ratio")), label, "exact member fields required")
        weight <- .s2_number(row[[first]], paste0(label, ".", first), 0, if (first == "risk_share") 1 else NULL)
        sr <- .s2_number(row$solvency_ratio, paste0(label, ".solvency_ratio"), 0)
        if (first == "eligible_own_funds") .s2_require(sr > 0, label, "zero inner SR needs separate limiting-case assessment", "REVIEW_REQUIRED")
        result[[label]] <- stats::setNames(list(weight, sr), c(first, "solvency_ratio"))
      }
      result
    }
    inside <- validate(x$in_scope_members, "eligible_own_funds")
    outside <- validate(x$out_of_scope_members, "risk_share")
    share <- sum(vapply(outside, function(row) row$risk_share, numeric(1)))
    .s2_require(share <= 1, "risk_shares", "outer shares exceed whole pool")
    outer <- sum(vapply(outside, function(row) .s2_number(row$risk_share * row$solvency_ratio, "outer_contribution"), numeric(1)))
    inner <- NULL
    if (share < 1) {
      .s2_require(length(inside) > 0L, "in_scope_members", "inner members required for residual pool share", "MISSING_INPUT")
      eof <- sum(vapply(inside, function(row) row$eligible_own_funds, numeric(1)))
      denominator <- sum(vapply(inside, function(row) .s2_number(row$eligible_own_funds / row$solvency_ratio, "EOF_over_SR"), numeric(1)))
      .s2_require(eof > 0 && denominator > 0, "in_scope_members", "positive inner EOF and denominator required", "REVIEW_REQUIRED")
      inner <- .s2_number(eof / denominator, "inner_solvency_ratio", 0)
    }
    list(value = .s2_number(if (!is.null(inner)) (1 - share) * inner + outer else outer, "pool_solvency_ratio", 0),
      details = list(in_scope_members = inside, out_of_scope_members = outside,
        out_of_scope_risk_share = share, inner_solvency_ratio = inner, outer_contribution = outer,
        membership_reference = x$membership_reference, membership_status = "EXTERNAL_UNVERIFIED", scope = "supplied_members_only"))
  },
  default_single_name_count = function(x, context) {
    .s2_reference(x$grouping_reference, "grouping_reference")
    .s2_require(.s2_mapping(x$exposures), "exposures", "labelled exposure mapping required")
    .s2_require(length(x$exposures) > 0L, "exposures", "explicit nonempty exposure set required", "MISSING_INPUT")
    rows <- .s2_object()
    for (label in names(x$exposures)) {
      .s2_identifier(label, "exposure_id")
      row <- x$exposures[[label]]
      .s2_keys(row, c("counterparty_id", "group_id", "pool_id"), label)
      .s2_identifier(row$counterparty_id, paste0(label, ".counterparty_id"))
      .s2_identifier(row$group_id, paste0(label, ".group_id"), TRUE)
      .s2_identifier(row$pool_id, paste0(label, ".pool_id"), TRUE)
      rows[[label]] <- row
    }
    membership <- function(rows) {
      parties <- .s2_object(); pools <- character()
      for (label in names(rows)) {
        row <- rows[[label]]
        if (row$counterparty_id %in% names(parties)) .s2_require(identical(parties[[row$counterparty_id]], row$group_id),
          paste0(label, ".group_id"), "same counterparty has conflicting group declarations", "REVIEW_REQUIRED")
        parties[row$counterparty_id] <- list(row$group_id)
        if (!is.null(row$pool_id)) pools <- union(pools, row$pool_id)
      }
      pools
    }
    membership(rows)
    original <- rows; substitutions <- .s2_object()
    if (!is.null(x$provider_substitutions)) {
      .s2_require(.s2_mapping(x$provider_substitutions), "provider_substitutions", "explicit mapping required")
      .s2_require(all(names(x$provider_substitutions) %in% names(rows)), "provider_substitutions", "unknown exposure")
      for (label in names(x$provider_substitutions)) {
        field <- paste0("provider_substitutions.", label)
        protection <- x$provider_substitutions[[label]]
        .s2_keys(protection, c("provider_id", "group_id", "pool_id", "full_protection", "mitigation_qualified", "qualification_reference"), field)
        .s2_identifier(protection$provider_id, paste0(field, ".provider_id"))
        .s2_identifier(protection$group_id, paste0(field, ".group_id"), TRUE)
        .s2_identifier(protection$pool_id, paste0(field, ".pool_id"), TRUE)
        .s2_reference(protection$qualification_reference, paste0(field, ".qualification_reference"))
        full <- .s2_known_flag(protection$full_protection, paste0(field, ".full_protection"))
        qualified <- .s2_known_flag(protection$mitigation_qualified, paste0(field, ".mitigation_qualified"))
        .s2_require(full && qualified, field, "full qualifying209-215 protection required", "REVIEW_REQUIRED")
        rows[[label]] <- list(counterparty_id = protection$provider_id, group_id = protection$group_id, pool_id = protection$pool_id)
        substitutions[[label]] <- protection
      }
    }
    pools <- membership(rows)
    .s2_keys(x$pool_treatment, pools, "pool_treatment")
    policies <- .s2_object()
    for (pool in sort(pools, method = "radix")) {
      policy <- x$pool_treatment[[pool]]
      .s2_keys(policy, c("separate_members", "pd_lgd_qualified"), paste0("pool.", pool))
      separate <- .s2_known_flag(policy$separate_members, paste0("pool.", pool, ".separate_members"))
      qualified <- policy$pd_lgd_qualified
      .s2_require(is.null(qualified) || .s2_isinstance(qualified, "bool"), paste0("pool.", pool, ".pd_lgd_qualified"), "boolean qualification or unknown NULL required")
      .s2_require(!separate || identical(qualified, TRUE), paste0("pool.", pool), "separation requires declared PD and LGD qualification", "REVIEW_REQUIRED")
      policies[[pool]] <- policy
    }
    parent <- stats::setNames(as.list(names(rows)), names(rows))
    find <- function(label) {
      while (!identical(parent[[label]], label)) label <- parent[[label]]
      label
    }
    representatives <- .s2_object()
    for (label in sort(names(rows), method = "radix")) {
      row <- rows[[label]]
      keys <- paste0("party:", row$counterparty_id)
      if (!is.null(row$group_id)) keys <- c(keys, paste0("group:", row$group_id))
      if (!is.null(row$pool_id) && !policies[[row$pool_id]]$separate_members) keys <- c(keys, paste0("pool:", row$pool_id))
      for (key in keys) {
        if (key %in% names(representatives)) {
          roots <- sort(c(find(label), find(representatives[[key]])), method = "radix")
          parent[[roots[[2L]]]] <- roots[[1L]]
        } else representatives[[key]] <- label
      }
    }
    groups <- assignment <- .s2_object()
    for (label in sort(names(rows), method = "radix")) {
      root <- find(label)
      groups[[root]] <- c(groups[[root]], list(label))
      assignment[[label]] <- root
    }
    list(value = length(groups), details = list(single_names = groups, exposure_to_single_name = assignment,
      exposures = .s2_ordered(rows), original_exposures = .s2_ordered(original), provider_substitutions = .s2_ordered(substitutions),
      pool_treatment = policies, grouping_reference = x$grouping_reference, membership_status = "EXTERNAL_UNVERIFIED",
      portfolio_completeness_verified = FALSE, pool_pd_lgd_compliance_verified = FALSE,
      provider_substitution_performed = length(substitutions) > 0L, provider_qualification_verified = FALSE,
      pd_or_lgd_substitution_performed = FALSE, capital_charge_computed = FALSE,
      scope = if (length(substitutions)) "declared_article189_5_substitution_then190_count_only" else "declared_article190_relationships_only_no_189_5_substitution",
      `_sources` = if (length(substitutions)) list("DR:article-189:5") else list()))
  }
))
