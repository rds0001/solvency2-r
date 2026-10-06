# Health stresses share parameter access, but retain distinct regulatory scope.
.s2_health_bounds <- function(segment, risk, context) {
  .s2_require(.s2_string(segment) && segment %in% c("1", "2", "3"), "segment", "Article 149 standard HRES scope is segments 1-3")
  .s2_require(.s2_string(risk) && risk %in% c("premium", "reserve"), "risk", "premium or reserve required")
  rows <- .s2_effective("profile.json", context)$tables$health_segments$rows
  row <- Filter(function(row) identical(row[[1L]], segment), rows)[[1L]]
  sigma <- .s2_number(row[[if (risk == "premium") 2L else 3L]], "standard_sigma", 0)
  if (risk == "premium") sigma <- sigma * .s2_number(.s2_scalar("health_nonprop_adjustment", context), "health_nonprop_adjustment", 0)
  sigma <- .s2_number(sigma, "standard_sigma", 0)
  fraction <- .s2_number(.s2_scalar("health_hres_floor_fraction", context), "health_hres_floor_fraction", 0, 1)
  c(sigma * fraction, sigma)
}
.s2_direction <- function(direction) {
  .s2_require(.s2_string(direction) && direction %in% c("up", "down"), "direction", "up or down required")
  if (direction == "up") 1 else -1
}
.s2_implementations <- c(.s2_implementations, list(
  health_hres_sigma_bound = function(x, context) {
    estimate <- .s2_number(x$representative_sigma, "representative_sigma", 0)
    bounds <- .s2_health_bounds(x$segment, x$risk, context)
    list(value = min(bounds[[2L]], max(bounds[[1L]], estimate)), details = list(lower_bound = bounds[[1L]],
      standard_sigma = bounds[[2L]], representative_sigma = estimate, parameter_determination = "NOT_ESTABLISHED",
      scope = "arithmetic_only_not_an_official_hres_parameter"))
  },
  health_hres_mixed_sigma = function(x, context) {
    bounds <- .s2_health_bounds(x$segment, x$risk, context)
    lower <- bounds[[1L]]; standard <- bounds[[2L]]
    outside <- .s2_number(x$non_hres_volume, "non_hres_volume", 0)
    inside <- .s2_number(x$hres_volume, "hres_volume", 0)
    supplied <- .s2_number(x$hres_sigma, "hres_sigma", 0)
    .s2_reference(x$parameter_reference, "parameter_reference")
    qualified <- x$qualification
    if (!is.null(qualified)) {
      .s2_keys(qualified, c("conditions", "assessment_reference", "implementing_act_reference", "methodology_publication_reference"), "qualification")
      for (key in c("assessment_reference", "implementing_act_reference", "methodology_publication_reference"))
        .s2_reference(qualified[[key]], paste0("qualification.", key))
      required <- c("segregated_without_transfer", "separate_segment_and_risk_parameters", "appropriate_methods",
        "complete_accurate_appropriate_data", "current_credible_information", "residual_and_common_risks_considered",
        "methodology_and_calculation_public", "implementing_act_applicable", "parameter_matches_act", "article147_separate_volumes")
      .s2_keys(qualified$conditions, required, "qualification.conditions")
      for (key in required) qualified$conditions[[key]] <- .s2_known_flag(qualified$conditions[[key]], paste0("qualification.", key))
      .s2_require(all(unlist(qualified$conditions)), "qualification.conditions", "declared Article149 prerequisites not met", "REVIEW_REQUIRED")
      .s2_require(lower <= supplied && supplied <= standard, "hres_sigma", "declared implementing-act parameter contradicts Article149 arithmetic bounds", "REVIEW_REQUIRED")
      qualified <- c(qualified, list(official_act_authenticated = FALSE, evidence_content_verified = FALSE,
        supervisory_approval_granted = FALSE, usp_substitution_included = FALSE))
    }
    scale <- max(outside, inside)
    .s2_require(scale > 0, "volumes", "zero denominator has no defined mixed sigma", "REVIEW_REQUIRED")
    outside_share <- (outside / scale) / (outside / scale + inside / scale)
    inside_share <- (inside / scale) / (outside / scale + inside / scale)
    list(value = standard * outside_share + supplied * inside_share, details = list(standard_sigma = standard,
      hres_sigma = supplied, hres_share = inside_share, non_hres_share = outside_share,
      parameter_reference = x$parameter_reference, parameter_reference_status = "EXTERNAL_UNVERIFIED",
      qualification = qualified, qualification_status = if (!is.null(qualified)) "DECLARED_PREREQUISITES_CHECKED" else "NOT_CHECKED",
      parameter_bounds_status = if (lower <= supplied && supplied <= standard) "WITHIN_ARITHMETIC_BOUNDS" else "REVIEW_REQUIRED",
      lower_bound = lower, scope = "arithmetic_only_not_hres_applicability_or_complete_scr",
      `_sources` = if (is.null(qualified)) list() else list("DR:article-149:1", "DR:article-149:2:a-h",
        if (x$risk == "premium") "DR:article-149:3" else "DR:article-149:4")))
  },
  health_biometric_stress = function(x, context) {
    .s2_require(.s2_string(x$shock) && x$shock %in% c("mortality", "longevity"), "shock", "mortality or longevity required")
    .s2_require(.s2_string(x$rate_basis) && x$rate_basis %in% c("probability", "intensity"), "rate_basis", "probability or intensity required")
    base <- .s2_number(x$rate, "rate", 0, if (x$rate_basis == "probability") 1 else NULL)
    selected <- .s2_known_flag(x$selected, "selected")
    .s2_reference(x$selection_reference, "selection_reference")
    reduction <- x$shock == "longevity"
    key <- paste0("health_", x$shock)
    change <- .s2_number(.s2_scalar(key, context), key, 0, if (reduction) 1 else NULL)
    relative <- if (reduction) -change else change
    stressed <- .s2_number(if (selected) base * (1 + relative) else base, "stressed_rate", 0)
    .s2_require(x$rate_basis != "probability" || stressed <= 1, "stressed_rate", "stress exceeds probability range; no cap specified", "REVIEW_REQUIRED")
    list(value = stressed, details = list(base_rate = base, shock = x$shock, rate_basis = x$rate_basis,
      relative_change = relative, selected = selected, selection_reference = x$selection_reference,
      selection_status = "EXTERNAL_UNVERIFIED", scope = "health_biometric_assumption_only_not_scr"))
  },
  health_income_incidence_stress = function(x, context) {
    keys <- c(first_twelve_months = "health_income_first_year", later_years = "health_income_later_years")
    .s2_require(.s2_string(x$period) && x$period %in% names(keys), "period", "explicit supported period required")
    .s2_require(.s2_string(x$rate_basis) && x$rate_basis %in% c("probability", "intensity"), "rate_basis", "probability or intensity required")
    base <- .s2_number(x$rate, "rate", 0, if (x$rate_basis == "probability") 1 else NULL)
    key <- keys[[x$period]]
    change <- .s2_number(.s2_scalar(key, context), key, 0)
    stressed <- .s2_number(base * (1 + change), "stressed_rate", 0)
    .s2_require(x$rate_basis != "probability" || stressed <= 1, "stressed_rate", "stress exceeds probability range; no cap specified", "REVIEW_REQUIRED")
    list(value = stressed, details = list(period = x$period, rate_basis = x$rate_basis, base_rate = base,
      relative_change = change, parameter_id = paste0("scalars/", key), joint_projection_status = "EXTERNAL_INPUT_ONLY",
      scope = "income_incidence_assumption_only_not_scr"))
  },
  health_income_transition_stress = function(x, context) {
    .s2_require(.s2_string(x$transition) && x$transition %in% c("recovery", "continuation"), "transition", "recovery or continuation required")
    base <- .s2_number(x$rate, "rate", 0, 1)
    threshold <- .s2_number(.s2_scalar("health_income_transition_threshold", context), "health_income_transition_threshold", 0, 1)
    recovery <- x$transition == "recovery"
    key <- if (recovery) "health_income_recovery" else "health_income_continuation"
    change <- .s2_number(.s2_scalar(key, context), key, 0, if (recovery) 1 else NULL)
    applies <- if (recovery) base < threshold else base <= threshold
    relative <- if (recovery) -change else change
    stressed <- if (applies) base * (1 + relative) else base
    .s2_require(stressed <= 1, "stressed_rate", "stress exceeds probability range; no cap specified", "REVIEW_REQUIRED")
    list(value = stressed, details = list(transition = x$transition, base_rate = base, threshold = threshold,
      comparison = if (recovery) "<" else "<=", stress_applied = applies, relative_change = relative,
      transition_model_status = "EXTERNAL_INPUT_ONLY", scope = "income_transition_assumption_only_not_scr"))
  },
  health_expense_level_stress = function(x, context) {
    base <- .s2_number(x$expense, "expense", 0)
    change <- .s2_number(.s2_scalar("health_expense", context), "health_expense", 0)
    list(value = base * (1 + change), details = list(base_expense = base, relative_change = change,
      required_joint_stress = "health_expense_inflation_stress", scope = "health_expense_assumption_only_not_scr"))
  },
  health_expense_inflation_stress = function(x, context) {
    base <- .s2_number(x$inflation_rate, "inflation_rate")
    change <- .s2_number(.s2_scalar("health_expense_inflation", context), "health_expense_inflation", 0)
    list(value = base + change, details = list(base_inflation = base, absolute_change = change,
      required_joint_stress = "health_expense_level_stress", scope = "health_expense_inflation_assumption_only_not_scr"))
  },
  health_revision_stress = function(x, context) {
    base <- .s2_number(x$annuity_benefit, "annuity_benefit", 0)
    exposed <- .s2_known_flag(x$revision_exposed, "revision_exposed")
    .s2_reference(x$qualification_reference, "qualification_reference")
    change <- .s2_number(.s2_scalar("health_revision", context), "health_revision", 0)
    list(value = if (exposed) base * (1 + change) else base, details = list(base_annuity_benefit = base,
      revision_exposed = exposed, relative_change = change, qualification_reference = x$qualification_reference,
      qualification_status = "EXTERNAL_UNVERIFIED", scope = "health_revision_assumption_only_not_scr"))
  },
  health_medical_payment_stress = function(x, context) {
    sign <- .s2_direction(x$direction)
    base <- .s2_number(x$payment, "payment", 0)
    change <- .s2_number(.s2_scalar("health_medical_payment_change", context), "health_medical_payment_change", 0, 1)
    list(value = base * (1 + sign * change), details = list(base_payment = base, relative_change = sign * change,
      required_joint_stress = "health_medical_inflation_stress", direction = x$direction,
      scope = "slt_medical_payment_assumption_only_not_scr"))
  },
  health_medical_inflation_stress = function(x, context) {
    sign <- .s2_direction(x$direction)
    base <- .s2_number(x$inflation_rate, "inflation_rate")
    change <- .s2_number(.s2_scalar("health_medical_inflation_change", context), "health_medical_inflation_change", 0)
    list(value = base + sign * change, details = list(base_inflation = base, absolute_change = sign * change,
      required_joint_stress = "health_medical_payment_stress", direction = x$direction,
      scope = "slt_medical_inflation_assumption_only_not_scr"))
  },
  health_medical_risk = function(x, context) {
    up <- .s2_number(x$up_charge, "up_charge", 0); down <- .s2_number(x$down_charge, "down_charge", 0)
    .s2_reference(x$valuation_reference, "valuation_reference")
    list(value = max(up, down), details = list(up_charge = up, down_charge = down,
      valuation_reference = x$valuation_reference, valuation_status = "EXTERNAL_UNVERIFIED", scope = "supplied_slt_medical_scenarios_only"))
  },
  health_disability_risk = function(x, context) {
    medical <- .s2_number(x$medical_charge, "medical_charge", 0)
    income <- .s2_number(x$income_protection_charge, "income_protection_charge", 0)
    .s2_reference(x$classification_reference, "classification_reference")
    list(value = sum(c(medical, income)), details = list(medical_charge = medical, income_protection_charge = income,
      classification_reference = x$classification_reference, classification_status = "EXTERNAL_UNVERIFIED",
      scope = "supplied_slt_disability_components_only"))
  }
))
