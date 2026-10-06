# Native market and life assumptions. Stresses are not BOF valuation models.
.s2_reference <- function(value, field) {
  .s2_require(.s2_string(value) && nzchar(trimws(value)), field,
              "external qualification reference required", "MISSING_INPUT")
  value
}
.s2_known_flag <- function(value, field) {
  .s2_require(!is.null(value), field, "unresolved declaration", "REVIEW_REQUIRED")
  .s2_require(is.logical(value) && length(value) == 1L && !is.na(value), field,
              "explicit boolean required")
  value
}
.s2_select_scenario <- function(gross_losses, net_losses, tie_break = NULL) {
  .s2_require(.s2_mapping(gross_losses) && length(gross_losses) > 0L,
              "gross_losses", "nonempty mapping required")
  .s2_keys(net_losses, names(gross_losses), "net_losses")
  gross <- lapply(names(gross_losses), function(k) .s2_number(gross_losses[[k]], k, 0))
  names(gross) <- names(gross_losses)
  net <- lapply(names(net_losses), function(k) .s2_number(net_losses[[k]], k, 0))
  names(net) <- names(net_losses)
  maximum <- max(unlist(net))
  winners <- sort(names(net)[unlist(net) == maximum], method = "radix")
  if (!is.null(tie_break)) {
    .s2_require(.s2_string(tie_break) && tie_break %in% winners,
                "tie_break", "selection must maximize net loss")
    selected <- tie_break
  } else {
    .s2_require(length(unique(unlist(gross[winners]))) == 1L,
                "tie_break", "ambiguous gross charges at net tie", "AMBIGUOUS_SELECTION")
    selected <- winners[[1L]]
  }
  list(value = gross[[selected]], details = list(selected_scenario = selected,
    selected_net_loss = net[[selected]], tied_scenarios = as.list(winners),
    gross_losses = gross, net_losses = net))
}
.s2_gross_assumptions <- function(checks) {
  components <- c("risk_margin", "deferred_tax_assets", "deferred_tax_liabilities", "future_discretionary_benefits")
  required <- c("post_scenario_management_article23_qualified", "adverse_policyholder_option_effects_included",
                "qualifying_risk_mitigation_effects_included")
  .s2_keys(checks, c(components, "management_actions_during_scenario", required, "valuation_reference"), "assumption_checks")
  .s2_reference(checks$valuation_reference, "assumption_checks.valuation_reference")
  observed <- .s2_object()
  for (field in components) {
    row <- checks[[field]]
    .s2_keys(row, c("base", "stressed"), paste0("assumption_checks.", field))
    base <- .s2_number(row$base, paste0(field, ".base"))
    stressed <- .s2_number(row$stressed, paste0(field, ".stressed"))
    .s2_require(base == stressed, field, "gross scenario component must remain constant", "REVIEW_REQUIRED")
    observed[[field]] <- list(base = base, stressed = stressed, unchanged = TRUE)
  }
  during <- .s2_known_flag(checks$management_actions_during_scenario, "management_actions_during_scenario")
  .s2_require(!during, "management_actions_during_scenario", "no management actions during stress", "REVIEW_REQUIRED")
  declared <- list(management_actions_during_scenario = during)
  for (field in required) {
    declared[[field]] <- .s2_known_flag(checks[[field]], field)
    .s2_require(declared[[field]], field, "gross scenario qualification not established", "REVIEW_REQUIRED")
  }
  list(component_constancy = observed, declared_controls = declared,
    valuation_reference = checks$valuation_reference, qualification_status = "EXTERNAL_UNVERIFIED",
    bof_reconciliation_verified = FALSE, simplification_eligibility_verified = FALSE,
    scope = "gross_article83_only_not_net_fdb_or_lac_tp")
}
.s2_currency <- function(value, field) {
  .s2_require(.s2_string(value) && grepl("^[A-Z]{3}$", value), field, "three uppercase ASCII currency letters required")
  value
}
.s2_lapse_rate <- function(rate, direction, relative, cap) {
  rate <- .s2_number(rate, "rate", 0, 1)
  .s2_require(.s2_string(direction) && direction %in% c("up", "down"), "direction", "up or down required")
  relative <- .s2_number(relative, "lapse_relative", 0, 1)
  if (direction == "up") return(min(1, rate * (1 + relative)))
  cap <- .s2_number(cap, "lapse_down_cap", 0, 1)
  rate - min(rate * relative, cap)
}

.s2_implementations <- c(.s2_implementations, list(
  select_scenario = function(x, context) .s2_select_scenario(x$gross_losses, x$net_losses, x$tie_break),
  scenario_loss = function(x, context) {
    checks <- if (!is.null(x$assumption_checks)) .s2_gross_assumptions(x$assumption_checks) else NULL
    loss <- .s2_number(x$base_bof, "base_bof") - .s2_number(x$stressed_bof, "stressed_bof")
    list(value = max(loss, 0), details = list(unfloored_loss = loss,
      valuation_support = "EXTERNAL_INPUT_ONLY", gross_assumption_checks = checks,
      `_sources` = if (is.null(checks)) list() else list("DR:article-83:1-2", "DR:article-83:4")))
  },
  property_value_stress = function(x, context) {
    .s2_reference(x$valuation_reference, "valuation_reference")
    value <- .s2_number(x$property_value, "property_value", 0)
    shock <- .s2_number(.s2_scalar("property", context), "property_shock", 0, 1)
    list(value = value * (1 - shock), details = list(base_value = value, relative_shock = shock,
      valuation_reference = x$valuation_reference, valuation_status = "EXTERNAL_UNVERIFIED",
      scope = "property_asset_value_only_not_capital"))
  },
  credit_spread_stress = function(x, context) {
    .s2_require(.s2_string(x$direction) && x$direction %in% c("widening", "tightening"), "direction", "widening or tightening required")
    .s2_require(.s2_string(x$category) && x$category %in% c("rated", "unrated"), "category", "rated or unrated required", "UNSUPPORTED_METHOD")
    .s2_reference(x$qualification_reference, "qualification_reference")
    base <- .s2_number(x$spread, "spread")
    if (x$category == "rated") {
      rows <- .s2_effective("profile.json", context)$tables$credit_spread_widening$rows
      keys <- vapply(rows, function(row) row[[1L]], character(1))
      .s2_require(.s2_string(x$credit_quality_step) && x$credit_quality_step %in% keys, "credit_quality_step", "explicit CQS0 through6 required")
      widening <- .s2_number(rows[[match(x$credit_quality_step, keys)]][[2L]], "widening", 0)
    } else {
      .s2_require(is.null(x$credit_quality_step), "credit_quality_step", "omit unused CQS for unrated")
      widening <- .s2_number(.s2_scalar("credit_spread_unrated_widening", context), "widening", 0)
    }
    tightening <- .s2_number(.s2_scalar("credit_spread_tightening", context), "tightening", 0, 1)
    list(value = if (x$direction == "widening") base + widening else base * (1 - tightening),
      details = list(base_spread = base, direction = x$direction, category = x$category,
        credit_quality_step = x$credit_quality_step, absolute_widening = widening, relative_tightening = tightening,
        qualification_reference = x$qualification_reference, qualification_status = "EXTERNAL_UNVERIFIED",
        scope = "underlying_spread_scenario_only_not_derivative_valuation"))
  },
  interest_stress = function(x, context) {
    rate <- .s2_number(x$rate, "rate", -1)
    .s2_require(rate > -1, "rate", "annual discount rate must exceed -1")
    maturity <- .s2_number(x$maturity, "maturity", 0)
    .s2_require(.s2_string(x$direction) && x$direction %in% c("up", "down"), "direction", "up or down required")
    rows <- .s2_effective("profile.json", context)$tables$interest$rows
    column <- if (x$direction == "up") 2L else 3L
    factor <- rows[[length(rows)]][[column]]
    if (maturity <= rows[[1L]][[1L]]) factor <- rows[[1L]][[column]] else {
      for (i in seq_len(length(rows) - 1L)) {
        lo <- rows[[i]]; hi <- rows[[i + 1L]]
        if (lo[[1L]] <= maturity && maturity <= hi[[1L]]) {
          weight <- (maturity - lo[[1L]]) / (hi[[1L]] - lo[[1L]])
          factor <- lo[[column]] + weight * (hi[[column]] - lo[[column]])
          break
        }
      }
    }
    value <- if (x$direction == "up") rate + max(rate * factor, .s2_scalar("interest_up_floor", context)) else if (rate < 0) rate else rate * (1 - factor)
    list(value = value, details = list(relative_factor = factor, absolute_change = value - rate, scope = "basic_curve_shock_only"))
  },
  symmetric_adjustment = function(x, context) {
    current <- .s2_number(x$current_index, "current_index", 0)
    average <- .s2_number(x$average_index, "average_index", 0)
    .s2_require(current > 0 && average > 0, "indices", "positive index levels required")
    raw <- .s2_scalar("sa_scale", context) * ((current - average) / average - .s2_scalar("sa_offset", context))
    limit <- .s2_number(.s2_scalar("sa_limit", context), "sa_limit", 0)
    months <- .s2_number(.s2_scalar("sa_window_months", context), "sa_window_months", 1)
    .s2_require(months == trunc(months), "sa_window_months", "whole months required")
    list(value = max(-limit, min(limit, raw)), details = list(uncapped_adjustment = raw,
      adjustment_limit = limit, required_average_window_months = months,
      average_qualification = "EXTERNAL_INPUT_ONLY", average_method = "equal_weight_daily_index_excluding_days_without_index"))
  },
  lapse_stress = function(x, context) {
    list(value = .s2_lapse_rate(x$rate, x$direction, .s2_scalar("lapse_relative", context), .s2_scalar("lapse_down_cap", context)),
      details = list(direction = x$direction, scope = "selected_contract_rate_only"))
  },
  currency_participation_bof_change = function(x, context) {
    .s2_reference(x$qualification_reference, "qualification_reference")
    .s2_require(.s2_mapping(x$participations) || identical(x$participations, list()), "participations", "explicit participation mapping required", "MISSING_INPUT")
    .s2_require(all(nzchar(trimws(names(x$participations)))), "participations", "nonempty string participation IDs required")
    amounts <- excluded <- .s2_object()
    for (id in names(x$participations)) {
      row <- x$participations[[id]]
      .s2_keys(row, c("nondeducted_bof_change", "deducted_bof_change"), id)
      nondeducted <- .s2_number(row$nondeducted_bof_change, paste0(id, ".nondeducted_bof_change"))
      deducted <- .s2_number(row$deducted_bof_change, paste0(id, ".deducted_bof_change"))
      amounts[[id]] <- .s2_number(sum(c(nondeducted, max(deducted, 0))), paste0(id, ".included_change"))
      excluded[[id]] <- min(deducted, 0)
    }
    list(value = sum(unlist(amounts)), details = list(included_bof_changes = amounts,
      excluded_deducted_losses = excluded, empty_population = !length(x$participations),
      qualification_reference = x$qualification_reference, qualification_status = "EXTERNAL_UNVERIFIED",
      scope = "signed_participation_bof_delta_only_not_own_funds_deduction_or_currency_scr"))
  },
  currency_scenario_charge = function(x, context) {
    .s2_reference(x$valuation_reference, "valuation_reference")
    .s2_keys(x$gross_losses, c("up", "down"), "gross_losses")
    .s2_keys(x$net_losses, c("up", "down"), "net_losses")
    result <- .s2_select_scenario(x$gross_losses, x$net_losses, x$tie_break)
    result$details <- c(result$details, list(valuation_reference = x$valuation_reference,
      valuation_status = "EXTERNAL_UNVERIFIED", scope = "single_currency_selected_gross_charge_not_valuation_or_total_currency_risk"))
    result
  },
  currency_peg_factor = function(x, context) {
    .s2_currency(x$foreign_currency, "foreign_currency")
    .s2_require(x$foreign_currency != context$currency, "foreign_currency", "distinct local and foreign currencies required")
    .s2_reference(x$qualification_reference, "qualification_reference")
    table <- .s2_effective("profile.json", context)$tables$currency_peg_factors
    rows <- Filter(function(row) setequal(unlist(row[1:2]), c(context$currency, x$foreign_currency)), table$rows)
    .s2_require(length(rows) == 1L, "currency_pair", "no unique supported original pair; no standard-factor fallback", "UNSUPPORTED_PARAMETER")
    list(value = .s2_number(rows[[1L]][[3L]], "currency_peg_factor", 0, 1), details = list(
      local_currency = context$currency, foreign_currency = x$foreign_currency, original_pair = rows[[1L]][1:2],
      source_version = table$version, qualification_reference = x$qualification_reference,
      qualification_status = "EXTERNAL_UNVERIFIED", scope = "document_pair_factor_only_not_current_peg_or_currency_status_confirmation",
      `_sources` = list(table$source)))
  },
  currency_peg_rate_stress = function(x, context) {
    .s2_require(.s2_string(x$direction) && x$direction %in% c("up", "down"), "direction", "explicit up/down direction required", "UNSUPPORTED_METHOD")
    .s2_reference(x$valuation_reference, "valuation_reference")
    rate <- .s2_number(x$exchange_rate, "exchange_rate", 0)
    .s2_require(rate > 0, "exchange_rate", "positive local-per-foreign quote required")
    factor <- currency_peg_factor(x$foreign_currency, qualification_reference = x$qualification_reference, context = context)
    list(value = rate * (if (x$direction == "up") 1 + factor$value else 1 - factor$value),
      details = list(exchange_rate = rate, direction = x$direction, shock = factor$value,
        quote_convention = "local_currency_per_one_foreign_unit", valuation_reference = x$valuation_reference,
        valuation_status = "EXTERNAL_UNVERIFIED", factor_details = factor$details,
        scope = "qualified_original_peg_rate_only_not_BOF_valuation", `_sources` = factor$sources))
  },
  currency_rate_stress = function(x, context) {
    .s2_require(.s2_string(x$direction) && x$direction %in% c("up", "down"), "direction", "explicit up/down direction required", "UNSUPPORTED_METHOD")
    .s2_reference(x$valuation_reference, "valuation_reference")
    rate <- .s2_number(x$exchange_rate, "exchange_rate", 0)
    .s2_require(rate > 0, "exchange_rate", "positive local-per-foreign quote required")
    shock <- .s2_number(.s2_scalar("currency_standard", context), "currency_standard", 0, 1)
    list(value = rate * (if (x$direction == "up") 1 + shock else 1 - shock),
      details = list(exchange_rate = rate, direction = x$direction, shock = shock,
        quote_convention = "local_currency_per_one_foreign_unit", valuation_reference = x$valuation_reference,
        valuation_status = "EXTERNAL_UNVERIFIED", scope = "standard_currency_rate_only_no_peg_relief_or_BOF_valuation"))
  },
  currency_risk = function(x, context) {
    .s2_reference(x$classification_reference, "classification_reference")
    .s2_require(.s2_mapping(x$currency_charges) && length(x$currency_charges) > 0L,
                "currency_charges", "nonempty labelled foreign-currency charges required", "MISSING_INPUT")
    charges <- .s2_object()
    for (currency in names(x$currency_charges)) {
      .s2_currency(currency, "currency")
      .s2_require(currency != context$currency, "currency", "local accounting currency is not a foreign currency")
      charges[[currency]] <- .s2_number(x$currency_charges[[currency]], paste0("currency_charges.", currency), 0)
    }
    list(value = sum(unlist(charges)), details = list(currency_charges = charges, local_currency = context$currency,
      classification_reference = x$classification_reference, classification_status = "EXTERNAL_UNVERIFIED",
      scope = "sum_of_preselected_gross_BOF_charges_no_currency_conversion"))
  },
  life_catastrophe_rate_stress = function(x, context) {
    base <- .s2_number(x$rate, "rate", 0, 1)
    selected <- .s2_known_flag(x$selected, "selected")
    .s2_reference(x$selection_reference, "selection_reference")
    change <- .s2_number(.s2_scalar("life_cat", context), "life_cat", 0)
    stressed <- if (selected) base + change else base
    .s2_require(stressed <= 1, "stressed_rate", "stress exceeds probability range; no cap specified", "REVIEW_REQUIRED")
    list(value = stressed, details = list(base_rate = base, absolute_change = change, selected = selected,
      selection_reference = x$selection_reference, selection_status = "EXTERNAL_UNVERIFIED",
      period = "NEXT_TWELVE_MONTHS_ONLY", scope = "mortality_assumption_only_not_scr"))
  },
  life_biometric_stress = function(x, context) {
    parameters <- c(mortality = 1, longevity = -1, disability_first_year = 1, disability_later_years = 1, disability_recovery = -1)
    .s2_require(.s2_string(x$shock) && x$shock %in% names(parameters), "shock", "supported life shock required", "UNSUPPORTED_METHOD")
    .s2_require(.s2_string(x$rate_basis) && x$rate_basis %in% c("probability", "intensity"), "rate_basis", "explicit rate basis required")
    base <- .s2_number(x$rate, "rate", 0, if (x$rate_basis == "probability") 1 else NULL)
    direction <- parameters[[x$shock]]
    change <- .s2_number(.s2_scalar(x$shock, context), x$shock, 0, if (direction < 0) 1 else NULL)
    stressed <- .s2_number(base * (1 + direction * change), "stressed_rate", 0)
    .s2_require(x$rate_basis != "probability" || stressed <= 1, "stressed_rate", "stress exceeds probability range; no probability cap specified here", "REVIEW_REQUIRED")
    list(value = stressed, details = list(base_rate = base, shock = x$shock, rate_basis = x$rate_basis,
      relative_change = direction * change, parameter_id = paste0("scalars/", x$shock),
      selection_and_period_status = "EXTERNAL_INPUT_ONLY", scope = "life_rate_assumption_only_not_scr"))
  },
  life_expense_level_stress = function(x, context) {
    base <- .s2_number(x$expense, "expense", 0)
    change <- .s2_number(.s2_scalar("life_expense", context), "life_expense", 0)
    list(value = base * (1 + change), details = list(base_expense = base, relative_change = change,
      required_joint_stress = "life_expense_inflation_stress", scope = "expense_level_only_not_projection_or_scr"))
  },
  life_expense_inflation_stress = function(x, context) {
    base <- .s2_number(x$inflation_rate, "inflation_rate")
    change <- .s2_number(.s2_scalar("expense_inflation", context), "expense_inflation", 0)
    list(value = base + change, details = list(base_inflation_rate = base, absolute_change = change,
      required_joint_stress = "life_expense_level_stress", scope = "expense_inflation_only_not_projection_or_scr"))
  },
  life_revision_stress = function(x, context) {
    base <- .s2_number(x$annuity_benefit, "annuity_benefit", 0)
    exposed <- .s2_known_flag(x$revision_exposed, "revision_exposed")
    change <- .s2_number(.s2_scalar("life_revision", context), "life_revision", 0)
    list(value = if (exposed) base * (1 + change) else base, details = list(base_annuity_benefit = base,
      revision_exposed = exposed, relative_change = change, selection_status = "EXTERNAL_UNVERIFIED",
      scope = "annuity_benefit_stress_only_not_scr"))
  }
))
