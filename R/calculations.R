# Native public signatures and Rd inputs; formula bodies live in *-native.R.

#' Aggregate already determined module charges using an explicit regulatory matrix.
#'
#' @param components Components. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param matrix_id Matrix id. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("default-baseline-matrix-3-4")
#' result <- do.call(aggregate_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
aggregate_risk <- function(components, matrix_id, context, ...) {
  .s2_require(length(list(...)) == 0L, "aggregate_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("aggregate_risk", list(components = components, matrix_id = matrix_id),
    context, .s2_implementations[["aggregate_risk"]])
}

#' Return eligibility for the CHOSEN route, never an internally modelled CQS.
#'
#' Route3 excludes already assigned176a(1)-CQS2 instruments, not every instrument
#' that might hypothetically qualify for2. Choice and substantive assessments
#' are explicit external inputs. Existing numeric screens remain authoritative
#' for their limited arithmetic and profile-specific analyst overrides.
#' @param issuer_financials Issuer financials. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param yield_inputs Yield inputs. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param target_cqs Target cqs. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param assessment Assessment. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("assigned-credit-eligibility-baseline-financial-fails")
#' result <- do.call(assigned_credit_eligibility_screen,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
assigned_credit_eligibility_screen <- function(issuer_financials, yield_inputs, conditions, target_cqs, qualification_reference, context, assessment = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "assigned_credit_eligibility_screen", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("assigned_credit_eligibility_screen", list(issuer_financials = issuer_financials, yield_inputs = yield_inputs, conditions = conditions, target_cqs = target_cqs, qualification_reference = qualification_reference, assessment = assessment),
    context, .s2_implementations[["assigned_credit_eligibility_screen"]])
}

#' Screen seven numeric issuer criteria on qualified EUR financial values.
#'
#' Rows are oldest to newest, consecutive completed financial years. All
#' supplied rows are validated; only the latest prescribed windows are used.
#' EUR inputs remain EUR even if Context.currency differs. Definitions,
#' chronology, latest-value alignment and currency-revenue scope are external.
#' A pass does not satisfy other Article176a conditions or assign a CQS.
#' @param financial_years Financial years. Reference type: `list`. Supply an ordered R list or the documented numeric vector.
#' @param latest_financials Latest financials. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param eligible_currency_revenue_share Eligible currency revenue share. Reference type: `float`.
#' @param years_without_credit_event Years without credit event. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("assigned-credit-baseline-one-small-year")
#' result <- do.call(assigned_credit_issuer_financial_screen,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
assigned_credit_issuer_financial_screen <- function(financial_years, latest_financials, eligible_currency_revenue_share, years_without_credit_event, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "assigned_credit_issuer_financial_screen", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("assigned_credit_issuer_financial_screen", list(financial_years = financial_years, latest_financials = latest_financials, eligible_currency_revenue_share = eligible_currency_revenue_share, years_without_credit_event = years_without_credit_event, qualification_reference = qualification_reference),
    context, .s2_implementations[["assigned_credit_issuer_financial_screen"]])
}

#' Check every qualified issue against both prescribed yield ceilings.
#'
#' Index keys are 2/4 or 3/4, not sorted by observed yield. All rates are
#' absolute decimal rates, allowing negative yields. Complete comparable own
#' issues, issue-date indices, maturity/currency and any adjustments under
#' paragraphs8/9 must be supplied and qualified externally.
#' @param yield_observations Yield observations. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param index_yields Index yields. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param current_issue_id Current issue id. Reference type: `str`.
#' @param target_cqs Target cqs. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("assigned-credit-baseline-yield-not-capped-at-one")
#' result <- do.call(assigned_credit_yield_screen,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
assigned_credit_yield_screen <- function(yield_observations, index_yields, current_issue_id, target_cqs, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "assigned_credit_yield_screen", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("assigned_credit_yield_screen", list(yield_observations = yield_observations, index_yields = index_yields, current_issue_id = current_issue_id, target_cqs = target_cqs, qualification_reference = qualification_reference),
    context, .s2_implementations[["assigned_credit_yield_screen"]])
}

#' Five-module aggregation plus the charge on recognised intangible assets.
#'
#' @param components Components. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param intangible_charge Intangible charge. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("bscr-2027-five-modules")
#' result <- do.call(basic_scr,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
basic_scr <- function(components, intangible_charge, context, ...) {
  .s2_require(length(list(...)) == 0L, "basic_scr", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("basic_scr", list(components = components, intangible_charge = intangible_charge),
    context, .s2_implementations[["basic_scr"]])
}

#' Check declared data-deficiency exception, not create an approximation.
#'
#' Inadequate internal processes cannot justify this exception. Neither an
#' invented materiality percentage nor a preferred actuarial model is supplied.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("be-approx-baseline-combination-31")
#' result <- do.call(best_estimate_approximation_conditions,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
best_estimate_approximation_conditions <- function(conditions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "best_estimate_approximation_conditions", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("best_estimate_approximation_conditions", list(conditions = conditions, qualification_reference = qualification_reference),
    context, .s2_implementations[["best_estimate_approximation_conditions"]])
}

#' Discount already probability-weighted, gross-of-reinsurance cashflows.
#'
#' No projection model, curve interpolation or compounding convention is
#' invented here. Discount factors must match the payment dates and currency.
#' Inflows include premiums, not separately valued reinsurance recoverables.
#' @param cashflows Cashflows. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param runoff_complete Runoff complete. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param cashflow_reference Cashflow reference. Reference type: `str`.
#' @param curve_reference Curve reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("be-at-valuation-date")
#' result <- do.call(best_estimate_cashflows,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
best_estimate_cashflows <- function(cashflows, runoff_complete, cashflow_reference, curve_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "best_estimate_cashflows", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("best_estimate_cashflows", list(cashflows = cashflows, runoff_complete = runoff_complete, cashflow_reference = cashflow_reference, curve_reference = curve_reference),
    context, .s2_implementations[["best_estimate_cashflows"]])
}

#' Select entire qualified obligations, never truncate by a payment date.
#'
#' Every row identifies a disjoint obligation or contract part. Its evidence
#' qualifies legal enforceability, unilateral rights, full-risk reflection,
#' profile-specific life rules, discernibility and any unbundling. Accepted
#' reinsurance is assessed independently of underlying contract boundaries.
#' These substantive qualifications remain external, not certified here.
#' 
#' Article18(5)(c) is cross-checked against the same-version English text;
#' the incomplete German source is preserved. Paid premiums, discernible
#' cover/guarantee or enforceable future premiums disapply this exception.
#' Unbundled parts are independently qualified, not inferred from cash flows.
#' @param obligations Obligations. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param runoff_complete Runoff complete. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param cashflow_reference Cashflow reference. Reference type: `str`.
#' @param curve_reference Curve reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("contract-exception-baseline-enforceable")
#' result <- do.call(best_estimate_contract_cashflows,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
best_estimate_contract_cashflows <- function(obligations, runoff_complete, cashflow_reference, curve_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "best_estimate_contract_cashflows", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("best_estimate_contract_cashflows", list(obligations = obligations, runoff_complete = runoff_complete, cashflow_reference = cashflow_reference, curve_reference = curve_reference),
    context, .s2_implementations[["best_estimate_contract_cashflows"]])
}

#' Use modified duration, floored at1; floats need externally assessed equivalent fixed duration.
#'
#' Assigned CQS eligibility and exclusions are external. The published CQS4
#' jump above20 years is retained, not interpolated away.
#' The 2027 scope explicitly excludes defaulted and forborne loans; callers
#' must qualify that scope before supplying a position to this factor API.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param category Category. Reference type: `str`.
#' @param classification_reference Classification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param assigned_eligibility Assigned eligibility. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param coinvestment Coinvestment. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("simplified-market-baseline-captive-quality")
#' result <- do.call(bond_spread_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
bond_spread_stress <- function(modified_duration, category, classification_reference, credit_quality_step = NULL, context, assigned_eligibility = NULL, coinvestment = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "bond_spread_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("bond_spread_stress", list(modified_duration = modified_duration, category = category, classification_reference = classification_reference, credit_quality_step = credit_quality_step, assigned_eligibility = assigned_eligibility, coinvestment = coinvestment),
    context, .s2_implementations[["bond_spread_stress"]])
}

#' Prescribed add-on arithmetic, not a supervisory imposition or risk model.
#'
#' SCR inputs exclude prior/concurrent add-ons. Modified valuations and any
#' permitted offsetting under283(7) are externally qualified. Article279's
#' risk-profile significance thresholds are separate from imposing an add-on.
#' No quantitative governance/additional2027 Article37(1)e formula is invented.
#' @param base_scr Base scr. Reference type: `float`.
#' @param modified_scr Modified scr. Reference type: `float`.
#' @param method Method. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param base_eligible_own_funds Base eligible own funds. Reference type: `float | None`.
#' @param modified_eligible_own_funds Modified eligible own funds. Reference type: `float | None`.
#' @param rebuttal_established Rebuttal established. Reference type: `bool | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("capital-addon-baseline-risk-zero-base")
#' result <- do.call(capital_add_on_amount,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
capital_add_on_amount <- function(base_scr, modified_scr, method, qualification_reference, context, base_eligible_own_funds = NULL, modified_eligible_own_funds = NULL, rebuttal_established = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "capital_add_on_amount", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("capital_add_on_amount", list(base_scr = base_scr, modified_scr = modified_scr, method = method, qualification_reference = qualification_reference, base_eligible_own_funds = base_eligible_own_funds, modified_eligible_own_funds = modified_eligible_own_funds, rebuttal_established = rebuttal_established),
    context, .s2_implementations[["capital_add_on_amount"]])
}

#' Select notification/plan duties using externally established breach facts.
#'
#' Future MCR plans also respond to prospective breach; historical MCR and
#' SCR plans here respond to actual breach. No grace period for notification,
#' calendar deadline, automatic extension, filing or approval is produced.
#' Optional Article136 evidence adds its independent notification trigger;
#' absence is not evidence that financial deterioration has been ruled out.
#' Combined-group minimum breaches use233a(6) only in the2027 profile;
#' SCR own funds never substitute for minimum-eligible basic own funds.
#' @param capital_requirement Capital requirement. Reference type: `str`.
#' @param actual_breach Actual breach. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param breach_risk_within_horizon Breach risk within horizon. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param deterioration Deterioration. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param consolidated_own_funds Consolidated own funds. Reference type: `float | None`.
#' @param consolidated_scr Consolidated scr. Reference type: `float | None`.
#' @param transition_measures Transition measures. Reference type: `list[str] | None`. Supply an ordered R list or the documented numeric vector.
#' @param transition_plan_active Transition plan active. Reference type: `bool | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("capital-breach-baseline-scr-11")
#' result <- do.call(capital_breach_requirements,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
capital_breach_requirements <- function(capital_requirement, actual_breach, breach_risk_within_horizon, qualification_reference, deterioration = NULL, consolidated_own_funds = NULL, consolidated_scr = NULL, transition_measures = NULL, transition_plan_active = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "capital_breach_requirements", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("capital_breach_requirements", list(capital_requirement = capital_requirement, actual_breach = actual_breach, breach_risk_within_horizon = breach_risk_within_horizon, qualification_reference = qualification_reference, deterioration = deterioration, consolidated_own_funds = consolidated_own_funds, consolidated_scr = consolidated_scr, transition_measures = transition_measures, transition_plan_active = transition_plan_active),
    context, .s2_implementations[["capital_breach_requirements"]])
}

#' Inventory the five common plan contents; do not project or approve them.
#'
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param plan_kind Plan kind. Reference type: `str`.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param scope_reference Scope reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("capital-breach-baseline-scr_recovery-empty")
#' result <- do.call(capital_recovery_plan_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
capital_recovery_plan_evidence_check <- function(evidence, plan_kind, assessment_reference, scope_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "capital_recovery_plan_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("capital_recovery_plan_evidence_check", list(evidence = evidence, plan_kind = plan_kind, assessment_reference = assessment_reference, scope_reference = scope_reference),
    context, .s2_implementations[["capital_recovery_plan_evidence_check"]])
}

#' Evaluate one currency's literal asset-minus-BE formula for a declared scenario.
#'
#' Relative stresses, rate/duration consistency and eligibility are externally
#' qualified. Signed inputs are preserved: there is no hidden down-direction
#' sign flip, floor or stress-curve generation (including blocked 2027 curves).
#' All five asset bands and all 36 Annex I LoBs must be supplied explicitly.
#' @param assets Assets. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param technical_provisions Technical provisions. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param direction Direction. Reference type: `str`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param stress_reference Stress reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("captive-interest-baseline-zero")
#' result <- do.call(captive_interest_currency,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
captive_interest_currency <- function(assets, technical_provisions, direction, eligibility_reference, valuation_reference, stress_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "captive_interest_currency", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("captive_interest_currency", list(assets = assets, technical_provisions = technical_provisions, direction = direction, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference, stress_reference = stress_reference),
    context, .s2_implementations[["captive_interest_currency"]])
}

#' Look up an explicitly qualified lettered band; do not invent boundary rules.
#'
#' @param band Band. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: years.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("captive-interest-baseline-duration-a")
#' result <- do.call(captive_interest_duration,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
captive_interest_duration <- function(band, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "captive_interest_duration", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("captive_interest_duration", list(band = band, qualification_reference = qualification_reference),
    context, .s2_implementations[["captive_interest_duration"]])
}

#' Aggregate all twelve DR90 segment charges, with explicit inactive zeros.
#'
#' Weights are separate profile parameters, not ordinary nonlife correlations.
#' A full set prevents omitted exposures from silently becoming zero.
#' @param segment_charges Segment charges. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("captive-baseline-aggregate-zero")
#' result <- do.call(captive_nonlife_aggregation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
captive_nonlife_aggregation <- function(segment_charges, eligibility_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "captive_nonlife_aggregation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("captive_nonlife_aggregation", list(segment_charges = segment_charges, eligibility_reference = eligibility_reference),
    context, .s2_implementations[["captive_nonlife_aggregation"]])
}

#' Calculate a segment charge from already qualified Article116 volumes.
#'
#' Normalising before squaring avoids overflow for representable charges;
#' neither reinsurance adjustments nor geographic diversification are repeated.
#' @param premium_volume Premium volume. Reference type: `float`.
#' @param reserve_volume Reserve volume. Reference type: `float`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param volume_reference Volume reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("captive-baseline-zero")
#' result <- do.call(captive_nonlife_segment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
captive_nonlife_segment <- function(premium_volume, reserve_volume, eligibility_reference, volume_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "captive_nonlife_segment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("captive_nonlife_segment", list(premium_volume = premium_volume, reserve_volume = reserve_volume, eligibility_reference = eligibility_reference, volume_reference = volume_reference),
    context, .s2_implementations[["captive_nonlife_segment"]])
}

#' Check externally assessed DR89 facts, not substantive eligibility itself.
#'
#' Absent insurance/reinsurance business must be explicitly qualified by the
#' caller; missing evidence never means that a condition is met.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("captive-baseline-eligible")
#' result <- do.call(captive_simplification_conditions,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
captive_simplification_conditions <- function(conditions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "captive_simplification_conditions", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("captive_simplification_conditions", list(conditions = conditions, qualification_reference = qualification_reference),
    context, .s2_implementations[["captive_simplification_conditions"]])
}

#' Qualified recovery subtraction or explicit gross fallback for inadequate risk capture.
#'
#' Recoveries conditional on unrelated claims must have been excluded externally.
#' For vessel components apply separately; for other perils use the qualified
#' combined object/group sum. The returned amount is not a BOF loss or SCR.
#' @param gross_sum Gross sum. Reference type: `float`.
#' @param recoverable Recoverable. Reference type: `float`.
#' @param use_gross Use gross. Reference type: `bool | None`.
#' @param peril Peril. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("concentration-2027-fire-gross")
#' result <- do.call(catastrophe_effective_sum,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
catastrophe_effective_sum <- function(gross_sum, recoverable, use_gross, peril, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "catastrophe_effective_sum", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("catastrophe_effective_sum", list(gross_sum = gross_sum, recoverable = recoverable, use_gross = use_gross, peril = peril, qualification_reference = qualification_reference),
    context, .s2_implementations[["catastrophe_effective_sum"]])
}

#' Q*max(group zone weights)*qualified group SI; never invent a group matrix.
#'
#' The named region fixes the common regional scope. Source-blocked weights
#' remain blocked through the ordinary selector. A group amount cannot be
#' repeated into every original matrix zone: downstream group treatment must
#' be qualified separately. Future90b(6) requires120(1a) evidence for wind/soil.
#' @param peril Peril. Reference type: `str`.
#' @param region Region. Reference type: `str`.
#' @param zone_ordinals Zone ordinals. Reference type: `list[int]`. Supply an ordered R list or the documented numeric vector.
#' @param insured_sum Insured sum. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param exposure_basis_reference Exposure basis reference. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2026-10-05",
#'   known_at = "2026-10-06",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("simpl-close-baseline-group-single")
#' result <- do.call(catastrophe_group_weighted_sum,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
catastrophe_group_weighted_sum <- function(peril, region, zone_ordinals, insured_sum, qualification_reference, exposure_basis_reference = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "catastrophe_group_weighted_sum", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("catastrophe_group_weighted_sum", list(peril = peril, region = region, zone_ordinals = zone_ordinals, insured_sum = insured_sum, qualification_reference = qualification_reference, exposure_basis_reference = exposure_basis_reference),
    context, .s2_implementations[["catastrophe_group_weighted_sum"]])
}

#' Aggregate externally determined regional catastrophe charges plus other-region charge.
#'
#' @param peril Peril. Reference type: `str`.
#' @param region_charges Region charges. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param other_charge Other charge. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-cat-region-subsidence-zero")
#' result <- do.call(catastrophe_region_aggregation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
catastrophe_region_aggregation <- function(peril, region_charges, other_charge, context, ...) {
  .s2_require(length(list(...)) == 0L, "catastrophe_region_aggregation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("catastrophe_region_aggregation", list(peril = peril, region_charges = region_charges, other_charge = other_charge),
    context, .s2_implementations[["catastrophe_region_aggregation"]])
}

#' Q*W*SI for an externally qualified zone ordinal, not an address-to-zone mapping.
#'
#' Future subsidence uses Annex VIIIA Q. Unqualified future geographic links
#' stay explicitly absent instead of inheriting historical labels.
#' @param peril Peril. Reference type: `str`.
#' @param region Region. Reference type: `str`.
#' @param zone_ordinal Zone ordinal. Reference type: `int`.
#' @param insured_sum Insured sum. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-cat-weight-zero-si")
#' result <- do.call(catastrophe_weighted_sum,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
catastrophe_weighted_sum <- function(peril, region, zone_ordinal, insured_sum, context, ...) {
  .s2_require(length(list(...)) == 0L, "catastrophe_weighted_sum", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("catastrophe_weighted_sum", list(peril = peril, region = region, zone_ordinal = zone_ordinal, insured_sum = insured_sum),
    context, .s2_implementations[["catastrophe_weighted_sum"]])
}

#' sqrt(WSI' C WSI) for one Annex XXII–XXVI peril/region; WSI supplied externally.
#'
#' @param peril Peril. Reference type: `str`.
#' @param region Region. Reference type: `str`.
#' @param weighted_sums Weighted sums. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-cat-zone-earthquake-GU-one")
#' result <- do.call(catastrophe_zone_aggregation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
catastrophe_zone_aggregation <- function(peril, region, weighted_sums, context, ...) {
  .s2_require(length(list(...)) == 0L, "catastrophe_zone_aggregation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("catastrophe_zone_aggregation", list(peril = peril, region = region, weighted_sums = weighted_sums),
    context, .s2_implementations[["catastrophe_zone_aggregation"]])
}

#' Resolve an explicitly supplied published code to its zone ordinal.
#'
#' Callers must first apply the region's documented postcode/province rule.
#' This function never guesses a prefix from a full address or repairs source
#' conflicts; the latter produce REVIEW_REQUIRED for the affected code only.
#' @param region Region. Reference type: `str`.
#' @param geographic_code Geographic code. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: zone_ordinal.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-cat-geography-BE-1")
#' result <- do.call(catastrophe_zone_ordinal,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
catastrophe_zone_ordinal <- function(region, geographic_code, context, ...) {
  .s2_require(length(list(...)) == 0L, "catastrophe_zone_ordinal", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("catastrophe_zone_ordinal", list(region = region, geographic_code = geographic_code),
    context, .s2_implementations[["catastrophe_zone_ordinal"]])
}

#' Check strict issuer size/revenue and inclusive retention for EACH security.
#'
#' Model approvals, contract requirements, CQS/PD mapping, consolidated issuer
#' scope, fiscal-year selection and EUR conversion remain external. There is
#' no averaging of retention across securities and no internal credit model.
#' @param revenue_currency_share Revenue currency share. Reference type: `float`.
#' @param financial_years Financial years. Reference type: `documented input`.
#' @param retentions Retentions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("coinvestment-baseline-employee-route")
#' result <- do.call(coinvestment_numeric_screen,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
coinvestment_numeric_screen <- function(revenue_currency_share, financial_years, retentions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "coinvestment_numeric_screen", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("coinvestment_numeric_screen", list(revenue_currency_share = revenue_currency_share, financial_years = financial_years, retentions = retentions, qualification_reference = qualification_reference),
    context, .s2_implementations[["coinvestment_numeric_screen"]])
}

#' Ei excluding zero-gi counterparties; does not alter Assets or quality weights.
#'
#' @param exposures Exposures. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param grouping_reference Grouping reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-concentration-adjusted-zero")
#' result <- do.call(concentration_adjusted_exposure,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
concentration_adjusted_exposure <- function(exposures, grouping_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "concentration_adjusted_exposure", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("concentration_adjusted_exposure", list(exposures = exposures, grouping_reference = grouping_reference),
    context, .s2_implementations[["concentration_adjusted_exposure"]])
}

#' Immediate asset-value loss, explicitly NOT the resulting BOF capital charge.
#'
#' @param excess Excess. Reference type: `float`.
#' @param risk_factor Risk factor. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-concentration-core-5")
#' result <- do.call(concentration_asset_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
concentration_asset_loss <- function(excess, risk_factor, context, ...) {
  .s2_require(length(list(...)) == 0L, "concentration_asset_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("concentration_asset_loss", list(excess = excess, risk_factor = risk_factor),
    context, .s2_implementations[["concentration_asset_loss"]])
}

#' Select Assets from externally classified holdings; intragroup needs all five flags.
#'
#' Exclusions are not inferred from names. Intragroup category itself asserts
#' group membership externally; a failed exclusion condition retains the asset.
#' @param assets Assets. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param classification_reference Classification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-concentration-assets-all-excluded")
#' result <- do.call(concentration_assets,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
concentration_assets <- function(assets, classification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "concentration_assets", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("concentration_assets", list(assets = assets, classification_reference = classification_reference),
    context, .s2_implementations[["concentration_assets"]])
}

#' Position quality before single-name weighting/ceiling; eligibility is external.
#'
#' Here the quality itself is interpolated, unlike Article180 stress-factor
#' interpolation. MCR/SFCR routing in Article182(6-8) concerns unrated insurers.
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | float | None`.
#' @param solvency_ratio Solvency ratio. Reference type: `float | None`.
#' @param mcr_compliant Mcr compliant. Reference type: `bool | None`.
#' @param sfcr_published Sfcr published. Reference type: `bool | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: credit_quality_step.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-concentration-quality-residual")
#' result <- do.call(concentration_credit_quality,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
concentration_credit_quality <- function(category, qualification_reference, credit_quality_step = NULL, solvency_ratio = NULL, mcr_compliant = NULL, sfcr_published = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "concentration_credit_quality", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("concentration_credit_quality", list(category = category, qualification_reference = qualification_reference, credit_quality_step = credit_quality_step, solvency_ratio = solvency_ratio, mcr_compliant = mcr_compliant, sfcr_published = sfcr_published),
    context, .s2_implementations[["concentration_credit_quality"]])
}

#' Threshold excess on already scoped Assets and zero-factor-adjusted exposure.
#'
#' @param exposure Exposure. Reference type: `float`.
#' @param assets Assets. Reference type: `float`.
#' @param threshold Threshold. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-concentration-core-3")
#' result <- do.call(concentration_excess,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
concentration_excess <- function(exposure, assets, threshold, context, ...) {
  .s2_require(length(list(...)) == 0L, "concentration_excess", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("concentration_excess", list(exposure = exposure, assets = assets, threshold = threshold),
    context, .s2_implementations[["concentration_excess"]])
}

#' Sum externally grouped counterparty/single-name exposures, not all Assets.
#'
#' @param exposures Exposures. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param grouping_reference Grouping reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-concentration-core-13")
#' result <- do.call(concentration_exposure_sum,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
concentration_exposure_sum <- function(exposures, grouping_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "concentration_exposure_sum", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("concentration_exposure_sum", list(exposures = exposures, grouping_reference = grouping_reference),
    context, .s2_implementations[["concentration_exposure_sum"]])
}

#' gi selection, not grouping or CT selection; regional CQS2 reference affects gi only.
#'
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | float | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-concentration_factor-property-none")
#' result <- do.call(concentration_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
concentration_factor <- function(category, qualification_reference, credit_quality_step = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "concentration_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("concentration_factor", list(category = category, qualification_reference = qualification_reference, credit_quality_step = credit_quality_step),
    context, .s2_implementations[["concentration_factor"]])
}

#' Euclidean norm of already valued single-name BOF charges; not asset losses.
#'
#' @param single_name_charges Single name charges. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-concentration-core-7")
#' result <- do.call(concentration_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
concentration_risk <- function(single_name_charges, context, ...) {
  .s2_require(length(list(...)) == 0L, "concentration_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("concentration_risk", list(single_name_charges = single_name_charges),
    context, .s2_implementations[["concentration_risk"]])
}

#' Ceil value-weighted quality only after aggregation, without epsilon rounding.
#'
#' Exact arithmetic on validated binary inputs protects integer boundaries and
#' avoids overflow in weighted products. Input qualities may be fractional.
#' @param exposures Exposures. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param grouping_reference Grouping reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: credit_quality_step.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-concentration-core-9")
#' result <- do.call(concentration_single_name_quality,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
concentration_single_name_quality <- function(exposures, grouping_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "concentration_single_name_quality", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("concentration_single_name_quality", list(exposures = exposures, grouping_reference = grouping_reference),
    context, .s2_implementations[["concentration_single_name_quality"]])
}

#' CT on externally established single-name quality or qualified special exposure.
#'
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | float | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-concentration_threshold-property-none")
#' result <- do.call(concentration_threshold,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
concentration_threshold <- function(category, qualification_reference, credit_quality_step = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "concentration_threshold", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("concentration_threshold", list(category = category, qualification_reference = qualification_reference, credit_quality_step = credit_quality_step),
    context, .s2_implementations[["concentration_threshold"]])
}

#' Only the Article 18(3) negative screen; zero never proves contract membership.
#'
#' 'After' concerns coverage, not payment dates. Qualification of unilateral
#' rights, repricing and legal enforceability is supplied externally.
#' @param coverage_after_qualifying_right Coverage after qualifying right. Reference type: `bool | None`.
#' @param premium_payment_enforceable Premium payment enforceable. Reference type: `bool | None`.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-boundary-exclusion-true-true")
#' result <- do.call(contract_boundary_exclusion,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
contract_boundary_exclusion <- function(coverage_after_qualifying_right, premium_payment_enforceable, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "contract_boundary_exclusion", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("contract_boundary_exclusion", list(coverage_after_qualifying_right = coverage_after_qualifying_right, premium_payment_enforceable = premium_payment_enforceable, assessment_reference = assessment_reference),
    context, .s2_implementations[["contract_boundary_exclusion"]])
}

#' Has the first recognition time arrived? This does not reverse later derecognition.
#'
#' @param contract_party_date Contract party date. Reference type: `str`.
#' @param coverage_start_date Coverage start date. Reference type: `str`.
#' @param evidence_reference Evidence reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-recognition-party-first")
#' result <- do.call(contract_recognition_due,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
contract_recognition_due <- function(contract_party_date, coverage_start_date, evidence_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "contract_recognition_due", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("contract_recognition_due", list(contract_party_date = contract_party_date, coverage_start_date = coverage_start_date, evidence_reference = evidence_reference),
    context, .s2_implementations[["contract_recognition_due"]])
}

#' Guideline2 unilateral-right screen, not full contract-boundary membership.
#'
#' Restrictions must be externally qualified effective restrictions, including
#' the Article18 economic-effect assessment. Relevant third parties exclude
#' supervisors/governance bodies. Mere reputation or competition is not a veto.
#' Full risk reflection and the profile-specific life repricing clause remain
#' separate for an amendment right, even when this screen returns1.
#' @param right_type Right type. Reference type: `str`.
#' @param facts Facts. Reference type: `Mapping[str, bool | None]`. Supply a named R list with the keys shown in the example/reference case.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("unilateral-baseline-amend-31")
#' result <- do.call(contract_unilateral_right,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
contract_unilateral_right <- function(right_type, facts, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "contract_unilateral_right", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("contract_unilateral_right", list(right_type = right_type, facts = facts, assessment_reference = assessment_reference),
    context, .s2_implementations[["contract_unilateral_right"]])
}

#' Divide externally qualified latest eligible funds by latest positive SCR.
#'
#' Values must refer to the correct counterparty, currency and unit. Their
#' recency and regulatory eligibility are not established by this division.
#' Signed funds are preserved; neither MCR compliance nor a rating is inferred.
#' @param eligible_own_funds Eligible own funds. Reference type: `float`.
#' @param scr Scr. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("counterparty-ratio-baseline-zero")
#' result <- do.call(counterparty_solvency_ratio,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
counterparty_solvency_ratio <- function(eligible_own_funds, scr, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "counterparty_solvency_ratio", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("counterparty_solvency_ratio", list(eligible_own_funds = eligible_own_funds, scr = scr, qualification_reference = qualification_reference),
    context, .s2_implementations[["counterparty_solvency_ratio"]])
}

#' Select largest NET default exposures and report their gross scenario claims.
#'
#' Each recovery is externally valued against this exposure's default claim,
#' not against its full insured sum. Ambiguous cutoff ties with differing gross
#' amounts require explicit selection, still constrained to largest net losses.
#' @param exposures Exposures. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param selected_exposure_ids Selected exposure ids. Reference type: `Sequence[str] | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("credit-cat-empty")
#' result <- do.call(credit_cat_default_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
credit_cat_default_loss <- function(exposures, qualification_reference, selected_exposure_ids = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "credit_cat_default_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("credit_cat_default_loss", list(exposures = exposures, qualification_reference = qualification_reference, selected_exposure_ids = selected_exposure_ids),
    context, .s2_implementations[["credit_cat_default_loss"]])
}

#' Sudden gross claim from next12month earned LOB9/21 premiums, not capital.
#'
#' @param gross_premium Gross premium. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("liability-credit-2027-credit-recession")
#' result <- do.call(credit_cat_recession_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
credit_cat_recession_loss <- function(gross_premium, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "credit_cat_recession_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("credit_cat_recession_loss", list(gross_premium = gross_premium, qualification_reference = qualification_reference),
    context, .s2_implementations[["credit_cat_recession_loss"]])
}

#' Select gross credit-derivative charge by maximal net-FDB scenario loss.
#'
#' Widening/tightening BOF valuations and Article179(3) hedge exclusions must
#' be qualified externally. This is neither derivative pricing nor hedge approval.
#' Article180(9) can instead use an explicitly qualified public debt underlying;
#' no partial guarantee, other underlying or missing scenarios implies zero.
#' @param gross_losses Gross losses. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param net_losses Net losses. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param tie_break Tie break. Reference type: `str | None`.
#' @param public_underlying Public underlying. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param hedge_exemption Hedge exemption. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("credit-derivative-select-baseline-equal")
#' result <- do.call(credit_derivative_scenario_charge,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
credit_derivative_scenario_charge <- function(gross_losses, net_losses, valuation_reference, tie_break = NULL, public_underlying = NULL, hedge_exemption = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "credit_derivative_scenario_charge", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("credit_derivative_scenario_charge", list(gross_losses = gross_losses, net_losses = net_losses, valuation_reference = valuation_reference, tie_break = tie_break, public_underlying = public_underlying, hedge_exemption = hedge_exemption),
    context, .s2_implementations[["credit_derivative_scenario_charge"]])
}

#' Absolute widening versus relative tightening; hedge exclusions require separate assessment.
#'
#' @param spread Spread. Reference type: `float`.
#' @param direction Direction. Reference type: `str`.
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: annual_rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-credit-spread-tighten-0")
#' result <- do.call(credit_spread_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
credit_spread_stress <- function(spread, direction, category, qualification_reference, credit_quality_step = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "credit_spread_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("credit_spread_stress", list(spread = spread, direction = direction, category = category, qualification_reference = qualification_reference, credit_quality_step = credit_quality_step),
    context, .s2_implementations[["credit_spread_stress"]])
}

#' Signed FX BOF change: deducted holdings contribute gains but not losses.
#'
#' Inputs are externally valued BOF deltas for each holding's Art68-deducted
#' and non-deducted parts, not market values or deduction amounts. Apply the
#' positive-part rule per holding before summation; do not net excluded losses.
#' @param participations Participations. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("currency-participation-baseline-empty")
#' result <- do.call(currency_participation_bof_change,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
currency_participation_bof_change <- function(participations, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "currency_participation_bof_change", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("currency_participation_bof_change", list(participations = participations, qualification_reference = qualification_reference),
    context, .s2_implementations[["currency_participation_bof_change"]])
}

#' Historical document-profile pair factor, not confirmation of a current currency peg.
#'
#' All 15 unordered pairs are explicit original values, not cross-rate-derived
#' estimates. External evidence must establish date-specific applicability.
#' @param foreign_currency Foreign currency. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("currency-peg-factor-EUR-DKK")
#' result <- do.call(currency_peg_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
currency_peg_factor <- function(foreign_currency, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "currency_peg_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("currency_peg_factor", list(foreign_currency = foreign_currency, qualification_reference = qualification_reference),
    context, .s2_implementations[["currency_peg_factor"]])
}

#' Apply a qualified original peg factor to local currency per one foreign unit.
#'
#' @param exchange_rate Exchange rate. Reference type: `float`.
#' @param foreign_currency Foreign currency. Reference type: `str`.
#' @param direction Direction. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: exchange_rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("currency-peg-rate-EUR-DKK-up")
#' result <- do.call(currency_peg_rate_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
currency_peg_rate_stress <- function(exchange_rate, foreign_currency, direction, qualification_reference, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "currency_peg_rate_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("currency_peg_rate_stress", list(exchange_rate = exchange_rate, foreign_currency = foreign_currency, direction = direction, qualification_reference = qualification_reference, valuation_reference = valuation_reference),
    context, .s2_implementations[["currency_peg_rate_stress"]])
}

#' Stress local currency per one foreign unit, not its reciprocal or a BOF amount.
#'
#' @param exchange_rate Exchange rate. Reference type: `float`.
#' @param direction Direction. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: exchange_rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("currency-rate-1-up")
#' result <- do.call(currency_rate_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
currency_rate_stress <- function(exchange_rate, direction, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "currency_rate_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("currency_rate_stress", list(exchange_rate = exchange_rate, direction = direction, valuation_reference = valuation_reference),
    context, .s2_implementations[["currency_rate_stress"]])
}

#' Sum already net-scenario-selected gross charges, all valued in Context.currency.
#'
#' @param currency_charges Currency charges. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param classification_reference Classification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("currency-sum-zero")
#' result <- do.call(currency_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
currency_risk <- function(currency_charges, classification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "currency_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("currency_risk", list(currency_charges = currency_charges, classification_reference = classification_reference),
    context, .s2_implementations[["currency_risk"]])
}

#' One currency: select gross charge by the largest externally valued net-FDB loss.
#'
#' Each mapping contains exactly up/down in the context's local currency.
#' This does not value exposures or approve their currency classification.
#' @param gross_losses Gross losses. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param net_losses Net losses. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param tie_break Tie break. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("currency-select-baseline-equal")
#' result <- do.call(currency_scenario_charge,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
currency_scenario_charge <- function(gross_losses, net_losses, valuation_reference, tie_break = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "currency_scenario_charge", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("currency_scenario_charge", list(gross_losses = gross_losses, net_losses = net_losses, valuation_reference = valuation_reference, tie_break = tie_break),
    context, .s2_implementations[["currency_scenario_charge"]])
}

#' Calculate a qualified German branch asset minimum or domestic deposit.
#'
#' The input is the unhalved applicable AMCR, not the branch MCR. A German
#' deposit waiver does not waive the deposit at the chosen supervisory state.
#' Asset location, admissibility and any higher actual SCR/MCR stay separate.
#' @param unadjusted_absolute_floor Unadjusted absolute floor. Reference type: `float`.
#' @param requirement Requirement. Reference type: `str`.
#' @param reinsurance_branch Reinsurance branch. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param domestic_deposit_relief Domestic deposit relief. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param relief_reference Relief reference. Reference type: `str | None`.
#' @param source_version Source version. Reference type: `str`.
#' @param applicability_reference Applicability reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: EUR.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2026-10-05",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("de-close-baseline-reinsurer")
#' result <- do.call(de_branch_minimum_amount,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
de_branch_minimum_amount <- function(unadjusted_absolute_floor, requirement, reinsurance_branch, domestic_deposit_relief, relief_reference = NULL, source_version, applicability_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "de_branch_minimum_amount", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("de_branch_minimum_amount", list(unadjusted_absolute_floor = unadjusted_absolute_floor, requirement = requirement, reinsurance_branch = reinsurance_branch, domestic_deposit_relief = domestic_deposit_relief, relief_reference = relief_reference, source_version = source_version, applicability_reference = applicability_reference),
    context, .s2_implementations[["de_branch_minimum_amount"]])
}

#' German book-value cover pool minimum, distinct from Solvency-II TP.
#'
#' Gross direct-insurance figures precede reinsurance deductions. Eligible
#' current pool value must already reflect the separate Article126 rules.
#' Extra supervisory requirements under127(2) are not inferred here.
#' @param components Components. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param source_version Source version. Reference type: `str`.
#' @param applicability_reference Applicability reference. Reference type: `str`.
#' @param current_cover_assets Current cover assets. Reference type: `float | None`.
#' @param foreign_security_exception Foreign security exception. Reference type: `float`.
#' @param foreign_security_reference Foreign security reference. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: EUR.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2026-10-05",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("cover-close-baseline-minimum")
#' result <- do.call(de_cover_assets_minimum,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
de_cover_assets_minimum <- function(components, source_version, applicability_reference, current_cover_assets = NULL, foreign_security_exception = 0, foreign_security_reference = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "de_cover_assets_minimum", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("de_cover_assets_minimum", list(components = components, source_version = source_version, applicability_reference = applicability_reference, current_cover_assets = current_cover_assets, foreign_security_exception = foreign_security_exception, foreign_security_reference = foreign_security_reference),
    context, .s2_implementations[["de_cover_assets_minimum"]])
}

#' Book/market minimum, or explicitly supervisor-determined property value.
#'
#' A doubled market-to-book threshold permits review of an increase; it does
#' not prescribe the increase. Encumbered property always needs an external
#' supervisory value. Neither value may feed a Solvency-II market valuation
#' merely because this separate cover-pool rule accepts it.
#' @param book_value Book value. Reference type: `float`.
#' @param market_value Market value. Reference type: `float`.
#' @param encumbered Encumbered. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param supervisory_value Supervisory value. Reference type: `float | None`.
#' @param supervisory_reference Supervisory reference. Reference type: `str | None`.
#' @param expert_valuation_reference Expert valuation reference. Reference type: `str | None`.
#' @param source_version Source version. Reference type: `str`.
#' @param applicability_reference Applicability reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: EUR.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2026-10-05",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("cover-close-baseline-property-low")
#' result <- do.call(de_cover_property_value,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
de_cover_property_value <- function(book_value, market_value, encumbered, supervisory_value = NULL, supervisory_reference = NULL, expert_valuation_reference = NULL, source_version, applicability_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "de_cover_property_value", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("de_cover_property_value", list(book_value = book_value, market_value = market_value, encumbered = encumbered, supervisory_value = supervisory_value, supervisory_reference = supervisory_reference, expert_valuation_reference = expert_valuation_reference, source_version = source_version, applicability_reference = applicability_reference),
    context, .s2_implementations[["de_cover_property_value"]])
}

#' Coverage, cash-flow gap and liquidity level for one qualified horizon.
#'
#' Inflows and cash are disjoint sources; no netting or double counting.
#' Projections, liquidation assumptions and scenarios are insurer inputs.
#' The circular specifies no universal minimum ratio or fixed time bands.
#' @param gross_inflows Gross inflows. Reference type: `float`.
#' @param gross_outflows Gross outflows. Reference type: `float`.
#' @param realisable_cash Realisable cash. Reference type: `float`.
#' @param total_investments Total investments. Reference type: `float`.
#' @param source_version Source version. Reference type: `str`.
#' @param applicability_reference Applicability reference. Reference type: `str`.
#' @param horizon_reference Horizon reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2026-10-05",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("de-close-baseline-liquidity-zero-assets")
#' result <- do.call(de_liquidity_coverage,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
de_liquidity_coverage <- function(gross_inflows, gross_outflows, realisable_cash, total_investments, source_version, applicability_reference, horizon_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "de_liquidity_coverage", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("de_liquidity_coverage", list(gross_inflows = gross_inflows, gross_outflows = gross_outflows, realisable_cash = realisable_cash, total_investments = total_investments, source_version = source_version, applicability_reference = applicability_reference, horizon_reference = horizon_reference),
    context, .s2_implementations[["de_liquidity_coverage"]])
}

#' LGD of an externally qualified CCP default-fund contribution, not CCP eligibility.
#'
#' @param contribution Contribution. Reference type: `float`.
#' @param prefunded Prefunded. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("default-2027-ccp-fund-true")
#' result <- do.call(default_ccp_fund_lgd_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_ccp_fund_lgd_2027 <- function(contribution, prefunded, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_ccp_fund_lgd_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_ccp_fund_lgd_2027", list(contribution = contribution, prefunded = prefunded, qualification_reference = qualification_reference),
    context, .s2_implementations[["default_ccp_fund_lgd_2027"]])
}

#' Check one externally selected clearing route, not legal eligibility.
#'
#' A failed declaration screen never implies zero capital or a fallback LGD
#' branch. The client joint-default exception relaxes only the named joint
#' failure case. Historical legal opinions must not substitute for the changed
#' 2027 legal-review requirement. Direct own-account clearing is future-only.
#' @param route Route. Reference type: `str`.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("clearing-conditions-future-member_own_account-all")
#' result <- do.call(default_clearing_conditions,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_clearing_conditions <- function(route, conditions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_clearing_conditions", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_clearing_conditions", list(route = route, conditions = conditions, qualification_reference = qualification_reference),
    context, .s2_implementations[["default_clearing_conditions"]])
}

#' Select a coefficient, not an insolvency-law interpretation or collateral valuation.
#'
#' @param factor_type Factor type. Reference type: `str`.
#' @param collateral_ignored_in_estate_share Collateral ignored in estate share. Reference type: `bool | None`.
#' @param legal_reference Legal reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-lgd-factor-F-true")
#' result <- do.call(default_collateral_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_collateral_factor <- function(factor_type, collateral_ignored_in_estate_share, legal_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_collateral_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_collateral_factor", list(factor_type = factor_type, collateral_ignored_in_estate_share = collateral_ignored_in_estate_share, legal_reference = legal_reference),
    context, .s2_implementations[["default_collateral_factor"]])
}

#' Literal document-profile order; signed adjustment, not collateral eligibility.
#'
#' External valuations must assess FX against exposure/loan currency. Do not
#' silently reverse this source's subtraction or apply the Article196 floor.
#' @param scr_without Scr without. Reference type: `float`.
#' @param scr_with Scr with. Reference type: `float`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-collateral-adjustment-negative")
#' result <- do.call(default_collateral_market_adjustment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_collateral_market_adjustment <- function(scr_without, scr_with, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_collateral_market_adjustment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_collateral_market_adjustment", list(scr_without = scr_without, scr_with = scr_with, valuation_reference = valuation_reference),
    context, .s2_implementations[["default_collateral_market_adjustment"]])
}

#' Article192(4a) floor; Loan is the source-defined qualified mortgage value.
#'
#' Classification and guideline-compliant recovery valuation are external. This
#' does not extend the source-defined scope to arbitrary unsecured loans.
#' @param loan Loan. Reference type: `float`.
#' @param recoverables Recoverables. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("default-2027-loan-0-0")
#' result <- do.call(default_defaulted_loan_lgd_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_defaulted_loan_lgd_2027 <- function(loan, recoverables, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_defaulted_loan_lgd_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_defaulted_loan_lgd_2027", list(loan = loan, recoverables = recoverables, qualification_reference = qualification_reference),
    context, .s2_implementations[["default_defaulted_loan_lgd_2027"]])
}

#' Four externally qualified variants, with explicit market/risk-adjusted collateral.
#'
#' For a qualified netting set, RM and collateral must be assessed at set level;
#' summing stand-alone risk mitigation is not justified by this function.
#' @param derivative_value Derivative value. Reference type: `float`.
#' @param risk_mitigation Risk mitigation. Reference type: `float`.
#' @param collateral_value Collateral value. Reference type: `float`.
#' @param variant Variant. Reference type: `str`.
#' @param collateral_basis Collateral basis. Reference type: `str`.
#' @param collateral_ignored_in_estate_share Collateral ignored in estate share. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-lgd-derivative-3-true")
#' result <- do.call(default_derivative_lgd,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_derivative_lgd <- function(derivative_value, risk_mitigation, collateral_value, variant, collateral_basis, collateral_ignored_in_estate_share, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_derivative_lgd", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_derivative_lgd", list(derivative_value = derivative_value, risk_mitigation = risk_mitigation, collateral_value = collateral_value, variant = variant, collateral_basis = collateral_basis, collateral_ignored_in_estate_share = collateral_ignored_in_estate_share, qualification_reference = qualification_reference),
    context, .s2_implementations[["default_derivative_lgd"]])
}

#' Combine market values, never stand-alone RM, collateral stress or LGD.
#'
#' All values must already be in context currency and Article75-compatible.
#' Netting eligibility and completeness of the set remain externally assessed.
#' Fraction over decimal float representations prevents intermediate overflow
#' and preserves signed cancellation. Only the final value becomes float.
#' @param positions Positions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param netting_qualified Netting qualified. Reference type: `bool | None`.
#' @param liabilities_settled_before_clearing Liabilities settled before clearing. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("derivative-netting-baseline-single")
#' result <- do.call(default_derivative_netting_value,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_derivative_netting_value <- function(positions, netting_qualified, liabilities_settled_before_clearing, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_derivative_netting_value", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_derivative_netting_value", list(positions = positions, netting_qualified = netting_qualified, liabilities_settled_before_clearing = liabilities_settled_before_clearing, qualification_reference = qualification_reference),
    context, .s2_implementations[["default_derivative_netting_value"]])
}

#' Return1/2 for default types or0 for exclusion FROM THIS MODULE ONLY.
#'
#' The category must already describe the precise qualified contractual risk,
#' including any spread/mortgage/CCP/guarantee conditions. An excluded risk
#' may attract capital elsewhere; zero is a routing code, not a capital value.
#' Only cedant deposits and called-unpaid commitments use the declared count
#' and optional legal election. Article190 grouping and Article189(5) provider
#' substitution, and the all-relevant-exposures scope of the election, remain
#' external prerequisites; no count is inferred from arbitrary asset IDs.
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param single_name_count Single name count. Reference type: `int | None`.
#' @param elect_type1 Elect type1. Reference type: `bool | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: exposure_type.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("default-type-future-securities_financing")
#' result <- do.call(default_exposure_type,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_exposure_type <- function(category, qualification_reference, single_name_count = NULL, elect_type1 = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_exposure_type", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_exposure_type", list(category = category, qualification_reference = qualification_reference, single_name_count = single_name_count, elect_type1 = elect_type1),
    context, .s2_implementations[["default_exposure_type"]])
}

#' Use a qualified stress guarantee, not an arbitrary guarantee nominal amount.
#'
#' The future Article192(4) adds a floor on the non-guaranteed loan value.
#' Historical results retain the published zero floor, not the new 5% floor.
#' @param loan_value Loan value. Reference type: `float`.
#' @param risk_adjusted_mortgage Risk adjusted mortgage. Reference type: `float`.
#' @param eligible_guarantee Eligible guarantee. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param guarantee_conditions Guarantee conditions. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("lgd-mortgage-10")
#' result <- do.call(default_mortgage_lgd,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_mortgage_lgd <- function(loan_value, risk_adjusted_mortgage, eligible_guarantee, qualification_reference, context, guarantee_conditions = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "default_mortgage_lgd", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_mortgage_lgd", list(loan_value = loan_value, risk_adjusted_mortgage = risk_adjusted_mortgage, eligible_guarantee = eligible_guarantee, qualification_reference = qualification_reference, guarantee_conditions = guarantee_conditions),
    context, .s2_implementations[["default_mortgage_lgd"]])
}

#' Two externally classified nonnegative valuation conventions; no hidden offsetting.
#'
#' @param recognised_value Recognised value. Reference type: `float`.
#' @param exposure_type Exposure type. Reference type: `str`.
#' @param classification_reference Classification reference. Reference type: `str`.
#' @param nominal_value Nominal value. Reference type: `float | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-lgd-other-receivable-0")
#' result <- do.call(default_other_lgd,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_other_lgd <- function(recognised_value, exposure_type, classification_reference, nominal_value = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_other_lgd", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_other_lgd", list(recognised_value = recognised_value, exposure_type = exposure_type, classification_reference = classification_reference, nominal_value = nominal_value),
    context, .s2_implementations[["default_other_lgd"]])
}

#' Externally qualified type B/C pool formula, not pool classification.
#'
#' B uses already net BEC/BEU; C uses the whole pool's BECE. The recovery
#' threshold is inclusive, unlike Article192(2). DeltaRM is not halved.
#' @param best_estimate Best estimate. Reference type: `float`.
#' @param risk_mitigation_contribution Risk mitigation contribution. Reference type: `float`.
#' @param risk_adjusted_collateral Risk adjusted collateral. Reference type: `float`.
#' @param variant Variant. Reference type: `str`.
#' @param encumbered_asset_fraction Encumbered asset fraction. Reference type: `float`.
#' @param collateral_ignored_in_estate_share Collateral ignored in estate share. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param undertaking_share Undertaking share. Reference type: `float | None`.
#' @param counterparty_share Counterparty share. Reference type: `float | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-pool-C-boundary")
#' result <- do.call(default_pool_lgd,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_pool_lgd <- function(best_estimate, risk_mitigation_contribution, risk_adjusted_collateral, variant, encumbered_asset_fraction, collateral_ignored_in_estate_share, qualification_reference, undertaking_share = NULL, counterparty_share = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_pool_lgd", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_pool_lgd", list(best_estimate = best_estimate, risk_mitigation_contribution = risk_mitigation_contribution, risk_adjusted_collateral = risk_adjusted_collateral, variant = variant, encumbered_asset_fraction = encumbered_asset_fraction, collateral_ignored_in_estate_share = collateral_ignored_in_estate_share, qualification_reference = qualification_reference, undertaking_share = undertaking_share, counterparty_share = counterparty_share),
    context, .s2_implementations[["default_pool_lgd"]])
}

#' Select the pool SR convention, not the counterparty PD table.
#'
#' @param category Category. Reference type: `str`.
#' @param classification_reference Classification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | None`.
#' @param current_solvency_ratio Current solvency ratio. Reference type: `float | None`.
#' @param equivalent Equivalent. Reference type: `bool | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-pool-member-rating-0")
#' result <- do.call(default_pool_member_ratio,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_pool_member_ratio <- function(category, classification_reference, credit_quality_step = NULL, current_solvency_ratio = NULL, equivalent = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_pool_member_ratio", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_pool_member_ratio", list(category = category, classification_reference = classification_reference, credit_quality_step = credit_quality_step, current_solvency_ratio = current_solvency_ratio, equivalent = equivalent),
    context, .s2_implementations[["default_pool_member_ratio"]])
}

#' Harmonic EOF-weighted inner group plus risk-share-weighted outer group.
#'
#' Membership must include any central intermediary. Zero inner SR requires
#' a separately assessed limiting case; it is not silently replaced by zero.
#' @param in_scope_members In scope members. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param out_of_scope_members Out of scope members. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param membership_reference Membership reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-pool-ratio-outside")
#' result <- do.call(default_pool_solvency_ratio,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_pool_solvency_ratio <- function(in_scope_members, out_of_scope_members, membership_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_pool_solvency_ratio", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_pool_solvency_ratio", list(in_scope_members = in_scope_members, out_of_scope_members = out_of_scope_members, membership_reference = membership_reference),
    context, .s2_implementations[["default_pool_solvency_ratio"]])
}

#' Apply type-A liability rule to an externally qualified Article192 LGD.
#'
#' Low SR legitimately gives a risk-share factor above1 in this source;
#' there is no invented reduction-only clamp.
#' @param base_lgd Base lgd. Reference type: `float`.
#' @param joint_liability Joint liability. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param pool_solvency_ratio Pool solvency ratio. Reference type: `float | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-pool-A-proportional")
#' result <- do.call(default_pool_type_a_lgd,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_pool_type_a_lgd <- function(base_lgd, joint_liability, qualification_reference, pool_solvency_ratio = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_pool_type_a_lgd", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_pool_type_a_lgd", list(base_lgd = base_lgd, joint_liability = joint_liability, qualification_reference = qualification_reference, pool_solvency_ratio = pool_solvency_ratio),
    context, .s2_implementations[["default_pool_type_a_lgd"]])
}

#' Apply an externally qualified Article 199 category; not a rating/classification engine.
#'
#' Clearing exceptions in (12)/(13) take precedence in the external category
#' decision. A 'rated' call cannot establish that those exceptions are absent.
#' @param category Category. Reference type: `str`.
#' @param classification_reference Classification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | None`.
#' @param solvency_ratio Solvency ratio. Reference type: `float | None`.
#' @param mcr_compliant Mcr compliant. Reference type: `bool | None`.
#' @param sfcr_published Sfcr published. Reference type: `bool | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: probability.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("pd-other")
#' result <- do.call(default_probability,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_probability <- function(category, classification_reference, credit_quality_step = NULL, solvency_ratio = NULL, mcr_compliant = NULL, sfcr_published = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_probability", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_probability", list(category = category, classification_reference = classification_reference, credit_quality_step = credit_quality_step, solvency_ratio = solvency_ratio, mcr_compliant = mcr_compliant, sfcr_published = sfcr_published),
    context, .s2_implementations[["default_probability"]])
}

#' Select a qualified provider PD, never the minimum of two probabilities.
#'
#' Full protection and Articles209–215 compliance are externally assessed.
#' Clearing precedence is explicit. This does not regroup names under189(5),
#' change LGD, or infer sovereign zero-PD eligibility from a provider identity.
#' @param original_pd Original pd. Reference type: `float`.
#' @param substitute_provider Substitute provider. Reference type: `bool | None`.
#' @param protection Protection. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param clearing_category Clearing category. Reference type: `str | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param central_government_guarantee Central government guarantee. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: probability.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("protected-pd-baseline-original")
#' result <- do.call(default_protected_probability,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_protected_probability <- function(original_pd, substitute_provider, protection, clearing_category, qualification_reference, context, central_government_guarantee = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "default_protected_probability", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_protected_probability", list(original_pd = original_pd, substitute_provider = substitute_provider, protection = protection, clearing_category = clearing_category, qualification_reference = qualification_reference, central_government_guarantee = central_government_guarantee),
    context, .s2_implementations[["default_protected_probability"]])
}

#' Choose standard/high-encumbrance formula; input includes corresponding receivables.
#'
#' This SCR LGD is distinct from the technical-provisions loss in Article42.
#' Netting, collateral eligibility and contract qualifications remain external.
#' @param recoverables Recoverables. Reference type: `float`.
#' @param risk_mitigation Risk mitigation. Reference type: `float`.
#' @param risk_adjusted_collateral Risk adjusted collateral. Reference type: `float`.
#' @param counterparty_is_insurer Counterparty is insurer. Reference type: `bool | None`.
#' @param collateral_ignored_in_estate_share Collateral ignored in estate share. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param encumbered_asset_fraction Encumbered asset fraction. Reference type: `float | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param method Method. Reference type: `str`.
#' @param proportionality_reference Proportionality reference. Reference type: `str | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("lgd-ri-full-collateral")
#' result <- do.call(default_reinsurance_lgd,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_reinsurance_lgd <- function(recoverables, risk_mitigation, risk_adjusted_collateral, counterparty_is_insurer, collateral_ignored_in_estate_share, qualification_reference, encumbered_asset_fraction = NULL, context, method = "standard", proportionality_reference = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "default_reinsurance_lgd", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_reinsurance_lgd", list(recoverables = recoverables, risk_mitigation = risk_mitigation, risk_adjusted_collateral = risk_adjusted_collateral, counterparty_is_insurer = counterparty_is_insurer, collateral_ignored_in_estate_share = collateral_ignored_in_estate_share, qualification_reference = qualification_reference, encumbered_asset_fraction = encumbered_asset_fraction, method = method, proportionality_reference = proportionality_reference),
    context, .s2_implementations[["default_reinsurance_lgd"]])
}

#' Literal collateral branches, with signed results and explicit legal evidence.
#'
#' Article197(3) says neither condition is met. Mixed failure is therefore
#' not silently treated as zero. A negative result remains visible; downstream
#' LGD APIs explicitly reject collateral outside their nonnegative contract.
#' Article112 is an explicit alternative on market value, not an additional
#' haircut after a market adjustment. Article88 qualification stays external.
#' Optional Article214(2) assessment checks the deposit institution's CQS;
#' its legal applicability and all other Article214 conditions stay external.
#' Omission preserves the qualified-input API, not a numeric eligibility check.
#' @param market_value Market value. Reference type: `float`.
#' @param market_adjustment Market adjustment. Reference type: `float | None`.
#' @param collateral_type Collateral type. Reference type: `str`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool | None]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param method Method. Reference type: `str`.
#' @param proportionality_reference Proportionality reference. Reference type: `str | None`.
#' @param custodian_assessment Custodian assessment. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("risk-adjusted-collateral-baseline-title")
#' result <- do.call(default_risk_adjusted_collateral,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_risk_adjusted_collateral <- function(market_value, market_adjustment, collateral_type, conditions, qualification_reference, context, method = "standard", proportionality_reference = NULL, custodian_assessment = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "default_risk_adjusted_collateral", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_risk_adjusted_collateral", list(market_value = market_value, market_adjustment = market_adjustment, collateral_type = collateral_type, conditions = conditions, qualification_reference = qualification_reference, method = method, proportionality_reference = proportionality_reference, custodian_assessment = custodian_assessment),
    context, .s2_implementations[["default_risk_adjusted_collateral"]])
}

#' Subtract adjustment from externally assessed Article198(2) property value.
#'
#' Monitoring, prior claims and the independent valuation's market-value cap
#' must already be reflected in property_value; no appraisal is invented here.
#' @param property_value Property value. Reference type: `float`.
#' @param market_adjustment Market adjustment. Reference type: `float`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("risk-adjusted-mortgage-baseline-zero")
#' result <- do.call(default_risk_adjusted_mortgage,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_risk_adjusted_mortgage <- function(property_value, market_adjustment, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_risk_adjusted_mortgage", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_risk_adjusted_mortgage", list(property_value = property_value, market_adjustment = market_adjustment, valuation_reference = valuation_reference),
    context, .s2_implementations[["default_risk_adjusted_mortgage"]])
}

#' Difference for the same relevant risk module; never substitute overall SCR.
#'
#' @param scr_without Scr without. Reference type: `float`.
#' @param scr_with Scr with. Reference type: `float`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-lgd-rm-floor")
#' result <- do.call(default_risk_mitigation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_risk_mitigation <- function(scr_without, scr_with, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_risk_mitigation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_risk_mitigation", list(scr_without = scr_without, scr_with = scr_with, valuation_reference = valuation_reference),
    context, .s2_implementations[["default_risk_mitigation"]])
}

#' Qualified repo/reverse-repo/securities-lending LGD, not a netting engine.
#'
#' Netted inputs must already represent the same legally qualified set's
#' economic effect; collateral is assessed at set level, not blindly summed.
#' @param exposure Exposure. Reference type: `float`.
#' @param risk_adjusted_collateral Risk adjusted collateral. Reference type: `float`.
#' @param netting_applied Netting applied. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-lgd-financing-zero")
#' result <- do.call(default_securities_financing_lgd_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_securities_financing_lgd_2027 <- function(exposure, risk_adjusted_collateral, netting_applied, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_securities_financing_lgd_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_securities_financing_lgd_2027", list(exposure = exposure, risk_adjusted_collateral = risk_adjusted_collateral, netting_applied = netting_applied, qualification_reference = qualification_reference),
    context, .s2_implementations[["default_securities_financing_lgd_2027"]])
}

#' Count declared single names and retain every exposure-to-group assignment.
#'
#' None for group/pool means externally established absence, NOT unknown.
#' Pool separation suppresses the pool edge only, not the corporate-group
#' rule. Optional189(5) substitutions are an explicit election, restricted to
#' this count. Original/effective legal memberships and completeness of the
#' relevant portfolio are external qualifications, never inferred from IDs.
#' @param exposures Exposures. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param pool_treatment Pool treatment. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param grouping_reference Grouping reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param provider_substitutions Provider substitutions. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: single_names.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("default-grouping-baseline-same-group")
#' result <- do.call(default_single_name_count,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_single_name_count <- function(exposures, pool_treatment, grouping_reference, context, provider_substitutions = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "default_single_name_count", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_single_name_count", list(exposures = exposures, pool_treatment = pool_treatment, grouping_reference = grouping_reference, provider_substitutions = provider_substitutions),
    context, .s2_implementations[["default_single_name_count"]])
}

#' Sum already assessed LGDs; qualified netting sets must not be counted twice.
#'
#' @param exposure_lgds Exposure lgds. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param grouping_reference Grouping reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-lgd-single-name-zero")
#' result <- do.call(default_single_name_lgd,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_single_name_lgd <- function(exposure_lgds, grouping_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_single_name_lgd", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_single_name_lgd", list(exposure_lgds = exposure_lgds, grouping_reference = grouping_reference),
    context, .s2_implementations[["default_single_name_lgd"]])
}

#' LGD-weighted PD for one externally grouped single name, not legal grouping.
#'
#' @param exposures Exposures. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param grouping_reference Grouping reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: probability.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("pd-weighted-zero")
#' result <- do.call(default_single_name_pd,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_single_name_pd <- function(exposures, grouping_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_single_name_pd", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_single_name_pd", list(exposures = exposures, grouping_reference = grouping_reference),
    context, .s2_implementations[["default_single_name_pd"]])
}

#' Type-1 variance and charge; PD/LGD supplied per already grouped single name.
#'
#' @param counterparties Counterparties. Reference type: `list[dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param method Method. Reference type: `str`.
#' @param proportionality_reference Proportionality reference. Reference type: `str | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("default-2027-type1-single-0")
#' result <- do.call(default_type1,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_type1 <- function(counterparties, context, method = "standard", proportionality_reference = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "default_type1", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_type1", list(counterparties = counterparties, method = method, proportionality_reference = proportionality_reference),
    context, .s2_implementations[["default_type1"]])
}

#' Value-decrease amount; BOF must still be revalued to determine the capital charge.
#'
#' @param overdue_intermediary_lgd Overdue intermediary lgd. Reference type: `float`.
#' @param other_lgd Other lgd. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("type2-shock")
#' result <- do.call(default_type2_shock,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_type2_shock <- function(overdue_intermediary_lgd, other_lgd, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_type2_shock", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_type2_shock", list(overdue_intermediary_lgd = overdue_intermediary_lgd, other_lgd = other_lgd),
    context, .s2_implementations[["default_type2_shock"]])
}

#' Three disjoint2027 LGD pools; no implicit zero for defaulted/forborne loans.
#'
#' Inputs are qualified externally and the residual pool excludes both special
#' pools. The result is the exposure-value shock, not a completed BOF revaluation.
#' @param overdue_intermediary_lgd Overdue intermediary lgd. Reference type: `float`.
#' @param defaulted_forborne_lgd Defaulted forborne lgd. Reference type: `float`.
#' @param other_lgd Other lgd. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("default-2027-type2-4")
#' result <- do.call(default_type2_shock_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
default_type2_shock_2027 <- function(overdue_intermediary_lgd, defaulted_forborne_lgd, other_lgd, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "default_type2_shock_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("default_type2_shock_2027", list(overdue_intermediary_lgd = overdue_intermediary_lgd, defaulted_forborne_lgd = defaulted_forborne_lgd, other_lgd = other_lgd, qualification_reference = qualification_reference),
    context, .s2_implementations[["default_type2_shock_2027"]])
}

#' Shock factor with optional qualified168 route checks, never asset valuation.
#'
#' Without classification, eligibility remains external. Optional route inputs
#' bind market/fund scope or existing qualification screens to the chosen factor.
#' Strategic/long-term recognition and own-funds deductions remain separate.
#' @param category Category. Reference type: `str`.
#' @param symmetric_adjustment Symmetric adjustment. Reference type: `float`.
#' @param duration_based Duration based. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param classification Classification. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("equity-shock-type1-1")
#' result <- do.call(equity_shock,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
equity_shock <- function(category, symmetric_adjustment, duration_based = NULL, context, classification = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "equity_shock", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("equity_shock", list(category = category, symmetric_adjustment = symmetric_adjustment, duration_based = duration_based, classification = classification),
    context, .s2_implementations[["equity_shock"]])
}

#' Apply an externally qualified169 factor to the prescribed position basis.
#'
#' A negative investment with losses beyond the invested amount uses its
#' absolute value; other negative non-short investments have a zero stress
#' basis, not a zero balance-sheet value. Short pricing is deliberately not
#' inferred from a signed value or from the absolute-value exception.
#' @param equity_value Equity value. Reference type: `float`.
#' @param shock Shock. Reference type: `float`.
#' @param short_position Short position. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param losses_can_exceed_investment Losses can exceed investment. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("equity-position-zero")
#' result <- do.call(equity_stress_amount_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
equity_stress_amount_2027 <- function(equity_value, shock, short_position, losses_can_exceed_investment, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "equity_stress_amount_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("equity_stress_amount_2027", list(equity_value = equity_value, shock = shock, short_position = short_position, losses_can_exceed_investment = losses_can_exceed_investment, qualification_reference = qualification_reference),
    context, .s2_implementations[["equity_stress_amount_2027"]])
}

#' Forward an externally computed value after declared-evidence checks.
#'
#' AVAILABLE references do not prove their contents, model approval, adequacy
#' or lawful integration. Partial-model results must already follow the
#' externally approved integration; this function applies no correlations.
#' Period metadata is not a calendar-deadline monitor or submission service.
#' @param value Value. Reference type: `float`.
#' @param evidence Evidence. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param model_kind Model kind. Reference type: `str`.
#' @param output_kind Output kind. Reference type: `str`.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param model_reference Model reference. Reference type: `str`.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param scope_reference Scope reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("model-standards-close-baseline-full")
#' result <- do.call(external_model_checked_value,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
external_model_checked_value <- function(value, evidence, model_kind, output_kind, conditions, model_reference, assessment_reference, scope_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "external_model_checked_value", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("external_model_checked_value", list(value = value, evidence = evidence, model_kind = model_kind, output_kind = output_kind, conditions = conditions, model_reference = model_reference, assessment_reference = assessment_reference, scope_reference = scope_reference),
    context, .s2_implementations[["external_model_checked_value"]])
}

#' Route insurer and externally qualified financial-issuer factors, not legal eligibility.
#'
#' After first SFCR, MCR breach takes precedence even over an available rating.
#' Before SFCR, compliance and current solvency inputs are not used/accepted.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | None`.
#' @param solvency_ratio Solvency ratio. Reference type: `float | None`.
#' @param mcr_compliant Mcr compliant. Reference type: `bool | None`.
#' @param sfcr_published Sfcr published. Reference type: `bool | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-financial-issuer-spread-9")
#' result <- do.call(financial_issuer_spread_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
financial_issuer_spread_stress <- function(modified_duration, category, qualification_reference, credit_quality_step = NULL, solvency_ratio = NULL, mcr_compliant = NULL, sfcr_published = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "financial_issuer_spread_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("financial_issuer_spread_stress", list(modified_duration = modified_duration, category = category, qualification_reference = qualification_reference, credit_quality_step = credit_quality_step, solvency_ratio = solvency_ratio, mcr_compliant = mcr_compliant, sfcr_published = sfcr_published),
    context, .s2_implementations[["financial_issuer_spread_stress"]])
}

#' Inventory applicable declared controls and expose versioned review periods.
#'
#' No references are retrieved, substantive evidence reviewed, or actual calendar
#' deadlines monitored. Combination eligibility is conditional on external facts.
#' The optional29d/327d approval changes only the policy-review maximum, not
#' function-combination approval or any other governance interval.
#' Optional archived guidance requires its own explicit applicability reference;
#' omission is NOT_ASSESSED, never a statement that the guidelines do not apply.
#' Optional2027 liquidity_assessment separates solo/group plans, exemptions and
#' supervisory measures; no cashflow projection or supervisory decision is made.
#' @param evidence Evidence. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param supervisory_policy_interval_years Supervisory policy interval years. Reference type: `float | None`.
#' @param non_snc_policy_approval_reference Non snc policy approval reference. Reference type: `str | None`.
#' @param de_ppp_assessment De ppp assessment. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param guidance_assessment Guidance assessment. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param liquidity_assessment Liquidity assessment. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("proportionality-scope-approved-policies-five")
#' result <- do.call(governance_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
governance_evidence_check <- function(evidence, conditions, assessment_reference, supervisory_policy_interval_years = NULL, non_snc_policy_approval_reference = NULL, de_ppp_assessment = NULL, guidance_assessment = NULL, liquidity_assessment = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "governance_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("governance_evidence_check", list(evidence = evidence, conditions = conditions, assessment_reference = assessment_reference, supervisory_policy_interval_years = supervisory_policy_interval_years, non_snc_policy_approval_reference = non_snc_policy_approval_reference, de_ppp_assessment = de_ppp_assessment, guidance_assessment = guidance_assessment, liquidity_assessment = liquidity_assessment),
    context, .s2_implementations[["governance_evidence_check"]])
}

#' Minimum deferral period for an externally qualified relevant staff member.
#'
#' No prescribed significant-deferred-share percentage is invented. This is not
#' permission to ignore payment sustainability, vesting or malus/clawback rules.
#' @param annual_variable Annual variable. Reference type: `float`.
#' @param annual_total Annual total. Reference type: `float`.
#' @param supervisory_exception_denied Supervisory exception denied. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: years.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("governance-pay-baseline-zero")
#' result <- do.call(governance_remuneration_deferral_years,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
governance_remuneration_deferral_years <- function(annual_variable, annual_total, supervisory_exception_denied, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "governance_remuneration_deferral_years", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("governance_remuneration_deferral_years", list(annual_variable = annual_variable, annual_total = annual_total, supervisory_exception_denied = supervisory_exception_denied, qualification_reference = qualification_reference),
    context, .s2_implementations[["governance_remuneration_deferral_years"]])
}

#' Compose externally approved disjoint methods, not a consolidation model.
#'
#' Consolidated inputs already exclude M2/sector holdings and reflect required
#' currency sensitivity. Do not repeat pure-M2 participation-value deductions.
#' M2 insurer subsidiary deficits retain Article221 treatment; sector amounts
#' are already weighted and availability-adjusted under Article228.
#' @param consolidated_own_funds Consolidated own funds. Reference type: `float`.
#' @param consolidated_scr Consolidated scr. Reference type: `float`.
#' @param minimum Minimum. Reference type: `float`.
#' @param method2_related Method2 related. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param sector_contributions Sector contributions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param method2_own_funds_adjustment Method2 own funds adjustment. Reference type: `float`.
#' @param method2_capital_addon Method2 capital addon. Reference type: `float`.
#' @param approval_reference Approval reference. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-combined-negative")
#' result <- do.call(group_combined_component_surplus_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_combined_component_surplus_2027 <- function(consolidated_own_funds, consolidated_scr, minimum, method2_related, sector_contributions, method2_own_funds_adjustment, method2_capital_addon, approval_reference, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_combined_component_surplus_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_combined_component_surplus_2027", list(consolidated_own_funds = consolidated_own_funds, consolidated_scr = consolidated_scr, minimum = minimum, method2_related = method2_related, sector_contributions = sector_contributions, method2_own_funds_adjustment = method2_own_funds_adjustment, method2_capital_addon = method2_capital_addon, approval_reference = approval_reference, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_combined_component_surplus_2027"]])
}

#' Positive holding excess for external consolidated currency scenarios.
#'
#' Amounts use the same qualified currency. This is not a currency charge and
#' no equity shock applies to this holding under the cited combined-method rule.
#' @param participation_value Participation value. Reference type: `float`.
#' @param entity_scr Entity scr. Reference type: `float`.
#' @param share Share. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-combined-fx-50-0")
#' result <- do.call(group_combined_currency_exposure_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_combined_currency_exposure_2027 <- function(participation_value, entity_scr, share, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_combined_currency_exposure_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_combined_currency_exposure_2027", list(participation_value = participation_value, entity_scr = entity_scr, share = share, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_combined_currency_exposure_2027"]])
}

#' Sum BE already adjusted externally for all intra-group transactions.
#'
#' Method1 Article335(1)a/c scope and consolidation shares are external. The
#' receiving reinsurer excludes intra-group fulfilment cashflows; the cedant
#' excludes corresponding recoverables. This function does not perform netting.
#' @param parent_best_estimate Parent best estimate. Reference type: `float`.
#' @param related Related. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-consolidated-be-positive")
#' result <- do.call(group_consolidated_best_estimate,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_consolidated_best_estimate <- function(parent_best_estimate, related, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_consolidated_best_estimate", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_consolidated_best_estimate", list(parent_best_estimate = parent_best_estimate, related = related, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_consolidated_best_estimate"]])
}

#' Qualified MCR sum plus future local licence-withdrawal requirements.
#'
#' Future third-country rows are explicit, including an empty mapping. They
#' must not duplicate the EU-MCR entity set; legal scope is assessed externally.
#' @param parent_mcr Parent mcr. Reference type: `float`.
#' @param related Related. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param third_country Third country. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-minimum-parent")
#' result <- do.call(group_consolidated_minimum,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_consolidated_minimum <- function(parent_mcr, related, qualification_reference, third_country = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_consolidated_minimum", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_consolidated_minimum", list(parent_mcr = parent_mcr, related = related, qualification_reference = qualification_reference, third_country = third_country),
    context, .s2_implementations[["group_consolidated_minimum"]])
}

#' Proportionate sum of externally qualified risk margins, no added diversification.
#'
#' @param parent_risk_margin Parent risk margin. Reference type: `float`.
#' @param related Related. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-consolidated-rm-empty")
#' result <- do.call(group_consolidated_risk_margin,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_consolidated_risk_margin <- function(parent_risk_margin, related, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_consolidated_risk_margin", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_consolidated_risk_margin", list(parent_risk_margin = parent_risk_margin, related = related, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_consolidated_risk_margin"]])
}

#' Sum qualified Article336 amounts plus externally set group add-on.
#'
#' Contributions b/c already include proportional shares. Components d/e are
#' externally determined risk amounts, not invented sums of individual SCRs.
#' From 2027, only a-d belong here; separate Article336b additions must be
#' explicit and disjoint from those components. Article228 sector contributions
#' are outside this consolidated amount. Their valuation is not inferred.
#' Apply the Article230 minimum separately. No sector/Look-through valuation
#' or supervisory determination is implemented by this composition function.
#' @param components Components. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param capital_addon Capital addon. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param simplified_contributions Simplified contributions. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-scr-four")
#' result <- do.call(group_consolidated_scr,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_consolidated_scr <- function(components, capital_addon, qualification_reference, simplified_contributions = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_consolidated_scr", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_consolidated_scr", list(components = components, capital_addon = capital_addon, qualification_reference = qualification_reference, simplified_contributions = simplified_contributions),
    context, .s2_implementations[["group_consolidated_scr"]])
}

#' External consolidated eligible EOF minus SCR after its qualified minimum.
#'
#' This is not consolidation by adding individual SCRs. Input SCR must include
#' externally required add-ons; EOF qualification and minimum coverage remain
#' externally assessed. A negative surplus is deliberately retained.
#' Future Article228 sectors must be excluded from consolidated inputs and
#' added separately, after the minimum is applied to consolidated SCR only.
#' @param eligible_own_funds Eligible own funds. Reference type: `float`.
#' @param calculated_scr Calculated scr. Reference type: `float`.
#' @param minimum Minimum. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param sector_contributions Sector contributions. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("group-consolidated-deficit")
#' result <- do.call(group_consolidated_surplus,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_consolidated_surplus <- function(eligible_own_funds, calculated_scr, minimum, qualification_reference, sector_contributions = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_consolidated_surplus", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_consolidated_surplus", list(eligible_own_funds = eligible_own_funds, calculated_scr = calculated_scr, minimum = minimum, qualification_reference = qualification_reference, sector_contributions = sector_contributions),
    context, .s2_implementations[["group_consolidated_surplus"]])
}

#' Strict numerical thresholds only, never supervisory permission.
#'
#' All relevant holdings and consolidated assets are externally qualified.
#' Exact decimal comparisons avoid a rounding tolerance at either boundary.
#' @param consolidated_assets Consolidated assets. Reference type: `float`.
#' @param book_values Book values. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-immaterial-zero")
#' result <- do.call(group_immaterial_participation_screen_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_immaterial_participation_screen_2027 <- function(consolidated_assets, book_values, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_immaterial_participation_screen_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_immaterial_participation_screen_2027", list(consolidated_assets = consolidated_assets, book_values = book_values, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_immaterial_participation_screen_2027"]])
}

#' Declared holding criteria, not an automatic group or entity classification.
#'
#' Shares must use the precisely qualified212(1)f(vi) consolidated perimeter.
#' Do not cap ratios at1: offsetting consolidated amounts can change ratios.
#' Ongoing satisfaction and the other legal conditions remain external facts.
#' @param indicator_shares Indicator shares. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param other_supervisory_shares Other supervisory shares. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("holding-definition-equal")
#' result <- do.call(group_insurance_holding_conditions_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_insurance_holding_conditions_2027 <- function(indicator_shares, other_supervisory_shares, conditions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_insurance_holding_conditions_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_insurance_holding_conditions_2027", list(indicator_shares = indicator_shares, other_supervisory_shares = other_supervisory_shares, conditions = conditions, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_insurance_holding_conditions_2027"]])
}

#' Method1/combined LTE cap, not group SCR or an automatic classification.
#'
#' Article335(1)a amounts enter in full,335(1)c proportionally. On a qualified
#' supervisory request replace only1(a) with the externally recomputed amount.
#' @param full_amounts Full amounts. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param proportional_amounts Proportional amounts. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param supervisory_recalculation_required Supervisory recalculation required. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param recalculated_full_amount Recalculated full amount. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-close-group-zero")
#' result <- do.call(group_long_term_equity_limit_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_long_term_equity_limit_2027 <- function(full_amounts, proportional_amounts, supervisory_recalculation_required, recalculated_full_amount, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_long_term_equity_limit_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_long_term_equity_limit_2027", list(full_amounts = full_amounts, proportional_amounts = proportional_amounts, supervisory_recalculation_required = supervisory_recalculation_required, recalculated_full_amount = recalculated_full_amount, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_long_term_equity_limit_2027"]])
}

#' Parent plus proportionate related BE, preserving signed external valuations.
#'
#' Calculate before/after removal of intra-group transactions separately; this
#' component does not invent an EOF-adjustment sign or revalue transactions.
#' @param parent_best_estimate Parent best estimate. Reference type: `float`.
#' @param related Related. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-be-empty")
#' result <- do.call(group_method2_best_estimate_sum,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_method2_best_estimate_sum <- function(parent_best_estimate, related, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_method2_best_estimate_sum", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_method2_best_estimate_sum", list(parent_best_estimate = parent_best_estimate, related = related, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_method2_best_estimate_sum"]])
}

#' Signed method2 aggregation component, not complete approved group solvency.
#'
#' Participation values already represent the held direct/indirect investment:
#' never apply share twice. External EOF adjustments must not duplicate those
#' values or the full-subsidiary-deficit correction calculated here. Group
#' availability, tiers, intra-group transactions and scope are not inferred.
#' Future Article228 sectors are explicit, already weighted and disjoint;
#' their holding values are deducted once, not passed through insurer deficits.
#' @param parent_own_funds Parent own funds. Reference type: `float`.
#' @param parent_scr Parent scr. Reference type: `float`.
#' @param related Related. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param own_funds_adjustment Own funds adjustment. Reference type: `float`.
#' @param capital_addon Capital addon. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param sector_contributions Sector contributions. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("group-method2-negative")
#' result <- do.call(group_method2_component_surplus,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_method2_component_surplus <- function(parent_own_funds, parent_scr, related, own_funds_adjustment, capital_addon, qualification_reference, sector_contributions = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_method2_component_surplus", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_method2_component_surplus", list(parent_own_funds = parent_own_funds, parent_scr = parent_scr, related = related, own_funds_adjustment = own_funds_adjustment, capital_addon = capital_addon, qualification_reference = qualification_reference, sector_contributions = sector_contributions),
    context, .s2_implementations[["group_method2_component_surplus"]])
}

#' Literal 2027 minority formula, on explicitly pre-qualified net amounts.
#'
#' Both own-funds inputs exclude intra-group subordinated liabilities and
#' ancillary own funds. The other-nonavailable input also excludes paragraph4
#' items. A negative formula result is NOT an automatic own-funds credit.
#' @param eligible_own_funds_net Eligible own funds net. Reference type: `float`.
#' @param other_nonavailable_own_funds_net Other nonavailable own funds net. Reference type: `float`.
#' @param scr_contribution Scr contribution. Reference type: `float`.
#' @param parent_subscribed_capital_share Parent subscribed capital share. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("group-availability-future-minority-zero")
#' result <- do.call(group_minority_excess_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_minority_excess_2027 <- function(eligible_own_funds_net, other_nonavailable_own_funds_net, scr_contribution, parent_subscribed_capital_share, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_minority_excess_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_minority_excess_2027", list(eligible_own_funds_net = eligible_own_funds_net, other_nonavailable_own_funds_net = other_nonavailable_own_funds_net, scr_contribution = scr_contribution, parent_subscribed_capital_share = parent_subscribed_capital_share, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_minority_excess_2027"]])
}

#' Apply externally supplied model percentages; never develop an insurer model.
#'
#' The full declared entity set is checked before selecting one contribution.
#' A1e-12 numerical sum tolerance is not regulatory materiality. Percentages
#' are never renormalised, and neither ownership weights nor the future6a
#' standard-formula cap are applied to this separate6b branch.
#' @param entity Entity. Reference type: `str`.
#' @param model_outputs Model outputs. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param complete_scope_confirmed Complete scope confirmed. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param model_reference Model reference. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("group-model-baseline-zero-share")
#' result <- do.call(group_model_scr_contribution,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_model_scr_contribution <- function(entity, model_outputs, complete_scope_confirmed, model_reference, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_model_scr_contribution", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_model_scr_contribution", list(entity = entity, model_outputs = model_outputs, complete_scope_confirmed = complete_scope_confirmed, model_reference = model_reference, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_model_scr_contribution"]])
}

#' Cap ONE externally qualified availability scope, without repeating the cap.
#'
#' The caller must not re-use one entity contribution across separate items
#' and sum the individual results. This is neither an Article330(4) exemption
#' nor a computation of total available group own funds.
#' Historical DIR222(4) additionally limits its qualified(2)/(3)pool to the
#' related insurer SCR. That optional scope is not imposed on other pools,
#' nor carried into the future wording which instead uses the contribution.
#' @param amount Amount. Reference type: `float`.
#' @param scr_contribution Scr contribution. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param related_insurer_scr Related insurer scr. Reference type: `float | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("group-availability-baseline-cap-zero")
#' result <- do.call(group_nonavailable_own_funds_cap,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_nonavailable_own_funds_cap <- function(amount, scr_contribution, qualification_reference, context, related_insurer_scr = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "group_nonavailable_own_funds_cap", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_nonavailable_own_funds_cap", list(amount = amount, scr_contribution = scr_contribution, qualification_reference = qualification_reference, related_insurer_scr = related_insurer_scr),
    context, .s2_implementations[["group_nonavailable_own_funds_cap"]])
}

#' Inventory group-ORSA scope, method1 and single-document evidence.
#'
#' No consent is inferred from availability of a group document. Subsidiary
#' Article45 duties persist. The future profile separately includes material
#' non-insurance activities and actual/potential risks/interdependencies.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param method1_applied Method1 applied. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param single_document Single document. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("group-orsa-baseline-missing-conditional")
#' result <- do.call(group_orsa_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_orsa_evidence_check <- function(evidence, method1_applied, single_document, qualification_reference, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_orsa_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_orsa_evidence_check", list(evidence = evidence, method1_applied = method1_applied, single_document = single_document, qualification_reference = qualification_reference, assessment_reference = assessment_reference),
    context, .s2_implementations[["group_orsa_evidence_check"]])
}

#' Signed related-SCR sum minus consolidated SCR, never floored.
#'
#' The externally qualified perimeter must match Article246(4), method1,
#' valuation date and currency. This arithmetic is not the required substantive
#' explanation of the difference, nor a release of diversification benefits.
#' @param related_scrs Related scrs. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param consolidated_scr Consolidated scr. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("group-orsa-baseline-zero")
#' result <- do.call(group_orsa_method1_scr_difference,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_orsa_method1_scr_difference <- function(related_scrs, consolidated_scr, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_orsa_method1_scr_difference", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_orsa_method1_scr_difference", list(related_scrs = related_scrs, consolidated_scr = consolidated_scr, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_orsa_method1_scr_difference"]])
}

#' Check only the declared nine-month horizon, not effective availability.
#'
#' @param months_to_availability Months to availability. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("group-availability-baseline-timing-zero")
#' result <- do.call(group_own_funds_transfer_timing_screen,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_own_funds_transfer_timing_screen <- function(months_to_availability, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_own_funds_transfer_timing_screen", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_own_funds_transfer_timing_screen", list(months_to_availability = months_to_availability, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_own_funds_transfer_timing_screen"]])
}

#' Future pre-acquisition exception with an explicit issuer-specific effect.
#'
#' The elapsed FINANCIAL-year measure is externally qualified. This screen
#' does not rewrite MCR, assign a tier, or waive other own-funds conditions.
#' Three YEARS of group supervision and the prescribed forward ORSA evidence
#' are additional mandatory gates. Insurers use331/332's solo-SCR definition;
#' qualified intermediate holdings/ancillary subsidiaries instead use333(4)'s
#' temporary disapplication of333(2)(a). These are not interchangeable effects.
#' @param related_financial_years Related financial years. Reference type: `float`.
#' @param issued_before_joining Issued before joining. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param included_in_group_calculation Included in group calculation. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param group_supervision_years Group supervision years. Reference type: `float | None`.
#' @param orsa_support_confirmed Orsa support confirmed. Reference type: `bool | None`.
#' @param issuer_type Issuer type. Reference type: `str`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("group-availability-future-pre-acquisition-zero")
#' result <- do.call(group_pre_acquisition_scr_exception_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_pre_acquisition_scr_exception_2027 <- function(related_financial_years, issued_before_joining, included_in_group_calculation, qualification_reference, context, group_supervision_years = NULL, orsa_support_confirmed = NULL, issuer_type = "eu_insurer", ...) {
  .s2_require(length(list(...)) == 0L, "group_pre_acquisition_scr_exception_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_pre_acquisition_scr_exception_2027", list(related_financial_years = related_financial_years, issued_before_joining = issued_before_joining, included_in_group_calculation = included_in_group_calculation, qualification_reference = qualification_reference, group_supervision_years = group_supervision_years, orsa_support_confirmed = orsa_support_confirmed, issuer_type = issuer_type),
    context, .s2_implementations[["group_pre_acquisition_scr_exception_2027"]])
}

#' Remove qualified internal reinsurance values before consolidation shares.
#'
#' Entity gross BE and recoverables must include the supplied disjoint internal
#' transactions, but already exclude all OTHER required intragroup adjustments.
#' Assumed obligation PV and ceded recoverable need not agree: valuation bases
#' and shares can differ. Neither side is inferred from the other or floored.
#' This is a two-sided ledger adjustment, not a projection or legal approval.
#' @param entities Entities. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param transactions Transactions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param parent_id Parent id. Reference type: `str`.
#' @param measure Measure. Reference type: `str`.
#' @param other_intragroup_adjustments_complete Other intragroup adjustments complete. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("group-reinsurance-baseline-recoverables")
#' result <- do.call(group_reinsurance_elimination,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_reinsurance_elimination <- function(entities, transactions, parent_id, measure, other_intragroup_adjustments_complete, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_reinsurance_elimination", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_reinsurance_elimination", list(entities = entities, transactions = transactions, parent_id = parent_id, measure = measure, other_intragroup_adjustments_complete = other_intragroup_adjustments_complete, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_reinsurance_elimination"]])
}

#' Full subsidiary deficit unless an explicit externally approved exception applies.
#'
#' @param eligible_own_funds Eligible own funds. Reference type: `float`.
#' @param scr Scr. Reference type: `float`.
#' @param share Share. Reference type: `float`.
#' @param is_subsidiary Is subsidiary. Reference type: `bool | None`.
#' @param proportionate_deficit_approved Proportionate deficit approved. Reference type: `bool | None`.
#' @param approval_reference Approval reference. Reference type: `str | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-shortfall-full")
#' result <- do.call(group_required_shortfall,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_required_shortfall <- function(eligible_own_funds, scr, share, is_subsidiary, proportionate_deficit_approved, approval_reference = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_required_shortfall", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_required_shortfall", list(eligible_own_funds = eligible_own_funds, scr = scr, share = share, is_subsidiary = is_subsidiary, proportionate_deficit_approved = proportionate_deficit_approved, approval_reference = approval_reference, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_required_shortfall"]])
}

#' Select the prescribed maximum of complete externally calculated branches.
#'
#' Bank inputs include applicable buffers/add-ons, not just Pillar1 minima.
#' Pension inputs cover both the required margin and national requirement.
#' This component implements no CRR, IFR, IORP or third-country valuation.
#' @param sector Sector. Reference type: `str`.
#' @param requirements Requirements. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-sector-bank-zero")
#' result <- do.call(group_sector_capital_requirement_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_sector_capital_requirement_2027 <- function(sector, requirements, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_sector_capital_requirement_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_sector_capital_requirement_2027", list(sector = sector, requirements = requirements, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_sector_capital_requirement_2027"]])
}

#' Sum disjoint qualified sector amounts using subscribed-capital shares.
#'
#' Eligible own funds must already reflect Article228 availability exclusions.
#' Subgroup inclusion, exclusion of its individual members and supervisory
#' decisions are externally qualified. Signed own funds are not floored.
#' @param entities Entities. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param measure Measure. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-sector-empty-eligible_own_funds")
#' result <- do.call(group_sector_contribution_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_sector_contribution_2027 <- function(entities, measure, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_sector_contribution_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_sector_contribution_2027", list(entities = entities, measure = measure, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_sector_contribution_2027"]])
}

#' Strict 5% group OR host-market test used by group reporting definitions.
#'
#' Latest applicable accounts, currency, host-market life/nonlife scope and
#' branch perimeter are externally qualified. No college invitation or broad
#' materiality judgement follows automatically from this numeric indicator.
#' @param branch_gross_premiums Branch gross premiums. Reference type: `float`.
#' @param group_gross_premiums Group gross premiums. Reference type: `float`.
#' @param host_market_gross_premiums Host market gross premiums. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2026-10-05",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("group-report-close-baseline-branch-host")
#' result <- do.call(group_significant_branch_screen,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_significant_branch_screen <- function(branch_gross_premiums, group_gross_premiums, host_market_gross_premiums, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_significant_branch_screen", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_significant_branch_screen", list(branch_gross_premiums = branch_gross_premiums, group_gross_premiums = group_gross_premiums, host_market_gross_premiums = host_market_gross_premiums, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_significant_branch_screen"]])
}

#' Externally approved simplified contribution; no extra ownership weighting.
#'
#' Article228 entities are outside scope. Third-country capital information
#' must be supplied, never replaced with zero when unavailable.
#' @param participation_risk Participation risk. Reference type: `float`.
#' @param entity_type Entity type. Reference type: `str`.
#' @param capital_requirement Capital requirement. Reference type: `float | None`.
#' @param approval_reference Approval reference. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-simplified-other")
#' result <- do.call(group_simplified_participation_scr_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_simplified_participation_scr_2027 <- function(participation_risk, entity_type, capital_requirement = NULL, approval_reference, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_simplified_participation_scr_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_simplified_participation_scr_2027", list(participation_risk = participation_risk, entity_type = entity_type, capital_requirement = capital_requirement, approval_reference = approval_reference, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_simplified_participation_scr_2027"]])
}

#' Standard-formula contribution for externally qualified consolidated entities.
#'
#' Numerator uses the entity's proportional SCR; denominator is the sum of the
#' supplied SCRs, without applying shares a second time. No internal-model
#' allocation is inferred. Scaling preserves the formula at large magnitudes.
#' @param entity Entity. Reference type: `str`.
#' @param share Share. Reference type: `float`.
#' @param diversified_component Diversified component. Reference type: `float`.
#' @param component_scrs Component scrs. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-group-contribution-full")
#' result <- do.call(group_standard_scr_contribution,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
group_standard_scr_contribution <- function(entity, share, diversified_component, component_scrs, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "group_standard_scr_contribution", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("group_standard_scr_contribution", list(entity = entity, share = share, diversified_component = diversified_component, component_scrs = component_scrs, qualification_reference = qualification_reference),
    context, .s2_implementations[["group_standard_scr_contribution"]])
}

#' Qualified Article180(10)/(10a) factor; historical name retained for callers.
#'
#' The 2027 text no longer separately names STS; CRR243 compliance remains
#' required and externally qualified, not inferred from a non-STS declaration.
#' Paragraph10a requires external pre-2019 issue and exact 2018-rule assessment,
#' not a current type label or an engine-generated historical classification.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param transition Transition. Reference type: `Mapping[str, bool] | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("spread-zero-baseline-transition-eligible")
#' result <- do.call(guaranteed_sts_securitisation_spread_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
guaranteed_sts_securitisation_spread_stress <- function(conditions, qualification_reference, transition = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "guaranteed_sts_securitisation_spread_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("guaranteed_sts_securitisation_spread_stress", list(conditions = conditions, qualification_reference = qualification_reference, transition = transition),
    context, .s2_implementations[["guaranteed_sts_securitisation_spread_stress"]])
}

#' Average only event-insured persons in the externally selected concentration.
#'
#' The denominator is event-specific Ne, not the whole building count Cc.
#' Empty event populations require review, never an invented zero mean.
#' @param benefits Benefits. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param group_counts Group counts. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param grouping_reference Grouping reference. Reference type: `str | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-cat-2027-benefit-mean")
#' result <- do.call(health_accident_benefit_mean,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_accident_benefit_mean <- function(benefits, valuation_reference, context, group_counts = NULL, grouping_reference = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "health_accident_benefit_mean", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_accident_benefit_mean", list(benefits = benefits, valuation_reference = valuation_reference, group_counts = group_counts, grouping_reference = grouping_reference),
    context, .s2_implementations[["health_accident_benefit_mean"]])
}

#' Sum externally qualified event benefits of resident insured persons.
#'
#' @param benefits Benefits. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param group_counts Group counts. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param grouping_reference Grouping reference. Reference type: `str | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-accident-benefit-empty")
#' result <- do.call(health_accident_benefit_sum,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_accident_benefit_sum <- function(benefits, valuation_reference, context, group_counts = NULL, grouping_reference = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "health_accident_benefit_sum", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_accident_benefit_sum", list(benefits = benefits, valuation_reference = valuation_reference, group_counts = group_counts, grouping_reference = grouping_reference),
    context, .s2_implementations[["health_accident_benefit_sum"]])
}

#' Use the externally identified largest qualified building population, not largest loss.
#'
#' @param persons Persons. Reference type: `float`.
#' @param mean_benefits Mean benefits. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param concentration_reference Concentration reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-concentration-loss")
#' result <- do.call(health_accident_concentration_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_accident_concentration_loss <- function(persons, mean_benefits, concentration_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_accident_concentration_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_accident_concentration_loss", list(persons = persons, mean_benefits = mean_benefits, concentration_reference = concentration_reference),
    context, .s2_implementations[["health_accident_concentration_loss"]])
}

#' Root-sum-square of already valued country BOF charges, never of gross claims.
#'
#' @param country_charges Country charges. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param peril Peril. Reference type: `str`.
#' @param scope_reference Scope reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-accident-concentration-empty")
#' result <- do.call(health_accident_country_aggregation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_accident_country_aggregation <- function(country_charges, peril, scope_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_accident_country_aggregation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_accident_country_aggregation", list(country_charges = country_charges, peril = peril, scope_reference = scope_reference),
    context, .s2_implementations[["health_accident_country_aggregation"]])
}

#' Scenario indemnity for one externally qualified ADC, before cession/prudence.
#'
#' Do not add multiple covers automatically or substitute the discounted
#' claims volume for the source-defined nominal net best estimate.
#' @param segment Segment. Reference type: `str`.
#' @param nominal_net_best_estimate Nominal net best estimate. Reference type: `float`.
#' @param attachment_point Attachment point. Reference type: `float`.
#' @param cover_size Cover size. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-nslt-2027-adc-recovery-uncapped")
#' result <- do.call(health_adc_recovery_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_adc_recovery_2027 <- function(segment, nominal_net_best_estimate, attachment_point, cover_size, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_adc_recovery_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_adc_recovery_2027", list(segment = segment, nominal_net_best_estimate = nominal_net_best_estimate, attachment_point = attachment_point, cover_size = cover_size, qualification_reference = qualification_reference),
    context, .s2_implementations[["health_adc_recovery_2027"]])
}

#' Reserve-sigma adjustment; no invented upper cap or automatic eligibility.
#'
#' Recovery is the paragraph5 indemnity before applying cession and prudence.
#' Additional premium is supplied before its prescribed zero floor. A zero
#' denominator needs review, not an implicit no-adjustment factor.
#' @param segment Segment. Reference type: `str`.
#' @param nominal_net_best_estimate Nominal net best estimate. Reference type: `float`.
#' @param recovery Recovery. Reference type: `float`.
#' @param cession_share Cession share. Reference type: `float`.
#' @param additional_premium Additional premium. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-nslt-2027-adc-factor-zero-floor")
#' result <- do.call(health_adc_reserve_factor_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_adc_reserve_factor_2027 <- function(segment, nominal_net_best_estimate, recovery, cession_share, additional_premium, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_adc_reserve_factor_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_adc_reserve_factor_2027", list(segment = segment, nominal_net_best_estimate = nominal_net_best_estimate, recovery = recovery, cession_share = cession_share, additional_premium = additional_premium, qualification_reference = qualification_reference),
    context, .s2_implementations[["health_adc_reserve_factor_2027"]])
}

#' Select already qualified SLT-health units; no health TP projection or SCR.
#'
#' Under reinsurance, units represent underlying insurance, not treaty TP.
#' The result counts valuation units, never infers the number of contracts.
#' @param shock Shock. Reference type: `str`.
#' @param valuations Valuations. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_basis Valuation basis. Reference type: `str`.
#' @param grouping_mode Grouping mode. Reference type: `str`.
#' @param grouping_qualified Grouping qualified. Reference type: `bool | None`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: selection_units.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-selection-baseline-mortality-qualified-aggregate")
#' result <- do.call(health_biometric_selection,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_biometric_selection <- function(shock, valuations, valuation_basis, grouping_mode, grouping_qualified = NULL, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_biometric_selection", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_biometric_selection", list(shock = shock, valuations = valuations, valuation_basis = valuation_basis, grouping_mode = grouping_mode, grouping_qualified = grouping_qualified, valuation_reference = valuation_reference),
    context, .s2_implementations[["health_biometric_selection"]])
}

#' Stress only explicitly selected SLT health contracts, not their BOF valuation.
#'
#' Selection, permitted grouping and underlying reinsurance contracts remain
#' external. Probability and intensity bases are never silently interconverted.
#' @param rate Rate. Reference type: `float`.
#' @param shock Shock. Reference type: `str`.
#' @param rate_basis Rate basis. Reference type: `str`.
#' @param selected Selected. Reference type: `bool | None`.
#' @param selection_reference Selection reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-2027-biometric-intensity")
#' result <- do.call(health_biometric_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_biometric_stress <- function(rate, shock, rate_basis, selected, selection_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_biometric_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_biometric_stress", list(rate = rate, shock = shock, rate_basis = rate_basis, selected = selected, selection_reference = selection_reference),
    context, .s2_implementations[["health_biometric_stress"]])
}

#' Sum disjoint SLT medical/income charges; no diversification or classification inference.
#'
#' @param medical_charge Medical charge. Reference type: `float`.
#' @param income_protection_charge Income protection charge. Reference type: `float`.
#' @param classification_reference Classification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-2027-disability-sum")
#' result <- do.call(health_disability_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_disability_risk <- function(medical_charge, income_protection_charge, classification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_disability_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_disability_risk", list(medical_charge = medical_charge, income_protection_charge = income_protection_charge, classification_reference = classification_reference),
    context, .s2_implementations[["health_disability_risk"]])
}

#' Add the expense inflation shock without choosing a projection convention.
#'
#' @param inflation_rate Inflation rate. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-inflation")
#' result <- do.call(health_expense_inflation_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_expense_inflation_stress <- function(inflation_rate, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_expense_inflation_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_expense_inflation_stress", list(inflation_rate = inflation_rate),
    context, .s2_implementations[["health_expense_inflation_stress"]])
}

#' Stress eligible own/cedent costs jointly with inflation in external projection.
#'
#' @param expense Expense. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-expense-zero")
#' result <- do.call(health_expense_level_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_expense_level_stress <- function(expense, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_expense_level_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_expense_level_stress", list(expense = expense),
    context, .s2_implementations[["health_expense_level_stress"]])
}

#' Mix separately calculated volumes; a source reference is not a legal applicability decision.
#'
#' Bounds are reported independently instead of silently changing a supplied
#' external parameter. Optional qualification additionally rejects declared
#' ineligibility or bound contradictions; omission stays explicitly unchecked.
#' This component is not the complete HRES risk module.
#' @param segment Segment. Reference type: `str`.
#' @param risk Risk. Reference type: `str`.
#' @param non_hres_volume Non hres volume. Reference type: `float`.
#' @param hres_volume Hres volume. Reference type: `float`.
#' @param hres_sigma Hres sigma. Reference type: `float`.
#' @param parameter_reference Parameter reference. Reference type: `str`.
#' @param qualification Qualification. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-hres-premium-mixed")
#' result <- do.call(health_hres_mixed_sigma,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_hres_mixed_sigma <- function(segment, risk, non_hres_volume, hres_volume, hres_sigma, parameter_reference, qualification = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_hres_mixed_sigma", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_hres_mixed_sigma", list(segment = segment, risk = risk, non_hres_volume = non_hres_volume, hres_volume = hres_volume, hres_sigma = hres_sigma, parameter_reference = parameter_reference, qualification = qualification),
    context, .s2_implementations[["health_hres_mixed_sigma"]])
}

#' Bound an external representative estimate; this does not determine an official HRES sigma.
#'
#' @param segment Segment. Reference type: `str`.
#' @param risk Risk. Reference type: `str`.
#' @param representative_sigma Representative sigma. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-hres-premium-floor")
#' result <- do.call(health_hres_sigma_bound,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_hres_sigma_bound <- function(segment, risk, representative_sigma, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_hres_sigma_bound", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_hres_sigma_bound", list(segment = segment, risk = risk, representative_sigma = representative_sigma),
    context, .s2_implementations[["health_hres_sigma_bound"]])
}

#' Stress incidence for an explicitly assigned period; jointly project all156 changes.
#'
#' @param rate Rate. Reference type: `float`.
#' @param period Period. Reference type: `str`.
#' @param rate_basis Rate basis. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-income-later_years-intensity")
#' result <- do.call(health_income_incidence_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_income_incidence_stress <- function(rate, period, rate_basis, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_income_incidence_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_income_incidence_stress", list(rate = rate, period = period, rate_basis = rate_basis),
    context, .s2_implementations[["health_income_incidence_stress"]])
}

#' Apply strict recovery / inclusive continuation thresholds without normalisation.
#'
#' No complementary transition is inferred or reconstructed. The supplied
#' valuation model must define coherent state transitions and joint projection.
#' @param rate Rate. Reference type: `float`.
#' @param transition Transition. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: probability.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-income-recovery-0")
#' result <- do.call(health_income_transition_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_income_transition_stress <- function(rate, transition, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_income_transition_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_income_transition_stress", list(rate = rate, transition = transition),
    context, .s2_implementations[["health_income_transition_stress"]])
}

#' Stress relevant exercise/non-exercise probability with health-specific parameters.
#'
#' @param exercise_rate Exercise rate. Reference type: `float`.
#' @param direction Direction. Reference type: `str`.
#' @param option_kind Option kind. Reference type: `str`.
#' @param selected Selected. Reference type: `bool | None`.
#' @param selection_reference Selection reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: probability.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-2027-extension-up")
#' result <- do.call(health_lapse_option_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_lapse_option_stress <- function(exercise_rate, direction, option_kind, selected, selection_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_lapse_option_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_lapse_option_stress", list(exercise_rate = exercise_rate, direction = direction, option_kind = option_kind, selected = selected, selection_reference = selection_reference),
    context, .s2_implementations[["health_lapse_option_stress"]])
}

#' Select the gross SLT health lapse charge corresponding to maximum net loss.
#'
#' @param gross_losses Gross losses. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param net_losses Net losses. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param tie_break Tie break. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-2027-lapse-selection")
#' result <- do.call(health_lapse_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_lapse_risk <- function(gross_losses, net_losses, valuation_reference, tie_break = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_lapse_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_lapse_risk", list(gross_losses = gross_losses, net_losses = net_losses, valuation_reference = valuation_reference, tie_break = tie_break),
    context, .s2_implementations[["health_lapse_risk"]])
}

#' Gross claim for one printed AnnexXVI segment, before reinsurance and BOF revaluation.
#'
#' @param segment Segment. Reference type: `str`.
#' @param event_exposures Event exposures. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param exposure_reference Exposure reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-cat-2027-region-AT")
#' result <- do.call(health_mass_accident_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_mass_accident_loss <- function(segment, event_exposures, exposure_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_mass_accident_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_mass_accident_loss", list(segment = segment, event_exposures = event_exposures, exposure_reference = exposure_reference),
    context, .s2_implementations[["health_mass_accident_loss"]])
}

#' Return the40% event share, not proportional capital or life special-category70%.
#'
#' Uniform joint events, underlying reinsurance contracts and the worst
#' termination form require external qualification in either reference profile.
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-mass-future_159_6_b")
#' result <- do.call(health_mass_lapse_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_mass_lapse_factor <- function(category, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_mass_lapse_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_mass_lapse_factor", list(category = category, qualification_reference = qualification_reference),
    context, .s2_implementations[["health_mass_lapse_factor"]])
}

#' Add/subtract percentage points without inventing a compounding model.
#'
#' @param inflation_rate Inflation rate. Reference type: `float`.
#' @param direction Direction. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-medical-inflation-up")
#' result <- do.call(health_medical_inflation_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_medical_inflation_stress <- function(inflation_rate, direction, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_medical_inflation_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_medical_inflation_stress", list(inflation_rate = inflation_rate, direction = direction),
    context, .s2_implementations[["health_medical_inflation_stress"]])
}

#' Stress payments jointly with inflation in the external permanent projection.
#'
#' @param payment Payment. Reference type: `float`.
#' @param direction Direction. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-medical-payment-zero")
#' result <- do.call(health_medical_payment_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_medical_payment_stress <- function(payment, direction, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_medical_payment_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_medical_payment_stress", list(payment = payment, direction = direction),
    context, .s2_implementations[["health_medical_payment_stress"]])
}

#' Maximum of two joint-scenario charges; no extra net-selection rule is inferred.
#'
#' @param up_charge Up charge. Reference type: `float`.
#' @param down_charge Down charge. Reference type: `float`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-2027-medical-maximum")
#' result <- do.call(health_medical_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_medical_risk <- function(up_charge, down_charge, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_medical_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_medical_risk", list(up_charge = up_charge, down_charge = down_charge, valuation_reference = valuation_reference),
    context, .s2_implementations[["health_medical_risk"]])
}

#' Look up a qualified LOB, not the contract's economic classification.
#'
#' Both NSLT and SLT health obligations enter the health catastrophe scope.
#' This does not qualify any individual accident/pandemic scenario or amount.
#' Zero means outside this module's LOB scope, never zero insurer capital.
#' @param lob Lob. Reference type: `str`.
#' @param module Module. Reference type: `str`.
#' @param classification_reference Classification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-scope-baseline-1-health")
#' result <- do.call(health_module_scope,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_module_scope <- function(lob, module, classification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_module_scope", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_module_scope", list(lob = lob, module = module, classification_reference = classification_reference),
    context, .s2_implementations[["health_module_scope"]])
}

#' NSLT health event share; do not apply SLT's three-scenario selection here.
#'
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-nslt-2027-lapse-termination")
#' result <- do.call(health_nslt_lapse_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_nslt_lapse_factor <- function(category, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_nslt_lapse_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_nslt_lapse_factor", list(category = category, qualification_reference = qualification_reference),
    context, .s2_implementations[["health_nslt_lapse_factor"]])
}

#' Sum externally valued permanent-disability benefits with no recovery assumed.
#'
#' Occupational accident obligations are excluded by external classification;
#' recurring benefits require externally supplied best estimates, not face sums.
#' @param benefits Benefits. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-pandemic-income-empty")
#' result <- do.call(health_pandemic_income_exposure,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_pandemic_income_exposure <- function(benefits, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_pandemic_income_exposure", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_pandemic_income_exposure", list(benefits = benefits, valuation_reference = valuation_reference),
    context, .s2_implementations[["health_pandemic_income_exposure"]])
}

#' Gross sudden scenario claim before reinsurance; country residence/scope external.
#'
#' @param income_exposure Income exposure. Reference type: `float`.
#' @param countries Countries. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param exposure_reference Exposure reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-pandemic-zero")
#' result <- do.call(health_pandemic_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_pandemic_loss <- function(income_exposure, countries, exposure_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_pandemic_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_pandemic_loss", list(income_exposure = income_exposure, countries = countries, exposure_reference = exposure_reference),
    context, .s2_implementations[["health_pandemic_loss"]])
}

#' Weight all three externally valued care types; never silently normalise overrides.
#'
#' @param costs Costs. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-cat-2027-care-hospital")
#' result <- do.call(health_pandemic_medical_cost,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_pandemic_medical_cost <- function(costs, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_pandemic_medical_cost", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_pandemic_medical_cost", list(costs = costs, valuation_reference = valuation_reference),
    context, .s2_implementations[["health_pandemic_medical_cost"]])
}

#' Four-segment standard selector; USP/HRES are deliberately not substituted.
#'
#' Use premium_reserve_parameterised_risk for already qualified effective
#' USP/HRES sigmas. This automatic standard selector does not choose them.
#' 
#' The2027 profile requires all four externally qualified reserve factors,
#' including explicit ones for segments without a qualifying ADC adjustment.
#' Historical calculations reject these future-only inputs.
#' @param segments Segments. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param uses_usp Uses usp. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param uses_hres Uses hres. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param reserve_adjustments Reserve adjustments. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param adjustment_reference Adjustment reference. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-pr-zero")
#' result <- do.call(health_premium_reserve_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_premium_reserve_risk <- function(segments, uses_usp, uses_hres, reserve_adjustments = NULL, adjustment_reference = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_premium_reserve_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_premium_reserve_risk", list(segments = segments, uses_usp = uses_usp, uses_hres = uses_hres, reserve_adjustments = reserve_adjustments, adjustment_reference = adjustment_reference),
    context, .s2_implementations[["health_premium_reserve_risk"]])
}

#' Health-NSLT net premium volume; cohort/time selection and evidence are external.
#'
#' @param expected_next_year Expected next year. Reference type: `float`.
#' @param earned_last_year Earned last year. Reference type: `float`.
#' @param existing_future_pv Existing future pv. Reference type: `float`.
#' @param future_short_pv Future short pv. Reference type: `float`.
#' @param future_long_pv Future long pv. Reference type: `float`.
#' @param method Method. Reference type: `str`.
#' @param cap_evidence Cap evidence. Reference type: `Mapping[str, str] | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-nslt-2027-premium-standard")
#' result <- do.call(health_premium_volume,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_premium_volume <- function(expected_next_year, earned_last_year, existing_future_pv, future_short_pv, future_long_pv, method, cap_evidence = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_premium_volume", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_premium_volume", list(expected_next_year = expected_next_year, earned_last_year = earned_last_year, existing_future_pv = existing_future_pv, future_short_pv = future_short_pv, future_long_pv = future_long_pv, method = method, cap_evidence = cap_evidence),
    context, .s2_implementations[["health_premium_volume"]])
}

#' Health-NSLT claims volume; recoverables exclude208(2) finite/similar RI.
#'
#' @param claims_best_estimate Claims best estimate. Reference type: `float`.
#' @param eligible_recoverables Eligible recoverables. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("health-reserve-net")
#' result <- do.call(health_reserve_volume,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_reserve_volume <- function(claims_best_estimate, eligible_recoverables, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_reserve_volume", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_reserve_volume", list(claims_best_estimate = claims_best_estimate, eligible_recoverables = eligible_recoverables),
    context, .s2_implementations[["health_reserve_volume"]])
}

#' Stress externally selected inflation/legal/health revision-exposed annuities.
#'
#' @param annuity_benefit Annuity benefit. Reference type: `float`.
#' @param revision_exposed Revision exposed. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("health-2027-revision-true")
#' result <- do.call(health_revision_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
health_revision_stress <- function(annuity_benefit, revision_exposed, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "health_revision_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("health_revision_stress", list(annuity_benefit = annuity_benefit, revision_exposed = revision_exposed, qualification_reference = qualification_reference),
    context, .s2_implementations[["health_revision_stress"]])
}

#' Screen declared corporate criteria with conditional controls and numbers.
#'
#' The caller qualifies the infrastructure majority, EEA/OECD location,
#' revenue pattern and (where needed) offtaker. No percentage for a 'clear
#' majority' or numeric interpretation of Article164a(2)biii is invented.
#' Corporate CQS0..3 in Article164b(7) is distinct and explicit.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param instrument Instrument. Reference type: `str`.
#' @param revenue_route Revenue route. Reference type: `str`.
#' @param many_users Many users. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param rating_available Rating available. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | None`.
#' @param operating_years Operating years. Reference type: `float | None`.
#' @param history_basis History basis. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("infrastructure-corporate-baseline-cqs-0")
#' result <- do.call(infrastructure_corporate_eligibility_screen,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
infrastructure_corporate_eligibility_screen <- function(conditions, instrument, revenue_route, many_users, rating_available, qualification_reference, credit_quality_step = NULL, operating_years = NULL, history_basis = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "infrastructure_corporate_eligibility_screen", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("infrastructure_corporate_eligibility_screen", list(conditions = conditions, instrument = instrument, revenue_route = revenue_route, many_users = many_users, rating_available = rating_available, qualification_reference = qualification_reference, credit_quality_step = credit_quality_step, operating_years = operating_years, history_basis = history_basis),
    context, .s2_implementations[["infrastructure_corporate_eligibility_screen"]])
}

#' Apply only the declared, branch-specific project prerequisites.
#'
#' Equity and unrated debt carry the additional paragraph1(f) criteria.
#' Instrument rating availability is externally qualified, not inferred from
#' a corporate rating. Revenue/offtaker/stress/financial assessments remain
#' external; a successful screen does not select a reduced capital factor.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param investment_kind Investment kind. Reference type: `str`.
#' @param revenue_route Revenue route. Reference type: `str`.
#' @param many_users Many users. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param security_route Security route. Reference type: `str | None`.
#' @param alternative_security_mechanisms Alternative security mechanisms. Reference type: `list[str] | None`. Supply an ordered R list or the documented numeric vector.
#' @param in_construction In construction. Reference type: `bool | None`.
#' @param construction_risk Construction risk. Reference type: `bool | None`.
#' @param material_operational_risk Material operational risk. Reference type: `bool | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("infrastructure-project-baseline-rated-debt")
#' result <- do.call(infrastructure_project_eligibility_screen,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
infrastructure_project_eligibility_screen <- function(conditions, investment_kind, revenue_route, many_users, qualification_reference, security_route = NULL, alternative_security_mechanisms = NULL, in_construction = NULL, construction_risk = NULL, material_operational_risk = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "infrastructure_project_eligibility_screen", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("infrastructure_project_eligibility_screen", list(conditions = conditions, investment_kind = investment_kind, revenue_route = revenue_route, many_users = many_users, qualification_reference = qualification_reference, security_route = security_route, alternative_security_mechanisms = alternative_security_mechanisms, in_construction = in_construction, construction_risk = construction_risk, material_operational_risk = material_operational_risk),
    context, .s2_implementations[["infrastructure_project_eligibility_screen"]])
}

#' Deposit guarantee concentration gi only; no default/spread risk exemption.
#'
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-deposit-zero-qualified")
#' result <- do.call(insured_deposit_concentration_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
insured_deposit_concentration_factor <- function(conditions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "insured_deposit_concentration_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("insured_deposit_concentration_factor", list(conditions = conditions, qualification_reference = qualification_reference),
    context, .s2_implementations[["insured_deposit_concentration_factor"]])
}

#' Interpolate stress, not rating, for externally qualified unrated insurers.
#'
#' Duplicate published 75% knots share CQS5_6. MCR/SFCR eligibility is external.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param solvency_ratio Solvency ratio. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-insurer-solvency-spread-1-0")
#' result <- do.call(insurer_solvency_spread_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
insurer_solvency_spread_stress <- function(modified_duration, solvency_ratio, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "insurer_solvency_spread_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("insurer_solvency_spread_stress", list(modified_duration = modified_duration, solvency_ratio = solvency_ratio, qualification_reference = qualification_reference),
    context, .s2_implementations[["insurer_solvency_spread_stress"]])
}

#' Charge on intangible value already recognised under Article 12.
#'
#' @param recognised_value Recognised value. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("capital-2027-intangible-0")
#' result <- do.call(intangible_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
intangible_risk <- function(recognised_value, context, ...) {
  .s2_require(length(list(...)) == 0L, "intangible_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("intangible_risk", list(recognised_value = recognised_value),
    context, .s2_implementations[["intangible_risk"]])
}

#' Return the nonnegative b magnitude; its application sign belongs to the scenario.
#'
#' @param maturity Maturity. Reference type: `float`.
#' @param direction Direction. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: annual_rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("interest-original-row-up-1-interest_additive_shift_2027")
#' result <- do.call(interest_additive_shift_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
interest_additive_shift_2027 <- function(maturity, direction, context, ...) {
  .s2_require(length(list(...)) == 0L, "interest_additive_shift_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("interest_additive_shift_2027", list(maturity = maturity, direction = direction),
    context, .s2_implementations[["interest_additive_shift_2027"]])
}

#' Published floor from1year; subannual ambiguity is not filled by extrapolation.
#'
#' @param maturity Maturity. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: annual_rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("interest-floor-2027-1")
#' result <- do.call(interest_down_floor_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
interest_down_floor_2027 <- function(maturity, context, ...) {
  .s2_require(length(list(...)) == 0L, "interest_down_floor_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("interest_down_floor_2027", list(maturity = maturity),
    context, .s2_implementations[["interest_down_floor_2027"]])
}

#' Literal downward stress only at/before an externally qualified first smoothing point.
#'
#' @param rate Rate. Reference type: `float`.
#' @param maturity Maturity. Reference type: `float`.
#' @param first_smoothing_point First smoothing point. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: annual_rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("interest-down-2027-rate-1")
#' result <- do.call(interest_down_stress_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
interest_down_stress_2027 <- function(rate, maturity, first_smoothing_point, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "interest_down_stress_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("interest_down_stress_2027", list(rate = rate, maturity = maturity, first_smoothing_point = first_smoothing_point, qualification_reference = qualification_reference),
    context, .s2_implementations[["interest_down_stress_2027"]])
}

#' Retain gains but exclude losses on Art68-deducted financial holdings.
#'
#' Signed BOF deltas are externally valued for one up/down scenario and one
#' consistent gross/net-FDB basis. Apply before full currency-scenario losses,
#' never subtract the result from an already floored SCR. No pricing or legal
#' deduction qualification is inferred. The arithmetic is shared with188(6).
#' @param participations Participations. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param direction Direction. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("interest-participation-baseline-up-empty")
#' result <- do.call(interest_participation_bof_change,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
interest_participation_bof_change <- function(participations, direction, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "interest_participation_bof_change", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("interest_participation_bof_change", list(participations = participations, direction = direction, qualification_reference = qualification_reference),
    context, .s2_implementations[["interest_participation_bof_change"]])
}

#' Interpolate s using its own90year tail, not the additive60year tail.
#'
#' @param maturity Maturity. Reference type: `float`.
#' @param direction Direction. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("interest-original-row-up-1-interest_relative_factor_2027")
#' result <- do.call(interest_relative_factor_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
interest_relative_factor_2027 <- function(maturity, direction, context, ...) {
  .s2_require(length(list(...)) == 0L, "interest_relative_factor_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("interest_relative_factor_2027", list(maturity = maturity, direction = direction),
    context, .s2_implementations[["interest_relative_factor_2027"]])
}

#' Sum each direction across qualified currency groups before scenario selection.
#'
#' Future EUR/peg joint scenarios must already be qualified and grouped externally,
#' with no duplicate exposure. Currency/group keys never trigger automatic merging.
#' @param gross_by_currency Gross by currency. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param net_by_currency Net by currency. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param tie_break Tie break. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("interest-sum-net-down")
#' result <- do.call(interest_scenario_charge,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
interest_scenario_charge <- function(gross_by_currency, net_by_currency, valuation_reference, tie_break = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "interest_scenario_charge", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("interest_scenario_charge", list(gross_by_currency = gross_by_currency, net_by_currency = net_by_currency, valuation_reference = valuation_reference, tie_break = tie_break),
    context, .s2_implementations[["interest_scenario_charge"]])
}

#' Stress a basic annual spot rate; linear factor interpolation including endpoints.
#'
#' @param rate Rate. Reference type: `float`.
#' @param maturity Maturity. Reference type: `float`.
#' @param direction Direction. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: annual_rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rate-interpolation")
#' result <- do.call(interest_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
interest_stress <- function(rate, maturity, direction, context, ...) {
  .s2_require(length(list(...)) == 0L, "interest_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("interest_stress", list(rate = rate, maturity = maturity, direction = direction),
    context, .s2_implementations[["interest_stress"]])
}

#' Shift an externally supplied UFR, without constructing a stressed extrapolation.
#'
#' @param ufr Ufr. Reference type: `float`.
#' @param direction Direction. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: annual_rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("interest-ufr-2027-up")
#' result <- do.call(interest_ufr_stress_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
interest_ufr_stress_2027 <- function(ufr, direction, context, ...) {
  .s2_require(length(list(...)) == 0L, "interest_ufr_stress_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("interest_ufr_stress_2027", list(ufr = ufr, direction = direction),
    context, .s2_implementations[["interest_ufr_stress_2027"]])
}

#' Signed tax adjustment from externally valued balances after the Article 207(1) loss.
#'
#' A nonempty evidence reference records the caller's basis for increased DTA;
#' it does not substantively establish future profits or legal recoverability.
#' @param base_dta Base dta. Reference type: `float`.
#' @param base_dtl Base dtl. Reference type: `float`.
#' @param stressed_dta Stressed dta. Reference type: `float`.
#' @param stressed_dtl Stressed dtl. Reference type: `float`.
#' @param recoverability_reference Recoverability reference. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("tax-2027-positive-zeroed")
#' result <- do.call(lac_dt,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
lac_dt <- function(base_dta, base_dtl, stressed_dta, stressed_dtl, recoverability_reference = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "lac_dt", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("lac_dt", list(base_dta = base_dta, base_dtl = base_dtl, stressed_dta = stressed_dta, stressed_dtl = stressed_dtl, recoverability_reference = recoverability_reference),
    context, .s2_implementations[["lac_dt"]])
}

#' Check one externally qualified projection/return-pair and declared evidence.
#'
#' Five years bounds new-business revenue assumptions, not all future profits.
#' A longer profit forecast activates the haircut condition, without inventing
#' its amount. Lower-than-forward returns require review of the literal source
#' rather than treating conservative assumptions as automatically unlawful.
#' @param projection Projection. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param management_actions_assumed Management actions assumed. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("lac-dt-profit-baseline-boundary-five")
#' result <- do.call(lac_dt_profit_assumptions_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
lac_dt_profit_assumptions_check <- function(projection, conditions, management_actions_assumed, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "lac_dt_profit_assumptions_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("lac_dt_profit_assumptions_check", list(projection = projection, conditions = conditions, management_actions_assumed = management_actions_assumed, qualification_reference = qualification_reference),
    context, .s2_implementations[["lac_dt_profit_assumptions_check"]])
}

#' Loss for the external tax revaluation, before (not after) the tax adjustment.
#'
#' @param bscr Bscr. Reference type: `float`.
#' @param operational Operational. Reference type: `float`.
#' @param adjustment_tp Adjustment tp. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("tax-stress-2027-zero")
#' result <- do.call(lac_dt_stress_amount,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
lac_dt_stress_amount <- function(bscr, operational, adjustment_tp, context, ...) {
  .s2_require(length(list(...)) == 0L, "lac_dt_stress_amount", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("lac_dt_stress_amount", list(bscr = bscr, operational = operational, adjustment_tp = adjustment_tp),
    context, .s2_implementations[["lac_dt_stress_amount"]])
}

#' Signed (nonpositive) loss-absorbing adjustment for technical provisions.
#'
#' @param bscr Bscr. Reference type: `float`.
#' @param net_bscr Net bscr. Reference type: `float`.
#' @param future_discretionary_benefits Future discretionary benefits. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("capital-2027-lac-tp-3")
#' result <- do.call(lac_tp,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
lac_tp <- function(bscr, net_bscr, future_discretionary_benefits, context, ...) {
  .s2_require(length(list(...)) == 0L, "lac_tp", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("lac_tp", list(bscr = bscr, net_bscr = net_bscr, future_discretionary_benefits = future_discretionary_benefits),
    context, .s2_implementations[["lac_tp"]])
}

#' Stress the relevant option-exercise rate after the required contract selection.
#'
#' @param rate Rate. Reference type: `float`.
#' @param direction Direction. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: probability.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("life-2027-lapse-up-cap")
#' result <- do.call(lapse_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
lapse_stress <- function(rate, direction, context, ...) {
  .s2_require(length(list(...)) == 0L, "lapse_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("lapse_stress", list(rate = rate, direction = direction),
    context, .s2_implementations[["lapse_stress"]])
}

#' Smallest integer STRICTLY above the ratio, or one for unlimited coverage.
#'
#' Fraction arithmetic on decimal representations preserves exact boundaries
#' such as 115/(1.15*100)=1; ceil would be wrong there. Representative loss
#' allocation across those claims and reinsurance response remain external.
#' @param loss Loss. Reference type: `float`.
#' @param unlimited Unlimited. Reference type: `bool | None`.
#' @param largest_limit Largest limit. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: count.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("liability-credit-2027-unlimited")
#' result <- do.call(liability_cat_claim_count,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
liability_cat_claim_count <- function(loss, unlimited, largest_limit = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "liability_cat_claim_count", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("liability_cat_claim_count", list(loss = loss, unlimited = unlimited, largest_limit = largest_limit, qualification_reference = qualification_reference),
    context, .s2_implementations[["liability_cat_claim_count"]])
}

#' Group factor times next12month earned gross premium before reinsurance.
#'
#' AnnexXI classifications and exclusions are external, including the group4
#' exclusions for groups1–3, private households and specified craftsmen.
#' @param group Group. Reference type: `str`.
#' @param gross_premium Gross premium. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("liability-credit-2027-liability-group1")
#' result <- do.call(liability_cat_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
liability_cat_loss <- function(group, gross_premium, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "liability_cat_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("liability_cat_loss", list(group = group, gross_premium = gross_premium, qualification_reference = qualification_reference),
    context, .s2_implementations[["liability_cat_loss"]])
}

#' Select qualified life units by strictly increased TP excluding risk margin.
#'
#' Direct/underlying basis, grouping and external TP valuation requirements
#' are unchanged; the shared private helper does not contribute health sources.
#' @param shock Shock. Reference type: `str`.
#' @param valuations Valuations. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_basis Valuation basis. Reference type: `str`.
#' @param grouping_mode Grouping mode. Reference type: `str`.
#' @param grouping_qualified Grouping qualified. Reference type: `bool | None`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: selection_units.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("life-selection-baseline-mortality-qualified-aggregate")
#' result <- do.call(life_biometric_selection,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
life_biometric_selection <- function(shock, valuations, valuation_basis, grouping_mode, grouping_qualified = NULL, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "life_biometric_selection", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("life_biometric_selection", list(shock = shock, valuations = valuations, valuation_basis = valuation_basis, grouping_mode = grouping_mode, grouping_qualified = grouping_qualified, valuation_reference = valuation_reference),
    context, .s2_implementations[["life_biometric_selection"]])
}

#' Apply a selected biometric stress without inventing a probability/intensity conversion.
#'
#' Disability incidence/recovery stresses must be projected jointly. Contract
#' selection and time-period assignment are external, not inferred from rate.
#' @param rate Rate. Reference type: `float`.
#' @param shock Shock. Reference type: `str`.
#' @param rate_basis Rate basis. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("life-rate-intensity")
#' result <- do.call(life_biometric_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
life_biometric_stress <- function(rate, shock, rate_basis, context, ...) {
  .s2_require(length(list(...)) == 0L, "life_biometric_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("life_biometric_stress", list(rate = rate, shock = shock, rate_basis = rate_basis),
    context, .s2_implementations[["life_biometric_stress"]])
}

#' Check declared Article35 grouping safeguards, not a clustering model.
#'
#' Individual-contract projection is the default. The insurer assesses undue
#' burden, risk similarity, faithful expenses/risks and comparable best estimates
#' including guarantees/options. No quantitative materiality cutoff is invented.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("life-grouping-baseline-combination-15")
#' result <- do.call(life_cashflow_grouping_conditions,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
life_cashflow_grouping_conditions <- function(conditions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "life_cashflow_grouping_conditions", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("life_cashflow_grouping_conditions", list(conditions = conditions, qualification_reference = qualification_reference),
    context, .s2_implementations[["life_cashflow_grouping_conditions"]])
}

#' Add the twelve-month mortality shock only to externally selected contracts.
#'
#' The input is a mortality probability for the next twelve months, not an
#' intensity. No lifetime extrapolation, contract selection or BOF valuation
#' is inferred; article 143 specifies no probability cap.
#' @param rate Rate. Reference type: `float`.
#' @param selected Selected. Reference type: `bool | None`.
#' @param selection_reference Selection reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: probability.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("life-2027-cat-true")
#' result <- do.call(life_catastrophe_rate_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
life_catastrophe_rate_stress <- function(rate, selected, selection_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "life_catastrophe_rate_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("life_catastrophe_rate_stress", list(rate = rate, selected = selected, selection_reference = selection_reference),
    context, .s2_implementations[["life_catastrophe_rate_stress"]])
}

#' Add the prescribed percentage-point shock without choosing a compounding model.
#'
#' @param inflation_rate Inflation rate. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("life-expense-inflation")
#' result <- do.call(life_expense_inflation_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
life_expense_inflation_stress <- function(inflation_rate, context, ...) {
  .s2_require(length(list(...)) == 0L, "life_expense_inflation_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("life_expense_inflation_stress", list(inflation_rate = inflation_rate),
    context, .s2_implementations[["life_expense_inflation_stress"]])
}

#' Stress eligible costs; combine with inflation stress in the external projection.
#'
#' @param expense Expense. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("life-expense-zero")
#' result <- do.call(life_expense_level_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
life_expense_level_stress <- function(expense, context, ...) {
  .s2_require(length(list(...)) == 0L, "life_expense_level_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("life_expense_level_stress", list(expense = expense),
    context, .s2_implementations[["life_expense_level_stress"]])
}

#' Return exercise probability, stressing non-exercise for extension options.
#'
#' Selection depends on the relevant technical-provision effect, not merely
#' the option label. Underlying/reinsurance rights are classified externally.
#' @param exercise_rate Exercise rate. Reference type: `float`.
#' @param direction Direction. Reference type: `str`.
#' @param option_kind Option kind. Reference type: `str`.
#' @param selected Selected. Reference type: `bool | None`.
#' @param selection_reference Selection reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: probability.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("life-2027-extension-up")
#' result <- do.call(life_lapse_option_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
life_lapse_option_stress <- function(exercise_rate, direction, option_kind, selected, selection_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "life_lapse_option_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("life_lapse_option_stress", list(exercise_rate = exercise_rate, direction = direction, option_kind = option_kind, selected = selected, selection_reference = selection_reference),
    context, .s2_implementations[["life_lapse_option_stress"]])
}

#' Select gross capital using all three externally valued net scenarios.
#'
#' @param gross_losses Gross losses. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param net_losses Net losses. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param tie_break Tie break. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("life-lapse-net-selection")
#' result <- do.call(life_lapse_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
life_lapse_risk <- function(gross_losses, net_losses, valuation_reference, tie_break = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "life_lapse_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("life_lapse_risk", list(gross_losses = gross_losses, net_losses = net_losses, valuation_reference = valuation_reference, tie_break = tie_break),
    context, .s2_implementations[["life_lapse_risk"]])
}

#' Return event share, never a proportional BOF/TP loss or standalone mass SCR.
#'
#' Categories a/b are disjoint and a/b/c apply jointly where relevant. Special
#' category qualification and worst termination form remain external.
#' In2027, uniform application to affected contracts and application to the
#' underlying insurance contracts for reinsurance a/b must also be qualified.
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("life-mass-future_142_6_c")
#' result <- do.call(life_mass_lapse_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
life_mass_lapse_factor <- function(category, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "life_mass_lapse_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("life_mass_lapse_factor", list(category = category, qualification_reference = qualification_reference),
    context, .s2_implementations[["life_mass_lapse_factor"]])
}

#' Screen declared142(6)a facts; a negative result does NOT select category b.
#'
#' Related beneficiaries are excluded independently of count. The20-person
#' limit belongs to the separate estate/inheritance-purpose exclusion only.
#' Underlying reinsurance scope and worst termination valuation remain external.
#' @param policyholder_type Policyholder type. Reference type: `str`.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param beneficiary_count Beneficiary count. Reference type: `int | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("mass-lapse-special-baseline-non-natural-consent-true")
#' result <- do.call(life_mass_lapse_special_eligibility,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
life_mass_lapse_special_eligibility <- function(policyholder_type, conditions, beneficiary_count = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "life_mass_lapse_special_eligibility", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("life_mass_lapse_special_eligibility", list(policyholder_type = policyholder_type, conditions = conditions, beneficiary_count = beneficiary_count, qualification_reference = qualification_reference),
    context, .s2_implementations[["life_mass_lapse_special_eligibility"]])
}

#' Increase only externally qualified revision-exposed annuity benefits.
#'
#' @param annuity_benefit Annuity benefit. Reference type: `float`.
#' @param revision_exposed Revision exposed. Reference type: `bool | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("life-2027-revision-true")
#' result <- do.call(life_revision_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
life_revision_stress <- function(annuity_benefit, revision_exposed, context, ...) {
  .s2_require(length(list(...)) == 0L, "life_revision_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("life_revision_stress", list(annuity_benefit = annuity_benefit, revision_exposed = revision_exposed),
    context, .s2_implementations[["life_revision_stress"]])
}

#' Sum one underlying exposure across all qualified direct/indirect fund paths.
#'
#' Factors are supplied linear multipliers, NOT inferred ownership fractions.
#' Signed/leverage inputs remain externally qualified; no normalisation, NAV
#' solving, asset classification, fallback completion or risk-charge netting.
#' Kahn traversal avoids recursion limits and exponential path enumeration.
#' @param funds Funds. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param holdings Holdings. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param exposure_id Exposure id. Reference type: `str`.
#' @param complete_scope Complete scope. Reference type: `bool | None`.
#' @param basis_reference Basis reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("lookthrough-exposure-baseline-known-zero-position")
#' result <- do.call(lookthrough_exposure_amount,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
lookthrough_exposure_amount <- function(funds, holdings, exposure_id, complete_scope, basis_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "lookthrough_exposure_amount", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("lookthrough_exposure_amount", list(funds = funds, holdings = holdings, exposure_id = exposure_id, complete_scope = complete_scope, basis_reference = basis_reference),
    context, .s2_implementations[["lookthrough_exposure_amount"]])
}

#' Test target-first fallback conditions, not infer a fund's asset allocation.
#'
#' Only externally qualified collective/fund-form investments are in scope.
#' Prudence and any data-grouping cap must be checked separately.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param allocation_basis Allocation basis. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("lookthrough-baseline-target")
#' result <- do.call(lookthrough_fallback_conditions,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
lookthrough_fallback_conditions <- function(conditions, allocation_basis, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "lookthrough_fallback_conditions", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("lookthrough_fallback_conditions", list(conditions = conditions, allocation_basis = allocation_basis, qualification_reference = qualification_reference),
    context, .s2_implementations[["lookthrough_fallback_conditions"]])
}

#' Check an inclusive cap on externally scope-adjusted grouping amounts.
#'
#' Article84(3a) exclusion/classification and prudent risk grouping are external;
#' zero/zero means only the inequality holds, not that a method is approved.
#' @param grouped_assets_in_scope Grouped assets in scope. Reference type: `float`.
#' @param total_assets_in_scope Total assets in scope. Reference type: `float`.
#' @param article84_3a_scope_qualified Article84 3a scope qualified. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param prudent_grouping Prudent grouping. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("lookthrough-baseline-group-zero")
#' result <- do.call(lookthrough_grouping_limit,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
lookthrough_grouping_limit <- function(grouped_assets_in_scope, total_assets_in_scope, article84_3a_scope_qualified, prudent_grouping, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "lookthrough_grouping_limit", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("lookthrough_grouping_limit", list(grouped_assets_in_scope = grouped_assets_in_scope, total_assets_in_scope = total_assets_in_scope, article84_3a_scope_qualified = article84_3a_scope_qualified, prudent_grouping = prudent_grouping, qualification_reference = qualification_reference),
    context, .s2_implementations[["lookthrough_grouping_limit"]])
}

#' Maximum combined effective insured sum of externally qualified objects.
#'
#' Fire inputs are complete, externally qualified building groups, not isolated
#' points. Components are after qualified recovery/fallback; never deduct twice.
#' Empty eligible populations return explicitly documented conventional zero.
#' This is not the hypothetical Article196(a) capital calculation; its2027
#' object-selection requirements need separate qualification.
#' @param exposures Exposures. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param peril Peril. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("concentration-2027-empty")
#' result <- do.call(manmade_concentration_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
manmade_concentration_loss <- function(exposures, peril, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "manmade_concentration_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("manmade_concentration_loss", list(exposures = exposures, peril = peril, qualification_reference = qualification_reference),
    context, .s2_implementations[["manmade_concentration_loss"]])
}

#' Aggregate supplied market submodules, with the source's interest-charge equality rule.
#'
#' Keys identify externally qualified stress currencies/groups; all monetary inputs are already in context
#' currency. This function neither values instruments nor certifies completeness.
#' The selected profile supplies its own matrices, including the2027 interest/spread B coefficient.
#' @param other_components Other components. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param gross_by_currency Gross by currency. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param net_by_currency Net by currency. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param tie_break Tie break. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("market-zero")
#' result <- do.call(market_risk_from_interest_scenarios,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
market_risk_from_interest_scenarios <- function(other_components, gross_by_currency, net_by_currency, valuation_reference, tie_break = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "market_risk_from_interest_scenarios", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("market_risk_from_interest_scenarios", list(other_components = other_components, gross_by_currency = gross_by_currency, net_by_currency = net_by_currency, valuation_reference = valuation_reference, tie_break = tie_break),
    context, .s2_implementations[["market_risk_from_interest_scenarios"]])
}

#' Fundamental-spread increment, not MA/technical-provision revaluation.
#'
#' Use actual modified duration here, not the floor used to determine the
#' separately supplied asset shock. The last paragraph overrides CQS3 for
#' qualified infrastructure investments and infrastructure corporate assets.
#' @param asset_stress Asset stress. Reference type: `float`.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: annual_rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-matching-spread-increase-7")
#' result <- do.call(matching_fundamental_spread_increase,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
matching_fundamental_spread_increase <- function(asset_stress, modified_duration, category, qualification_reference, credit_quality_step = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "matching_fundamental_spread_increase", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("matching_fundamental_spread_increase", list(asset_stress = asset_stress, modified_duration = modified_duration, category = category, qualification_reference = qualification_reference, credit_quality_step = credit_quality_step),
    context, .s2_implementations[["matching_fundamental_spread_increase"]])
}

#' Select a printed EUR floor, including qualified Article166 direct branches.
#'
#' The half-floor is not a half-SCR or half-MCR multiplier. Pass it as the
#' absolute floor into the ordinary branch-activity MCR calculation. Deposit,
#' asset location, authorisation and Article167 relief remain separate.
#' @param undertaking_type Undertaking type. Reference type: `str`.
#' @param covers_nonlife_classes_10_to_15 Covers nonlife classes 10 to 15. Reference type: `bool | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param third_country_branch Third country branch. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str | None`.
#' @param mixed_premiums Mixed premiums. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: EUR.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("mcr-2027-amcr-life")
#' result <- do.call(mcr_absolute_floor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
mcr_absolute_floor <- function(undertaking_type, covers_nonlife_classes_10_to_15 = NULL, context, third_country_branch = FALSE, qualification_reference = NULL, mixed_premiums = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "mcr_absolute_floor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("mcr_absolute_floor", list(undertaking_type = undertaking_type, covers_nonlife_classes_10_to_15 = covers_nonlife_classes_10_to_15, third_country_branch = third_country_branch, qualification_reference = qualification_reference, mixed_premiums = mixed_premiums),
    context, .s2_implementations[["mcr_absolute_floor"]])
}

#' Article251 contract-level floors; external benefit/BE qualification unchanged.
#'
#' @param contracts Contracts. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("mcr-car-baseline-empty")
#' result <- do.call(mcr_capital_at_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
mcr_capital_at_risk <- function(contracts, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "mcr_capital_at_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("mcr_capital_at_risk", list(contracts = contracts, qualification_reference = qualification_reference),
    context, .s2_implementations[["mcr_capital_at_risk"]])
}

#' Combined MCR including an explicitly supplied applicable absolute floor.
#'
#' @param linear Linear. Reference type: `float`.
#' @param scr Scr. Reference type: `float`.
#' @param absolute_floor Absolute floor. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("mcr-2027-combined-floor")
#' result <- do.call(mcr_combined,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
mcr_combined <- function(linear, scr, absolute_floor, context, ...) {
  .s2_require(length(list(...)) == 0L, "mcr_combined", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("mcr_combined", list(linear = linear, scr = scr, absolute_floor = absolute_floor),
    context, .s2_implementations[["mcr_combined"]])
}

#' Linear life MCR; no premature floor on the signed linear total.
#'
#' @param components Components. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("mcr-2027-life-guaranteed")
#' result <- do.call(mcr_life,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
mcr_life <- function(components, context, ...) {
  .s2_require(length(list(...)) == 0L, "mcr_life", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("mcr_life", list(components = components),
    context, .s2_implementations[["mcr_life"]])
}

#' Add the separately determined Art.250/251 parts; life may be signed.
#'
#' @param nonlife Nonlife. Reference type: `float`.
#' @param life Life. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("mcr-linear-zero")
#' result <- do.call(mcr_linear,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
mcr_linear <- function(nonlife, life, context, ...) {
  .s2_require(length(list(...)) == 0L, "mcr_linear", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("mcr_linear", list(nonlife = nonlife, life = life),
    context, .s2_implementations[["mcr_linear"]])
}

#' Linear nonlife/health MCR using all 16 annex-XIX segments, including explicit zeros.
#'
#' @param segments Segments. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("mcr-2027-tp-1")
#' result <- do.call(mcr_nonlife,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
mcr_nonlife <- function(segments, context, ...) {
  .s2_require(length(list(...)) == 0L, "mcr_nonlife", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("mcr_nonlife", list(segments = segments),
    context, .s2_implementations[["mcr_nonlife"]])
}

#' Notional activity MCR with a separately supervisor-assigned add-on.
#'
#' Preserve the printed min(max(...),...) ordering even for signed inputs.
#' The applicable absolute floor and activity delimitation remain external.
#' This is not a total-company MCR or an automatic Article253 decision.
#' @param linear Linear. Reference type: `float`.
#' @param notional_scr Notional scr. Reference type: `float`.
#' @param capital_addon Capital addon. Reference type: `float`.
#' @param absolute_floor Absolute floor. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("mcr-notional-low")
#' result <- do.call(mcr_notional_requirement,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
mcr_notional_requirement <- function(linear, notional_scr, capital_addon, absolute_floor, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "mcr_notional_requirement", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("mcr_notional_requirement", list(linear = linear, notional_scr = notional_scr, capital_addon = capital_addon, absolute_floor = absolute_floor, qualification_reference = qualification_reference),
    context, .s2_implementations[["mcr_notional_requirement"]])
}

#' Allocate SCR excluding add-ons by the two notional linear MCR components.
#'
#' Signed linear components are not silently floored. A zero denominator has
#' no specified allocation here and requires review. Supervisory add-ons are
#' allocated separately by the supervisor, not by this ratio.
#' @param nonlife_linear Nonlife linear. Reference type: `float`.
#' @param life_linear Life linear. Reference type: `float`.
#' @param scr_excluding_addon Scr excluding addon. Reference type: `float`.
#' @param activity Activity. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("mcr-2027-notional-life")
#' result <- do.call(mcr_notional_scr,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
mcr_notional_scr <- function(nonlife_linear, life_linear, scr_excluding_addon, activity, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "mcr_notional_scr", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("mcr_notional_scr", list(nonlife_linear = nonlife_linear, life_linear = life_linear, scr_excluding_addon = scr_excluding_addon, activity = activity, qualification_reference = qualification_reference),
    context, .s2_implementations[["mcr_notional_scr"]])
}

#' Check declared method conditions and two external SCR floors, not raise SCR.
#'
#' @param model_scr Model scr. Reference type: `float`.
#' @param standard_va_scr Standard va scr. Reference type: `float`.
#' @param own_portfolio_va_scr Own portfolio va scr. Reference type: `float`.
#' @param permitted_by_member_state Permitted by member state. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param excludes_company_adjustment Excludes company adjustment. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param excludes_macro_va Excludes macro va. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("model-va-future-equal")
#' result <- do.call(model_va_scr_floor_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
model_va_scr_floor_2027 <- function(model_scr, standard_va_scr, own_portfolio_va_scr, permitted_by_member_state, excludes_company_adjustment, excludes_macro_va, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "model_va_scr_floor_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("model_va_scr_floor_2027", list(model_scr = model_scr, standard_va_scr = standard_va_scr, own_portfolio_va_scr = own_portfolio_va_scr, permitted_by_member_state = permitted_by_member_state, excludes_company_adjustment = excludes_company_adjustment, excludes_macro_va = excludes_macro_va, qualification_reference = qualification_reference),
    context, .s2_implementations[["model_va_scr_floor_2027"]])
}

#' Screen declarations and numerical limits, not the truth of legal conditions.
#'
#' Debt is explicitly EUR and includes overdue/connected-party/group creditor
#' amounts. Context.currency causes no implicit FX conversion. A <=3-year age
#' does not waive more frequent monitoring under material market changes.
#' The future CRR alternative is an external qualification, not a CRR calculator.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param total_debt_eur Total debt eur. Reference type: `float`.
#' @param years_since_value_monitoring Years since value monitoring. Reference type: `float`.
#' @param repayment_route Repayment route. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("mortgage-eligibility-future-crr124-zero")
#' result <- do.call(mortgage_type2_eligibility_screen,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
mortgage_type2_eligibility_screen <- function(conditions, total_debt_eur, years_since_value_monitoring, repayment_route, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "mortgage_type2_eligibility_screen", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("mortgage_type2_eligibility_screen", list(conditions = conditions, total_debt_eur = total_debt_eur, years_since_value_monitoring = years_since_value_monitoring, repayment_route = repayment_route, qualification_reference = qualification_reference),
    context, .s2_implementations[["mortgage_type2_eligibility_screen"]])
}

#' Externally choose total, absent-total property/personal, or per-victim basis.
#'
#' @param amounts Amounts. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param basis Basis. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: EUR.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("motor-count-2027-total-limit")
#' result <- do.call(motor_assumed_limit,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
motor_assumed_limit <- function(amounts, basis, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "motor_assumed_limit", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("motor_assumed_limit", list(amounts = amounts, basis = basis, qualification_reference = qualification_reference),
    context, .s2_implementations[["motor_assumed_limit"]])
}

#' Gross sudden claim, with literal floor even at zero supplied counts.
#'
#' Counts are already proportionally weighted. Whether this submodule applies
#' to the undertaking is external; this API does not invent a zero-book waiver.
#' @param high_count High count. Reference type: `float`.
#' @param low_count Low count. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: EUR.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("motor-floor")
#' result <- do.call(motor_cat_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
motor_cat_loss <- function(high_count, low_count, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "motor_cat_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("motor_cat_loss", list(high_count = high_count, low_count = low_count, qualification_reference = qualification_reference),
    context, .s2_implementations[["motor_cat_loss"]])
}

#' Aggregate qualified LOB4/16 vehicle groups, proportional shares applied once.
#'
#' Vehicle counts are whole numbers before weighting; the resulting count can
#' be fractional. Each group must have a single assumed coverage limit/share.
#' @param vehicles Vehicles. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: count.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("motor-count-2027-empty")
#' result <- do.call(motor_vehicle_count,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
motor_vehicle_count <- function(vehicles, category, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "motor_vehicle_count", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("motor_vehicle_count", list(vehicles = vehicles, category = category, qualification_reference = qualification_reference),
    context, .s2_implementations[["motor_vehicle_count"]])
}

#' One ordered gross event; do not replace sequential recoveries with a sum.
#'
#' Article126 independence, no new mitigation between events, and realistic
#' existing reinstatements must be reflected in external sequence valuation.
#' @param prescribed_loss Prescribed loss. Reference type: `float`.
#' @param peril Peril. Reference type: `str`.
#' @param scenario Scenario. Reference type: `str`.
#' @param event Event. Reference type: `int`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-natural-event-hail-0")
#' result <- do.call(natural_cat_event_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
natural_cat_event_loss <- function(prescribed_loss, peril, scenario, event, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "natural_cat_event_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("natural_cat_event_loss", list(prescribed_loss = prescribed_loss, peril = peril, scenario = scenario, event = event, qualification_reference = qualification_reference),
    context, .s2_implementations[["natural_cat_event_loss"]])
}

#' Sum eligible zone exposures; flood/hail motor weights differ.
#'
#' Classify LOB7/19 property, LOB6/18 onshore property and LOB5/17 motor
#' externally. Subsidence covers eligible residential property; its region
#' scope is France historically, Annex VIIIA regions in the 2027 profile.
#' @param peril Peril. Reference type: `str`.
#' @param components Components. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-natural-si-subsidence")
#' result <- do.call(natural_cat_insured_sum,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
natural_cat_insured_sum <- function(peril, components, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "natural_cat_insured_sum", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("natural_cat_insured_sum", list(peril = peril, components = components, qualification_reference = qualification_reference),
    context, .s2_implementations[["natural_cat_insured_sum"]])
}

#' Return sum of squared premium shares over all explicit regions5..18.
#'
#' All amounts must use the externally qualified peril, gross-premium basis,
#' period, currency/unit and risk-location classification. These region IDs
#' are AnnexIII macroregions, not NatCat risk-zone IDs. No reserve amounts,
#' premium/reserve segment exceptions, USP override or implicit default enter
#' this CAT-only calculation. Zero total is undefined, not an assumed one.
#' @param peril Peril. Reference type: `str`.
#' @param regional_premiums Regional premiums. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("natural-cat-div-baseline-hail-all-equal")
#' result <- do.call(natural_cat_other_diversification,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
natural_cat_other_diversification <- function(peril, regional_premiums, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "natural_cat_other_diversification", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("natural_cat_other_diversification", list(peril = peril, regional_premiums = regional_premiums, qualification_reference = qualification_reference),
    context, .s2_implementations[["natural_cat_other_diversification"]])
}

#' Outside AnnexXIII: eligible next12month gross premiums and external DIV.
#'
#' DIV uses the prescribed premium basis for AnnexIII regions5–18, not generic
#' premium/reserve volumes. Geographic membership is not inferred here.
#' @param peril Peril. Reference type: `str`.
#' @param gross_premium Gross premium. Reference type: `float`.
#' @param diversification Diversification. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-natural-other-hail")
#' result <- do.call(natural_cat_other_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
natural_cat_other_loss <- function(peril, gross_premium, diversification, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "natural_cat_other_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("natural_cat_other_loss", list(peril = peril, gross_premium = gross_premium, diversification = diversification, qualification_reference = qualification_reference),
    context, .s2_implementations[["natural_cat_other_loss"]])
}

#' Maximum of externally valued A/B sequence capital, not sum of event losses.
#'
#' @param peril Peril. Reference type: `str`.
#' @param scenario_charges Scenario charges. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-natural-selection-hail")
#' result <- do.call(natural_cat_scenario_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
natural_cat_scenario_risk <- function(peril, scenario_charges, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "natural_cat_scenario_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("natural_cat_scenario_risk", list(peril = peril, scenario_charges = scenario_charges, qualification_reference = qualification_reference),
    context, .s2_implementations[["natural_cat_scenario_risk"]])
}

#' Optional externally justified lower WSI, never a reinsurance-net amount.
#'
#' @param weighted_sum Weighted sum. Reference type: `float`.
#' @param peril Peril. Reference type: `str`.
#' @param use_alternative Use alternative. Reference type: `bool | None`.
#' @param potential_gross_loss Potential gross loss. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-natural-primary-hail")
#' result <- do.call(natural_cat_weighted_cap,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
natural_cat_weighted_cap <- function(weighted_sum, peril, use_alternative, potential_gross_loss = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "natural_cat_weighted_cap", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("natural_cat_weighted_cap", list(weighted_sum = weighted_sum, peril = peril, use_alternative = use_alternative, potential_gross_loss = potential_gross_loss, qualification_reference = qualification_reference),
    context, .s2_implementations[["natural_cat_weighted_cap"]])
}

#' Shared327b/c/d/e/g size component; each measure's other conditions remain.
#'
#' A declared low-risk supervisory exception applies ONLY to size condition c,
#' never to all governance/ORSA/liquidity or actual approval requirements.
#' The existing327f prudent-deterministic screen is separate and unchanged.
#' @param measure Measure. Reference type: `str`.
#' @param undertaking_type Undertaking type. Reference type: `str`.
#' @param life_gross_tp Life gross tp. Reference type: `float`.
#' @param total_gross_tp Total gross tp. Reference type: `float`.
#' @param nonlife_gwp Nonlife gwp. Reference type: `float`.
#' @param total_gwp Total gwp. Reference type: `float`.
#' @param market_shares Market shares. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param size_exception_confirmed Size exception confirmed. Reference type: `bool | None`.
#' @param exception_reference Exception reference. Reference type: `str | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("proportionality-scope-non-snc-rsr")
#' result <- do.call(non_snc_proportionality_size_screen_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
non_snc_proportionality_size_screen_2027 <- function(measure, undertaking_type, life_gross_tp, total_gross_tp, nonlife_gwp, total_gwp, market_shares, size_exception_confirmed, exception_reference = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "non_snc_proportionality_size_screen_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("non_snc_proportionality_size_screen_2027", list(measure = measure, undertaking_type = undertaking_type, life_gross_tp = life_gross_tp, total_gross_tp = total_gross_tp, nonlife_gwp = nonlife_gwp, total_gwp = total_gwp, market_shares = market_shares, size_exception_confirmed = size_exception_confirmed, exception_reference = exception_reference, qualification_reference = qualification_reference),
    context, .s2_implementations[["non_snc_proportionality_size_screen_2027"]])
}

#' One qualified ADC indemnity, before cession/prudence, using AnnexII sigma.
#'
#' Contract group, distinct tranche and source-defined nominal net valuation
#' remain externally qualified. This is not discounted claims capital or an
#' automatic aggregation of several overlapping covers.
#' @param segment Segment. Reference type: `str`.
#' @param nominal_net_best_estimate Nominal net best estimate. Reference type: `float`.
#' @param attachment_point Attachment point. Reference type: `float`.
#' @param cover_size Cover size. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("nonlife-2027-adc-recovery-cover-cap")
#' result <- do.call(nonlife_adc_recovery_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
nonlife_adc_recovery_2027 <- function(segment, nominal_net_best_estimate, attachment_point, cover_size, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "nonlife_adc_recovery_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("nonlife_adc_recovery_2027", list(segment = segment, nominal_net_best_estimate = nominal_net_best_estimate, attachment_point = attachment_point, cover_size = cover_size, qualification_reference = qualification_reference),
    context, .s2_implementations[["nonlife_adc_recovery_2027"]])
}

#' Qualified reserve-sigma factor, no upper cap; zero denominator needs review.
#'
#' Recovery is before cession; supplied additional premium is floored at zero
#' exactly once. No eligibility approval is inferred from the reference string.
#' @param segment Segment. Reference type: `str`.
#' @param nominal_net_best_estimate Nominal net best estimate. Reference type: `float`.
#' @param recovery Recovery. Reference type: `float`.
#' @param cession_share Cession share. Reference type: `float`.
#' @param additional_premium Additional premium. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("nonlife-2027-adc-factor-no-upper-cap")
#' result <- do.call(nonlife_adc_reserve_factor_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
nonlife_adc_reserve_factor_2027 <- function(segment, nominal_net_best_estimate, recovery, cession_share, additional_premium, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "nonlife_adc_reserve_factor_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("nonlife_adc_reserve_factor_2027", list(segment = segment, nominal_net_best_estimate = nominal_net_best_estimate, recovery = recovery, cession_share = cession_share, additional_premium = additional_premium, qualification_reference = qualification_reference),
    context, .s2_implementations[["nonlife_adc_reserve_factor_2027"]])
}

#' Event share, not capital multiplier; underlying/worst selection external.
#'
#' Article90a permits contract-group selection for118(1)(a) only with88 and
#' 35(a-c) grouping qualification. The external reference must evidence this
#'  if used; it does not change the event share or remove joint valuation.
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("nonlife-2027-lapse-termination")
#' result <- do.call(nonlife_lapse_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
nonlife_lapse_factor <- function(category, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "nonlife_lapse_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("nonlife_lapse_factor", list(category = category, qualification_reference = qualification_reference),
    context, .s2_implementations[["nonlife_lapse_factor"]])
}

#' Article 116 premium volume from externally selected net premium cashflows.
#'
#' Future short contracts exclude premiums earned in their first twelve months
#' after recognition; future long contracts use premiums beyond the next twelve
#' months from valuation. The supplied long-contract PV is still unweighted.
#' Evidence references record, but cannot substantively verify, paragraph 4.
#' @param expected_next_year Expected next year. Reference type: `float`.
#' @param earned_last_year Earned last year. Reference type: `float`.
#' @param existing_future_pv Existing future pv. Reference type: `float`.
#' @param future_short_pv Future short pv. Reference type: `float`.
#' @param future_long_pv Future long pv. Reference type: `float`.
#' @param method Method. Reference type: `str`.
#' @param cap_evidence Cap evidence. Reference type: `Mapping[str, str] | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("nl-premium-zero")
#' result <- do.call(nonlife_premium_volume,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
nonlife_premium_volume <- function(expected_next_year, earned_last_year, existing_future_pv, future_short_pv, future_long_pv, method, cap_evidence = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "nonlife_premium_volume", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("nonlife_premium_volume", list(expected_next_year = expected_next_year, earned_last_year = earned_last_year, existing_future_pv = existing_future_pv, future_short_pv = future_short_pv, future_long_pv = future_long_pv, method = method, cap_evidence = cap_evidence),
    context, .s2_implementations[["nonlife_premium_volume"]])
}

#' Net claims BE; eligible recoverables must exclude208(2) finite/similar RI.
#'
#' @param claims_best_estimate Claims best estimate. Reference type: `float`.
#' @param eligible_recoverables Eligible recoverables. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("nl-reserve-zero")
#' result <- do.call(nonlife_reserve_volume,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
nonlife_reserve_volume <- function(claims_best_estimate, eligible_recoverables, context, ...) {
  .s2_require(length(list(...)) == 0L, "nonlife_reserve_volume", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("nonlife_reserve_volume", list(claims_best_estimate = claims_best_estimate, eligible_recoverables = eligible_recoverables),
    context, .s2_implementations[["nonlife_reserve_volume"]])
}

#' Next12month eligible LOB28 earned gross premiums; AnnexIII DIV supplied.
#'
#' Exclude nonproportional obligations relating to LOB9/21. DIV is based on
#' earned premiums, not the premium/reserve volume measure. Qualification and
#' DIV derivation remain external; the reference is not regulatory approval.
#' @param gross_premium Gross premium. Reference type: `float`.
#' @param diversification Diversification. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("cat-premium-2027-property-zero")
#' result <- do.call(nonprop_property_cat_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
nonprop_property_cat_loss <- function(gross_premium, diversification, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "nonprop_property_cat_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("nonprop_property_cat_loss", list(gross_premium = gross_premium, diversification = diversification, qualification_reference = qualification_reference),
    context, .s2_implementations[["nonprop_property_cat_loss"]])
}

#' Qualified obligation state only, never automatic contract-end derecognition.
#'
#' A terminated policy can retain claims obligations. The supplied status must
#' concern the specific obligation assessed, and remains externally unverified.
#' @param obligation_status Obligation status. Reference type: `str | None`.
#' @param evidence_reference Evidence reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("derecognition-baseline-active")
#' result <- do.call(obligation_derecognition_condition,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
obligation_derecognition_condition <- function(obligation_status, evidence_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "obligation_derecognition_condition", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("obligation_derecognition_condition", list(obligation_status = obligation_status, evidence_reference = evidence_reference),
    context, .s2_implementations[["obligation_derecognition_condition"]])
}

#' Gross premium/provision calculation; provisions exclude the risk margin.
#'
#' @param bscr Bscr. Reference type: `float`.
#' @param earned_life Earned life. Reference type: `float`.
#' @param earned_life_ul Earned life ul. Reference type: `float`.
#' @param earned_nonlife Earned nonlife. Reference type: `float`.
#' @param previous_life Previous life. Reference type: `float`.
#' @param previous_life_ul Previous life ul. Reference type: `float`.
#' @param previous_nonlife Previous nonlife. Reference type: `float`.
#' @param tp_life Tp life. Reference type: `float`.
#' @param tp_life_ul Tp life ul. Reference type: `float`.
#' @param tp_nonlife Tp nonlife. Reference type: `float`.
#' @param expenses_ul Expenses ul. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("capital-2027-op-zero")
#' result <- do.call(operational_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
operational_risk <- function(bscr, earned_life, earned_life_ul, earned_nonlife, previous_life, previous_life_ul, previous_nonlife, tp_life, tp_life_ul, tp_nonlife, expenses_ul, context, ...) {
  .s2_require(length(list(...)) == 0L, "operational_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("operational_risk", list(bscr = bscr, earned_life = earned_life, earned_life_ul = earned_life_ul, earned_nonlife = earned_nonlife, previous_life = previous_life, previous_life_ul = previous_life_ul, previous_nonlife = previous_nonlife, tp_life = tp_life, tp_life_ul = tp_life_ul, tp_nonlife = tp_nonlife, expenses_ul = expenses_ul),
    context, .s2_implementations[["operational_risk"]])
}

#' Select only additional paragraph1(e) analysis; paragraph1(d) remains.
#'
#' Group-level analysis is not an automatic exemption: the supervisor must
#' consider it under paragraph9. Approval/request/classification are external.
#' @param reasoned_supervisory_request Reasoned supervisory request. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param small_non_complex Small non complex. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param prior_article29d_approval Prior article29d approval. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("orsa-macro-true-true-true")
#' result <- do.call(orsa_additional_macro_analysis_required_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
orsa_additional_macro_analysis_required_2027 <- function(reasoned_supervisory_request, small_non_complex, prior_article29d_approval, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "orsa_additional_macro_analysis_required_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("orsa_additional_macro_analysis_required_2027", list(reasoned_supervisory_request = reasoned_supervisory_request, small_non_complex = small_non_complex, prior_article29d_approval = prior_article29d_approval, qualification_reference = qualification_reference),
    context, .s2_implementations[["orsa_additional_macro_analysis_required_2027"]])
}

#' Declared captive eligibility for ORSA frequency, not DR89 simplification.
#'
#' Permitted insureds/beneficiaries are group legal persons or natural persons
#' eligible under group insurance contracts. Their TP share must be STRICTLY
#' below5%. Qualification of persons/contracts and TP allocation is external.
#' @param is_captive Is captive. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param permitted_policyholders Permitted policyholders. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param natural_person_tp_share Natural person tp share. Reference type: `float`.
#' @param no_compulsory_liability No compulsory liability. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("orsa-frequency-captive-boundary")
#' result <- do.call(orsa_biennial_captive_conditions_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
orsa_biennial_captive_conditions_2027 <- function(is_captive, permitted_policyholders, natural_person_tp_share, no_compulsory_liability, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "orsa_biennial_captive_conditions_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("orsa_biennial_captive_conditions_2027", list(is_captive = is_captive, permitted_policyholders = permitted_policyholders, natural_person_tp_share = natural_person_tp_share, no_compulsory_liability = no_compulsory_liability, qualification_reference = qualification_reference),
    context, .s2_implementations[["orsa_biennial_captive_conditions_2027"]])
}

#' Minimum scenario count from externally qualified materiality/SNC status.
#'
#' Zero exempts no undertaking from the paragraph1 materiality assessment.
#' This is a requirement selector, not a finding that scenarios exist.
#' @param material_climate_risk Material climate risk. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param small_non_complex Small non complex. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: count.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("orsa-climate-required-true-true")
#' result <- do.call(orsa_climate_scenario_requirement_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
orsa_climate_scenario_requirement_2027 <- function(material_climate_risk, small_non_complex, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "orsa_climate_scenario_requirement_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("orsa_climate_scenario_requirement_2027", list(material_climate_risk = material_climate_risk, small_non_complex = small_non_complex, qualification_reference = qualification_reference),
    context, .s2_implementations[["orsa_climate_scenario_requirement_2027"]])
}

#' Check required role metadata and maximum intervals, never model adequacy.
#'
#' 'Significantly above' has no invented numeric cutoff: both >2 degrees and
#' an explicit external qualification are required. Additional scenarios can
#' remain in the external assessment; this input describes the two roles.
#' Intervals are declared years, not dates or an automatic scheduling service.
#' @param scenarios Scenarios. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param impact_analysis_interval_years Impact analysis interval years. Reference type: `float`.
#' @param scenario_review_interval_years Scenario review interval years. Reference type: `float`.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("orsa-climate-low-boundary")
#' result <- do.call(orsa_climate_scenario_structure_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
orsa_climate_scenario_structure_2027 <- function(scenarios, impact_analysis_interval_years, scenario_review_interval_years, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "orsa_climate_scenario_structure_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("orsa_climate_scenario_structure_2027", list(scenarios = scenarios, impact_analysis_interval_years = impact_analysis_interval_years, scenario_review_interval_years = scenario_review_interval_years, assessment_reference = assessment_reference),
    context, .s2_implementations[["orsa_climate_scenario_structure_2027"]])
}

#' Inventory conditional comparisons/recalibration, never calculate a model.
#'
#' The phase-in flag means a comparison remains required after externally
#' qualified currency exemptions. Its false value is not inferred from
#' missing data. Comparability evidence is technical traceability, not a
#' prescribed projection method or an isolated-scenario requirement.
#' @param adjustments Adjustments. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param uses_internal_model Uses internal model. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("orsa-conditional-baseline-ma-missing")
#' result <- do.call(orsa_conditional_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
orsa_conditional_evidence_check <- function(adjustments, uses_internal_model, evidence, qualification_reference, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "orsa_conditional_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("orsa_conditional_evidence_check", list(adjustments = adjustments, uses_internal_model = uses_internal_model, evidence = evidence, qualification_reference = qualification_reference, assessment_reference = assessment_reference),
    context, .s2_implementations[["orsa_conditional_evidence_check"]])
}

#' Inventory explicitly declared core evidence, not adequacy or compliance.
#'
#' AVAILABLE means the caller supplies a reference; content is not inspected.
#' A value of1 certifies neither ORSA completeness nor a capital requirement.
#' Conditional comparisons, timing, recalibration and climate scenarios remain
#' separate. The 2027 table adds mandatory macro/liquidity/materiality topics.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("orsa-evidence-baseline-empty")
#' result <- do.call(orsa_core_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
orsa_core_evidence_check <- function(evidence, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "orsa_core_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("orsa_core_evidence_check", list(evidence = evidence, assessment_reference = assessment_reference),
    context, .s2_implementations[["orsa_core_evidence_check"]])
}

#' Inventory archived guideline evidence, not content or legal applicability.
#'
#' Group/entity applicability and conditional needs are externally qualified.
#' Incompatible scope flags are rejected. The future table explicitly excludes
#' archived blanket annual frequency. Future frequency and expanded group scope
#' are handled by separate APIs, not approved by this inventory.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("orsa-guidance-future-ordinary-empty")
#' result <- do.call(orsa_guidance_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
orsa_guidance_evidence_check <- function(evidence, conditions, qualification_reference, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "orsa_guidance_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("orsa_guidance_evidence_check", list(evidence = evidence, conditions = conditions, qualification_reference = qualification_reference, assessment_reference = assessment_reference),
    context, .s2_implementations[["orsa_guidance_evidence_check"]])
}

#' Maximum periodic interval; an event-triggered ORSA remains immediate.
#'
#' The reference must cover classifications AND supervisory directions.
#' None declares no additional shorter supervisory interval, not ignorance.
#' No calendar deadline, timely completion or supervisory approval is inferred.
#' A separate29d/327e non-SNC approval can enable the reduced maximum;
#' classification remains false and the approval is not inferred from size.
#' @param small_non_complex Small non complex. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualifying_captive Qualifying captive. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param supervisory_interval_years Supervisory interval years. Reference type: `float | None`.
#' @param non_snc_approval_reference Non snc approval reference. Reference type: `str | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: years.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("orsa-frequency-interval-true-true")
#' result <- do.call(orsa_max_assessment_interval_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
orsa_max_assessment_interval_2027 <- function(small_non_complex, qualifying_captive, supervisory_interval_years = NULL, non_snc_approval_reference = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "orsa_max_assessment_interval_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("orsa_max_assessment_interval_2027", list(small_non_complex = small_non_complex, qualifying_captive = qualifying_captive, supervisory_interval_years = supervisory_interval_years, non_snc_approval_reference = non_snc_approval_reference, qualification_reference = qualification_reference),
    context, .s2_implementations[["orsa_max_assessment_interval_2027"]])
}

#' Screen only the Article77a phase-in comparison exemption per currency.
#'
#' Both denominators are ALL future cashflows. Do not substitute cashflows
#' in this currency for the denominator of the extrapolated-share test.
#' Signed raw cashflows need externally qualified aggregation, not abs/netting
#' guessed by the engine. No exemption for MA, VA or other transition tests.
#' @param currency_cashflows Currency cashflows. Reference type: `float`.
#' @param extrapolated_currency_cashflows Extrapolated currency cashflows. Reference type: `float`.
#' @param total_cashflows Total cashflows. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("orsa-phase-in-five")
#' result <- do.call(orsa_phase_in_currency_exemption_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
orsa_phase_in_currency_exemption_2027 <- function(currency_cashflows, extrapolated_currency_cashflows, total_cashflows, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "orsa_phase_in_currency_exemption_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("orsa_phase_in_currency_exemption_2027", list(currency_cashflows = currency_cashflows, extrapolated_currency_cashflows = extrapolated_currency_cashflows, total_cashflows = total_cashflows, qualification_reference = qualification_reference),
    context, .s2_implementations[["orsa_phase_in_currency_exemption_2027"]])
}

#' Inventory report content/transmission evidence, not the report itself.
#'
#' Quantification of non-reflected risks is conditional on externally assessed
#' significance. Future electronic form also requires machine readability and
#' searchable text/figures. No format inspection, filing or deadline approval.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param significant_uncovered_risk_deviation Significant uncovered risk deviation. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("orsa-report-baseline-submission-missing")
#' result <- do.call(orsa_report_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
orsa_report_evidence_check <- function(evidence, significant_uncovered_risk_deviation, qualification_reference, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "orsa_report_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("orsa_report_evidence_check", list(evidence = evidence, significant_uncovered_risk_deviation = significant_uncovered_risk_deviation, qualification_reference = qualification_reference, assessment_reference = assessment_reference),
    context, .s2_implementations[["orsa_report_evidence_check"]])
}

#' All five AnnexXII groups, next12month earned gross premiums.
#'
#' Classify obligations and exclusions externally, including the conditional
#' extended-warranty exclusion in group3. Groups1/2 combine before squaring.
#' @param group_premiums Group premiums. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param classification_reference Classification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("cat-premium-2027-joint")
#' result <- do.call(other_nonlife_cat_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
other_nonlife_cat_loss <- function(group_premiums, classification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "other_nonlife_cat_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("other_nonlife_cat_loss", list(group_premiums = group_premiums, classification_reference = classification_reference),
    context, .s2_implementations[["other_nonlife_cat_loss"]])
}

#' Compose Article88 amounts before further deductions and eligibility limits.
#'
#' Excess must still include own shares; using an already adjusted surplus
#' here would double-deduct them. This is not eligible own funds or Tier1.
#' @param excess_assets_over_liabilities Excess assets over liabilities. Reference type: `float`.
#' @param subordinated_liabilities Subordinated liabilities. Reference type: `float`.
#' @param own_shares Own shares. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-of-basic-zero")
#' result <- do.call(own_funds_basic_before_adjustments,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_basic_before_adjustments <- function(excess_assets_over_liabilities, subordinated_liabilities, own_shares, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_basic_before_adjustments", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_basic_before_adjustments", list(excess_assets_over_liabilities = excess_assets_over_liabilities, subordinated_liabilities = subordinated_liabilities, own_shares = own_shares, qualification_reference = qualification_reference),
    context, .s2_implementations[["own_funds_basic_before_adjustments"]])
}

#' Inventory declared classification evidence, never classify an instrument.
#'
#' References identify the applicable instrument/subparagraph, justified
#' non-applicability and any approved exception. Periods describe those routes;
#' they are not unconditional maturity tests or permission to repay. Tier1
#' quality in excess of its cap may follow73(1)(j), not ordinary Tier2 terms.
#' Values for subsequent allocation remain independently qualified inputs.
#' @param evidence Evidence. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param tier Tier. Reference type: `str`.
#' @param own_funds_kind Own funds kind. Reference type: `str`.
#' @param unlisted Unlisted. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2026-10-05",
#'   known_at = "2026-10-06",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("of-class-close-baseline-basic-tier1-listed-missing")
#' result <- do.call(own_funds_classification_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_classification_evidence_check <- function(evidence, tier, own_funds_kind, unlisted, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_classification_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_classification_evidence_check", list(evidence = evidence, tier = tier, own_funds_kind = own_funds_kind, unlisted = unlisted, assessment_reference = assessment_reference),
    context, .s2_implementations[["own_funds_classification_evidence_check"]])
}

#' Select a qualified annual payout amount, in the context reporting currency.
#'
#' @param policy_usable Policy usable. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param policy_lower Policy lower. Reference type: `float | None`.
#' @param policy_upper Policy upper. Reference type: `float | None`.
#' @param historical_values Historical values. Reference type: `_NumericHistory | None`.
#' @param public_announcements Public announcements. Reference type: `_NumericHistory | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("distribution-amount-policy")
#' result <- do.call(own_funds_distribution_policy_amount,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_distribution_policy_amount <- function(policy_usable, qualification_reference, context, policy_lower = NULL, policy_upper = NULL, historical_values = NULL, public_announcements = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_distribution_policy_amount", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_distribution_policy_amount", list(policy_usable = policy_usable, qualification_reference = qualification_reference, policy_lower = policy_lower, policy_upper = policy_upper, historical_values = historical_values, public_announcements = public_announcements),
    context, .s2_implementations[["own_funds_distribution_policy_amount"]])
}

#' Select a qualified payout ratio; no invented cap at 100 percent.
#'
#' @param policy_usable Policy usable. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param policy_lower Policy lower. Reference type: `float | None`.
#' @param policy_upper Policy upper. Reference type: `float | None`.
#' @param historical_values Historical values. Reference type: `_NumericHistory | None`.
#' @param public_announcements Public announcements. Reference type: `_NumericHistory | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("distribution-ratio-policy-3")
#' result <- do.call(own_funds_distribution_policy_ratio,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_distribution_policy_ratio <- function(policy_usable, qualification_reference, context, policy_lower = NULL, policy_upper = NULL, historical_values = NULL, public_announcements = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_distribution_policy_ratio", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_distribution_policy_ratio", list(policy_usable = policy_usable, qualification_reference = qualification_reference, policy_lower = policy_lower, policy_upper = policy_upper, historical_values = historical_values, public_announcements = public_announcements),
    context, .s2_implementations[["own_funds_distribution_policy_ratio"]])
}

#' Signed fee-scenario difference for one qualified index/unit-linked scope.
#'
#' Both input valuations exclude risk margin. No transfer of the premium-only
#' grouping clause to fees, and no automatic addition of fee profit to EPIFP.
#' @param with_future_fees With future fees. Reference type: `float`.
#' @param without_future_fees Without future fees. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-profit-future-fee-zero")
#' result <- do.call(own_funds_expected_future_fee_profit_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_expected_future_fee_profit_2027 <- function(with_future_fees, without_future_fees, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_expected_future_fee_profit_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_expected_future_fee_profit_2027", list(with_future_fees = with_future_fees, without_future_fees = without_future_fees, qualification_reference = qualification_reference),
    context, .s2_implementations[["own_funds_expected_future_fee_profit_2027"]])
}

#' Aggregate without-minus-with valuations in externally qualified groups.
#'
#' Each row has exactly with_future_premiums and without_future_premiums,
#' both WITHOUT risk margin. Historical losses cannot offset other groups;
#' the published 2027 rule requires signed cross-group netting. No invented
#' total zero floor, insurer valuation model or additional own-funds credit.
#' @param group_valuations Group valuations. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("future-profit-baseline-zero")
#' result <- do.call(own_funds_expected_future_premium_profit,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_expected_future_premium_profit <- function(group_valuations, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_expected_future_premium_profit", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_expected_future_premium_profit", list(group_valuations = group_valuations, qualification_reference = qualification_reference),
    context, .s2_implementations[["own_funds_expected_future_premium_profit"]])
}

#' Select a remaining own-fund bucket after externally qualified deductions.
#'
#' All available amounts precede Article68 deductions. The deduction amounts
#' already reflect version-specific exemption/approval decisions. This does not
#' classify instruments, grant exemptions or calculate eligible own funds.
#' @param tier Tier. Reference type: `str`.
#' @param available Available. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param deductions Deductions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("of-financial-tier-base-7-tier2")
#' result <- do.call(own_funds_financial_deduction_tier,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_financial_deduction_tier <- function(tier, available, deductions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_financial_deduction_tier", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_financial_deduction_tier", list(tier = tier, available = available, deductions = deductions, qualification_reference = qualification_reference),
    context, .s2_implementations[["own_funds_financial_deduction_tier"]])
}

#' Version-specific declared-prerequisite screen, never approval of an exemption.
#'
#' Unknown necessary facts block the decision. A proved alternative can settle
#' an OR without inventing facts. The 2027 branch separately requires declared
#' actual supervisory permission; evidence remains externally qualified.
#' @param facts Facts. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param approval_reference Approval reference. Reference type: `str | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("of-exemption-base-unknown-alternative")
#' result <- do.call(own_funds_financial_exemption_screen,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_financial_exemption_screen <- function(facts, qualification_reference, context, approval_reference = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_financial_exemption_screen", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_financial_exemption_screen", list(facts = facts, qualification_reference = qualification_reference, approval_reference = approval_reference),
    context, .s2_implementations[["own_funds_financial_exemption_screen"]])
}

#' Deduct qualified nonexempt financial participations, with per-holding audit.
#'
#' Each input is a complete participation, not an arbitrary instrument slice.
#' Article68(3) exemptions and direct/indirect ownership are qualified before
#' calling. SRC-0135 Guideline5 / SRC-0067 paragraph26 require the basis
#' BEFORE Article68 deductions; exemptions are not inherited across versions.
#' Deduction amounts here are not yet allocated to own-funds tiers.
#' @param participations Participations. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param threshold_basis Threshold basis. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("of-financial-zero-basis")
#' result <- do.call(own_funds_financial_participation_deduction,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_financial_participation_deduction <- function(participations, threshold_basis, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_financial_participation_deduction", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_financial_participation_deduction", list(participations = participations, threshold_basis = threshold_basis, qualification_reference = qualification_reference),
    context, .s2_implementations[["own_funds_financial_participation_deduction"]])
}

#' Formal amount, or one of two permitted accrual methods before that stage.
#'
#' Formal decisions/proposals, accounting profit and fiscal-year fraction are
#' qualified externally. No internal profit model or day-count is invented.
#' Fees under Article70(1)(b) are separate, not calculated by this function.
#' @param method Method. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param formal_amount Formal amount. Reference type: `float | None`.
#' @param prior_year_distributions Prior year distributions. Reference type: `float | None`.
#' @param payout_ratio Payout ratio. Reference type: `float | None`.
#' @param cumulative_interim_profit Cumulative interim profit. Reference type: `float | None`.
#' @param estimated_annual_distribution Estimated annual distribution. Reference type: `float | None`.
#' @param elapsed_year_fraction Elapsed year fraction. Reference type: `float | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("distribution-foreseeable-formal0")
#' result <- do.call(own_funds_foreseeable_distributions_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_foreseeable_distributions_2027 <- function(method, qualification_reference, context, formal_amount = NULL, prior_year_distributions = NULL, payout_ratio = NULL, cumulative_interim_profit = NULL, estimated_annual_distribution = NULL, elapsed_year_fraction = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_foreseeable_distributions_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_foreseeable_distributions_2027", list(method = method, qualification_reference = qualification_reference, formal_amount = formal_amount, prior_year_distributions = prior_year_distributions, payout_ratio = payout_ratio, cumulative_interim_profit = cumulative_interim_profit, estimated_annual_distribution = estimated_annual_distribution, elapsed_year_fraction = elapsed_year_fraction),
    context, .s2_implementations[["own_funds_foreseeable_distributions_2027"]])
}

#' Validate one proposed full MCR-covering allocation of basic own funds.
#'
#' @param mcr Mcr. Reference type: `float`.
#' @param basic_tier1 Basic tier1. Reference type: `float`.
#' @param restricted_tier1 Restricted tier1. Reference type: `float`.
#' @param basic_tier2 Basic tier2. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("of-2027-mcr-limits")
#' result <- do.call(own_funds_mcr_allocation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_mcr_allocation <- function(mcr, basic_tier1, restricted_tier1, basic_tier2, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_mcr_allocation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_mcr_allocation", list(mcr = mcr, basic_tier1 = basic_tier1, restricted_tier1 = restricted_tier1, basic_tier2 = basic_tier2),
    context, .s2_implementations[["own_funds_mcr_allocation"]])
}

#' Subtract Article70 items, with explicit financial-participation overlap.
#'
#' Future-premium expected profit is already in the supplied surplus, not an
#' extra addition. A signed reserve is preserved; no Tier1 eligibility is
#' inferred from this arithmetic. All deduction categories must be supplied.
#' @param excess_assets_over_liabilities Excess assets over liabilities. Reference type: `float`.
#' @param deductions Deductions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("of-reserve-negative-excess")
#' result <- do.call(own_funds_reconciliation_reserve,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_reconciliation_reserve <- function(excess_assets_over_liabilities, deductions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_reconciliation_reserve", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_reconciliation_reserve", list(excess_assets_over_liabilities = excess_assets_over_liabilities, deductions = deductions, qualification_reference = qualification_reference),
    context, .s2_implementations[["own_funds_reconciliation_reserve"]])
}

#' Deduct restricted surplus, or the full elected immaterial RFF amount.
#'
#' Restricted funds already exclude future shareholder transfers. Materiality,
#' portfolio classification and notional SCR are externally qualified; no
#' internal model or arbitrary materiality threshold is supplied here.
#' @param restricted_own_funds Restricted own funds. Reference type: `float`.
#' @param portfolio_kind Portfolio kind. Reference type: `str`.
#' @param immaterial_simplification Immaterial simplification. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param notional_scr Notional scr. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("of-2027-restricted-immaterial")
#' result <- do.call(own_funds_restricted_deduction,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_restricted_deduction <- function(restricted_own_funds, portfolio_kind, immaterial_simplification, notional_scr = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_restricted_deduction", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_restricted_deduction", list(restricted_own_funds = restricted_own_funds, portfolio_kind = portfolio_kind, immaterial_simplification = immaterial_simplification, notional_scr = notional_scr, qualification_reference = qualification_reference),
    context, .s2_implementations[["own_funds_restricted_deduction"]])
}

#' Validate one proposed full SCR-covering allocation of preclassified tiers.
#'
#' @param scr Scr. Reference type: `float`.
#' @param tier1 Tier1. Reference type: `float`.
#' @param restricted_tier1 Restricted tier1. Reference type: `float`.
#' @param tier2 Tier2. Reference type: `float`.
#' @param tier3 Tier3. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("of-scr-exact-half")
#' result <- do.call(own_funds_scr_allocation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_scr_allocation <- function(scr, tier1, restricted_tier1, tier2, tier3, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_scr_allocation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_scr_allocation", list(scr = scr, tier1 = tier1, restricted_tier1 = restricted_tier1, tier2 = tier2, tier3 = tier3),
    context, .s2_implementations[["own_funds_scr_allocation"]])
}

#' Art73(4)/77(4) maximum modest one-off step-up, not whole-tier eligibility.
#'
#' Decimal rate units:100bp=.01. The original-to-stepped-index swap spread is
#' subtracted in BOTH branches; a negative limit is retained, never floored.
#' @param original_credit_spread Original credit spread. Reference type: `float`.
#' @param swap_spread Swap spread. Reference type: `float`.
#' @param tier Tier. Reference type: `str`.
#' @param one_off_coupon_increase_with_call_option One off coupon increase with call option. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("final-numeric-baseline-step-tier2-negative-limit")
#' result <- do.call(own_funds_step_up_limit,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_step_up_limit <- function(original_credit_spread, swap_spread, tier, one_off_coupon_increase_with_call_option, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_step_up_limit", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_step_up_limit", list(original_credit_spread = original_credit_spread, swap_spread = swap_spread, tier = tier, one_off_coupon_increase_with_call_option = one_off_coupon_increase_with_call_option, qualification_reference = qualification_reference),
    context, .s2_implementations[["own_funds_step_up_limit"]])
}

#' Article71(8) event screen, NOT write-down/conversion or Tier1 eligibility.
#'
#' Use distinct SCR/MCR eligible funds. Equality at either threshold triggers.
#' The three-month non-restoration history and any additional contractual event
#' are externally established; a current recovery does not erase a past event.
#' Article71(10) waiver may affect execution, never the fact of the event.
#' @param eligible_scr_own_funds Eligible scr own funds. Reference type: `float`.
#' @param scr Scr. Reference type: `float`.
#' @param eligible_mcr_own_funds Eligible mcr own funds. Reference type: `float`.
#' @param mcr Mcr. Reference type: `float`.
#' @param scr_not_restored_within_period Scr not restored within period. Reference type: `bool | None`.
#' @param additional_contractual_trigger Additional contractual trigger. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("tier1-trigger-baseline-decidable-or")
#' result <- do.call(own_funds_tier1_loss_absorption_trigger,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_tier1_loss_absorption_trigger <- function(eligible_scr_own_funds, scr, eligible_mcr_own_funds, mcr, scr_not_restored_within_period, additional_contractual_trigger, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_tier1_loss_absorption_trigger", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_tier1_loss_absorption_trigger", list(eligible_scr_own_funds = eligible_scr_own_funds, scr = scr, eligible_mcr_own_funds = eligible_mcr_own_funds, mcr = mcr, scr_not_restored_within_period = scr_not_restored_within_period, additional_contractual_trigger = additional_contractual_trigger, qualification_reference = qualification_reference),
    context, .s2_implementations[["own_funds_tier1_loss_absorption_trigger"]])
}

#' Select statutory execution requirements; no instrument amount or booking.
#'
#' a/b requires full absorption. c permits partial restoration or at-least-linear
#' absorption, unless the narrowly qualified supervisory exception is declared.
#' Contract-only additional triggers have no universal execution rule here.
#' @param eligible_scr_own_funds Eligible scr own funds. Reference type: `float`.
#' @param scr Scr. Reference type: `float`.
#' @param eligible_mcr_own_funds Eligible mcr own funds. Reference type: `float`.
#' @param mcr Mcr. Reference type: `float`.
#' @param scr_not_restored_within_period Scr not restored within period. Reference type: `bool | None`.
#' @param additional_contractual_trigger Additional contractual trigger. Reference type: `bool | None`.
#' @param mechanism Mechanism. Reference type: `str`.
#' @param partial_restoration_sufficient Partial restoration sufficient. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param waiver_conditions Waiver conditions. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param waiver_reference Waiver reference. Reference type: `str | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("tier1-mechanism-baseline-write_down-partial")
#' result <- do.call(own_funds_tier1_mechanism_requirements,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
own_funds_tier1_mechanism_requirements <- function(eligible_scr_own_funds, scr, eligible_mcr_own_funds, mcr, scr_not_restored_within_period, additional_contractual_trigger, mechanism, partial_restoration_sufficient, qualification_reference, context, waiver_conditions = NULL, waiver_reference = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "own_funds_tier1_mechanism_requirements", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("own_funds_tier1_mechanism_requirements", list(eligible_scr_own_funds = eligible_scr_own_funds, scr = scr, eligible_mcr_own_funds = eligible_mcr_own_funds, mcr = mcr, scr_not_restored_within_period = scr_not_restored_within_period, additional_contractual_trigger = additional_contractual_trigger, mechanism = mechanism, partial_restoration_sufficient = partial_restoration_sufficient, qualification_reference = qualification_reference, waiver_conditions = waiver_conditions, waiver_reference = waiver_reference),
    context, .s2_implementations[["own_funds_tier1_mechanism_requirements"]])
}

#' Annex XVIII techniques1-5, with recursive4/5 and certified external2.
#'
#' Model charges and scalar module inputs are externally qualified. Nested inputs
#' use the same technique, with no intangible charge below the outermost BSCR.
#'     Technique2 verifies a PSD dual certificate of constrained maximisation;
#'     it does not develop or run an insurer model or an optimisation solver.
#' @param technique Technique. Reference type: `int`.
#' @param inputs Inputs. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param intangible_charge Intangible charge. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("partial-integration-baseline-3")
#' result <- do.call(partial_model_bscr,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
partial_model_bscr <- function(technique, inputs, intangible_charge, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "partial_model_bscr", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("partial_model_bscr", list(technique = technique, inputs = inputs, intangible_charge = intangible_charge, qualification_reference = qualification_reference),
    context, .s2_implementations[["partial_model_bscr"]])
}

#' Validate explicit zero-treatment allocation, not guarantee pricing or capital.
#'
#' Nominal first-loss coverage and the externally determined guarantee value
#' are distinct inputs. Pool allocation is chosen by the analyst, never here.
#' Article215(f) payment coverage remains part of external qualification.
#' @param positions Positions. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param category Category. Reference type: `str`.
#' @param first_loss_cover First loss cover. Reference type: `float`.
#' @param guarantee_value Guarantee value. Reference type: `float`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param allocation_reference Allocation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("partial-public-guarantee-single")
#' result <- do.call(partial_public_guarantee_zero_amount_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
partial_public_guarantee_zero_amount_2027 <- function(positions, category, first_loss_cover, guarantee_value, conditions, qualification_reference, allocation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "partial_public_guarantee_zero_amount_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("partial_public_guarantee_zero_amount_2027", list(positions = positions, category = category, first_loss_cover = first_loss_cover, guarantee_value = guarantee_value, conditions = conditions, qualification_reference = qualification_reference, allocation_reference = allocation_reference),
    context, .s2_implementations[["partial_public_guarantee_zero_amount_2027"]])
}

#' Sum one selected net premium PV component, without long-term weighting.
#'
#' Month coordinates encode externally qualified calendar boundaries. Premium
#' rows must be split at relevant earning cutoffs; payment time is not earning
#' time. Input PVs already include expectations, eligible netting and discounting.
#' The downstream premium-volume APIs apply the30% long-contract factor once.
#' @param contracts Contracts. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param component Component. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("premium-future-baseline-empty-future_long_pv")
#' result <- do.call(premium_future_component,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
premium_future_component <- function(contracts, component, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "premium_future_component", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("premium_future_component", list(contracts = contracts, component = component, qualification_reference = qualification_reference),
    context, .s2_implementations[["premium_future_component"]])
}

#' Return signed net premiums, not recoverables, total volume or capital.
#'
#' All values refer to the same externally qualified earning/PV/currency basis.
#' Unknown qualifications cannot silently become either deductions or exclusions.
#' Excluded amounts are retained by row, not interpreted as zero contracts.
#' finite_reinsurance_ids declares finite AND economically similar transfers;
#' the default empty sequence explicitly qualifies all rows as non-finite.
#' @param gross_premium Gross premium. Reference type: `float`.
#' @param reinsurance_premiums Reinsurance premiums. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param finite_reinsurance_ids Finite reinsurance ids. Reference type: `list[str] | tuple`. Supply an ordered R list or the documented numeric vector.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("premium-netting-baseline-empty")
#' result <- do.call(premium_net_of_reinsurance,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
premium_net_of_reinsurance <- function(gross_premium, reinsurance_premiums, qualification_reference, finite_reinsurance_ids = list(), context, ...) {
  .s2_require(length(list(...)) == 0L, "premium_net_of_reinsurance", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("premium_net_of_reinsurance", list(gross_premium = gross_premium, reinsurance_premiums = reinsurance_premiums, qualification_reference = qualification_reference, finite_reinsurance_ids = finite_reinsurance_ids),
    context, .s2_implementations[["premium_net_of_reinsurance"]])
}

#' Geographic factor from already classified volumes, including mandatory exceptions.
#'
#' Regional maxima/floors must be calculated regionally: their sum need not
#' equal the separately calculated segment volume used by the risk function.
#' This function does not classify countries or validate USP authorisation.
#' @param risk_type Risk type. Reference type: `str`.
#' @param segment Segment. Reference type: `str`.
#' @param method Method. Reference type: `str`.
#' @param uses_usp Uses usp. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param regional_volumes Regional volumes. Reference type: `Mapping[str, dict] | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("geo-div-explicit-default")
#' result <- do.call(premium_reserve_diversification,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
premium_reserve_diversification <- function(risk_type, segment, method, uses_usp, regional_volumes = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "premium_reserve_diversification", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("premium_reserve_diversification", list(risk_type = risk_type, segment = segment, method = method, uses_usp = uses_usp, regional_volumes = regional_volumes),
    context, .s2_implementations[["premium_reserve_diversification"]])
}

#' Return the macroregion for an explicitly qualified original territory name.
#'
#' No translations/current-name aliases or sovereign-to-overseas shortcuts are
#' inferred. For a parent country, its printed exclusions must already have
#' been excluded from the caller's risk-location input; they remain in audit.
#' This lookup neither identifies a NatCat zone nor calculates regional volumes.
#' @param territory Territory. Reference type: `str`.
#' @param risk_location_reference Risk location reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: region_ordinal.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("macroregion-baseline-territory-054")
#' result <- do.call(premium_reserve_geographic_region,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
premium_reserve_geographic_region <- function(territory, risk_location_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "premium_reserve_geographic_region", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("premium_reserve_geographic_region", list(territory = territory, risk_location_reference = risk_location_reference),
    context, .s2_implementations[["premium_reserve_geographic_region"]])
}

#' Aggregate supplied effective sigmas, without repeating NP/ADC adjustments.
#'
#' Parameter admissibility, USP approval, HRES determination and sequencing
#' are external. Each row declares whether AnnexIII's USP-sigma restriction
#' applies; the engine enforces that numeric restriction, not its truth.
#' All12 nonlife or4 health-NSLT segments must be explicit, even when empty.
#' Existing standard-parameter APIs remain the default automatic selectors.
#' @param risk_type Risk type. Reference type: `str`.
#' @param segments Segments. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param parameter_reference Parameter reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("parameterised-pr-baseline-health_nslt-usp")
#' result <- do.call(premium_reserve_parameterised_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
premium_reserve_parameterised_risk <- function(risk_type, segments, parameter_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "premium_reserve_parameterised_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("premium_reserve_parameterised_risk", list(risk_type = risk_type, segments = segments, parameter_reference = parameter_reference),
    context, .s2_implementations[["premium_reserve_parameterised_risk"]])
}

#' Nonlife sigma aggregation with explicit2027 reinsurance/USP qualification.
#'
#' Future premium80% factors require a declared existing nonproportional cover
#' in segments1/4/5. Reserve factors are supplied separately; no absence of ADC
#' is inferred from missing input. Historical callers keep the original API.
#' @param segments Segments. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param premium_nonproportional_cover Premium nonproportional cover. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param reserve_adjustments Reserve adjustments. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param adjustment_reference Adjustment reference. Reference type: `str | None`.
#' @param uses_usp Uses usp. Reference type: `bool | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("nl-segment-one")
#' result <- do.call(premium_reserve_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
premium_reserve_risk <- function(segments, premium_nonproportional_cover = NULL, reserve_adjustments = NULL, adjustment_reference = NULL, uses_usp = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "premium_reserve_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("premium_reserve_risk", list(segments = segments, premium_nonproportional_cover = premium_nonproportional_cover, reserve_adjustments = reserve_adjustments, adjustment_reference = adjustment_reference, uses_usp = uses_usp),
    context, .s2_implementations[["premium_reserve_risk"]])
}

#' Stress an externally identified property asset, not total BOF or property SCR.
#'
#' @param property_value Property value. Reference type: `float`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("property-value-0")
#' result <- do.call(property_value_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
property_value_stress <- function(property_value, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "property_value_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("property_value_stress", list(property_value = property_value, valuation_reference = valuation_reference),
    context, .s2_implementations[["property_value_stress"]])
}

#' Application/notification inventory, separate from annual size screens.
#'
#' The qualification reference identifies the actual measure, classification,
#' specific approval/conditions or pre-existing2025 transitional entitlement.
#' Metadata exposes authority periods without calculating a decision deadline
#' or manufacturing approval through silence. Fiscal-year counting is external.
#' @param evidence Evidence. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param route Route. Reference type: `str`.
#' @param measure_list_changed Measure list changed. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param measure_ceased Measure ceased. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param supervisory_restriction Supervisory restriction. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-close-prop-snc-missing")
#' result <- do.call(proportionality_application_evidence_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
proportionality_application_evidence_2027 <- function(evidence, route, measure_list_changed, measure_ceased, supervisory_restriction, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "proportionality_application_evidence_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("proportionality_application_evidence_2027", list(evidence = evidence, route = route, measure_list_changed = measure_list_changed, measure_ceased = measure_ceased, supervisory_restriction = supervisory_restriction, qualification_reference = qualification_reference),
    context, .s2_implementations[["proportionality_application_evidence_2027"]])
}

#' BE_det + loading*SCR, with default5% or externally demonstrated loading.
#'
#' The insurer supplies BE, SCR, scenario-based time value and the Article34a(3)
#' SCR/LAC-TP treatment. No stress valuation or circular SCR solver is invented.
#' Negative deterministic/net BE is preserved; no unsupported100% loading cap.
#' @param deterministic_best_estimate Deterministic best estimate. Reference type: `float`.
#' @param time_value_options Time value options. Reference type: `float`.
#' @param undertaking_scr Undertaking scr. Reference type: `float`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param route Route. Reference type: `str`.
#' @param scenario_reference Scenario reference. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param alternative_loading Alternative loading. Reference type: `float | None`.
#' @param alternative_loading_demonstrated Alternative loading demonstrated. Reference type: `bool | None`.
#' @param alternative_loading_reference Alternative loading reference. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param approval_evidence Approval evidence. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("prudent-snc-negative-be")
#' result <- do.call(prudent_deterministic_best_estimate_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
prudent_deterministic_best_estimate_2027 <- function(deterministic_best_estimate, time_value_options, undertaking_scr, conditions, route, scenario_reference, qualification_reference, alternative_loading = NULL, alternative_loading_demonstrated = NULL, alternative_loading_reference = NULL, context, approval_evidence = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "prudent_deterministic_best_estimate_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("prudent_deterministic_best_estimate_2027", list(deterministic_best_estimate = deterministic_best_estimate, time_value_options = time_value_options, undertaking_scr = undertaking_scr, conditions = conditions, route = route, scenario_reference = scenario_reference, qualification_reference = qualification_reference, alternative_loading = alternative_loading, alternative_loading_demonstrated = alternative_loading_demonstrated, alternative_loading_reference = alternative_loading_reference, approval_evidence = approval_evidence),
    context, .s2_implementations[["prudent_deterministic_best_estimate_2027"]])
}

#' Strict5% time-value screen plus externally qualified route conditions.
#'
#' SNC requires written method intention; non-SNC requires existing approval
#' and current conditions instead. This never grants supervisory approval or
#' computes option time value from an invented scenario set.
#' @param time_value_options Time value options. Reference type: `float`.
#' @param undertaking_scr Undertaking scr. Reference type: `float`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param route Route. Reference type: `str`.
#' @param scenario_reference Scenario reference. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param approval_evidence Approval evidence. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("prudent-snc-boundary")
#' result <- do.call(prudent_deterministic_eligibility_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
prudent_deterministic_eligibility_2027 <- function(time_value_options, undertaking_scr, conditions, route, scenario_reference, qualification_reference, context, approval_evidence = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "prudent_deterministic_eligibility_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("prudent_deterministic_eligibility_2027", list(time_value_options = time_value_options, undertaking_scr = undertaking_scr, conditions = conditions, route = route, scenario_reference = scenario_reference, qualification_reference = qualification_reference, approval_evidence = approval_evidence),
    context, .s2_implementations[["prudent_deterministic_eligibility_2027"]])
}

#' Only Article327f size/market conditions, NOT supervisory approval.
#'
#' Use EUR inputs for the absolute threshold. Market shares require the proper
#' home-state denominators: life gross TP versus nonlife gross written premiums.
#' Pure nonlife undertakings do not automatically face the life-size cap;
#' mixed undertakings do only at or above20% life gross TP. Any waiver is an
#' explicit external supervisory finding under paragraph3, never inferred here.
#' @param undertaking_type Undertaking type. Reference type: `str`.
#' @param life_gross_technical_provisions Life gross technical provisions. Reference type: `float`.
#' @param total_gross_technical_provisions Total gross technical provisions. Reference type: `float`.
#' @param market_shares Market shares. Reference type: `Mapping[str, float | None]`. Supply a named R list with the keys shown in the example/reference case.
#' @param size_exception_approved Size exception approved. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param classification_reference Classification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("prudent-size-waived")
#' result <- do.call(prudent_deterministic_non_snc_size_screen_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
prudent_deterministic_non_snc_size_screen_2027 <- function(undertaking_type, life_gross_technical_provisions, total_gross_technical_provisions, market_shares, size_exception_approved, classification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "prudent_deterministic_non_snc_size_screen_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("prudent_deterministic_non_snc_size_screen_2027", list(undertaking_type = undertaking_type, life_gross_technical_provisions = life_gross_technical_provisions, total_gross_technical_provisions = total_gross_technical_provisions, market_shares = market_shares, size_exception_approved = size_exception_approved, classification_reference = classification_reference),
    context, .s2_implementations[["prudent_deterministic_non_snc_size_screen_2027"]])
}

#' Article215a equivalence gate, not guarantee valuation or zero capital.
#'
#' Only the directness requirement is waived for the counter-guarantee;
#' original guarantee requirements and all other counter-guarantee conditions
#' remain. Any downstream Article180/192 treatment needs its own scope checks.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-close-counter-all")
#' result <- do.call(public_counter_guarantee_conditions_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
public_counter_guarantee_conditions_2027 <- function(conditions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "public_counter_guarantee_conditions_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("public_counter_guarantee_conditions_2027", list(conditions = conditions, qualification_reference = qualification_reference),
    context, .s2_implementations[["public_counter_guarantee_conditions_2027"]])
}

#' Shared public-factor rule; Art180 debt scope and Art187 scope remain distinct.
#'
#' Public-body/list classification and underlying guarantee assessment are
#' external evidence, not conclusions drawn from these supplied booleans.
#' The future-only308b(12) category concerns the position's inception, not
#' merely the security's issue date; it does not extend public guarantees.
#' @param category Category. Reference type: `str`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-public-zero-ecb")
#' result <- do.call(public_exposure_zero_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
public_exposure_zero_factor <- function(category, conditions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "public_exposure_zero_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("public_exposure_zero_factor", list(category = category, conditions = conditions, qualification_reference = qualification_reference),
    context, .s2_implementations[["public_exposure_zero_factor"]])
}

#' Select declared RSR periodicity without waiving other reporting duties.
#'
#' None explicitly declares no additional supervisory decision. Classification
#' and authenticity of instructions/approval remain external qualifications.
#' non_snc_approval_reference enables the separately approved29d/327b route;
#' a size-screen result is never substituted for this supervisory decision.
#' @param small_non_complex Small non complex. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param supervisory_interval_years Supervisory interval years. Reference type: `float | None`.
#' @param supervisory_reference Supervisory reference. Reference type: `str | None`.
#' @param non_snc_approval_reference Non snc approval reference. Reference type: `str | None`.
#' @param scope Scope. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: years.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("group-close-baseline-rsr-default")
#' result <- do.call(regular_supervisory_report_interval,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
regular_supervisory_report_interval <- function(small_non_complex, supervisory_interval_years = NULL, supervisory_reference = NULL, non_snc_approval_reference = NULL, scope = "solo", qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "regular_supervisory_report_interval", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("regular_supervisory_report_interval", list(small_non_complex = small_non_complex, supervisory_interval_years = supervisory_interval_years, supervisory_reference = supervisory_reference, non_snc_approval_reference = non_snc_approval_reference, scope = scope, qualification_reference = qualification_reference),
    context, .s2_implementations[["regular_supervisory_report_interval"]])
}

#' Original EUR base times HICP change since31Dec2015, rounded upward.
#'
#' Review schedule, Eurostat aggregate, original base and last publication are
#' qualified inputs. The output is a calculated proposal; it never changes
#' the existing ruleset's MCR/scope parameters or asserts national adoption.
#' @param base_amount Base amount. Reference type: `float`.
#' @param last_published_amount Last published amount. Reference type: `float`.
#' @param cumulative_inflation Cumulative inflation. Reference type: `float`.
#' @param inflation_since_last_adjustment Inflation since last adjustment. Reference type: `float`.
#' @param review_due Review due. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: EUR.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("group-close-baseline-index-exact-grid")
#' result <- do.call(regulatory_euro_amount_indexation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
regulatory_euro_amount_indexation <- function(base_amount, last_published_amount, cumulative_inflation, inflation_since_last_adjustment, review_due, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "regulatory_euro_amount_indexation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("regulatory_euro_amount_indexation", list(base_amount = base_amount, last_published_amount = last_published_amount, cumulative_inflation = cumulative_inflation, inflation_since_last_adjustment = inflation_since_last_adjustment, review_due = review_due, qualification_reference = qualification_reference),
    context, .s2_implementations[["regulatory_euro_amount_indexation"]])
}

#' Subtract the separately calculated EXPECTED default adjustment exactly once.
#'
#' Values must already share counterparty/LoB/contract/provision scope, date,
#' currency and qualified valuation basis. This does not compute PD, reapply
#' an LGD floor, net gross TP, or choose a deposit/SPV-cap ordering.
#' A signed negative result is not automatically a recognised asset.
#' @param recoverables_before_default Recoverables before default. Reference type: `float`.
#' @param expected_default_adjustment Expected default adjustment. Reference type: `float`.
#' @param reconciliation_reference Reconciliation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("ri-reconciliation-baseline-zero")
#' result <- do.call(reinsurance_adjusted_recoverable,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
reinsurance_adjusted_recoverable <- function(recoverables_before_default, expected_default_adjustment, reconciliation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "reinsurance_adjusted_recoverable", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("reinsurance_adjusted_recoverable", list(recoverables_before_default = recoverables_before_default, expected_default_adjustment = expected_default_adjustment, reconciliation_reference = reconciliation_reference),
    context, .s2_implementations[["reinsurance_adjusted_recoverable"]])
}

#' Prescribed211 eligibility/haircut; not PD, LGD or recoverable valuation.
#'
#' Coverage is SCR/local-equivalent solvency coverage for the insurer route,
#' or assets/equivalent eligible amount divided by aggregate maximum SPV
#' exposure. Solvency dates, equivalence, legal status and credible restoration
#' evidence are external. Months are completed regulatory calendar periods;
#' at the expiry of6/3months an unresolved shortfall receives zero recognition.
#' Non-equivalent insurers use the separate CQS route, not an invented SCR.
#' @param counterparty_type Counterparty type. Reference type: `str`.
#' @param coverage_ratio Coverage ratio. Reference type: `float | None`.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param months_since_breach Months since breach. Reference type: `float | None`.
#' @param credit_quality_step Credit quality step. Reference type: `int | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("counterparty-close-baseline-spv-two")
#' result <- do.call(reinsurance_counterparty_recognition_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
reinsurance_counterparty_recognition_factor <- function(counterparty_type, coverage_ratio, conditions, months_since_breach = NULL, credit_quality_step = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "reinsurance_counterparty_recognition_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("reinsurance_counterparty_recognition_factor", list(counterparty_type = counterparty_type, coverage_ratio = coverage_ratio, conditions = conditions, months_since_breach = months_since_breach, credit_quality_step = credit_quality_step, qualification_reference = qualification_reference),
    context, .s2_implementations[["reinsurance_counterparty_recognition_factor"]])
}

#' PV of already probability-weighted shortfalls; positive result is a deduction.
#'
#' Payment-date differences may be negative (e.g. delayed receipts), but a
#' negative total needs review. Default times, PD, litigation and collateral
#' effects must already be in the external projection: never multiply PD twice.
#' @param cashflows Cashflows. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param partition Partition. Reference type: `Mapping[str, str]`. Supply a named R list with the keys shown in the example/reference case.
#' @param runoff_complete Runoff complete. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param projection_reference Projection reference. Reference type: `str`.
#' @param curve_reference Curve reference. Reference type: `str`.
#' @param noncollateral_mitigation_included Noncollateral mitigation included. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param separate_noncollateral_mitigation Separate noncollateral mitigation. Reference type: `Mapping[str, dict] | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-ri-adjustment-zero")
#' result <- do.call(reinsurance_default_adjustment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
reinsurance_default_adjustment <- function(cashflows, partition, runoff_complete, projection_reference, curve_reference, noncollateral_mitigation_included, context, separate_noncollateral_mitigation = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "reinsurance_default_adjustment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("reinsurance_default_adjustment", list(cashflows = cashflows, partition = partition, runoff_complete = runoff_complete, projection_reference = projection_reference, curve_reference = curve_reference, noncollateral_mitigation_included = noncollateral_mitigation_included, separate_noncollateral_mitigation = separate_noncollateral_mitigation),
    context, .s2_implementations[["reinsurance_default_adjustment"]])
}

#' Apply the loss-given-default floor or an explicitly evidenced alternative.
#'
#' The minimum is not a point estimate and is not a probability of default.
#' This result must not be used as the full expected default adjustment: its
#' timing, probabilities and segmentation require a separate calculation.
#' @param recoverables_before_adjustment Recoverables before adjustment. Reference type: `float`.
#' @param estimated_loss Estimated loss. Reference type: `float`.
#' @param method Method. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-ri-loss-zero")
#' result <- do.call(reinsurance_default_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
reinsurance_default_loss <- function(recoverables_before_adjustment, estimated_loss, method, valuation_reference = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "reinsurance_default_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("reinsurance_default_loss", list(recoverables_before_adjustment = recoverables_before_adjustment, estimated_loss = estimated_loss, method = method, valuation_reference = valuation_reference),
    context, .s2_implementations[["reinsurance_default_loss"]])
}

#' Allocate already eligible payments using their external gross-claims linkage.
#'
#' False means premium only after prior eligibility under Article 41: it is
#' not permission to include settled claims, unrelated payments or deposits.
#' Call separately for each date and homogeneous counterparty/LoB/type slice.
#' @param payments Payments. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param target_provision Target provision. Reference type: `str`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-ri-allocation-zero")
#' result <- do.call(reinsurance_nonlife_allocation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
reinsurance_nonlife_allocation <- function(payments, target_provision, eligibility_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "reinsurance_nonlife_allocation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("reinsurance_nonlife_allocation", list(payments = payments, target_provision = target_provision, eligibility_reference = eligibility_reference),
    context, .s2_implementations[["reinsurance_nonlife_allocation"]])
}

#' Discount externally selected expected recoveries at their own payment dates.
#'
#' Signed values remain signed. No gross-claim date substitution, default
#' adjustment, SPV cap or claim/deposit qualification is performed implicitly.
#' @param cashflows Cashflows. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param partition Partition. Reference type: `Mapping[str, str]`. Supply a named R list with the keys shown in the example/reference case.
#' @param runoff_complete Runoff complete. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param projection_reference Projection reference. Reference type: `str`.
#' @param curve_reference Curve reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-ri-cashflow-zero")
#' result <- do.call(reinsurance_recoverable_cashflows,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
reinsurance_recoverable_cashflows <- function(cashflows, partition, runoff_complete, projection_reference, curve_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "reinsurance_recoverable_cashflows", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("reinsurance_recoverable_cashflows", list(cashflows = cashflows, partition = partition, runoff_complete = runoff_complete, projection_reference = projection_reference, curve_reference = curve_reference),
    context, .s2_implementations[["reinsurance_recoverable_cashflows"]])
}

#' Article57 signed gross-minus-net difference BEFORE expected default loss.
#'
#' The externally qualified net BE is based on homogeneous groups, each with
#' at most one treaty/SPV unless homogeneous risks are transferred. Both BEs
#' share scope/date/basis. Neither supplied BE includes the counterparty loss
#' adjustment; apply the separate default-adjustment component afterwards.
#' @param gross_best_estimate Gross best estimate. Reference type: `float`.
#' @param unadjusted_net_best_estimate Unadjusted net best estimate. Reference type: `float`.
#' @param proportionality_satisfied Proportionality satisfied. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2026-10-05",
#'   known_at = "2026-10-06",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("net-recovery-close-baseline-zero")
#' result <- do.call(reinsurance_recoverables_from_net,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
reinsurance_recoverables_from_net <- function(gross_best_estimate, unadjusted_net_best_estimate, proportionality_satisfied, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "reinsurance_recoverables_from_net", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("reinsurance_recoverables_from_net", list(gross_best_estimate = gross_best_estimate, unadjusted_net_best_estimate = unadjusted_net_best_estimate, proportionality_satisfied = proportionality_satisfied, qualification_reference = qualification_reference),
    context, .s2_implementations[["reinsurance_recoverables_from_net"]])
}

#' Market-value arithmetic with mandatory exclusions, not replication approval.
#'
#' Features concern the selected cashflows. References to all-scenario and
#' market evidence are retained but their substantive adequacy is external.
#' @param market_values Market values. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param cashflow_features Cashflow features. Reference type: `Mapping[str, bool | None]`. Supply a named R list with the keys shown in the example/reference case.
#' @param replication_reference Replication reference. Reference type: `str | None`.
#' @param market_reference Market reference. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-replication-zero-value")
#' result <- do.call(replicated_cashflow_value,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
replicated_cashflow_value <- function(market_values, cashflow_features, replication_reference = NULL, market_reference = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "replicated_cashflow_value", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("replicated_cashflow_value", list(market_values = market_values, cashflow_features = cashflow_features, replication_reference = replication_reference, market_reference = market_reference),
    context, .s2_implementations[["replicated_cashflow_value"]])
}

#' Declared Article35a(3) eligibility only for subannual itemised reporting.
#'
#' Policyholder/beneficiary scope and underlying reinsurance contracts must
#' already be qualified. Neither ORSA eligibility nor a general waiver is inferred.
#' @param is_captive Is captive. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param permitted_policyholders Permitted policyholders. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param natural_person_tp_share Natural person tp share. Reference type: `float`.
#' @param no_compulsory_liability No compulsory liability. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param reporting_interval_years Reporting interval years. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("reporting-captive-natural_person_tp_share-0")
#' result <- do.call(reporting_captive_itemised_exemption_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
reporting_captive_itemised_exemption_2027 <- function(is_captive, permitted_policyholders, natural_person_tp_share, no_compulsory_liability, reporting_interval_years, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "reporting_captive_itemised_exemption_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("reporting_captive_itemised_exemption_2027", list(is_captive = is_captive, permitted_policyholders = permitted_policyholders, natural_person_tp_share = natural_person_tp_share, no_compulsory_liability = no_compulsory_liability, reporting_interval_years = reporting_interval_years, qualification_reference = qualification_reference),
    context, .s2_implementations[["reporting_captive_itemised_exemption_2027"]])
}

#' One portfolio's nonnegative charge under the COMMON worst net scenario.
#'
#' Nested losses are portfolio -> scenario -> signed BOF loss, already adjusted
#' for217(5) profit participation. A gain is negative and offsets losses when
#' selecting the whole-undertaking scenario, never after independent choices.
#' net includes qualified FDB absorption; gross and net retain the same scenario.
#' @param portfolio_kinds Portfolio kinds. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param gross_losses Gross losses. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param net_losses Net losses. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param target_portfolio Target portfolio. Reference type: `str`.
#' @param article217_scope_confirmed Article217 scope confirmed. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param view View. Reference type: `str`.
#' @param tie_break Tie break. Reference type: `str | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("restricted-portfolio-baseline-common")
#' result <- do.call(restricted_portfolio_scenario_charge,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
restricted_portfolio_scenario_charge <- function(portfolio_kinds, gross_losses, net_losses, target_portfolio, article217_scope_confirmed, qualification_reference, context, view = "gross", tie_break = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "restricted_portfolio_scenario_charge", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("restricted_portfolio_scenario_charge", list(portfolio_kinds = portfolio_kinds, gross_losses = gross_losses, net_losses = net_losses, target_portfolio = target_portfolio, article217_scope_confirmed = article217_scope_confirmed, qualification_reference = qualification_reference, view = view, tie_break = tie_break),
    context, .s2_implementations[["restricted_portfolio_scenario_charge"]])
}

#' Sum complete qualified notional SCRs, including remainder; no diversification.
#'
#' @param notional_scr Notional scr. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param portfolio_kinds Portfolio kinds. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param article217_scope_confirmed Article217 scope confirmed. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("restricted-portfolio-baseline-zero")
#' result <- do.call(restricted_portfolio_scr_sum,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
restricted_portfolio_scr_sum <- function(notional_scr, portfolio_kinds, article217_scope_confirmed, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "restricted_portfolio_scr_sum", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("restricted_portfolio_scr_sum", list(notional_scr = notional_scr, portfolio_kinds = portfolio_kinds, article217_scope_confirmed = article217_scope_confirmed, qualification_reference = qualification_reference),
    context, .s2_implementations[["restricted_portfolio_scr_sum"]])
}

#' Read the default probability, NOT the similarly named PD credit spread.
#'
#' @param series_id Series id. Reference type: `str`.
#' @param sector Sector. Reference type: `str`.
#' @param maturity_years Maturity years. Reference type: `float`.
#' @param credit_quality_step Credit quality step. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param market_data Market data. Reference type: `dict | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-31",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("synthetic-market-pd-financial")
#' result <- do.call(rfr_archived_corporate_pd,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_archived_corporate_pd <- function(series_id, sector, maturity_years, credit_quality_step, qualification_reference, context, market_data = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_archived_corporate_pd", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_archived_corporate_pd", list(series_id = series_id, sector = sector, maturity_years = maturity_years, credit_quality_step = credit_quality_step, qualification_reference = qualification_reference, market_data = market_data),
    context, .s2_implementations[["rfr_archived_corporate_pd"]])
}

#' Read published PD/FS/downgrade spreads, converting percent to decimal rates.
#'
#' Exact published tenors only: no interpolation or automatic extension beyond
#' year 30. Classification and applicability remain externally qualified.
#' @param series_id Series id. Reference type: `str`.
#' @param sector Sector. Reference type: `str`.
#' @param maturity_years Maturity years. Reference type: `float`.
#' @param credit_quality_step Credit quality step. Reference type: `float`.
#' @param table Table. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param market_data Market data. Reference type: `dict | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-31",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("synthetic-market-pd_spread-financial")
#' result <- do.call(rfr_archived_corporate_spread,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_archived_corporate_spread <- function(series_id, sector, maturity_years, credit_quality_step, table, qualification_reference, context, market_data = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_archived_corporate_spread", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_archived_corporate_spread", list(series_id = series_id, sector = sector, maturity_years = maturity_years, credit_quality_step = credit_quality_step, table = table, qualification_reference = qualification_reference, market_data = market_data),
    context, .s2_implementations[["rfr_archived_corporate_spread"]])
}

#' Read a published government LTAS/FS parameter and convert percent to decimal.
#'
#' Country is the published parameter axis, including the EUR aggregate; it is
#' not a currency inference or automatic asset classification. Do not transfer
#' spot-curve currency metadata blocks to this distinct government-table axis.
#' @param country_id Country id. Reference type: `str`.
#' @param maturity_years Maturity years. Reference type: `float`.
#' @param table Table. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param market_data Market data. Reference type: `dict | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-31",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("synthetic-market-government-ltas")
#' result <- do.call(rfr_archived_government_spread,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_archived_government_spread <- function(country_id, maturity_years, table, qualification_reference, context, market_data = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_archived_government_spread", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_archived_government_spread", list(country_id = country_id, maturity_years = maturity_years, table = table, qualification_reference = qualification_reference, market_data = market_data),
    context, .s2_implementations[["rfr_archived_government_spread"]])
}

#' Read supplemental LTAS cells with explicit original row identifiers.
#'
#' Corporate, country-relative basic-RFR and named-index axes are different;
#' none is automatically assigned from context currency. The specific index
#' scalar has no maturity axis. Qualification remains external.
#' @param table Table. Reference type: `str`.
#' @param series_id Series id. Reference type: `str`.
#' @param maturity_years Maturity years. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param market_data Market data. Reference type: `dict | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-31",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("synthetic-market-specific_ltas")
#' result <- do.call(rfr_archived_ltas,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_archived_ltas <- function(table, series_id, maturity_years = NULL, qualification_reference, context, market_data = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_archived_ltas", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_archived_ltas", list(table = table, series_id = series_id, maturity_years = maturity_years, qualification_reference = qualification_reference, market_data = market_data),
    context, .s2_implementations[["rfr_archived_ltas"]])
}

#' Read one published annual zero-coupon rate, preserving cell provenance.
#'
#' Archive retrieval date is a conservative knowledge bound for this exact file
#' revision, not an inferred original publication date. Raw currency matches are
#' necessary but not sufficient qualification; known anomalies remain blocked.
#' @param series_id Series id. Reference type: `str`.
#' @param maturity_years Maturity years. Reference type: `float`.
#' @param variant Variant. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param market_data Market data. Reference type: `dict | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-31",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("synthetic-market-spot-no_va")
#' result <- do.call(rfr_archived_spot_rate,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_archived_spot_rate <- function(series_id, maturity_years, variant, qualification_reference, context, market_data = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_archived_spot_rate", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_archived_spot_rate", list(series_id = series_id, maturity_years = maturity_years, variant = variant, qualification_reference = qualification_reference, market_data = market_data),
    context, .s2_implementations[["rfr_archived_spot_rate"]])
}

#' Bound half the externally qualified one-year mean spread (decimal rates).
#'
#' The mean's observation calendar, market quality and maturity matching are
#' external inputs. This component does not bootstrap or extrapolate a curve.
#' Under the 2027 Article44 rule this arithmetic is only for qualified non-OIS
#' swaps requiring adjustment; the external reference must establish that scope.
#' @param annual_mean_spread Annual mean spread. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-rfr-cra-2")
#' result <- do.call(rfr_credit_risk_adjustment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_credit_risk_adjustment <- function(annual_mean_spread, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_credit_risk_adjustment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_credit_risk_adjustment", list(annual_mean_spread = annual_mean_spread, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_credit_risk_adjustment"]])
}

#' CSSR from separately qualified PVBP amounts, not their revaluation model.
#'
#' @param asset_pvbp Asset pvbp. Reference type: `float | None`.
#' @param liability_pvbp Liability pvbp. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param reuse Reuse. Reference type: `dict | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-cssr-0")
#' result <- do.call(rfr_credit_spread_sensitivity,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_credit_spread_sensitivity <- function(asset_pvbp, liability_pvbp, qualification_reference, context, reuse = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_credit_spread_sensitivity", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_credit_spread_sensitivity", list(asset_pvbp = asset_pvbp, liability_pvbp = liability_pvbp, qualification_reference = qualification_reference, reuse = reuse),
    context, .s2_implementations[["rfr_credit_spread_sensitivity"]])
}

#' Signed CURA: archived uniform value or a qualified formula illustration.
#'
#' EIOPA-BoS-24-533 8.3.2 / EIOPA-BoS-26-198 7.3.2. A company's formula
#' result is NOT the uniform adjustment published for all undertakings.
#' Qualification includes Article48(1), currency/date and input convention.
#' In particular, lac_ratio is the source ratio, not a negative LAC amount.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param calibration Calibration. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "DKK",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rfr-cura-baseline-published-dkk")
#' result <- do.call(rfr_currency_risk_adjustment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_currency_risk_adjustment <- function(qualification_reference, context, calibration = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_currency_risk_adjustment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_currency_risk_adjustment", list(qualification_reference = qualification_reference, calibration = calibration),
    context, .s2_implementations[["rfr_currency_risk_adjustment"]])
}

#' Build a basic/VA/MA curve once, including all requested output maturities.
#'
#' Market rates are already CRA/CURA-adjusted and selected under external
#' DLT, FSP/RVC and missing-data/previous-trading-day qualification. Keys are
#' canonical positive integer years. VA is an annual decimal rate already
#' rounded to whole basis points; its continuous equivalent enters LLFR.
#' This is a numerical engine component, not an official publication.
#' @param market_rates Market rates. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param first_smoothing_point First smoothing point. Reference type: `float`.
#' @param maturity Maturity. Reference type: `float`.
#' @param instrument_kind Instrument kind. Reference type: `str`.
#' @param coupon_frequency Coupon frequency. Reference type: `int | None`.
#' @param average_notionals Average notionals. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ufr Ufr. Reference type: `float`.
#' @param phase_in Phase in. Reference type: `bool | None`.
#' @param approval_reference Approval reference. Reference type: `str | None`.
#' @param volatility_adjustment Volatility adjustment. Reference type: `float | None`.
#' @param matching_adjustment Matching adjustment. Reference type: `float | None`.
#' @param interest_stress Interest stress. Reference type: `str | None`.
#' @param output_maturities Output maturities. Reference type: `documented input`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("rfr-curve-2027-flat")
#' result <- do.call(rfr_curve_spot_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_curve_spot_2027 <- function(market_rates, first_smoothing_point, maturity, instrument_kind, coupon_frequency = NULL, average_notionals = NULL, ufr, phase_in, approval_reference = NULL, volatility_adjustment = NULL, matching_adjustment = NULL, interest_stress = NULL, output_maturities = list(), qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_curve_spot_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_curve_spot_2027", list(market_rates = market_rates, first_smoothing_point = first_smoothing_point, maturity = maturity, instrument_kind = instrument_kind, coupon_frequency = coupon_frequency, average_notionals = average_notionals, ufr = ufr, phase_in = phase_in, approval_reference = approval_reference, volatility_adjustment = volatility_adjustment, matching_adjustment = matching_adjustment, interest_stress = interest_stress, output_maturities = output_maturities, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_curve_spot_2027"]])
}

#' De-risk one asset cashflow under EIOPA-BoS-24-533 section 12.5.9.
#'
#' The probability must match this cashflow's horizon; it is not a credit
#' spread. Do not apply this twice to already adjusted cashflows. This does
#' not project cashflows, select eligible assets or authorize matching.
#' @param cashflow Cashflow. Reference type: `float`.
#' @param default_probability Default probability. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rfr-derisk-zero-pd")
#' result <- do.call(rfr_default_adjusted_cashflow,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_default_adjusted_cashflow <- function(cashflow, default_probability, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_default_adjusted_cashflow", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_default_adjusted_cashflow", list(cashflow = cashflow, default_probability = default_probability, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_default_adjusted_cashflow"]])
}

#' Loss conditional on default under the fundamental-spread recovery assumption.
#'
#' @param market_value Market value. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-rfr-default-loss-2")
#' result <- do.call(rfr_default_loss_on_market_value,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_default_loss_on_market_value <- function(market_value, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_default_loss_on_market_value", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_default_loss_on_market_value", list(market_value = market_value, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_default_loss_on_market_value"]])
}

#' Weight qualified immediate replacement losses, not a rating/migration model.
#'
#' Distinct downgrade states share a horizon and valuation basis. Residual
#' probability is no downgrade, with no downgrade loss; never renormalize.
#' The result is a currency amount, NOT a spread or a market-value percentage.
#' @param scenarios Scenarios. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rfr-downgrade-baseline-zero")
#' result <- do.call(rfr_downgrade_expected_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_downgrade_expected_loss <- function(scenarios, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_downgrade_expected_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_downgrade_expected_loss", list(scenarios = scenarios, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_downgrade_expected_loss"]])
}

#' Solve the unique annual rate for nonnegative externally qualified cashflows.
#'
#' Times are supplied year fractions, not an implicit day-count convention.
#' Mixed signs need external review because uniqueness is not guaranteed.
#' Numerical tolerances below are implementation choices, not legal parameters.
#' @param cashflows Cashflows. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param present_value Present value. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-rfr-rate-negative")
#' result <- do.call(rfr_equivalent_annual_rate,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_equivalent_annual_rate <- function(cashflows, present_value, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_equivalent_annual_rate", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_equivalent_annual_rate", list(cashflows = cashflows, present_value = present_value, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_equivalent_annual_rate"]])
}

#' Return the continuous forward from annual-discrete UFR and continuous LLFR.
#'
#' LLFR determination, UFR publication and VA treatment are external. Alpha
#' comes from the profile, not an untracked arbitrary caller coefficient.
#' @param ufr Ufr. Reference type: `float`.
#' @param last_liquid_forward Last liquid forward. Reference type: `float`.
#' @param time_after_fsp Time after fsp. Reference type: `float`.
#' @param phase_in Phase in. Reference type: `bool | None`.
#' @param approval_reference Approval reference. Reference type: `str | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-extrap-forward-zero")
#' result <- do.call(rfr_extrapolated_forward_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_extrapolated_forward_2027 <- function(ufr, last_liquid_forward, time_after_fsp, phase_in, approval_reference = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_extrapolated_forward_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_extrapolated_forward_2027", list(ufr = ufr, last_liquid_forward = last_liquid_forward, time_after_fsp = time_after_fsp, phase_in = phase_in, approval_reference = approval_reference, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_extrapolated_forward_2027"]])
}

#' Convert the FSP spot and a matching continuous forward to an annual spot.
#'
#' Scaled time weights avoid overflow. This API is strictly beyond FSP; it
#' neither selects FSP nor adds VA/MA automatically.
#' @param fsp_years Fsp years. Reference type: `float`.
#' @param spot_at_fsp Spot at fsp. Reference type: `float`.
#' @param time_after_fsp Time after fsp. Reference type: `float`.
#' @param extrapolated_forward Extrapolated forward. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-extrap-spot-zero")
#' result <- do.call(rfr_extrapolated_spot_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_extrapolated_spot_2027 <- function(fsp_years, spot_at_fsp, time_after_fsp, extrapolated_forward, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_extrapolated_spot_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_extrapolated_spot_2027", list(fsp_years = fsp_years, spot_at_fsp = spot_at_fsp, time_after_fsp = time_after_fsp, extrapolated_forward = extrapolated_forward, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_extrapolated_spot_2027"]])
}

#' Own versioned alpha, with annual approved transition and no post-2032 trend.
#'
#' @param phase_in Phase in. Reference type: `bool | None`.
#' @param approval_reference Approval reference. Reference type: `str | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-extrap-alpha-EUR-final")
#' result <- do.call(rfr_extrapolation_alpha_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_extrapolation_alpha_2027 <- function(phase_in, approval_reference = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_extrapolation_alpha_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_extrapolation_alpha_2027", list(phase_in = phase_in, approval_reference = approval_reference, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_extrapolation_alpha_2027"]])
}

#' Prescribed percentage, not a market-data calibration or an FSP maturity.
#'
#' Initial EUR minimum is qualified at28Jan2025 for20years. A EUR source-change
#' minimum is qualified at the first new-source date against the previous
#' calendar-year FSP. A next-lower preserving value is externally identified;
#' Article43a(2) does not justify inventing a market selection algorithm here.
#' Other-currency initial LLP is qualified at29Jan2027, not today's maturity.
#' @param minimum_euro_share Minimum euro share. Reference type: `float`.
#' @param calibration Calibration. Reference type: `str`.
#' @param last_liquid_maturity_2027 Last liquid maturity 2027. Reference type: `float | None`.
#' @param rounded_preserves_fsp Rounded preserves fsp. Reference type: `bool | None`.
#' @param lower_preserving_share Lower preserving share. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("closing-rules-fsp-zero")
#' result <- do.call(rfr_fsp_currency_percentage_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_fsp_currency_percentage_2027 <- function(minimum_euro_share, calibration = "initial", last_liquid_maturity_2027 = NULL, rounded_preserves_fsp = NULL, lower_preserving_share = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_fsp_currency_percentage_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_fsp_currency_percentage_2027", list(minimum_euro_share = minimum_euro_share, calibration = calibration, last_liquid_maturity_2027 = last_liquid_maturity_2027, rounded_preserves_fsp = rounded_preserves_fsp, lower_preserving_share = lower_preserving_share, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_fsp_currency_percentage_2027"]])
}

#' Compose external credit spreads with the class floor or exact fallback.
#'
#' eu_central means EU member-state central governments/central banks, not
#' all public-sector entities. Statistics and 30-year history qualification
#' remain external; this does not derive PDs or downgrade replacement losses.
#' @param long_term_average_spread Long term average spread. Reference type: `float`.
#' @param asset_class Asset class. Reference type: `str`.
#' @param default_statistics_reliable Default statistics reliable. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param default_spread Default spread. Reference type: `float | None`.
#' @param downgrade_spread Downgrade spread. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-rfr-fs-6")
#' result <- do.call(rfr_fundamental_spread,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_fundamental_spread <- function(long_term_average_spread, asset_class, default_statistics_reliable, default_spread = NULL, downgrade_spread = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_fundamental_spread", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_fundamental_spread", list(long_term_average_spread = long_term_average_spread, asset_class = asset_class, default_statistics_reliable = default_statistics_reliable, default_spread = default_spread, downgrade_spread = downgrade_spread, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_fundamental_spread"]])
}

#' Published corporate residual-FS rule: max(CoD, FS-PD), decimal rates.
#'
#' Requires matching externally qualified sector/CQS/tenor inputs. Not an
#' automatic residual selection for every possible matching portfolio.
#' @param fundamental_spread Fundamental spread. Reference type: `float`.
#' @param default_spread Default spread. Reference type: `float`.
#' @param downgrade_spread Downgrade spread. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rfr-rest-fs-zero")
#' result <- do.call(rfr_fundamental_spread_other_than_pd,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_fundamental_spread_other_than_pd <- function(fundamental_spread, default_spread, downgrade_spread, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_fundamental_spread_other_than_pd", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_fundamental_spread_other_than_pd", list(fundamental_spread = fundamental_spread, default_spread = default_spread, downgrade_spread = downgrade_spread, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_fundamental_spread_other_than_pd"]])
}

#' Aggregate externally qualified continuous forwards using swap notionals.
#'
#' Component a is the one-year forward immediately preceding FSP in the
#' SRC-0211 construction; components b run
#' from FSP to later liquid maturities. Only a receives the explicit VA.
#' Market selection, forward derivation and VA qualification are external.
#' Here volatility_adjustment is a CONTINUOUS forward increment; the
#' integrated rfr_curve_spot_2027 converts annual VA using log1p first.
#' @param fsp_forward Fsp forward. Reference type: `float`.
#' @param later_forwards Later forwards. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param liquid_swaps_beyond_fsp Liquid swaps beyond fsp. Reference type: `bool | None`.
#' @param fsp_average_notional Fsp average notional. Reference type: `float | None`.
#' @param apply_volatility_adjustment Apply volatility adjustment. Reference type: `bool | None`.
#' @param volatility_adjustment Volatility adjustment. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-llfr-fallback")
#' result <- do.call(rfr_last_liquid_forward_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_last_liquid_forward_2027 <- function(fsp_forward, later_forwards, liquid_swaps_beyond_fsp, fsp_average_notional = NULL, apply_volatility_adjustment, volatility_adjustment = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_last_liquid_forward_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_last_liquid_forward_2027", list(fsp_forward = fsp_forward, later_forwards = later_forwards, liquid_swaps_beyond_fsp = liquid_swaps_beyond_fsp, fsp_average_notional = fsp_average_notional, apply_volatility_adjustment = apply_volatility_adjustment, volatility_adjustment = volatility_adjustment, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_last_liquid_forward_2027"]])
}

#' Country debt/all-assets share, not an insurer's own portfolio weight.
#'
#' @param country_risk_corrected_spread Country risk corrected spread. Reference type: `float`.
#' @param debt_asset_share Debt asset share. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-country-factor-3")
#' result <- do.call(rfr_macro_country_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_macro_country_factor <- function(country_risk_corrected_spread, debt_asset_share, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_macro_country_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_macro_country_factor", list(country_risk_corrected_spread = country_risk_corrected_spread, debt_asset_share = debt_asset_share, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_macro_country_factor"]])
}

#' Euro macro add-on; explicitly incompatible with undertaking-specific adjustment.
#'
#' @param country_risk_corrected_spread Country risk corrected spread. Reference type: `float`.
#' @param euro_risk_corrected_spread Euro risk corrected spread. Reference type: `float`.
#' @param credit_spread_sensitivity Credit spread sensitivity. Reference type: `float`.
#' @param country_factor Country factor. Reference type: `float`.
#' @param undertaking_specific_adjustment_applies Undertaking specific adjustment applies. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-macro-va-2")
#' result <- do.call(rfr_macro_volatility_adjustment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_macro_volatility_adjustment <- function(country_risk_corrected_spread, euro_risk_corrected_spread, credit_spread_sensitivity, country_factor, undertaking_specific_adjustment_applies, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_macro_volatility_adjustment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_macro_volatility_adjustment", list(country_risk_corrected_spread = country_risk_corrected_spread, euro_risk_corrected_spread = euro_risk_corrected_spread, credit_spread_sensitivity = credit_spread_sensitivity, country_factor = country_factor, undertaking_specific_adjustment_applies = undertaking_specific_adjustment_applies, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_macro_volatility_adjustment"]])
}

#' Apply CRA and signed CURA once before existing curve construction.
#'
#' Positive CRA is subtracted; nonpositive CURA is added. No zero floor.
#' Inputs must refer to the same qualified market instrument/tenor/date;
#' this is not a shift to an already extrapolated curve or a VA/MA operation.
#' @param market_rate Market rate. Reference type: `float`.
#' @param credit_adjustment Credit adjustment. Reference type: `float`.
#' @param currency_adjustment Currency adjustment. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "DKK",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rfr-cura-baseline-market-no-adjustments")
#' result <- do.call(rfr_market_rate_adjustment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_market_rate_adjustment <- function(market_rate, credit_adjustment, currency_adjustment, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_market_rate_adjustment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_market_rate_adjustment", list(market_rate = market_rate, credit_adjustment = credit_adjustment, currency_adjustment = currency_adjustment, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_market_rate_adjustment"]])
}

#' Subtract effective annual rates on the SAME liability cashflows and residual FS.
#'
#' The asset-equivalent rate matches their PV to eligible assigned asset value;
#' the liability-equivalent rate matches their PV to the basic-curve BE.
#' The residual FS excludes any part already included in asset cashflow
#' adjustment. Optional declared application/asset comparisons add controlled
#' eligibility and FS limits; without them these aspects stay NOT_ASSESSED.
#' No asset coupon substitution, automatic rating, permission or curve application.
#' @param asset_equivalent_rate Asset equivalent rate. Reference type: `float`.
#' @param liability_equivalent_rate Liability equivalent rate. Reference type: `float`.
#' @param residual_fundamental_spread Residual fundamental spread. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param application Application. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param restructured_assets Restructured assets. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param subinvestment_grade_comparison Subinvestment grade comparison. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-rfr-ma-5")
#' result <- do.call(rfr_matching_adjustment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_matching_adjustment <- function(asset_equivalent_rate, liability_equivalent_rate, residual_fundamental_spread, qualification_reference, context, application = NULL, restructured_assets = NULL, subinvestment_grade_comparison = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_matching_adjustment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_matching_adjustment", list(asset_equivalent_rate = asset_equivalent_rate, liability_equivalent_rate = liability_equivalent_rate, residual_fundamental_spread = residual_fundamental_spread, qualification_reference = qualification_reference, application = application, restructured_assets = restructured_assets, subinvestment_grade_comparison = subinvestment_grade_comparison),
    context, .s2_implementations[["rfr_matching_adjustment"]])
}

#' Headroom against the 5% BE condition, using the externally BOF-selected stress.
#'
#' Nonnegative means this numerical condition passes, not overall MA eligibility.
#' Nonpositive base BE is deliberately referred for review, not reinterpreted.
#' @param base_best_estimate Base best estimate. Reference type: `float`.
#' @param stressed_best_estimate Stressed best estimate. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-rfr-ma-headroom-1")
#' result <- do.call(rfr_matching_mortality_headroom,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_matching_mortality_headroom <- function(base_best_estimate, stressed_best_estimate, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_matching_mortality_headroom", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_matching_mortality_headroom", list(base_best_estimate = base_best_estimate, stressed_best_estimate = stressed_best_estimate, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_matching_mortality_headroom"]])
}

#' Return BE from the worst BOF scenario, not the scenario with maximum BE.
#'
#' Scenario valuations must share a base and currency. Equal BOF with distinct
#' BE values requires explicit selection among the ties; there is no legal
#' worst-BE tie rule invented here. Valuation and overall eligibility stay external.
#' @param scenarios Scenarios. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param selected_scenario Selected scenario. Reference type: `str | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param contract_selection Contract selection. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-rfr-ma-selection-6")
#' result <- do.call(rfr_matching_mortality_selected_be,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_matching_mortality_selected_be <- function(scenarios, selected_scenario = NULL, qualification_reference, context, contract_selection = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_matching_mortality_selected_be", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_matching_mortality_selected_be", list(scenarios = scenarios, selected_scenario = selected_scenario, qualification_reference = qualification_reference, contract_selection = contract_selection),
    context, .s2_implementations[["rfr_matching_mortality_selected_be"]])
}

#' MA qualification stress assumption, not life SCR or automatic scenario selection.
#'
#' @param rate Rate. Reference type: `float`.
#' @param scenario Scenario. Reference type: `str`.
#' @param rate_basis Rate basis. Reference type: `str`.
#' @param selected Selected. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-rfr-ma-mortality-6")
#' result <- do.call(rfr_matching_mortality_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_matching_mortality_stress <- function(rate, scenario, rate_basis, selected, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_matching_mortality_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_matching_mortality_stress", list(rate = rate, scenario = scenario, rate_basis = rate_basis, selected = selected, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_matching_mortality_stress"]])
}

#' Common CSSR from qualified combined amounts, never average individual CSSRs.
#'
#' Peg eligibility and use of the adjusted euro curve under Article48 are
#' external. The currency identifier alone is not evidence of their validity.
#' @param asset_pvbp Asset pvbp. Reference type: `float`.
#' @param liability_pvbp Liability pvbp. Reference type: `float`.
#' @param pegged_currency Pegged currency. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-cssr-pegged-3")
#' result <- do.call(rfr_pegged_credit_spread_sensitivity,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_pegged_credit_spread_sensitivity <- function(asset_pvbp, liability_pvbp, pegged_currency, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_pegged_credit_spread_sensitivity", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_pegged_credit_spread_sensitivity", list(asset_pvbp = asset_pvbp, liability_pvbp = liability_pvbp, pegged_currency = pegged_currency, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_pegged_credit_spread_sensitivity"]])
}

#' Aggregate category corrections under EIOPA-BoS-24-533 section 10.3.5.
#'
#' Weights refer to the whole reference portfolio, not just bonds. Category
#' corrections and classifications remain externally qualified inputs. This
#' published positive-part formula is distinct from calibrating each input.
#' @param government_weight Government weight. Reference type: `float`.
#' @param corporate_weight Corporate weight. Reference type: `float`.
#' @param government_risk_correction Government risk correction. Reference type: `float | None`.
#' @param corporate_risk_correction Corporate risk correction. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param fundamental_spread_inputs Fundamental spread inputs. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rfr-portfolio-rc-4")
#' result <- do.call(rfr_portfolio_risk_correction,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_portfolio_risk_correction <- function(government_weight, corporate_weight, government_risk_correction, corporate_risk_correction, qualification_reference, context, fundamental_spread_inputs = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_portfolio_risk_correction", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_portfolio_risk_correction", list(government_weight = government_weight, corporate_weight = corporate_weight, government_risk_correction = government_risk_correction, corporate_risk_correction = corporate_risk_correction, qualification_reference = qualification_reference, fundamental_spread_inputs = fundamental_spread_inputs),
    context, .s2_implementations[["rfr_portfolio_risk_correction"]])
}

#' Whole-portfolio weights: do not renormalize the two bond categories.
#'
#' @param government_weight Government weight. Reference type: `float`.
#' @param corporate_weight Corporate weight. Reference type: `float`.
#' @param government_spread Government spread. Reference type: `float`.
#' @param corporate_spread Corporate spread. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rfr-refspread-4")
#' result <- do.call(rfr_reference_portfolio_spread,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_reference_portfolio_spread <- function(government_weight, corporate_weight, government_spread, corporate_spread, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_reference_portfolio_spread", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_reference_portfolio_spread", list(government_weight = government_weight, corporate_weight = corporate_weight, government_spread = government_spread, corporate_spread = corporate_spread, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_reference_portfolio_spread"]])
}

#' Subtract an externally qualified risk correction without an output floor.
#'
#' @param spread Spread. Reference type: `float`.
#' @param risk_correction Risk correction. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rfr-riskspread-3")
#' result <- do.call(rfr_risk_corrected_spread,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_risk_corrected_spread <- function(spread, risk_correction, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_risk_corrected_spread", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_risk_corrected_spread", list(spread = spread, risk_correction = risk_correction, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_risk_corrected_spread"]])
}

#' Piecewise correction for a qualified homogeneous spread bucket, decimal rates.
#'
#' EEA sovereign classification, matching duration/quality and 30-year LTAS
#' estimation are external. This is not a choice of portfolio-weight denominator.
#' @param spread Spread. Reference type: `float`.
#' @param long_term_average_spread Long term average spread. Reference type: `float`.
#' @param asset_class Asset class. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-rc-other-0")
#' result <- do.call(rfr_risk_correction_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_risk_correction_2027 <- function(spread, long_term_average_spread, asset_class, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_risk_correction_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_risk_correction_2027", list(spread = spread, long_term_average_spread = long_term_average_spread, asset_class = asset_class, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_risk_correction_2027"]])
}

#' Fit qualified instruments once and return annual spot plus optional grid.
#'
#' Cashflow rows are payment dates, columns are instruments. Prices share
#' their cashflows' nominal basis. Signed coupons are allowed (section9.15).
#' The last payment time is the externally qualified LLP. Qualification
#' includes DLT selection, input availability, dates, CRA and currency basis.
#' Alpha search is the documented heuristic, not a global optimality proof.
#' The technical search bound is explicit and is NOT a regulatory alpha cap.
#' @param payment_times Payment times. Reference type: `documented input`.
#' @param cashflows Cashflows. Reference type: `documented input`.
#' @param prices Prices. Reference type: `documented input`.
#' @param maturity Maturity. Reference type: `documented input`.
#' @param ufr Ufr. Reference type: `documented input`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param output_maturities Output maturities. Reference type: `documented input`.
#' @param alpha_search_max Alpha search max. Reference type: `documented input`.
#' @param volatility_adjustment Volatility adjustment. Reference type: `documented input`.
#' @param matching_adjustment Matching adjustment. Reference type: `documented input`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rfr-smith-wilson-flat-zero")
#' result <- do.call(rfr_smith_wilson_spot,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_smith_wilson_spot <- function(payment_times, cashflows, prices, maturity, ufr, qualification_reference, output_maturities = list(), alpha_search_max = 2.0, volatility_adjustment = NULL, matching_adjustment = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_smith_wilson_spot", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_smith_wilson_spot", list(payment_times = payment_times, cashflows = cashflows, prices = prices, maturity = maturity, ufr = ufr, qualification_reference = qualification_reference, output_maturities = output_maturities, alpha_search_max = alpha_search_max, volatility_adjustment = volatility_adjustment, matching_adjustment = matching_adjustment),
    context, .s2_implementations[["rfr_smith_wilson_spot"]])
}

#' Convert a matching annual zero-coupon spot rate to a discount factor.
#'
#' Year fractions and curve qualification are explicit external inputs; this
#' performs no interpolation, day-count choice or VA/MA eligibility decision.
#' @param spot_rate Spot rate. Reference type: `float`.
#' @param time_years Time years. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("rfr-discount-future-five-two")
#' result <- do.call(rfr_spot_discount_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_spot_discount_factor <- function(spot_rate, time_years, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_spot_discount_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_spot_discount_factor", list(spot_rate = spot_rate, time_years = time_years, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_spot_discount_factor"]])
}

#' Add the approved currency-specific annual transition to one curve point.
#'
#' The equivalent annual rate must value the eligible portfolio on the matching
#' basis including applicable VA. Existing rfr_equivalent_annual_rate can solve
#' nonnegative cashflows; mixed-sign portfolios need qualified external roots.
#' Do not include eligible obligations in VA calculation or combine with MA/308d.
#' @param spot_rate Spot rate. Reference type: `float`.
#' @param legacy_rate Legacy rate. Reference type: `float`.
#' @param equivalent_annual_rate Equivalent annual rate. Reference type: `float`.
#' @param matching_adjustment_used Matching adjustment used. Reference type: `bool | None`.
#' @param technical_provisions_transition_used Technical provisions transition used. Reference type: `bool | None`.
#' @param approval_reference Approval reference. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("ltg-transition-baseline-rate-2025-01-17")
#' result <- do.call(rfr_transitional_spot_rate,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_transitional_spot_rate <- function(spot_rate, legacy_rate, equivalent_annual_rate, matching_adjustment_used, technical_provisions_transition_used, approval_reference, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_transitional_spot_rate", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_transitional_spot_rate", list(spot_rate = spot_rate, legacy_rate = legacy_rate, equivalent_annual_rate = equivalent_annual_rate, matching_adjustment_used = matching_adjustment_used, technical_provisions_transition_used = technical_provisions_transition_used, approval_reference = approval_reference, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_transitional_spot_rate"]])
}

#' Derive the context year's UFR using the prior year's annual calculation.
#'
#' Annual history ends one year before that calculation, hence two years
#' before application. AMECO/OECD source qualification and any no-target
#' ARMA projection remain external. No term premium is added. Fractions
#' preserve decimal grid/threshold boundaries and never fill missing years.
#' @param annual_rates Annual rates. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param previous_real_rate Previous real rate. Reference type: `float`.
#' @param previous_ufr Previous ufr. Reference type: `float`.
#' @param inflation Inflation. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rfr-ufr-complete-history-baseline")
#' result <- do.call(rfr_ultimate_forward_rate,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_ultimate_forward_rate <- function(annual_rates, previous_real_rate, previous_ufr, inflation, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_ultimate_forward_rate", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_ultimate_forward_rate", list(annual_rates = annual_rates, previous_real_rate = previous_real_rate, previous_ufr = previous_ufr, inflation = inflation, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_ultimate_forward_rate"]])
}

#' Literal revaluation difference/VA*, not a valuation model or per-bp rescaling.
#'
#' Qualified asset values exclude immaterial unit-linked spread exposure.
#' Qualified BEL includes future discretionary benefits without feeding back
#' the asset spread shock. Paired values are already in one reporting currency.
#' @param unadjusted_value Unadjusted value. Reference type: `float`.
#' @param adjusted_value Adjusted value. Reference type: `float`.
#' @param notional_va Notional va. Reference type: `float`.
#' @param valuation_kind Valuation kind. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param pegged_currency Pegged currency. Reference type: `str | None`.
#' @param pegged_notional_va Pegged notional va. Reference type: `float | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: currency_per_rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-pvbp-0")
#' result <- do.call(rfr_va_pvbp,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_va_pvbp <- function(unadjusted_value, adjusted_value, notional_va, valuation_kind, qualification_reference, context, pegged_currency = NULL, pegged_notional_va = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_va_pvbp", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_va_pvbp", list(unadjusted_value = unadjusted_value, adjusted_value = adjusted_value, notional_va = notional_va, valuation_kind = valuation_kind, qualification_reference = qualification_reference, pegged_currency = pegged_currency, pegged_notional_va = pegged_notional_va),
    context, .s2_implementations[["rfr_va_pvbp"]])
}

#' Compute VA from risk-corrected spreads; neither authorize nor apply it.
#'
#' Country uplift is added BEFORE the factor and requires both strict tests.
#' Official data, MA exclusion and pre-extrapolation application remain external.
#' @param currency_spread Currency spread. Reference type: `float`.
#' @param country_applicable Country applicable. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param country_spread Country spread. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rfr-va-1")
#' result <- do.call(rfr_volatility_adjustment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_volatility_adjustment <- function(currency_spread, country_applicable, country_spread = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_volatility_adjustment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_volatility_adjustment", list(currency_spread = currency_spread, country_applicable = country_applicable, country_spread = country_spread, qualification_reference = qualification_reference),
    context, .s2_implementations[["rfr_volatility_adjustment"]])
}

#' Currency VA only; no automatic macro addition or undertaking-specific approval.
#'
#' @param risk_corrected_spread Risk corrected spread. Reference type: `float`.
#' @param credit_spread_sensitivity Credit spread sensitivity. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param firm_adjustment Firm adjustment. Reference type: `dict | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: rate.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-va-2")
#' result <- do.call(rfr_volatility_adjustment_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rfr_volatility_adjustment_2027 <- function(risk_corrected_spread, credit_spread_sensitivity, qualification_reference, context, firm_adjustment = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "rfr_volatility_adjustment_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rfr_volatility_adjustment_2027", list(risk_corrected_spread = risk_corrected_spread, credit_spread_sensitivity = credit_spread_sensitivity, qualification_reference = qualification_reference, firm_adjustment = firm_adjustment),
    context, .s2_implementations[["rfr_volatility_adjustment_2027"]])
}

#' Discount SCR(t) of the Art.38 reference undertaking at basic rates t+1.
#'
#' @param projected_scr Projected scr. Reference type: `list[float]`. Supply an ordered R list or the documented numeric vector.
#' @param basic_rates Basic rates. Reference type: `list[float]`. Supply an ordered R list or the documented numeric vector.
#' @param runoff_complete Runoff complete. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("risk-margin-2027-one-year")
#' result <- do.call(risk_margin,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
risk_margin <- function(projected_scr, basic_rates, runoff_complete, context, ...) {
  .s2_require(length(list(...)) == 0L, "risk_margin", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("risk_margin", list(projected_scr = projected_scr, basic_rates = basic_rates, runoff_complete = runoff_complete),
    context, .s2_implementations[["risk_margin"]])
}

#' Future life formula from deterministic annual net cashflows, first at k=1.
#'
#' The supplied rate is the externally qualified IRR under the basic RFR term
#' structure. Signed individual cashflows are allowed; whole-path BE suitability
#' remains an external condition. Lambda is included in adjusted duration ONCE.
#' @param initial_scr Initial scr. Reference type: `float`.
#' @param cashflows Cashflows. Reference type: `list[float]`. Supply an ordered R list or the documented numeric vector.
#' @param internal_rate Internal rate. Reference type: `float`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param basis_reference Basis reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("rm-duration-future-zero-scr")
#' result <- do.call(risk_margin_adjusted_duration_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
risk_margin_adjusted_duration_2027 <- function(initial_scr, cashflows, internal_rate, conditions, eligibility_reference, basis_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "risk_margin_adjusted_duration_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("risk_margin_adjusted_duration_2027", list(initial_scr = initial_scr, cashflows = cashflows, internal_rate = internal_rate, conditions = conditions, eligibility_reference = eligibility_reference, basis_reference = basis_reference),
    context, .s2_implementations[["risk_margin_adjusted_duration_2027"]])
}

#' Baseline life formula including the divisor printed in archived Annex IV.
#'
#' Duration and rate conventions are externally qualified. No curve fitting,
#' runoff model or automatic suitability determination is performed.
#' @param initial_scr Initial scr. Reference type: `float`.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param discount_rate Discount rate. Reference type: `float`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param basis_reference Basis reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rm-duration-baseline-zero-scr")
#' result <- do.call(risk_margin_duration,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
risk_margin_duration <- function(initial_scr, modified_duration, discount_rate, conditions, eligibility_reference, basis_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "risk_margin_duration", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("risk_margin_duration", list(initial_scr = initial_scr, modified_duration = modified_duration, discount_rate = discount_rate, conditions = conditions, eligibility_reference = eligibility_reference, basis_reference = basis_reference),
    context, .s2_implementations[["risk_margin_duration"]])
}

#' Regulatory selection condition only; insurer supplies the approved model.
#'
#' A zero result does not approve another method. Suitability covers EVERY
#' runoff time; no such finding is inferred from one successful valuation.
#' @param uses_approved_internal_model Uses approved internal model. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param suitable_all_runoff_times Suitable all runoff times. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rm-model-baseline-required")
#' result <- do.call(risk_margin_internal_model_required,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
risk_margin_internal_model_required <- function(uses_approved_internal_model, suitable_all_runoff_times, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "risk_margin_internal_model_required", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("risk_margin_internal_model_required", list(uses_approved_internal_model = uses_approved_internal_model, suitable_all_runoff_times = suitable_all_runoff_times, qualification_reference = qualification_reference),
    context, .s2_implementations[["risk_margin_internal_model_required"]])
}

#' Apply a nonnegative externally qualified share; never infer it from SCR0.
#'
#' Shares cover the actual portfolio's LOBs. Omitted unused LOBs are permitted
#' only with explicit full-scope confirmation. A1e-12 sum tolerance is numerical,
#' not a regulatory materiality threshold; shares are never renormalised.
#' Signed allocations are outside this component, not declared legally banned.
#' @param total_risk_margin Total risk margin. Reference type: `float`.
#' @param allocation_shares Allocation shares. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param lob Lob. Reference type: `str`.
#' @param lifetime_contributions_qualified Lifetime contributions qualified. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param complete_scope_confirmed Complete scope confirmed. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param allocation_reference Allocation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rm-allocation-baseline-lob-1")
#' result <- do.call(risk_margin_lob_allocation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
risk_margin_lob_allocation <- function(total_risk_margin, allocation_shares, lob, lifetime_contributions_qualified, complete_scope_confirmed, allocation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "risk_margin_lob_allocation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("risk_margin_lob_allocation", list(total_risk_margin = total_risk_margin, allocation_shares = allocation_shares, lob = lob, lifetime_contributions_qualified = lifetime_contributions_qualified, complete_scope_confirmed = complete_scope_confirmed, allocation_reference = allocation_reference),
    context, .s2_implementations[["risk_margin_lob_allocation"]])
}

#' Externally calibrated alpha_lob * BE_net(0), not a prescribed default alpha.
#'
#' Percentage is a fraction (0.03 means3%). No unsupported100% cap is invented.
#' Scope, calibration, hierarchy, future BE and options/guarantees restrictions
#' are documented external qualifications, not inferred from the two numbers.
#' @param net_best_estimate Net best estimate. Reference type: `float`.
#' @param percentage Percentage. Reference type: `float`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param percentage_reference Percentage reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rm-simple-baseline-m4-zero")
#' result <- do.call(risk_margin_percentage,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
risk_margin_percentage <- function(net_best_estimate, percentage, conditions, eligibility_reference, percentage_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "risk_margin_percentage", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("risk_margin_percentage", list(net_best_estimate = net_best_estimate, percentage = percentage, conditions = conditions, eligibility_reference = eligibility_reference, percentage_reference = percentage_reference),
    context, .s2_implementations[["risk_margin_percentage"]])
}

#' SCR(t)=SCR(0)*BE_net(t)/BE_net(0); never apply lambda inside SCR(t).
#'
#' BE(t) is valued AT time t, not a time-zero discounted cashflow surrogate.
#' Whole-path nonnegative BE and risk-profile assumptions are externally
#' qualified. No BE forecast or automatic method selection is produced.
#' @param initial_scr Initial scr. Reference type: `float`.
#' @param initial_net_best_estimate Initial net best estimate. Reference type: `float`.
#' @param future_net_best_estimate Future net best estimate. Reference type: `float`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param projection_reference Projection reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rm-simple-baseline-m2-zero")
#' result <- do.call(risk_margin_proportional_scr,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
risk_margin_proportional_scr <- function(initial_scr, initial_net_best_estimate, future_net_best_estimate, conditions, eligibility_reference, projection_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "risk_margin_proportional_scr", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("risk_margin_proportional_scr", list(initial_scr = initial_scr, initial_net_best_estimate = initial_net_best_estimate, future_net_best_estimate = future_net_best_estimate, conditions = conditions, eligibility_reference = eligibility_reference, projection_reference = projection_reference),
    context, .s2_implementations[["risk_margin_proportional_scr"]])
}

#' SCR + TP - recoverables, not an asset portfolio or optimisation result.
#'
#' Preserve signs and disclose a negative implied balance. Qualification of
#' values and a feasible market-risk-minimising portfolio remain external.
#' @param reference_scr Reference scr. Reference type: `float`.
#' @param technical_provisions Technical provisions. Reference type: `float`.
#' @param recoverables Recoverables. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rm-reference-baseline-negative")
#' result <- do.call(risk_margin_reference_asset_balance,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
risk_margin_reference_asset_balance <- function(reference_scr, technical_provisions, recoverables, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "risk_margin_reference_asset_balance", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("risk_margin_reference_asset_balance", list(reference_scr = reference_scr, technical_provisions = technical_provisions, recoverables = recoverables, qualification_reference = qualification_reference),
    context, .s2_implementations[["risk_margin_reference_asset_balance"]])
}

#' Inventory reference-undertaking assumptions, never generate SCR runoff.
#'
#' Composite splitting and residual non-interest market risk have explicit
#' externally assessed conditions. No quantitative materiality cutoff is
#' invented; deferred-tax loss absorption must not be assumed.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param composite_undertaking Composite undertaking. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param material_residual_market_risk Material residual market risk. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rm-reference-baseline-tax-missing")
#' result <- do.call(risk_margin_reference_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
risk_margin_reference_evidence_check <- function(evidence, composite_undertaking, material_residual_market_risk, qualification_reference, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "risk_margin_reference_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("risk_margin_reference_evidence_check", list(evidence = evidence, composite_undertaking = composite_undertaking, material_residual_market_risk = material_residual_market_risk, qualification_reference = qualification_reference, assessment_reference = assessment_reference),
    context, .s2_implementations[["risk_margin_reference_evidence_check"]])
}

#' Reserve-only 3*sigma_res_mod*PCO_net(t), never the whole nonlife SCR.
#'
#' Zero premium volume must be externally justified; no materiality threshold
#' is invented. Sigma and claims projections are external, including any USP
#' or profile-specific reinsurance adjustments. No lambda is applied here.
#' @param net_claims_best_estimate Net claims best estimate. Reference type: `float`.
#' @param aggregated_reserve_sigma Aggregated reserve sigma. Reference type: `float`.
#' @param premium_volume Premium volume. Reference type: `float`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param projection_reference Projection reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rm-reserve-baseline-zero")
#' result <- do.call(risk_margin_reserve_scr,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
risk_margin_reserve_scr <- function(net_claims_best_estimate, aggregated_reserve_sigma, premium_volume, conditions, eligibility_reference, projection_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "risk_margin_reserve_scr", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("risk_margin_reserve_scr", list(net_claims_best_estimate = net_claims_best_estimate, aggregated_reserve_sigma = aggregated_reserve_sigma, premium_volume = premium_volume, conditions = conditions, eligibility_reference = eligibility_reference, projection_reference = projection_reference),
    context, .s2_implementations[["risk_margin_reserve_scr"]])
}

#' Full, excluded, or externally qualified time-proportionate recognition.
#'
#' Proportional_factor is required only for a qualified short contract without
#' valid209(3) replacement. Its exposure-dependent time basis is external.
#' Finite/similar arrangements are excluded from volume/USP even if209 passes.
#' Scenario valuation must already reflect only the actually transferred risk;
#' this factor is NOT applied blindly to recoverables, LGD or capital amounts.
#' counterparty_factor may bind the separately calculated211 haircut. Default1
#' explicitly declares full counterparty recognition, never verifies it.
#' Optional residential mortgage guarantee payment window implements only
#' future215(c)(iii)/(d); all other guarantee criteria remain in qualification.
#' @param remaining_months Remaining months. Reference type: `float`.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param purpose Purpose. Reference type: `str`.
#' @param instrument_kind Instrument kind. Reference type: `str`.
#' @param finite_or_similar Finite or similar. Reference type: `bool | None`.
#' @param replacement Replacement. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param proportional_factor Proportional factor. Reference type: `float | None`.
#' @param counterparty_factor Counterparty factor. Reference type: `float`.
#' @param residential_guarantee_payment_months Residential guarantee payment months. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("closing-rules-baseline-finite-usp")
#' result <- do.call(risk_mitigation_recognition_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
risk_mitigation_recognition_factor <- function(remaining_months, conditions, purpose, instrument_kind, finite_or_similar, replacement = NULL, proportional_factor = NULL, counterparty_factor = 1, residential_guarantee_payment_months = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "risk_mitigation_recognition_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("risk_mitigation_recognition_factor", list(remaining_months = remaining_months, conditions = conditions, purpose = purpose, instrument_kind = instrument_kind, finite_or_similar = finite_or_similar, replacement = replacement, proportional_factor = proportional_factor, counterparty_factor = counterparty_factor, residential_guarantee_payment_months = residential_guarantee_payment_months, qualification_reference = qualification_reference),
    context, .s2_implementations[["risk_mitigation_recognition_factor"]])
}

#' Extra impact/action disclosures for an externally qualified105a holding.
#'
#' Share is relative to ALL held assets, not the equity portfolio. The
#' counterfactual SCR-compliance fact must already have been established.
#' Exactly4% does not trigger the share branch. No general reporting waiver.
#' @param long_term_equity_asset_share Long term equity asset share. Reference type: `float`.
#' @param scr_compliant_without_treatment Scr compliant without treatment. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("rsr-content-future-lte-0-true")
#' result <- do.call(rsr_long_term_equity_extra_disclosure_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
rsr_long_term_equity_extra_disclosure_2027 <- function(long_term_equity_asset_share, scr_compliant_without_treatment, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "rsr_long_term_equity_extra_disclosure_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("rsr_long_term_equity_extra_disclosure_2027", list(long_term_equity_asset_share = long_term_equity_asset_share, scr_compliant_without_treatment = scr_compliant_without_treatment, qualification_reference = qualification_reference),
    context, .s2_implementations[["rsr_long_term_equity_extra_disclosure_2027"]])
}

#' Loss from external BOF; optional gross83 input checks are not a valuation model.
#'
#' @param base_bof Base bof. Reference type: `float`.
#' @param stressed_bof Stressed bof. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param assumption_checks Assumption checks. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("scenario-2027-bof-0")
#' result <- do.call(scenario_loss,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
scenario_loss <- function(base_bof, stressed_bof, context, assumption_checks = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "scenario_loss", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("scenario_loss", list(base_bof = base_bof, stressed_bof = stressed_bof, assumption_checks = assumption_checks),
    context, .s2_implementations[["scenario_loss"]])
}

#' Apply Article16's cross-branch conditions to external factual declarations.
#'
#' Credit/surety remain prohibited. Legal expenses require the assistance main
#' branch and the explicitly declared travel or sea-vessel exception; all base
#' conditions still apply. This function does not grant or verify a licence.
#' @param ancillary_branch Ancillary branch. Reference type: `int`.
#' @param main_branch Main branch. Reference type: `int`.
#' @param main_risk_authorised Main risk authorised. Reference type: `bool | None`.
#' @param linked_to_main Linked to main. Reference type: `bool | None`.
#' @param same_object Same object. Reference type: `bool | None`.
#' @param same_contract Same contract. Reference type: `bool | None`.
#' @param legal_expenses_exception Legal expenses exception. Reference type: `str | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("scope-ancillary-baseline-ordinary")
#' result <- do.call(scope_ancillary_risk_conditions,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
scope_ancillary_risk_conditions <- function(ancillary_branch, main_branch, main_risk_authorised, linked_to_main, same_object, same_contract, legal_expenses_exception = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "scope_ancillary_risk_conditions", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("scope_ancillary_risk_conditions", list(ancillary_branch = ancillary_branch, main_branch = main_branch, main_risk_authorised = main_risk_authorised, linked_to_main = linked_to_main, same_object = same_object, same_contract = same_contract, legal_expenses_exception = legal_expenses_exception, qualification_reference = qualification_reference),
    context, .s2_implementations[["scope_ancillary_risk_conditions"]])
}

#' Screen one year's literal Article4(1) conditions in the selected profile.
#'
#' Inputs are externally qualified gross EUR amounts. No netting or zero floor
#' is inferred. Article4(2-5) time rules, authorisation and cross-border business
#' must be assessed separately; a result of1 is not a scope exemption.
#' @param gross_written_premiums Gross written premiums. Reference type: `float`.
#' @param gross_technical_provisions Gross technical provisions. Reference type: `float`.
#' @param reinsurance_written_premiums Reinsurance written premiums. Reference type: `float`.
#' @param reinsurance_technical_provisions Reinsurance technical provisions. Reference type: `float`.
#' @param part_of_group Part of group. Reference type: `bool | None`.
#' @param group_gross_technical_provisions Group gross technical provisions. Reference type: `float | None`.
#' @param has_disallowed_business Has disallowed business. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("scope-volume-baseline-zero")
#' result <- do.call(scope_volume_conditions,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
scope_volume_conditions <- function(gross_written_premiums, gross_technical_provisions, reinsurance_written_premiums, reinsurance_technical_provisions, part_of_group, group_gross_technical_provisions = NULL, has_disallowed_business, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "scope_volume_conditions", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("scope_volume_conditions", list(gross_written_premiums = gross_written_premiums, gross_technical_provisions = gross_technical_provisions, reinsurance_written_premiums = reinsurance_written_premiums, reinsurance_technical_provisions = reinsurance_technical_provisions, part_of_group = part_of_group, group_gross_technical_provisions = group_gross_technical_provisions, has_disallowed_business = has_disallowed_business, qualification_reference = qualification_reference),
    context, .s2_implementations[["scope_volume_conditions"]])
}

#' Three-year breach of one identified amount, for the following-year trigger.
#'
#' Period consecutiveness and historically applicable thresholds are externally
#' qualified. Different monetary criteria must not be mixed within a series.
#' This component does not select an effective legal regime for the entity.
#' @param annual_exceedance Annual exceedance. Reference type: `Sequence`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("scope-history-baseline-entry-all")
#' result <- do.call(scope_volume_entry_trigger,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
scope_volume_entry_trigger <- function(annual_exceedance, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "scope_volume_entry_trigger", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("scope_volume_entry_trigger", list(annual_exceedance = annual_exceedance, qualification_reference = qualification_reference),
    context, .s2_implementations[["scope_volume_entry_trigger"]])
}

#' Only historical/prospective volume conditions, not supervisory release.
#'
#' Forecasts are external. Cross-border activity, voluntary authorisation and
#' supervisory satisfaction remain separate, even when this indicator is1.
#' @param past_any_exceedance Past any exceedance. Reference type: `Sequence`.
#' @param forecast_any_exceedance Forecast any exceedance. Reference type: `Sequence`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("scope-history-baseline-release-breach-0")
#' result <- do.call(scope_volume_release_history,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
scope_volume_release_history <- function(past_any_exceedance, forecast_any_exceedance, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "scope_volume_release_history", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("scope_volume_release_history", list(past_any_exceedance = past_any_exceedance, forecast_any_exceedance = forecast_any_exceedance, qualification_reference = qualification_reference),
    context, .s2_implementations[["scope_volume_release_history"]])
}

#' Compose standard formula and explicit imposed add-ons, not a full release.
#'
#' Inputs exclude these add-ons already. For the risk_margin view only the
#' governance Article37(1)c add-on is removed by37(5); all reference-undertaking
#' selection underDR38 remains external, not inferred from this view switch.
#' @param bscr Bscr. Reference type: `float`.
#' @param operational Operational. Reference type: `float`.
#' @param adjustment_tp Adjustment tp. Reference type: `float`.
#' @param adjustment_dt Adjustment dt. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param capital_addons Capital addons. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param purpose Purpose. Reference type: `str`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("scr-standard-zero")
#' result <- do.call(scr_standard_formula,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
scr_standard_formula <- function(bscr, operational, adjustment_tp, adjustment_dt, context, capital_addons = NULL, purpose = "scr", ...) {
  .s2_require(length(list(...)) == 0L, "scr_standard_formula", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("scr_standard_formula", list(bscr = bscr, operational = operational, adjustment_tp = adjustment_tp, adjustment_dt = adjustment_dt, capital_addons = capital_addons, purpose = purpose),
    context, .s2_implementations[["scr_standard_formula"]])
}

#' Three collateral branches on an externally qualified Article176(4) shock.
#'
#' Full coverage is inclusive; equality to the unsecured post-stress value
#' leaves the original shock unchanged. This does not value collateral.
#' @param unsecured_stress Unsecured stress. Reference type: `float`.
#' @param asset_value Asset value. Reference type: `float`.
#' @param risk_adjusted_collateral Risk adjusted collateral. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-secured-spread-extreme-1")
#' result <- do.call(secured_bond_spread_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
secured_bond_spread_stress <- function(unsecured_stress, asset_value, risk_adjusted_collateral, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "secured_bond_spread_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("secured_bond_spread_stress", list(unsecured_stress = unsecured_stress, asset_value = asset_value, risk_adjusted_collateral = risk_adjusted_collateral, qualification_reference = qualification_reference),
    context, .s2_implementations[["secured_bond_spread_stress"]])
}

#' Article178 factors on externally qualified positions; no automatic transition relief.
#'
#' Paragraph6 literally uses the paragraph3 senior table at CQS5. The unusual
#' source reference remains explicit, not silently redirected to paragraph4.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | None`.
#' @param transition Transition. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param imposed_increase_fraction Imposed increase fraction. Reference type: `float | None`.
#' @param supervisory_decision_reference Supervisory decision reference. Reference type: `str | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-securitisation-residual-na-0")
#' result <- do.call(securitisation_spread_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
securitisation_spread_stress <- function(modified_duration, category, qualification_reference, credit_quality_step = NULL, transition = NULL, context, imposed_increase_fraction = NULL, supervisory_decision_reference = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "securitisation_spread_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("securitisation_spread_stress", list(modified_duration = modified_duration, category = category, qualification_reference = qualification_reference, credit_quality_step = credit_quality_step, transition = transition, imposed_increase_fraction = imposed_increase_fraction, supervisory_decision_reference = supervisory_decision_reference),
    context, .s2_implementations[["securitisation_spread_stress"]])
}

#' Select the gross charge corresponding to the greatest net-of-FDB loss.
#'
#' @param gross_losses Gross losses. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param net_losses Net losses. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param tie_break Tie break. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("net-scenario-selection")
#' result <- do.call(select_scenario,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
select_scenario <- function(gross_losses, net_losses, tie_break = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "select_scenario", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("select_scenario", list(gross_losses = gross_losses, net_losses = net_losses, tie_break = tie_break),
    context, .s2_implementations[["select_scenario"]])
}

#' Select the balance-sheet audit duty, not the complete audit perimeter.
#'
#' Captive classification is not conditional on Article51 disclosure relief.
#' The caller must qualify national extensions explicitly; no default national
#' rule, automatic legal research or audit-content approval is inferred.
#' @param small_non_complex Small non complex. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param captive Captive. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param national_extension_applies National extension applies. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("sfcr-audit-111")
#' result <- do.call(sfcr_balance_sheet_audit_required_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
sfcr_balance_sheet_audit_required_2027 <- function(small_non_complex, captive, national_extension_applies, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "sfcr_balance_sheet_audit_required_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("sfcr_balance_sheet_audit_required_2027", list(small_non_complex = small_non_complex, captive = captive, national_extension_applies = national_extension_applies, qualification_reference = qualification_reference),
    context, .s2_implementations[["sfcr_balance_sheet_audit_required_2027"]])
}

#' Declared captive relief conditions, distinct from ORSA/reporting relief.
#'
#' Captive status, beneficiary qualification, loan/cash-pool scope and the
#' deterministic maximum-loss assessment are external. Reinsurance liability
#' exclusion concerns the underlying insurance contracts, not only the treaty.
#' @param captive_type Captive type. Reference type: `str`.
#' @param permitted_policyholders Permitted policyholders. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param natural_person_tp_share Natural person tp share. Reference type: `float`.
#' @param no_compulsory_liability No compulsory liability. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param group_loans_asset_share Group loans asset share. Reference type: `float | None`.
#' @param maximum_loss_deterministic Maximum loss deterministic. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("sfcr-insurance-share-zero")
#' result <- do.call(sfcr_captive_disclosure_conditions_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
sfcr_captive_disclosure_conditions_2027 <- function(captive_type, permitted_policyholders, natural_person_tp_share, no_compulsory_liability, group_loans_asset_share = NULL, maximum_loss_deterministic = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "sfcr_captive_disclosure_conditions_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("sfcr_captive_disclosure_conditions_2027", list(captive_type = captive_type, permitted_policyholders = permitted_policyholders, natural_person_tp_share = natural_person_tp_share, no_compulsory_liability = no_compulsory_liability, group_loans_asset_share = group_loans_asset_share, maximum_loss_deterministic = maximum_loss_deterministic, qualification_reference = qualification_reference),
    context, .s2_implementations[["sfcr_captive_disclosure_conditions_2027"]])
}

#' Inventory references for an externally qualified full narrative part.
#'
#' A conditional paragraph needs either its content evidence or a qualified
#' non-applicability assessment, never an unqualified silent omission.
#' References are not retrieved or assessed. Quantitative-only relief reports
#' require a different perimeter and are deliberately not certified here.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param report_part Report part. Reference type: `str`.
#' @param full_narrative_scope Full narrative scope. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param scope_reference Scope reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param structure_sections Structure sections. Reference type: `list[str] | None`. Supply an ordered R list or the documented numeric vector.
#' @param structure_reference Structure reference. Reference type: `str | None`.
#' @param de_materiality De materiality. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param reporting_scope Reporting scope. Reference type: `str`.
#' @param group_evidence Group evidence. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("sfcr-content-baseline-sfcr-empty")
#' result <- do.call(sfcr_content_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
sfcr_content_evidence_check <- function(evidence, report_part, full_narrative_scope, assessment_reference, scope_reference, context, structure_sections = NULL, structure_reference = NULL, de_materiality = NULL, reporting_scope = "solo", group_evidence = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "sfcr_content_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("sfcr_content_evidence_check", list(evidence = evidence, report_part = report_part, full_narrative_scope = full_narrative_scope, assessment_reference = assessment_reference, scope_reference = scope_reference, structure_sections = structure_sections, structure_reference = structure_reference, de_materiality = de_materiality, reporting_scope = reporting_scope, group_evidence = group_evidence),
    context, .s2_implementations[["sfcr_content_evidence_check"]])
}

#' Join versioned duty references and qualified event facts, not legal sign-off.
#'
#' Every control needs evidence, including reasoned non-applicability when
#' conditional. AVAILABLE merely records a reference. Elapsed months and
#' historical deadline facts are externally qualified; no calendar guess.
#' Missing optional screenings remain NOT_ASSESSED, never implicitly passed.
#' @param evidence Evidence. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param publication_facts Publication facts. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param capital_events Capital events. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("sfcr-publication-future-missing")
#' result <- do.call(sfcr_publication_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
sfcr_publication_evidence_check <- function(evidence, assessment_reference, context, publication_facts = NULL, capital_events = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "sfcr_publication_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("sfcr_publication_evidence_check", list(evidence = evidence, assessment_reference = assessment_reference, publication_facts = publication_facts, capital_events = capital_events),
    context, .s2_implementations[["sfcr_publication_evidence_check"]])
}

#' One host state's establishment AND services premiums; strict EUR threshold.
#'
#' The host authority's materiality decision is explicit, never inferred from
#' market shares. SNC exclusion applies to both branches. This definition is
#' not the different significant-branch screen in delegated Article354.
#' @param host_gross_premiums Host gross premiums. Reference type: `float`.
#' @param small_non_complex Small non complex. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param host_authority_significant Host authority significant. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-close-cross-host")
#' result <- do.call(significant_cross_border_activity_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
significant_cross_border_activity_2027 <- function(host_gross_premiums, small_non_complex, host_authority_significant, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "significant_cross_border_activity_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("significant_cross_border_activity_2027", list(host_gross_premiums = host_gross_premiums, small_non_complex = small_non_complex, host_authority_significant = host_authority_significant, qualification_reference = qualification_reference),
    context, .s2_implementations[["significant_cross_border_activity_2027"]])
}

#' Screen declared assessments and the higher-SCR exception to material error.
#'
#' Do not create a quantitative materiality threshold. A comparison with the
#' standard calculation is only requested where the exception is invoked;
#' the result records supplied facts, not a substantive adequacy conclusion.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param simplified_scr_higher Simplified scr higher. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("proportionality-baseline-ordinary")
#' result <- do.call(simplification_proportionality_conditions,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplification_proportionality_conditions <- function(conditions, simplified_scr_higher = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplification_proportionality_conditions", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplification_proportionality_conditions", list(conditions = conditions, simplified_scr_higher = simplified_scr_higher, qualification_reference = qualification_reference),
    context, .s2_implementations[["simplification_proportionality_conditions"]])
}

#' Check distinct strict SNC versus inclusive immaterial-risk thresholds.
#'
#' Shares refer to each risk's externally qualified reference calculation;
#' never infer a common denominator or double-count nested modules. The age
#' is externally qualified and covers the oldest required reference evidence.
#' @param risk_shares Risk shares. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param regime Regime. Reference type: `str`.
#' @param evidence_age_years Evidence age years. Reference type: `float`.
#' @param small_non_complex Small non complex. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("simplification-2027-individual-over")
#' result <- do.call(simplification_share_screen_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplification_share_screen_2027 <- function(risk_shares, regime, evidence_age_years, small_non_complex = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplification_share_screen_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplification_share_screen_2027", list(risk_shares = risk_shares, regime = regime, evidence_age_years = evidence_age_years, small_non_complex = small_non_complex, qualification_reference = qualification_reference),
    context, .s2_implementations[["simplification_share_screen_2027"]])
}

#' Sum qualified bucket losses plus signed, externally valued DeltaLiab.
#'
#' Amount weighting is algebraically identical to MV times portfolio shares.
#' Fractions avoid unnecessary intermediate overflow and cancellation. No
#' options/guarantee model, floor or general eligibility approval is inferred.
#' @param buckets Buckets. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param liability_change Liability change. Reference type: `float`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("simplified-market-baseline-empty-book")
#' result <- do.call(simplified_bond_spread_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_bond_spread_risk <- function(buckets, liability_change, eligibility_reference, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_bond_spread_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_bond_spread_risk", list(buckets = buckets, liability_change = liability_change, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference),
    context, .s2_implementations[["simplified_bond_spread_risk"]])
}

#' DR104 factor: rated values have no invented cap; only unrated is capped.
#'
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param credit_quality_step Credit quality step. Reference type: `str`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("simplified-market-baseline-slope-0")
#' result <- do.call(simplified_bond_spread_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_bond_spread_stress <- function(modified_duration, credit_quality_step, eligibility_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_bond_spread_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_bond_spread_stress", list(modified_duration = modified_duration, credit_quality_step = credit_quality_step, eligibility_reference = eligibility_reference),
    context, .s2_implementations[["simplified_bond_spread_stress"]])
}

#' Sum per-contract max(current net benefit + additional future PV - net BE,0).
#'
#' Inputs refer to the year/event specified by the chosen article. A net
#' portfolio difference is not a substitute for individually floored contracts.
#' @param contracts Contracts. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param kind Kind. Reference type: `str`.
#' @param proportionality_satisfied Proportionality satisfied. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("uw-biometric-baseline-car-health_income-negative-be")
#' result <- do.call(simplified_capital_at_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_capital_at_risk <- function(contracts, kind, proportionality_satisfied, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_capital_at_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_capital_at_risk", list(contracts = contracts, kind = kind, proportionality_satisfied = proportionality_satisfied, qualification_reference = qualification_reference),
    context, .s2_implementations[["simplified_capital_at_risk"]])
}

#' Conservative grouping PD, not LGD-weighted single-name PD under Article199.
#'
#' @param probabilities Probabilities. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param pool_rating_group Pool rating group. Reference type: `Mapping[str, dict] | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: probability.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("mitigation-baseline-zero-pd")
#' result <- do.call(simplified_default_group_pd,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_default_group_pd <- function(probabilities, eligibility_reference, valuation_reference, context, pool_rating_group = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_default_group_pd", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_default_group_pd", list(probabilities = probabilities, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference, pool_rating_group = pool_rating_group),
    context, .s2_implementations[["simplified_default_group_pd"]])
}

#' Three printed terms under Art93/100; no period inference or term floors.
#'
#' Health Art100(d) has conflicting DE/EN d2 descriptions. The supplied d2
#' basis must be explicitly externally qualified; its numeric value is retained.
#' @param capital_at_risk_1 Capital at risk 1. Reference type: `float`.
#' @param capital_at_risk_2 Capital at risk 2. Reference type: `float`.
#' @param incidence_rate_1 Incidence rate 1. Reference type: `float`.
#' @param incidence_rate_2 Incidence rate 2. Reference type: `float`.
#' @param termination_rate Termination rate. Reference type: `float`.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param best_estimate Best estimate. Reference type: `float`.
#' @param kind Kind. Reference type: `str`.
#' @param proportionality_satisfied Proportionality satisfied. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("uw-biometric-baseline-disability-life-no-incidence")
#' result <- do.call(simplified_disability_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_disability_risk <- function(capital_at_risk_1, capital_at_risk_2, incidence_rate_1, incidence_rate_2, termination_rate, modified_duration, best_estimate, kind, proportionality_satisfied, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_disability_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_disability_risk", list(capital_at_risk_1 = capital_at_risk_1, capital_at_risk_2 = capital_at_risk_2, incidence_rate_1 = incidence_rate_1, incidence_rate_2 = incidence_rate_2, termination_rate = termination_rate, modified_duration = modified_duration, best_estimate = best_estimate, kind = kind, proportionality_satisfied = proportionality_satisfied, qualification_reference = qualification_reference),
    context, .s2_implementations[["simplified_disability_risk"]])
}

#' Allocate default-zero BSCR mitigation using absolute EAD by external counterparty.
#'
#' Reinsurance/securitisation EAD here is BE recoverables under107a(2), not
#' automatically the BE-plus-debtors basis of107. Derivative/SPV EAD is external.
#' A complete scoped portfolio and both hypothetical BSCR valuations are required.
#' @param exposures Exposures. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param target_counterparty Target counterparty. Reference type: `str`.
#' @param bscr_without Bscr without. Reference type: `float`.
#' @param bscr_with Bscr with. Reference type: `float`.
#' @param default_module_zero_in_both Default module zero in both. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("mitigation-future-ead-floor")
#' result <- do.call(simplified_ead_mitigation_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_ead_mitigation_2027 <- function(exposures, target_counterparty, bscr_without, bscr_with, default_module_zero_in_both, eligibility_reference, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_ead_mitigation_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_ead_mitigation_2027", list(exposures = exposures, target_counterparty = target_counterparty, bscr_without = bscr_without, bscr_with = bscr_with, default_module_zero_in_both = default_module_zero_in_both, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference),
    context, .s2_implementations[["simplified_ead_mitigation_2027"]])
}

#' Art94/99/101: factor*A*n + A*(G(i+shift,n)-G(i,n)).
#'
#' G(r,n)=((1+r)**n-1)/r, continuously extended to G(0,n)=n. The amount is
#' last-year expenses (life/health) or medical payments, NOT a projected BE.
#' High-precision intermediates avoid cancellation around zero inflation.
#' @param previous_year_amount Previous year amount. Reference type: `float`.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param inflation_rate Inflation rate. Reference type: `float`.
#' @param kind Kind. Reference type: `str`.
#' @param proportionality_satisfied Proportionality satisfied. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("uw-simplified-baseline-life_expense-zero")
#' result <- do.call(simplified_expense_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_expense_risk <- function(previous_year_amount, modified_duration, inflation_rate, kind, proportionality_satisfied, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_expense_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_expense_risk", list(previous_year_amount = previous_year_amount, modified_duration = modified_duration, inflation_rate = inflation_rate, kind = kind, proportionality_satisfied = proportionality_satisfied, qualification_reference = qualification_reference),
    context, .s2_implementations[["simplified_expense_risk"]])
}

#' Maximum of qualified top-five building-neighbourhood exposures and theta.
#'
#' Each category explicitly supplies five ranked centre-neighbourhood sums;
#' nonexistent centres are qualified zero entries. Actual building geometry,
#' full/partial200m intersection, LoBs7/19, ranking by building-specific net
#' insured sum and recoveries not contingent on unrelated claims are external.
#' No point-distance proxy and no second deduction of reinsurance are used.
#' @param exposures Exposures. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param average_residential_sum Average residential sum. Reference type: `float`.
#' @param residential_market_shares Residential market shares. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2026-10-05",
#'   known_at = "2026-10-06",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("simpl-close-baseline-fire-no-residential")
#' result <- do.call(simplified_fire_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_fire_risk <- function(exposures, average_residential_sum, residential_market_shares, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_fire_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_fire_risk", list(exposures = exposures, average_residential_sum = average_residential_sum, residential_market_shares = residential_market_shares, qualification_reference = qualification_reference),
    context, .s2_implementations[["simplified_fire_risk"]])
}

#' Scale the reference SCR with volume, retaining its prescribed floor.
#'
#' Eligibility under DIR109(2/3), reference calculation and suitability of the
#' company-specific volume are externally established. A factor ratio in the
#' details avoids overflow/underflow from an unnecessary intermediate float.
#' @param base_scr Base scr. Reference type: `float`.
#' @param base_volume Base volume. Reference type: `float`.
#' @param current_volume Current volume. Reference type: `float`.
#' @param elapsed_years Elapsed years. Reference type: `float`.
#' @param is_market_risk Is market risk. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param volume_reference Volume reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("simplification-2027-zero-volume")
#' result <- do.call(simplified_immaterial_risk_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_immaterial_risk_2027 <- function(base_scr, base_volume, current_volume, elapsed_years, is_market_risk, eligibility_reference, volume_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_immaterial_risk_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_immaterial_risk_2027", list(base_scr = base_scr, base_volume = base_volume, current_volume = current_volume, elapsed_years = elapsed_years, is_market_risk = is_market_risk, eligibility_reference = eligibility_reference, volume_reference = volume_reference),
    context, .s2_implementations[["simplified_immaterial_risk_2027"]])
}

#' Art95/102 up: half * max(qualified rate, source threshold) * years * S+.
#'
#' Each contract provides termination_payment_net (after amounts recoverable
#' from the policyholder/intermediary) and technical_provisions_excluding_rm.
#' Negative strains never offset positive ones. Rate and period must describe
#' the positive-strain population. The printed down formula has an unresolved
#' sign issue and is deliberately not converted to positive capital.
#' @param contracts Contracts. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param average_lapse_rate Average lapse rate. Reference type: `float`.
#' @param runoff_period Runoff period. Reference type: `float`.
#' @param kind Kind. Reference type: `str`.
#' @param direction Direction. Reference type: `str`.
#' @param proportionality_satisfied Proportionality satisfied. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("uw-lapse-baseline-life-empty")
#' result <- do.call(simplified_lapse_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_lapse_risk <- function(contracts, average_lapse_rate, runoff_period, kind, direction, proportionality_satisfied, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_lapse_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_lapse_risk", list(contracts = contracts, average_lapse_rate = average_lapse_rate, runoff_period = runoff_period, kind = kind, direction = direction, proportionality_satisfied = proportionality_satisfied, qualification_reference = qualification_reference),
    context, .s2_implementations[["simplified_lapse_risk"]])
}

#' Art96 factor times the already contract-floored CAR sum, not death benefits.
#'
#' @param capital_at_risk Capital at risk. Reference type: `float`.
#' @param proportionality_satisfied Proportionality satisfied. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("uw-biometric-baseline-catastrophe-zero")
#' result <- do.call(simplified_life_catastrophe_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_life_catastrophe_risk <- function(capital_at_risk, proportionality_satisfied, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_life_catastrophe_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_life_catastrophe_risk", list(capital_at_risk = capital_at_risk, proportionality_satisfied = proportionality_satisfied, qualification_reference = qualification_reference),
    context, .s2_implementations[["simplified_life_catastrophe_risk"]])
}

#' Art92/98: factor*q*n*growth**((n-1)/2)*BE on qualified selected contracts.
#'
#' q is the sum-assured weighted NEXT-twelve-month mortality rate. Fractional
#' modified durations are retained; no invented rounding or contract selection.
#' @param mortality_rate Mortality rate. Reference type: `float`.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param best_estimate Best estimate. Reference type: `float`.
#' @param kind Kind. Reference type: `str`.
#' @param proportionality_satisfied Proportionality satisfied. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("uw-simplified-baseline-life-longevity-3")
#' result <- do.call(simplified_longevity_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_longevity_risk <- function(mortality_rate, modified_duration, best_estimate, kind, proportionality_satisfied, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_longevity_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_longevity_risk", list(mortality_rate = mortality_rate, modified_duration = modified_duration, best_estimate = best_estimate, kind = kind, proportionality_satisfied = proportionality_satisfied, qualification_reference = qualification_reference),
    context, .s2_implementations[["simplified_longevity_risk"]])
}

#' Art91/97 finite annual sum with midyear discount and survival weighting.
#'
#' q is the sum-assured weighted rate over all future years, unlike longevity.
#' The printed upper index is modified duration: fractional values remain
#' unqualified, never silently rounded, truncated or prorated.
#' @param capital_at_risk Capital at risk. Reference type: `list[float]`. Supply an ordered R list or the documented numeric vector.
#' @param basic_spot_rates Basic spot rates. Reference type: `list[float]`. Supply an ordered R list or the documented numeric vector.
#' @param mortality_rate Mortality rate. Reference type: `float`.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param kind Kind. Reference type: `str`.
#' @param proportionality_satisfied Proportionality satisfied. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("uw-biometric-baseline-mortality-life-empty")
#' result <- do.call(simplified_mortality_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_mortality_risk <- function(capital_at_risk, basic_spot_rates, mortality_rate, modified_duration, kind, proportionality_satisfied, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_mortality_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_mortality_risk", list(capital_at_risk = capital_at_risk, basic_spot_rates = basic_spot_rates, mortality_rate = mortality_rate, modified_duration = modified_duration, kind = kind, proportionality_satisfied = proportionality_satisfied, qualification_reference = qualification_reference),
    context, .s2_implementations[["simplified_mortality_risk"]])
}

#' Scale externally qualified BECEP to whole-pool BECE, retaining its sign.
#'
#' @param ceded_best_estimate Ceded best estimate. Reference type: `float`.
#' @param undertaking_share Undertaking share. Reference type: `float`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("pool-simplified-baseline-external-zero")
#' result <- do.call(simplified_pool_external_best_estimate,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_pool_external_best_estimate <- function(ceded_best_estimate, undertaking_share, eligibility_reference, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_pool_external_best_estimate", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_pool_external_best_estimate", list(ceded_best_estimate = ceded_best_estimate, undertaking_share = undertaking_share, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference),
    context, .s2_implementations[["simplified_pool_external_best_estimate"]])
}

#' Allocate signed external pool contributions; no extra floor on individual contributions.
#'
#' @param best_estimates Best estimates. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param target_counterparty Target counterparty. Reference type: `str`.
#' @param total_external_contribution Total external contribution. Reference type: `float`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("pool-simplified-baseline-contribution")
#' result <- do.call(simplified_pool_external_mitigation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_pool_external_mitigation <- function(best_estimates, target_counterparty, total_external_contribution, eligibility_reference, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_pool_external_mitigation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_pool_external_mitigation", list(best_estimates = best_estimates, target_counterparty = target_counterparty, total_external_contribution = total_external_contribution, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference),
    context, .s2_implementations[["simplified_pool_external_mitigation"]])
}

#' PC/PU times already externally netted BEU; do not net external recoveries twice.
#'
#' @param undertaking_best_estimate Undertaking best estimate. Reference type: `float`.
#' @param undertaking_share Undertaking share. Reference type: `float`.
#' @param counterparty_share Counterparty share. Reference type: `float`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("pool-simplified-baseline-member-zero")
#' result <- do.call(simplified_pool_member_best_estimate,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_pool_member_best_estimate <- function(undertaking_best_estimate, undertaking_share, counterparty_share, eligibility_reference, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_pool_member_best_estimate", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_pool_member_best_estimate", list(undertaking_best_estimate = undertaking_best_estimate, undertaking_share = undertaking_share, counterparty_share = counterparty_share, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference),
    context, .s2_implementations[["simplified_pool_member_best_estimate"]])
}

#' Article108 proportional-contract amount with the prescribed net-BE denominator.
#'
#' @param gross_best_estimate Gross best estimate. Reference type: `float`.
#' @param recoverables Recoverables. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param submodule_scr Submodule scr. Reference type: `float`.
#' @param target_counterparty Target counterparty. Reference type: `str`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("mitigation-baseline-proportional-zero")
#' result <- do.call(simplified_proportional_mitigation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_proportional_mitigation <- function(gross_best_estimate, recoverables, submodule_scr, target_counterparty, eligibility_reference, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_proportional_mitigation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_proportional_mitigation", list(gross_best_estimate = gross_best_estimate, recoverables = recoverables, submodule_scr = submodule_scr, target_counterparty = target_counterparty, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference),
    context, .s2_implementations[["simplified_proportional_mitigation"]])
}

#' Positive deduction corresponding to the printed negative Article61 AdjCD.
#'
#' One counterparty/homogeneous group; PD is next12months, duration and BE
#' refer to the same recoverables. Not a counterparty-default SCR or projection.
#' @param probability_of_default Probability of default. Reference type: `float`.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param recoverables_best_estimate Recoverables best estimate. Reference type: `float`.
#' @param proportionality_satisfied Proportionality satisfied. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("final-numeric-baseline-ri-zero-pd")
#' result <- do.call(simplified_reinsurance_default_adjustment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_reinsurance_default_adjustment <- function(probability_of_default, modified_duration, recoverables_best_estimate, proportionality_satisfied, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_reinsurance_default_adjustment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_reinsurance_default_adjustment", list(probability_of_default = probability_of_default, modified_duration = modified_duration, recoverables_best_estimate = recoverables_best_estimate, proportionality_satisfied = proportionality_satisfied, qualification_reference = qualification_reference),
    context, .s2_implementations[["simplified_reinsurance_default_adjustment"]])
}

#' Allocate underwriting mitigation using BE recoverables plus related debtors.
#'
#' Only the target's nonnegative condition is imposed on individual recoveries;
#' other signed entries are not silently floored or the resulting share capped.
#' @param recoverables Recoverables. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param target_counterparty Target counterparty. Reference type: `str`.
#' @param scr_without Scr without. Reference type: `float`.
#' @param scr_with Scr with. Reference type: `float`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("mitigation-baseline-negative-total-effect")
#' result <- do.call(simplified_reinsurance_mitigation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_reinsurance_mitigation <- function(recoverables, target_counterparty, scr_without, scr_with, eligibility_reference, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_reinsurance_mitigation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_reinsurance_mitigation", list(recoverables = recoverables, target_counterparty = target_counterparty, scr_without = scr_without, scr_with = scr_with, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference),
    context, .s2_implementations[["simplified_reinsurance_mitigation"]])
}

#' Literal111a quadratic formula; preserve signed differences, no subfloors.
#'
#' 'hyp' is the scenario without the arrangement. Despite its source label,
#' 'without' denotes the ordinary capital/volume defined in111a(b/e).
#' Recoverables are the qualified best estimate plus corresponding receivables.
#' Segment sigma follows117(3)/148(3), not total premium/reserve sigma.
#' @param cat_hyp Cat hyp. Reference type: `float`.
#' @param cat_without Cat without. Reference type: `float`.
#' @param premium_hyp Premium hyp. Reference type: `float`.
#' @param premium_without Premium without. Reference type: `float`.
#' @param sigma Sigma. Reference type: `float`.
#' @param recoverables Recoverables. Reference type: `float`.
#' @param single_segment Single segment. Reference type: `bool | None`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2026-10-05",
#'   known_at = "2026-10-06",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("simpl-close-baseline-mitigation-signed")
#' result <- do.call(simplified_single_segment_mitigation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_single_segment_mitigation <- function(cat_hyp, cat_without, premium_hyp, premium_without, sigma, recoverables, single_segment, eligibility_reference, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_single_segment_mitigation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_single_segment_mitigation", list(cat_hyp = cat_hyp, cat_without = cat_without, premium_hyp = premium_hyp, premium_without = premium_without, sigma = sigma, recoverables = recoverables, single_segment = single_segment, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference),
    context, .s2_implementations[["simplified_single_segment_mitigation"]])
}

#' Floor the difference of submodule sums once, not each submodule separately.
#'
#' @param scr_without Scr without. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param scr_with Scr with. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("mitigation-baseline-sum-floor")
#' result <- do.call(simplified_submodule_mitigation,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
simplified_submodule_mitigation <- function(scr_without, scr_with, eligibility_reference, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "simplified_submodule_mitigation", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("simplified_submodule_mitigation", list(scr_without = scr_without, scr_with = scr_with, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference),
    context, .s2_implementations[["simplified_submodule_mitigation"]])
}

#' One qualified year of Article29a criteria, not two-year status or approval.
#'
#' The three-year average NET combined ratio is externally qualified: the
#' source does not prescribe a universal weighting method for its construction.
#' investment_default_scr is only the listed investment exposures, not the
#' whole default module. Intangible charge follows the existing Article203 API.
#' Captive alternative never overrides the Article29a(3) absolute exclusions.
#' @param undertaking_type Undertaking type. Reference type: `str`.
#' @param metrics Metrics. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("proportionality-scope-parent-small-group")
#' result <- do.call(small_non_complex_annual_conditions_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
small_non_complex_annual_conditions_2027 <- function(undertaking_type, metrics, conditions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "small_non_complex_annual_conditions_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("small_non_complex_annual_conditions_2027", list(undertaking_type = undertaking_type, metrics = metrics, conditions = conditions, qualification_reference = qualification_reference),
    context, .s2_implementations[["small_non_complex_annual_conditions_2027"]])
}

#' Group213a annual criteria: NOT the solo29a business-share branching.
#'
#' Group status/history and each method2 entity's SNC classification remain
#' externally qualified. Group premium cap is TOTAL GWP; group surplus must
#' be strictly positive. Two distinct foreign-premium criteria must both pass.
#' Interest TP basis explicitly excludes method2 entities. Method2-only waives
#' consolidated interest/investment tests, never the other group conditions.
#' @param method Method. Reference type: `str`.
#' @param metrics Metrics. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param conditions Conditions. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("group-close-fail-combined_ratio_three_year_average")
#' result <- do.call(small_non_complex_group_annual_conditions_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
small_non_complex_group_annual_conditions_2027 <- function(method, metrics, conditions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "small_non_complex_group_annual_conditions_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("small_non_complex_group_annual_conditions_2027", list(method = method, metrics = metrics, conditions = conditions, qualification_reference = qualification_reference),
    context, .s2_implementations[["small_non_complex_group_annual_conditions_2027"]])
}

#' Article180 table factors on externally qualified debt, not eligibility or BOF SCR.
#'
#' Published rounded discontinuities are retained. Insurer MCR breach requires
#' external confirmation of the paragraph6 first-SFCR applicability condition.
#' @param modified_duration Modified duration. Reference type: `float`.
#' @param category Category. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param credit_quality_step Credit quality step. Reference type: `str | None`.
#' @param qualification Qualification. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-specific-spread-regional_local-none-12")
#' result <- do.call(specific_exposure_spread_stress,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
specific_exposure_spread_stress <- function(modified_duration, category, qualification_reference, credit_quality_step = NULL, qualification = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "specific_exposure_spread_stress", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("specific_exposure_spread_stress", list(modified_duration = modified_duration, category = category, qualification_reference = qualification_reference, credit_quality_step = credit_quality_step, qualification = qualification),
    context, .s2_implementations[["specific_exposure_spread_stress"]])
}

#' Sum three already valued/selected spread submodules; no missing-component defaults.
#'
#' @param bonds Bonds. Reference type: `float`.
#' @param securitisation Securitisation. Reference type: `float`.
#' @param credit_derivatives Credit derivatives. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-spread-sum-zero")
#' result <- do.call(spread_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
spread_risk <- function(bonds, securitisation, credit_derivatives, context, ...) {
  .s2_require(length(list(...)) == 0L, "spread_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("spread_risk", list(bonds = bonds, securitisation = securitisation, credit_derivatives = credit_derivatives),
    context, .s2_implementations[["spread_risk"]])
}

#' Cap one SPV's separately valued recoverable at its external aggregate exposure.
#'
#' Do not substitute collateral or sum unrelated counterparties here. Contract
#' boundaries, basis risk and the exposure evidence remain external prerequisites.
#' @param recoverable Recoverable. Reference type: `float`.
#' @param aggregate_maximum_exposure Aggregate maximum exposure. Reference type: `float`.
#' @param exposure_reference Exposure reference. Reference type: `str | None`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("future-spv-cap-zero-exposure")
#' result <- do.call(spv_recoverable_cap,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
spv_recoverable_cap <- function(recoverable, aggregate_maximum_exposure, exposure_reference = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "spv_recoverable_cap", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("spv_recoverable_cap", list(recoverable = recoverable, aggregate_maximum_exposure = aggregate_maximum_exposure, exposure_reference = exposure_reference),
    context, .s2_implementations[["spv_recoverable_cap"]])
}

#' Select an archived deadline length for an externally qualified type/scope.
#'
#' Historical rows are post-transition. Future ORSA has no inherited two-week
#' row. The returned length is not a legally determined calendar date.
#' Explicit group scope uses separate254/256/256b or historical373 rules.
#' Group sfcr/single_sfcr entries include public disclosure, not just filing.
#' Optional304e extension requires an externally qualified delegated act;
#' an extreme event alone never creates an extension. ORSA/SPV are excluded.
#' @param report_kind Report kind. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param scope Scope. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param exceptional_extension_weeks Exceptional extension weeks. Reference type: `float`.
#' @param delegated_act_reference Delegated act reference. Reference type: `str | None`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: weeks.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("reporting-timing-baseline-deadline-orsa")
#' result <- do.call(supervisory_reporting_deadline_weeks,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
supervisory_reporting_deadline_weeks <- function(report_kind, qualification_reference, scope = "solo", context, exceptional_extension_weeks = 0, delegated_act_reference = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "supervisory_reporting_deadline_weeks", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("supervisory_reporting_deadline_weeks", list(report_kind = report_kind, qualification_reference = qualification_reference, scope = scope, exceptional_extension_weeks = exceptional_extension_weeks, delegated_act_reference = delegated_act_reference),
    context, .s2_implementations[["supervisory_reporting_deadline_weeks"]])
}

#' Inventory core or full RSR references for a qualified request/period.
#'
#' Qualitative/quantitative, temporal and data-source alternatives remain
#' alternatives: one reference documents the externally justified selection.
#' No file inspection, content certification, board approval or filing occurs.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param scope_reference Scope reference. Reference type: `str`.
#' @param report_kind Report kind. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param structure_sections Structure sections. Reference type: `list[str] | None`. Supply an ordered R list or the documented numeric vector.
#' @param structure_reference Structure reference. Reference type: `str | None`.
#' @param reporting_scope Reporting scope. Reference type: `str`.
#' @param group_evidence Group evidence. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("rsr-content-baseline-empty")
#' result <- do.call(supervisory_reporting_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
supervisory_reporting_evidence_check <- function(evidence, assessment_reference, scope_reference, report_kind = "core", context, structure_sections = NULL, structure_reference = NULL, reporting_scope = "solo", group_evidence = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "supervisory_reporting_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("supervisory_reporting_evidence_check", list(evidence = evidence, assessment_reference = assessment_reference, scope_reference = scope_reference, report_kind = report_kind, structure_sections = structure_sections, structure_reference = structure_reference, reporting_scope = reporting_scope, group_evidence = group_evidence),
    context, .s2_implementations[["supervisory_reporting_evidence_check"]])
}

#' Use an externally qualified daily mean; the selected profile determines its window and cap.
#'
#' @param current_index Current index. Reference type: `float`.
#' @param average_index Average index. Reference type: `float`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: ratio.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("market-2027-sa-50")
#' result <- do.call(symmetric_adjustment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
symmetric_adjustment <- function(current_index, average_index, context, ...) {
  .s2_require(length(list(...)) == 0L, "symmetric_adjustment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("symmetric_adjustment", list(current_index = current_index, average_index = average_index),
    context, .s2_implementations[["symmetric_adjustment"]])
}

#' Average determined daily levels, not dates, months or carried-forward values.
#'
#' None declares that no index was determined that day; it is not permission
#' to hide missing observations. Exact endpoint selection and completeness
#' remain externally qualified. Never fetch or manufacture market data.
#' @param daily_indices Daily indices. Reference type: `Mapping[str, float | None]`. Supply a named R list with the keys shown in the example/reference case.
#' @param window_months Window months. Reference type: `int`.
#' @param complete_window Complete window. Reference type: `bool | None`.
#' @param index_qualified Index qualified. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: index_level.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("symmetric-average-baseline-single")
#' result <- do.call(symmetric_adjustment_average,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
symmetric_adjustment_average <- function(daily_indices, window_months, complete_window, index_qualified, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "symmetric_adjustment_average", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("symmetric_adjustment_average", list(daily_indices = daily_indices, window_months = window_months, complete_window = complete_window, index_qualified = index_qualified, qualification_reference = qualification_reference),
    context, .s2_implementations[["symmetric_adjustment_average"]])
}

#' Inventory assumptions, management actions, FDB and behaviour evidence.
#'
#' Financial models and insurer-specific behaviour remain external. Conditional
#' evidence is selected from explicit qualified scope decisions. A fully declared
#' inventory proves neither method appropriateness nor supervisory compliance.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param scope_reference Scope reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("tp-assumptions-baseline-all-empty")
#' result <- do.call(technical_provision_assumptions_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
technical_provision_assumptions_evidence_check <- function(evidence, conditions, assessment_reference, scope_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "technical_provision_assumptions_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("technical_provision_assumptions_evidence_check", list(evidence = evidence, conditions = conditions, assessment_reference = assessment_reference, scope_reference = scope_reference),
    context, .s2_implementations[["technical_provision_assumptions_evidence_check"]])
}

#' Inventory TP data-quality evidence without certifying data or methods.
#'
#' External-data use and known deficiencies activate their own controls.
#' The 2027 climate-history safeguard is unconditional, not a defect-only rule.
#' AVAILABLE declares a reference, never verified content or legal compliance.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param external_data_used External data used. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param data_deficiencies_present Data deficiencies present. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("tp-data-baseline-external-deficient-empty")
#' result <- do.call(technical_provision_data_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
technical_provision_data_evidence_check <- function(evidence, external_data_used, data_deficiencies_present, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "technical_provision_data_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("technical_provision_data_evidence_check", list(evidence = evidence, external_data_used = external_data_used, data_deficiencies_present = data_deficiencies_present, assessment_reference = assessment_reference),
    context, .s2_implementations[["technical_provision_data_evidence_check"]])
}

#' Inventory projection scope/uncertainty/expenses, not simulate cashflows.
#'
#' The 2027 expense basis reflects board decisions on new business; it does not
#' silently inherit the baseline assumption of future new business. Category
#' controls document relevance and appropriate inclusion, including reasoned
#' absence. Currency separation and gross reinsurance expenses remain explicit.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param scope_reference Scope reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("tp-projection-baseline-all-empty")
#' result <- do.call(technical_provision_projection_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
technical_provision_projection_evidence_check <- function(evidence, conditions, assessment_reference, scope_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "technical_provision_projection_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("technical_provision_projection_evidence_check", list(evidence = evidence, conditions = conditions, assessment_reference = assessment_reference, scope_reference = scope_reference),
    context, .s2_implementations[["technical_provision_projection_evidence_check"]])
}

#' Inventory validation and documentation references for an externally qualified scope.
#'
#' The declared interval is not a calendar or proof of actual completion.
#' Scope must identify all homogeneous groups and applicable segregated parts;
#' references alone do not prove completeness, adequacy, or expert review.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param declared_interval_years Declared interval years. Reference type: `float`.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param scope_reference Scope reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("tp-validation-audit-baseline-empty-evidence")
#' result <- do.call(technical_provision_validation_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
technical_provision_validation_evidence_check <- function(evidence, conditions, declared_interval_years, assessment_reference, scope_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "technical_provision_validation_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("technical_provision_validation_evidence_check", list(evidence = evidence, conditions = conditions, declared_interval_years = declared_interval_years, assessment_reference = assessment_reference, scope_reference = scope_reference),
    context, .s2_implementations[["technical_provision_validation_evidence_check"]])
}

#' Separate BE/RM method only; negative best estimates/total provisions are not floored.
#'
#' @param best_estimate Best estimate. Reference type: `float`.
#' @param risk_margin Risk margin. Reference type: `float`.
#' @param replication_applies Replication applies. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("tp-sum")
#' result <- do.call(technical_provisions_sum,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
technical_provisions_sum <- function(best_estimate, risk_margin, replication_applies, context, ...) {
  .s2_require(length(list(...)) == 0L, "technical_provisions_sum", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("technical_provisions_sum", list(best_estimate = best_estimate, risk_margin = risk_margin, replication_applies = replication_applies),
    context, .s2_implementations[["technical_provisions_sum"]])
}

#' Deduct the phased, optionally capped approved difference from current TP.
#'
#' Base amounts are NET on the Article308d(2) date or approved recalibration;
#' the current provision is gross, with current recoverables remaining separate.
#' None limit explicitly means no supervisory cap was imposed, not unknown.
#' A negative deduction is not silently floored or turned into relief.
#' @param current_gross_provisions Current gross provisions. Reference type: `float`.
#' @param base_solvency2_net_provisions Base solvency2 net provisions. Reference type: `float`.
#' @param base_legacy_net_provisions Base legacy net provisions. Reference type: `float`.
#' @param supervisory_deduction_limit Supervisory deduction limit. Reference type: `float | None`.
#' @param risk_free_transition_used Risk free transition used. Reference type: `bool | None`.
#' @param approval_reference Approval reference. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("ltg-transition-baseline-tp-cap-zero")
#' result <- do.call(technical_provisions_transitional_value,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
technical_provisions_transitional_value <- function(current_gross_provisions, base_solvency2_net_provisions, base_legacy_net_provisions, supervisory_deduction_limit, risk_free_transition_used, approval_reference, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "technical_provisions_transitional_value", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("technical_provisions_transitional_value", list(current_gross_provisions = current_gross_provisions, base_solvency2_net_provisions = base_solvency2_net_provisions, base_legacy_net_provisions = base_legacy_net_provisions, supervisory_deduction_limit = supervisory_deduction_limit, risk_free_transition_used = risk_free_transition_used, approval_reference = approval_reference, qualification_reference = qualification_reference),
    context, .s2_implementations[["technical_provisions_transitional_value"]])
}

#' Art86(a) lowest-level charge plus a fraction of its joint-FX difference.
#'
#' The hypothetical charge is externally valued under simultaneous Art188.
#' Multiple affected components need86(b)'s common capacity treatment, not
#' independent repeated limits or an invented allocation of shared capacity.
#' @param capital_charge Capital charge. Reference type: `float`.
#' @param joint_currency_stress_charge Joint currency stress charge. Reference type: `float`.
#' @param single_component_only Single component only. Reference type: `bool | None`.
#' @param eligibility_reference Eligibility reference. Reference type: `str`.
#' @param valuation_reference Valuation reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-10-05",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("final-numeric-baseline-fx-zero")
#' result <- do.call(underwriting_currency_basis_risk,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
underwriting_currency_basis_risk <- function(capital_charge, joint_currency_stress_charge, single_component_only, eligibility_reference, valuation_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "underwriting_currency_basis_risk", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("underwriting_currency_basis_risk", list(capital_charge = capital_charge, joint_currency_stress_charge = joint_currency_stress_charge, single_component_only = single_component_only, eligibility_reference = eligibility_reference, valuation_reference = valuation_reference),
    context, .s2_implementations[["underwriting_currency_basis_risk"]])
}

#' Literal formula; never infer percentage-point/fraction scaling of GM/ROCE.
#'
#' The archived DE definitions do not explicitly resolve that scaling. Means,
#' accounting equity definition and formula-native units need external review.
#' Debt/CFO is signed; no regulatory beta floor/cap is added.
#' @param gross_margin_mean Gross margin mean. Reference type: `float`.
#' @param debt Debt. Reference type: `float`.
#' @param cashflow_mean Cashflow mean. Reference type: `float`.
#' @param equity_return_mean Equity return mean. Reference type: `float`.
#' @param formula_units_qualified Formula units qualified. Reference type: `bool | None`.
#' @param input_basis_reference Input basis reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: beta.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("unlisted-equity-baseline-beta-intercept")
#' result <- do.call(unlisted_equity_beta,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
unlisted_equity_beta <- function(gross_margin_mean, debt, cashflow_mean, equity_return_mean, formula_units_qualified, input_basis_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "unlisted_equity_beta", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("unlisted_equity_beta", list(gross_margin_mean = gross_margin_mean, debt = debt, cashflow_mean = cashflow_mean, equity_return_mean = equity_return_mean, formula_units_qualified = formula_units_qualified, input_basis_reference = input_basis_reference),
    context, .s2_implementations[["unlisted_equity_beta"]])
}

#' Check all supplied issuers; book weights differ from concentration values.
#'
#' Input completeness, company consolidation, fiscal periods, FX conversion and
#' beta methodology are externally qualified. A failed condition yields0, an
#' unresolved condition raises REVIEW_REQUIRED instead of being treated asFalse.
#' @param investments Investments. Reference type: `Mapping`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("unlisted-equity-baseline-portfolio-negative-beta")
#' result <- do.call(unlisted_equity_portfolio_screen,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
unlisted_equity_portfolio_screen <- function(investments, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "unlisted_equity_portfolio_screen", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("unlisted_equity_portfolio_screen", list(investments = investments, qualification_reference = qualification_reference),
    context, .s2_implementations[["unlisted_equity_portfolio_screen"]])
}

#' Check the inclusive rated-portfolio share and qualified bond facts.
#'
#' The portfolio denominator excludes Article180(2-16) positions. Classification
#' against the referenced reporting definitions and liability cover is external;
#' a missing rating field is never treated as evidence of an unrated bond.
#' @param rated_share Rated share. Reference type: `float`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("proportionality-baseline-bond-all-rated")
#' result <- do.call(unrated_bond_simplification_conditions,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
unrated_bond_simplification_conditions <- function(rated_share, conditions, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "unrated_bond_simplification_conditions", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("unrated_bond_simplification_conditions", list(rated_share = rated_share, conditions = conditions, qualification_reference = qualification_reference),
    context, .s2_implementations[["unrated_bond_simplification_conditions"]])
}

#' Look up AnnexXVII credibility with the method's correct year basis.
#'
#' The year count is not evidence of data representativeness or required
#' consecutive history. Revision has no nonlife segment. Terminal table rows
#' apply to all later years; no interpolation or below-table extrapolation.
#' @param years Years. Reference type: `int`.
#' @param method Method. Reference type: `str`.
#' @param risk_module Risk module. Reference type: `str`.
#' @param segment Segment. Reference type: `str | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-credibility-baseline-life")
#' result <- do.call(usp_credibility_factor,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_credibility_factor <- function(years, method, risk_module, segment = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_credibility_factor", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_credibility_factor", list(years = years, method = method, risk_module = risk_module, segment = segment, qualification_reference = qualification_reference),
    context, .s2_implementations[["usp_credibility_factor"]])
}

#' Inventory active USP data references, never certify their content or quality.
#'
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param external_data_used External data used. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param multiple_external_sources Multiple external sources. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-governance-baseline-evidence-empty-true-true")
#' result <- do.call(usp_data_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_data_evidence_check <- function(evidence, external_data_used, multiple_external_sources, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_data_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_data_evidence_check", list(evidence = evidence, external_data_used = external_data_used, multiple_external_sources = multiple_external_sources, assessment_reference = assessment_reference),
    context, .s2_implementations[["usp_data_evidence_check"]])
}

#' Estimate raw NP′ for one homogeneous F1 excess-of-loss group.
#'
#' F1 uses the second RAW moment, not variance. ``upper_limit`` is the
#' absolute upper loss threshold b2, not layer width. Data representativeness,
#' five reporting years, expense exclusion, gross/net basis, lognormal fit
#' and treaty eligibility need external qualification. No approval or automatic
#' standard-formula substitution follows from this component.
#' @param ultimate_claims Ultimate claims. Reference type: `list[float]`. Supply an ordered R list or the documented numeric vector.
#' @param retention Retention. Reference type: `float`.
#' @param upper_limit Upper limit. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-f1-baseline-unlimited")
#' result <- do.call(usp_excess_of_loss_estimate,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_excess_of_loss_estimate <- function(ultimate_claims, retention, upper_limit = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_excess_of_loss_estimate", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_excess_of_loss_estimate", list(ultimate_claims = ultimate_claims, retention = retention, upper_limit = upper_limit, qualification_reference = qualification_reference),
    context, .s2_implementations[["usp_excess_of_loss_estimate"]])
}

#' Weight raw F1/F2 group estimates by externally qualified premium volumes.
#'
#' Same-segment homogeneity and Article116(3) volume measurement are not
#' inferred from group labels. This is a weighted mean, not diversification,
#' credibility weighting or permission to replace a standard parameter.
#' F2 only supports weighting externally qualified estimates here; its printed
#' estimator/moment anomalies are not repaired or evaluated. No inferred F2 cap.
#' @param estimated_factors Estimated factors. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param premium_volumes Premium volumes. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param method Method. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-f1-groups-baseline-single")
#' result <- do.call(usp_excess_of_loss_group_weighting,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_excess_of_loss_group_weighting <- function(estimated_factors, premium_volumes, qualification_reference, method = "nonproportional_1", context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_excess_of_loss_group_weighting", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_excess_of_loss_group_weighting", list(estimated_factors = estimated_factors, premium_volumes = premium_volumes, qualification_reference = qualification_reference, method = method),
    context, .s2_implementations[["usp_excess_of_loss_group_weighting"]])
}

#' Apply B/C1 finite-history correction BEFORE credibility blending.
#'
#' c*sigma_hat*sqrt((T+1)/(T-1)) + (1-c)*sigma_standard. This correction
#' does not belong to F1/F2 or reserve method2. Appropriate standard sigma
#' and fitted sigma qualification are explicit external inputs.
#' @param estimated_sigma Estimated sigma. Reference type: `float`.
#' @param standard_sigma Standard sigma. Reference type: `float`.
#' @param credibility Credibility. Reference type: `float`.
#' @param years Years. Reference type: `int`.
#' @param method Method. Reference type: `str`.
#' @param calibration_reference Calibration reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-lognormal-blend-baseline-premium-zero")
#' result <- do.call(usp_lognormal_credibility_blend,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_lognormal_credibility_blend <- function(estimated_sigma, standard_sigma, credibility, years, method, calibration_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_lognormal_credibility_blend", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_lognormal_credibility_blend", list(estimated_sigma = estimated_sigma, standard_sigma = standard_sigma, credibility = credibility, years = years, method = method, calibration_reference = calibration_reference),
    context, .s2_implementations[["usp_lognormal_credibility_blend"]])
}

#' Fit prescribed delta/gamma, returning raw sigma before credibility.
#'
#' Both boundary faces and deterministic interior starts are searched. Optional
#' numerical controls are reported, never treated as regulatory parameters.
#' Nonconvergence and constant log-ratios are REVIEW_REQUIRED, not zero sigma.
#' Data assumptions, consecutive history and supervisory approval stay external.
#' @param losses Losses. Reference type: `list[float]`. Supply an ordered R list or the documented numeric vector.
#' @param volumes Volumes. Reference type: `list[float]`. Supply an ordered R list or the documented numeric vector.
#' @param method Method. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param max_iterations Max iterations. Reference type: `int`.
#' @param gradient_tolerance Gradient tolerance. Reference type: `float`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-fit-baseline-premium-upper-boundary")
#' result <- do.call(usp_lognormal_fit,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_lognormal_fit <- function(losses, volumes, method, qualification_reference, context, max_iterations = 1000, gradient_tolerance = 1e-07, ...) {
  .s2_require(length(list(...)) == 0L, "usp_lognormal_fit", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_lognormal_fit", list(losses = losses, volumes = volumes, method = method, qualification_reference = qualification_reference, max_iterations = max_iterations, gradient_tolerance = gradient_tolerance),
    context, .s2_implementations[["usp_lognormal_fit"]])
}

#' Evaluate prescribed sigma and objective at explicit delta/gamma.
#'
#' This does NOT claim the supplied pair minimizes the objective. B uses
#' accident-year aggregated claims and earned premiums; C1 uses financial-year
#' end reserves plus payments for opening claims, and opening reserves.
#' Both include servicing expenses, unlike the F1 individual-claim estimator.
#' Consecutiveness, adjustments, statistical assumptions and fitting need
#' separate qualification. No automatic standard-formula replacement.
#' @param losses Losses. Reference type: `list[float]`. Supply an ordered R list or the documented numeric vector.
#' @param volumes Volumes. Reference type: `list[float]`. Supply an ordered R list or the documented numeric vector.
#' @param mixing Mixing. Reference type: `float`.
#' @param log_coefficient Log coefficient. Reference type: `float`.
#' @param method Method. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-lognormal-baseline-premium-delta0")
#' result <- do.call(usp_lognormal_sigma,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_lognormal_sigma <- function(losses, volumes, mixing, log_coefficient, method, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_lognormal_sigma", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_lognormal_sigma", list(losses = losses, volumes = volumes, mixing = mixing, log_coefficient = log_coefficient, method = method, qualification_reference = qualification_reference),
    context, .s2_implementations[["usp_lognormal_sigma"]])
}

#' Inventory method-specific references, with explicit gross/net and year bases.
#'
#' B/C/D/E include servicing expenses; F1/F2 exclude them. A reference is
#' never inspected or treated as verified data, calibration or approval.
#' This inventory neither enables the blocked F2 estimator nor extends the
#' square-triangle reserve2 calculator to rectangular development histories.
#' @param evidence Evidence. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param method Method. Reference type: `str`.
#' @param data_basis Data basis. Reference type: `str`.
#' @param assessment_reference Assessment reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-method-data-baseline-premium-net-empty")
#' result <- do.call(usp_method_data_evidence_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_method_data_evidence_check <- function(evidence, method, data_basis, assessment_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_method_data_evidence_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_method_data_evidence_check", list(evidence = evidence, method = method, data_basis = data_basis, assessment_reference = assessment_reference),
    context, .s2_implementations[["usp_method_data_evidence_check"]])
}

#' c*NP_est + (1-c)*NP_standard; separate from either F1/F2 estimator.
#'
#' Input-factor qualification and their source-defined calibration are external.
#' The engine neither estimates loss distributions here nor substitutes this
#' result into a standard-formula calculation or grants approval.
#' @param estimated_factor Estimated factor. Reference type: `float`.
#' @param standard_factor Standard factor. Reference type: `float`.
#' @param credibility Credibility. Reference type: `float`.
#' @param method Method. Reference type: `str`.
#' @param calibration_reference Calibration reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-blend-baseline-nonproportional_1-full")
#' result <- do.call(usp_nonproportional_credibility_blend,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_nonproportional_credibility_blend <- function(estimated_factor, standard_factor, credibility, method, calibration_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_nonproportional_credibility_blend", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_nonproportional_credibility_blend", list(estimated_factor = estimated_factor, standard_factor = standard_factor, credibility = credibility, method = method, calibration_reference = calibration_reference),
    context, .s2_implementations[["usp_nonproportional_credibility_blend"]])
}

#' Check declared replacement scope, not data/method fitness or authorization.
#'
#' Gross-premium sigma and the NP factor cannot both be replaced in one
#' segment. Revision is available only absent material inflation risk;
#' materiality follows decision relevance, not an invented numeric cutoff.
#' @param replacements Replacements. Reference type: `list[str]`. Supply an ordered R list or the documented numeric vector.
#' @param risk_module Risk module. Reference type: `str`.
#' @param segment Segment. Reference type: `str | None`.
#' @param qualified_reinsurance Qualified reinsurance. Reference type: `bool | None`.
#' @param material_inflation_risk Material inflation risk. Reference type: `bool | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-governance-baseline-segment-nonlife-1")
#' result <- do.call(usp_parameter_scope_check,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_parameter_scope_check <- function(replacements, risk_module, segment = NULL, qualified_reinsurance = NULL, material_inflation_risk = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_parameter_scope_check", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_parameter_scope_check", list(replacements = replacements, risk_module = risk_module, segment = segment, qualified_reinsurance = qualified_reinsurance, material_inflation_risk = material_inflation_risk, qualification_reference = qualification_reference),
    context, .s2_implementations[["usp_parameter_scope_check"]])
}

#' Check collective coverage and each contract's declared209/210/211/213 compliance.
#'
#' For multiple contracts the coverage conditions apply collectively, whereas
#' every individual contract must satisfy the cross-article requirements.
#' Qualified equivalent SPV arrangements are included; equivalent transfer is
#' externally assessed, never inferred from the name or number of contracts.
#' Finite/similar transfers invalidate this submitted contract set for USP;
#' omit none from the declaration (empty means all are qualified non-finite).
#' @param coverage_conditions Coverage conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param contract_requirements Contract requirements. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param contract_type Contract type. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param finite_reinsurance_ids Finite reinsurance ids. Reference type: `list[str] | tuple`. Supply an ordered R list or the documented numeric vector.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-governance-baseline-treaty-excess_of_loss-15")
#' result <- do.call(usp_reinsurance_contract_conditions,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_reinsurance_contract_conditions <- function(coverage_conditions, contract_requirements, contract_type, qualification_reference, finite_reinsurance_ids = list(), context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_reinsurance_contract_conditions", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_reinsurance_contract_conditions", list(coverage_conditions = coverage_conditions, contract_requirements = contract_requirements, contract_type = contract_type, qualification_reference = qualification_reference, finite_reinsurance_ids = finite_reinsurance_ids),
    context, .s2_implementations[["usp_reinsurance_contract_conditions"]])
}

#' D(4) linear blend, WITHOUT the different B/C1 finite-history correction.
#'
#' @param estimated_sigma Estimated sigma. Reference type: `float`.
#' @param standard_sigma Standard sigma. Reference type: `float`.
#' @param credibility Credibility. Reference type: `float`.
#' @param calibration_reference Calibration reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-reserve2-blend-baseline-large")
#' result <- do.call(usp_reserve2_credibility_blend,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_reserve2_credibility_blend <- function(estimated_sigma, standard_sigma, credibility, calibration_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_reserve2_credibility_blend", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_reserve2_credibility_blend", list(estimated_sigma = estimated_sigma, standard_sigma = standard_sigma, credibility = credibility, calibration_reference = calibration_reference),
    context, .s2_implementations[["usp_reserve2_credibility_blend"]])
}

#' Compute sqrt(MSEP)/reserve for a qualified square cumulative triangle.
#'
#' Rows run oldest to newest accident year with lengths n,n-1,...,1.
#' I>J is permitted by D(2)e but needs a separately qualified treatment of
#' the printed formula's out-of-range diagonal indices; no silent padding.
#' Consecutive years, expense inclusion, matching net reinsurance basis,
#' negligible oldest-year tail and stochastic assumptions remain external.
#' @param cumulative_claims Cumulative claims. Reference type: `list[list[float]]`. Supply an ordered R list or the documented numeric vector.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-reserve2-baseline-square")
#' result <- do.call(usp_reserve2_estimate,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_reserve2_estimate <- function(cumulative_claims, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_reserve2_estimate", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_reserve2_estimate", list(cumulative_claims = cumulative_claims, qualification_reference = qualification_reference),
    context, .s2_implementations[["usp_reserve2_estimate"]])
}

#' Return the upper numerical bracket of the prescribed compound quantile.
#'
#' Uses prescribed raw-data moments, not a Monte Carlo or internally chosen
#' insurer model. The lower/upper severity-rounding order is mathematical;
#' the floating-point guard is NOT formally certified interval arithmetic.
#' Numerical controls, backend versions, bounds and clipping are disclosed.
#' Nonconvergence or degenerate distribution fits require external review.
#' @param annual_benefits Annual benefits. Reference type: `Mapping[str, list[float]]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param relative_tolerance Relative tolerance. Reference type: `float`.
#' @param max_grid_power Max grid power. Reference type: `int`.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-revision-quantile-baseline-standard")
#' result <- do.call(usp_revision_compound_quantile,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_revision_compound_quantile <- function(annual_benefits, qualification_reference, context, relative_tolerance = 0.001, max_grid_power = 20, ...) {
  .s2_require(length(list(...)) == 0L, "usp_revision_compound_quantile", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_revision_compound_quantile", list(annual_benefits = annual_benefits, qualification_reference = qualification_reference, relative_tolerance = relative_tolerance, max_grid_power = max_grid_power),
    context, .s2_implementations[["usp_revision_compound_quantile"]])
}

#' c*(quantile-mean)/mean+(1-c)*S, preserving the source's absence of a floor.
#'
#' The quantile of the independent negative-binomial/lognormal compound sum is
#' externally qualified, NOT generated here. Probability must match the active
#' profile (including explicit analyst override). Credibility is supplied, not
#' inferred from the ambiguous printed E(4)a 'section7' cross-reference.
#' @param mean_increase Mean increase. Reference type: `float`.
#' @param quantile_increase Quantile increase. Reference type: `float`.
#' @param credibility Credibility. Reference type: `float`.
#' @param quantile_probability Quantile probability. Reference type: `float`.
#' @param risk_module Risk module. Reference type: `str`.
#' @param calibration_reference Calibration reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-revision-blend-baseline-life-no-floor")
#' result <- do.call(usp_revision_credibility_blend,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_revision_credibility_blend <- function(mean_increase, quantile_increase, credibility, quantile_probability, risk_module, calibration_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_revision_credibility_blend", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_revision_credibility_blend", list(mean_increase = mean_increase, quantile_increase = quantile_increase, credibility = credibility, quantile_probability = quantile_probability, risk_module = risk_module, calibration_reference = calibration_reference),
    context, .s2_implementations[["usp_revision_credibility_blend"]])
}

#' Return expected annual increases and prescribed sample moments.
#'
#' Each beneficiary supplies aligned A0..AT, including the initial baseline.
#' Only strictly positive changes enter severity statistics. Frequency includes
#' ALL years, including zero-event years. Negative changes are not netted.
#' Computable moments do not prove negative-binomial/lognormal suitability,
#' independence, immaterial inflation risk or the prescribed aggregate quantile.
#' @param annual_benefits Annual benefits. Reference type: `Mapping[str, list[float]]`. Supply a named R list with the keys shown in the example/reference case.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-revision-moments-baseline-mixed")
#' result <- do.call(usp_revision_moments,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_revision_moments <- function(annual_benefits, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_revision_moments", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_revision_moments", list(annual_benefits = annual_benefits, qualification_reference = qualification_reference),
    context, .s2_implementations[["usp_revision_moments"]])
}

#' Select a qualified more accurate method, otherwise conservative capital result.
#'
#' Capital requirements must be comparable under the same qualified inputs.
#' We do not assume that the largest raw parameter always gives the largest
#' capital requirement. Ties use a disclosed deterministic technical ordering,
#' not a claimed regulatory preference for one standardized method.
#' @param method_results Method results. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param parameter Parameter. Reference type: `str`.
#' @param superiority_demonstrated Superiority demonstrated. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param superior_method Superior method. Reference type: `str | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: factor.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("usp-governance-baseline-selection-single-premium_sigma-premium")
#' result <- do.call(usp_standard_method_selection,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
usp_standard_method_selection <- function(method_results, parameter, superiority_demonstrated, superior_method = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "usp_standard_method_selection", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("usp_standard_method_selection", list(method_results = method_results, parameter = parameter, superiority_demonstrated = superiority_demonstrated, superior_method = superior_method, qualification_reference = qualification_reference),
    context, .s2_implementations[["usp_standard_method_selection"]])
}

#' Deduct already scope-aligned goodwill and Article12-zero intangibles.
#'
#' Method eligibility, hierarchy and Article13(2) exclusions are external.
#' Inputs use the same held-participation basis; never apply ownership twice.
#' @param participation_value Participation value. Reference type: `float`.
#' @param goodwill_amount Goodwill amount. Reference type: `float`.
#' @param zero_valued_intangibles_amount Zero valued intangibles amount. Reference type: `float`.
#' @param method Method. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("valuation-baseline-accounting-negative-ias_equity_fallback")
#' result <- do.call(valuation_accounting_equity_adjustment,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
valuation_accounting_equity_adjustment <- function(participation_value, goodwill_amount, zero_valued_intangibles_amount, method, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "valuation_accounting_equity_adjustment", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("valuation_accounting_equity_adjustment", list(participation_value = participation_value, goodwill_amount = goodwill_amount, zero_valued_intangibles_amount = zero_valued_intangibles_amount, method = method, qualification_reference = qualification_reference),
    context, .s2_implementations[["valuation_accounting_equity_adjustment"]])
}

#' Check declared Article9(4) facts; never establish method compatibility.
#'
#' The IFRS-group exclusion is distinct from the four Article9 conditions.
#' Article13/16 restrictions and substantive comparison evidence remain separate.
#' @param conditions Conditions. Reference type: `Mapping[str, bool]`. Supply a named R list with the keys shown in the example/reference case.
#' @param in_ifrs_consolidation_group In ifrs consolidation group. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: indicator.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("valuation-accounts-baseline-group")
#' result <- do.call(valuation_accounts_method_conditions,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
valuation_accounts_method_conditions <- function(conditions, in_ifrs_consolidation_group, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "valuation_accounts_method_conditions", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("valuation_accounts_method_conditions", list(conditions = conditions, in_ifrs_consolidation_group = in_ifrs_consolidation_group, qualification_reference = qualification_reference),
    context, .s2_implementations[["valuation_accounts_method_conditions"]])
}

#' Apply the held share to externally qualified Article13 net assets.
#'
#' No floor, method-ranking decision, Article13(2) exclusion determination or
#' asset/technical-provision valuation is inferred from this arithmetic.
#' @param excess_assets_over_liabilities Excess assets over liabilities. Reference type: `float`.
#' @param share Share. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("valuation-baseline-equity-zero")
#' result <- do.call(valuation_adjusted_equity_component,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
valuation_adjusted_equity_component <- function(excess_assets_over_liabilities, share, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "valuation_adjusted_equity_component", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("valuation_adjusted_equity_component", list(excess_assets_over_liabilities = excess_assets_over_liabilities, share = share, qualification_reference = qualification_reference),
    context, .s2_implementations[["valuation_adjusted_equity_component"]])
}

#' Sum externally qualified positions, retaining negative values and surplus.
#'
#' Own shares remain assets and subordinated debt remains a liability here;
#' the separate Article88 component subsequently adjusts those exactly once.
#' A declared complete input scope does not certify a complete insurer balance.
#' @param assets Assets. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param liabilities Liabilities. Reference type: `Mapping[str, float]`. Supply a named R list with the keys shown in the example/reference case.
#' @param complete_scope Complete scope. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("valuation-balance-baseline-zero")
#' result <- do.call(valuation_balance_sheet_excess,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
valuation_balance_sheet_excess <- function(assets, liabilities, complete_scope, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "valuation_balance_sheet_excess", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("valuation_balance_sheet_excess", list(assets = assets, liabilities = liabilities, complete_scope = complete_scope, qualification_reference = qualification_reference),
    context, .s2_implementations[["valuation_balance_sheet_excess"]])
}

#' Discount externally expected settlement payments on a basic risk-free curve.
#'
#' Recognition/materiality, complete runoff, probability weighting and curve
#' construction are external. In particular VA/MA curves are not accepted.
#' @param cashflows Cashflows. Reference type: `Mapping[str, dict]`. Supply a named R list with the keys shown in the example/reference case.
#' @param recognised_under_article11 Recognised under article11. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param runoff_complete Runoff complete. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param curve_basis Curve basis. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param curve_reference Curve reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("valuation-extra-baseline-zero")
#' result <- do.call(valuation_contingent_liability,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
valuation_contingent_liability <- function(cashflows, recognised_under_article11, runoff_complete, curve_basis, qualification_reference, curve_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "valuation_contingent_liability", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("valuation_contingent_liability", list(cashflows = cashflows, recognised_under_article11 = recognised_under_article11, runoff_complete = runoff_complete, curve_basis = curve_basis, qualification_reference = qualification_reference, curve_reference = curve_reference),
    context, .s2_implementations[["valuation_contingent_liability"]])
}

#' Net externally recognised, undiscounted tax balances only when qualified.
#'
#' Positive results are net assets; negative results are net liabilities.
#' No tax rates, taxable-profit projections, expiry or LAC-DT are calculated.
#' @param deferred_tax_assets Deferred tax assets. Reference type: `float`.
#' @param deferred_tax_liabilities Deferred tax liabilities. Reference type: `float`.
#' @param legally_enforceable_offset Legally enforceable offset. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param same_tax_authority Same tax authority. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param same_taxable_entity Same taxable entity. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param undiscounted Undiscounted. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param excluded_participations Excluded participations. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param valuation_level Valuation level. Reference type: `str | None`.
#' @param recognition_conditions Recognition conditions. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param recognition_documents Recognition documents. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("valuation-tax-net-baseline-zero")
#' result <- do.call(valuation_deferred_tax_net,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
valuation_deferred_tax_net <- function(deferred_tax_assets, deferred_tax_liabilities, legally_enforceable_offset, same_tax_authority, same_taxable_entity, undiscounted, qualification_reference, context, excluded_participations = NULL, valuation_level = NULL, recognition_conditions = NULL, recognition_documents = NULL, ...) {
  .s2_require(length(list(...)) == 0L, "valuation_deferred_tax_net", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("valuation_deferred_tax_net", list(deferred_tax_assets = deferred_tax_assets, deferred_tax_liabilities = deferred_tax_liabilities, legally_enforceable_offset = legally_enforceable_offset, same_tax_authority = same_tax_authority, same_taxable_entity = same_taxable_entity, undiscounted = undiscounted, qualification_reference = qualification_reference, excluded_participations = excluded_participations, valuation_level = valuation_level, recognition_conditions = recognition_conditions, recognition_documents = recognition_documents),
    context, .s2_implementations[["valuation_deferred_tax_net"]])
}

#' Apply goodwill/other-intangible zero rules, not a fair-value model.
#'
#' Positive recognition requires both external Article12 qualifications and a
#' separately supplied Article10 value. Unused values are rejected, not ignored.
#' @param asset_type Asset type. Reference type: `str`.
#' @param separately_saleable Separately saleable. Reference type: `bool | None`.
#' @param active_market_value_evidenced Active market value evidenced. Reference type: `bool | None`.
#' @param qualified_value Qualified value. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-23",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("valuation-baseline-goodwill-zero")
#' result <- do.call(valuation_intangible_value,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
valuation_intangible_value <- function(asset_type, separately_saleable = NULL, active_market_value_evidenced = NULL, qualified_value = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "valuation_intangible_value", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("valuation_intangible_value", list(asset_type = asset_type, separately_saleable = separately_saleable, active_market_value_evidenced = active_market_value_evidenced, qualified_value = qualified_value, qualification_reference = qualification_reference),
    context, .s2_implementations[["valuation_intangible_value"]])
}

#' Remove externally isolated cumulative own-credit value change since initial recognition.
#'
#' The input is a monetary valuation effect, not a spread or a period-only
#' delta. Initial-recognition credit is retained; do not also apply this
#' correction to a bottom-up value already holding initial credit constant.
#' @param current_value Current value. Reference type: `float`.
#' @param own_credit_change Own credit change. Reference type: `float`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("valuation-own-credit-baseline-signed")
#' result <- do.call(valuation_liability_excluding_own_credit,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
valuation_liability_excluding_own_credit <- function(current_value, own_credit_change, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "valuation_liability_excluding_own_credit", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("valuation_liability_excluding_own_credit", list(current_value = current_value, own_credit_change = own_credit_change, qualification_reference = qualification_reference),
    context, .s2_implementations[["valuation_liability_excluding_own_credit"]])
}

#' Forward a value only on an explicitly supported Article16 method path.
#'
#' The closed method vocabulary describes this component's supported scope; a
#' rejected unimplemented technique is not thereby declared unlawful. Required
#' cost flags mean material costs were assessed and adjusted where necessary.
#' The future deposit exception delegates its profile, threshold and override
#' checks to the existing component. No zero/default substitutes for missing data.
#' @param qualified_value Qualified value. Reference type: `float`.
#' @param position_type Position type. Reference type: `str`.
#' @param method Method. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param conditions Conditions. Reference type: `Mapping[str, bool] | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param deposit_terms Deposit terms. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param hierarchy Hierarchy. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param own_credit Own credit. Reference type: `Mapping | None`. Supply a named R list with the keys shown in the example/reference case.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("valuation-method-baseline-16-3-pass")
#' result <- do.call(valuation_method_checked_value,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
valuation_method_checked_value <- function(qualified_value, position_type, method, qualification_reference, conditions = NULL, deposit_terms = NULL, hierarchy = NULL, own_credit = NULL, context, ...) {
  .s2_require(length(list(...)) == 0L, "valuation_method_checked_value", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("valuation_method_checked_value", list(qualified_value = qualified_value, position_type = position_type, method = method, qualification_reference = qualification_reference, conditions = conditions, deposit_terms = deposit_terms, hierarchy = hierarchy, own_credit = own_credit),
    context, .s2_implementations[["valuation_method_checked_value"]])
}

#' Apply two externally qualified exclusions, not a group-scope decision.
#'
#' Flags refer specifically to Directive214(2) and229. Otherwise the external
#' Article13 value passes through without a floor or a second ownership share.
#' @param excluded_from_group_supervision Excluded from group supervision. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param deducted_from_group_own_funds Deducted from group own funds. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param qualified_value Qualified value. Reference type: `float | None`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2025-01-17",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
#' case <- reference_case("valuation-extra-baseline-retained")
#' result <- do.call(valuation_participation_value,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
valuation_participation_value <- function(excluded_from_group_supervision, deducted_from_group_own_funds, qualified_value = NULL, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "valuation_participation_value", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("valuation_participation_value", list(excluded_from_group_supervision = excluded_from_group_supervision, deducted_from_group_own_funds = deducted_from_group_own_funds, qualified_value = qualified_value, qualification_reference = qualification_reference),
    context, .s2_implementations[["valuation_participation_value"]])
}

#' Apply the 2027 deposit exception, without computing cost or amortisation.
#'
#' Deposit classification, term basis, SNC status and materiality are external.
#' Both comparisons are strict; failure does not invent a fair-value fallback.
#' @param cost_value Cost value. Reference type: `float`.
#' @param term_years Term years. Reference type: `float`.
#' @param small_non_complex Small non complex. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param no_material_valuation_error No material valuation error. Reference type: `bool`. Supply a single TRUE or FALSE, not a numeric flag.
#' @param alternative_value Alternative value. Reference type: `float | None`.
#' @param method Method. Reference type: `str`.
#' @param qualification_reference Qualification reference. Reference type: `str`.
#' @param context Explicit [Context()] selecting currency, dates, rule profile and overrides.
#' @param ... Reserved; unknown inputs are rejected rather than ignored.
#' @return A `Result` list with `value`, `unit`, `formula_id`, `sources`,
#'   `context`, reproducible R `input_hash`/`profile_hash`, `details` and `status`.
#'   Value unit: context currency.
#' @details Monetary inputs use context currency, not implicit thousands.
#'   Ratios and rates are decimal fractions unless a named input says otherwise.
#'   Missing, NA, NaN and infinite numeric inputs are rejected; vectors are not
#'   silently recycled. Scope and applicability guards follow the Golden Source.
#' @seealso [Context()], [NumericOverride()], [reference_cases()]
#' @examples
#' context <- Context(
#'   as_of = "2027-01-30",
#'   known_at = "2026-09-30",
#'   currency = "EUR",
#'   profile_id = "EU-S2-DOCUMENTS-2027-v0.1")
#' case <- reference_case("valuation-deposit-future-cost")
#' result <- do.call(valuation_short_term_deposit_cost_2027,
#'   c(case$kwargs, list(context = context)))
#' result$value
#' @export
valuation_short_term_deposit_cost_2027 <- function(cost_value, term_years, small_non_complex, no_material_valuation_error, alternative_value = NULL, method, qualification_reference, context, ...) {
  .s2_require(length(list(...)) == 0L, "valuation_short_term_deposit_cost_2027", "unknown input argument")
  if (missing(context)) .s2_fail("MISSING_INPUT", "context", "Context required")
  .s2_calculate("valuation_short_term_deposit_cost_2027", list(cost_value = cost_value, term_years = term_years, small_non_complex = small_non_complex, no_material_valuation_error = no_material_valuation_error, alternative_value = alternative_value, method = method, qualification_reference = qualification_reference),
    context, .s2_implementations[["valuation_short_term_deposit_cost_2027"]])
}
