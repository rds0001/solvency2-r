test_that("independent reporting component fixtures retain their expected outcomes", {
  datasets <- c("reference_reporting_cases.json", "reference_reporting_fact_context_cases.json",
    "reference_reporting_string_cases.json", "reference_reporting_typed_cases.json",
    "reference_reporting_metric_cases.json", "reference_reporting_validation_status.json", "reference_reporting_scope_cases.json")
  for (dataset in datasets) for (case in reference_dataset(dataset)$cases) {
    actual <- tryCatch({
      if (!is.null(case$facts)) reporting_closed_rule_check(case$validation_code, case$facts, case$technical_version, case$fact_context, case$binding_reference) else
        if (!is.null(case$metric_code)) reporting_metric_value_check(case$metric_code, case$fact, case$technical_version) else
          if (dataset == "reference_reporting_validation_status.json") reporting_validation_status(case$validation_code, case$technical_version, case$module) else
            reporting_bound_rule_check(case$validation_code, case$bindings, case$technical_version, case$binding_reference)
    }, error = identity)
    if (!is.null(case$expected_error)) {
      expect_true(inherits(actual, "CalculationError"), info = case$id)
      expect_identical(actual$code, case$expected_error, info = case$id)
    } else {
      expect_false(inherits(actual, "error"), info = case$id)
      if (inherits(actual, "error")) next
      if (is.list(case$expected)) for (key in names(case$expected)) expect_identical(actual[[key]], case$expected[[key]], info = case$id) else
        expect_identical(actual$value, case$expected, info = case$id)
      expect_false(actual$full_report_validated, info = case$id)
    }
  }
})

test_that("same qualified portfolio facts support draft XML and selected rule checks", {
  for (p in reference_dataset("reference_reporting_portfolios.json")$portfolios) {
    facts <- lapply(p$facts, function(fact) list(business_code = fact$cell, atom = fact$fact))
    call <- function() reporting_instance(facts, p$technical_version, p$module,
      list(scheme = "SC", identifier = "SYNTHETIC-NOT-FOR-SUBMISSION"), p$fact_context$period,
      p$filing_indicators, p$binding_reference, list(p$validation_code))
    if (!all(unlist(p$expected$metric_values))) {
      expect_error(call(), class = "CalculationError")
    } else {
      result <- call()
      expect_identical(result$selected_rule_checks[[1L]]$status, if (p$expected$expression) "PASS" else "FAIL", info = p$id)
      expect_true(result$xml_well_formed)
      expect_false(result$submission_ready)
      expect_false(result$full_report_validated)
      expect_equal(length(result$reconciliation), length(facts))
      expect_identical(result$xml_sha256, digest::digest(enc2utf8(result$xml), algo = "sha256", serialize = FALSE))
    }
    condition <- reporting_rule_precondition(p$validation_code, p$filing_indicators, p$technical_version, p$module)
    expect_identical(condition$precondition_met, p$expected$filing_precondition_met)
  }
})

test_that("reporting decimal lexical forms are base ten and exact", {
  for (text in c("0.25", "00.25", ".25", "2.5e-1", "+0.25"))
    expect_equal(as.character(solvency2:::.s2_reporting_rational(text)), "1/4")
  expect_equal(solvency2:::.s2_xml_decimal(gmp::as.bigq(1, 8)), "0.125")
  expect_error(solvency2:::.s2_xml_decimal(gmp::as.bigq(1, 3)), "Nonterminating")
  expect_true(solvency2:::.s2_expression_evaluate("5.5677 i= exp(31,1,2)", function(selector) NULL))
})
