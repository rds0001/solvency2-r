test_that("native capital and aggregation functions match fixed independent references", {
  cases <- reference_cases()$cases
  implemented <- names(solvency2:::.s2_implementations)
  for (case in cases) {
    if (!(case[["function"]] %in% implemented)) next
    context <- list(as_of = "2025-01-17", known_at = "2026-09-23", currency = "EUR",
                    profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1")
    context[names(case$context)] <- case$context
    actual <- tryCatch(do.call(getExportedValue("solvency2", case[["function"]]),
      c(case$kwargs, list(context = do.call(Context, context)))), s2_error = identity)
    if (!is.null(case$expected_error)) {
      expect_true(inherits(actual, "s2_error"), info = case$id)
      expect_identical(actual$code, case$expected_error, info = case$id)
    } else {
      expect_true(inherits(actual, "Result"), info = case$id)
      if (inherits(actual, "error")) next
      tolerance <- if (is.null(case$absolute_tolerance)) 1e-12 else case$absolute_tolerance
      expect_true(abs(actual$value - case$expected) <= tolerance + 1e-12 * abs(case$expected), info = case$id)
      expected_status <- if (identical(actual$details$market_data_kind, "SYNTHETIC_DEMO")) "SYNTHETIC_DEMO" else
        if (length(context$overrides)) "ANALYST_SCENARIO" else "REFERENCE_COMPONENT"
      expect_identical(actual$status, expected_status, info = case$id)
      expect_length(actual$input_hash, 1L)
      expect_equal(nchar(actual$input_hash), 64L)
      expect_true(length(actual$sources) > 0L, info = case$id)
    }
  }
})

test_that("scalar and symmetric matrix overrides are isolated and traced", {
  base <- Context("2025-01-17", "2026-09-23", "EUR", "EU-S2-DOCUMENTS-2024-2025-v0.1")
  scenario <- base
  scenario$overrides <- list(NumericOverride("profile.json", list("scalars", "intangible", "value"), .7, "Synthetic"))
  expect_equal(intangible_risk(100, context = scenario)$value, 70)
  expect_equal(intangible_risk(100, context = base)$value, 80)
  expect_identical(intangible_risk(100, context = scenario)$status, "ANALYST_SCENARIO")
  scenario$overrides <- list(NumericOverride("profile.json", list("matrices", "health_nslt", "values", 0L, 1L), .5, "Synthetic"))
  matrix <- parameter("matrices/health_nslt", context = scenario)$values
  expect_equal(matrix[[1L]][[2L]], matrix[[2L]][[1L]])
  expect_equal(aggregate_risk(list(premium_reserve = 3, lapse = 4), "health_nslt", context = scenario)$value, sqrt(37))
  scenario$overrides[[2L]] <- NumericOverride("profile.json", list("matrices", "health_nslt", "values", 1L, 0L), .5, "Duplicate")
  expect_error(intangible_risk(1, context = scenario), class = "s2_error")
})

test_that("context validation rejects invalid dates, missing profiles and recycling", {
  expect_error(Context("2025-02-30", "2026-09-23", "EUR", "EU-S2-DOCUMENTS-2024-2025-v0.1"), class = "s2_error")
  base <- Context("2025-01-17", "2026-09-23", "EUR", "EU-S2-DOCUMENTS-2024-2025-v0.1")
  for (value in list(NA_real_, NaN, Inf, TRUE, c(1, 2))) {
    expect_error(intangible_risk(value, context = base), class = "s2_error")
  }
  expect_error(Context("2025-01-17", "2026-09-23", "EUR", "latest"), class = "s2_error")
})
