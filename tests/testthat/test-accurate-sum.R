test_that("accurate summation preserves coverage boundaries on every platform", {
  fsum <- solvency2:::.s2_fsum
  expect_identical(fsum(list(50, 35.01, 14.99)), 100)
  expect_identical(fsum(list(1e16, 1, -1e16)), 1)
  expect_identical(fsum(list(-1e16, 1, 1e16)), 1)
  expect_identical(fsum(list()), 0)
  expect_identical(fsum(list(35.01, 14.99)), 50)
  expect_identical(fsum(list(1, 2^-53, 2^-54)), 1 + 2^-52)

  ctx <- Context("2025-01-17", "2026-09-23", "EUR",
    "EU-S2-DOCUMENTS-2024-2025-v0.1")
  result <- own_funds_scr_allocation(100, 50, 0, 35.01, 14.99, context = ctx)
  expect_identical(result$value, 100)
  # The regulatory strict limit is unchanged: exactly 15% still fails.
  expect_error(own_funds_scr_allocation(100, 50, 0, 35, 15, context = ctx),
    class = "CalculationError")
})
