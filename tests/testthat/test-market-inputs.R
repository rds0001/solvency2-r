test_that("caller-owned market tables work without demo fallbacks", {
  context <- Context("2025-02-28", "2026-09-23", "EUR", "EU-S2-DOCUMENTS-2024-2025-v0.1")
  hash <- paste(rep("a", 64), collapse = "")
  dataset <- list(data_kind = "USER_SUPPLIED", source_id = "TEST_CALLER_INPUT", retrieved_at = "2025-03-01",
    archive_sha256 = hash, member = "local.json", member_sha256 = hash, maturities = list(1),
    rate_row_offset = 0, unit = "decimal_rate", blocked_series = setNames(list(), character()),
    series = list(EUR = list(raw_currency_metadata = "EUR", label = "Synthetic caller EUR", column = "C1",
      currency_metadata_status = "USER_SUPPLIED", variants = list(no_va = list(raw_metadata = list(VA = 0), sheet = "grid", rates = list(.07))))))
  data <- list(schema_version = 1L, status = "USER_SUPPLIED", datasets = list("2025-02-28" = dataset))
  spot <- function(payload) rfr_archived_spot_rate("EUR", 1, "no_va", "synthetic", context, market_data = payload)
  actual <- spot(data)
  expect_equal(actual$value, .07)
  expect_identical(actual$status, "REFERENCE_COMPONENT")
  expect_false("SYNTHETIC_DEMO" %in% unlist(actual$sources))
  bad <- data
  bad$datasets[[1]]$series$EUR$variants$no_va$rates <- NULL
  error <- tryCatch(spot(bad), CalculationError = identity)
  expect_identical(error$code, "INVALID_INPUT")
  expect_identical(error$field, "market_data")
  bad <- data; bad$datasets[[1]]$extra <- NaN
  expect_error(spot(bad), class = "CalculationError")
  expect_equal(data$datasets[[1]]$series$EUR$variants$no_va$rates[[1]], .07)
})
