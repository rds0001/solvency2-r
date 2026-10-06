# solvency2 for R

[![R package checks](https://github.com/rds0001/solvency2-r/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/rds0001/solvency2-r/actions/workflows/R-CMD-check.yaml)

Native R tools for offline Solvency II analysis: 399 granular calculations,
source-bound parameters and correlation matrices, dated rule profiles, synthetic
reference portfolios, and archived reporting definitions and checks.

Use one formula in a spreadsheet replacement, combine components in your own
valuation pipeline, inspect a regulatory parameter, or run a dependency graph.
You do not need to adopt a monolithic application or install Python.

## Start with a calculation

```r
library(solvency2)

ctx <- Context(
  as_of = "2025-01-17", known_at = "2026-09-23", currency = "EUR",
  profile_id = "EU-S2-DOCUMENTS-2024-2025-v0.1"
)

charge <- intangible_risk(100, context = ctx)
charge$value
charge$sources
charge$details
```

Results retain their units, formula identifier, explicit context, source
references, input/profile fingerprints and qualification limits. Monetary values
use the stated currency, not implicit thousands; rates are decimal fractions
unless the individual function documents another convention.

## What you can use

- Valuation and technical-provision components, explicit discounting, risk-free
  rate calculations, adjustments, risk margin and reinsurance.
- Market, counterparty-default, life, health and non-life standard-formula
  components, including catastrophe geography, weights and correlation matrices.
- SCR aggregation, own-funds classification and eligibility, MCR, group and
  restricted-portfolio calculations, and prescribed USP numerical methods.
- Dated applicability and evidence checks, including the future profiles actually
  contained in the reference version. No rule set is selected from today's date.
- Archived DPM catalogues, typed reporting expressions, closed-cell bindings,
  filing preconditions and activation evidence; deterministic XBRL draft assembly
  with currency, precision and explicitly selected rule checks.

The exact scope is available through `coverage()`, `rule_profiles()` and
`api_catalogue()`. Every calculation has an R help page and an executable example.
The catalogue preserves the Python reference's domain paths and source signatures;
use `?function_name` or `args(function_name)` for the R calling convention.

## Supply your own data

There is no runtime internet access, background update or market-data download.
Obtain production curves, cashflows, holdings and qualifications yourself.
`DiscountCurve()`, `load_discount_curve_json()`, `load_discount_curve_csv()` and
`discount_curve_from_spot_rates()` provide explicit local input routes.
`best_estimate_with_curve()` binds exact payment-date factors to supplied cashflows.
No interpolation, extrapolation, calendar convention or currency conversion is
silently selected. Archived rate APIs accept explicit `market_data` inputs.

Bundled market examples are invented and marked `SYNTHETIC_DEMO`; they are not
observed production curves. Regulatory constants and source matrices are not
synthetic.

## Inspect or change a parameter

```r
parameter("scalars/intangible", ctx)
parameter("matrices/health_nslt", ctx)

scenario <- ctx
scenario$overrides <- list(NumericOverride(
  "profile.json", list("scalars", "intangible", "value"),
  0.70, "Illustrative analyst sensitivity, not a regulatory amendment"
))
intangible_risk(100, context = scenario)$value
intangible_risk(100, context = ctx)$value  # installed baseline unchanged
```

Overrides are context-local, validated and audited with original values and
reasons. JSON array paths use **zero-based indices**, also in R. Symmetric matrix
overrides update the counterpart consistently. Version guards and nonnumeric
regulatory qualifications cannot be bypassed with a numeric override.

## Reproduce and combine

`reference_cases()` contains 12,382 fixed independent calculation cases and
`reference_portfolios()` contains 227 integration portfolios. Additional synthetic
reporting datasets are accessible with `reference_datasets()` and
`reference_dataset()`. These include valid, boundary and deliberately rejected
inputs; they are not all successful calculation examples.

```r
p <- reference_portfolios()$portfolios[[1]]
result <- calculate(p$nodes, context = do.call(Context, p$context))
names(result$results)
```

An `Engine()` instance offers the same `run()` interface with an instance-local
cache. Dependencies pass numeric values while provenance remains in node results.
Demo provenance also propagates through dependent nodes.

## Installation and documentation

From a checked-out source directory with R and the declared dependencies installed:

```sh
R CMD build solvency2-r
R CMD INSTALL solvency2_0.5.0.tar.gz
```

The package requires R >= 4.1, jsonlite, digest, gmp, Rmpfr and xml2. Source builds
of the last three may need the corresponding GMP, MPFR and libxml2 development
libraries; binary R packages already provide the compiled dependencies.
Python, NumPy and SciPy are not runtime dependencies.

Use `help(package = "solvency2")`, `vignette("getting-started", package = "solvency2")`
and `documentation()` for local help. `export_resources()` writes verified JSON
to an explicitly requested new directory; normal calculations do not write files.

## Reference compatibility and boundaries

The reference is the published [Python solvency2 0.5.0](https://github.com/rds0001/solvency2-python).
`GOLDEN_SOURCE.json` records its commit and source fingerprints. Data are preserved
losslessly, including provenance and dated scope restrictions. R uses native
GMP/MPFR arithmetic, `stats::optim` and `stats::fft`; numerical backend diagnostics,
R input hashes and XML serialization hashes identify the implementation actually
used rather than imitating Python runtime metadata.

The package implements the reference's documented components. It does not develop
an insurer's internal model, perform actuarial projection on behalf of the user,
grant supervisory approval, infer complete reporting obligations or submit filings.
Externally supplied model results and qualifications remain explicit inputs.
`REFERENCE_COMPONENT`, `ANALYST_SCENARIO`, `SYNTHETIC_DEMO`, `PARTIAL` and
`DRAFT_XBRL_INSTANCE` describe distinct outcomes; none means regulatory approval.
Unsupported or ambiguous situations remain visible through structured errors or
review findings, never an invented default result.

## License and attribution

Original code and invented demonstration data: RiskDataScience GmbH,
GPL-3.0-or-later. Contact: riskdatascience@web.de.
Regulatory and reporting extracts retain their separately identified reuse terms;
see `inst/THIRD_PARTY_NOTICES.md`, `inst/THIRD_PARTY_DATA_REGISTER.json` and
`inst/licenses/`. No observed historical market-data archive is included.
