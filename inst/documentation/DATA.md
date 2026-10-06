# Local inputs and reference data

The package never downloads market data. Supply your own dated inputs and keep
their provenance with your calculation. Bundled synthetic scenarios demonstrate
interfaces and are not production market observations.

Use `?DiscountCurve`, `?load_discount_curve_json` and
`?load_discount_curve_csv` for the supported local file schemas and runnable
examples. Curve factors are discount factors, not percentage interest rates.
Use `discount_curve_from_spot_rates()` for explicit spot-rate conversion.

JSON objects correspond to named R lists; JSON arrays to ordered lists.
Use `NULL` for an explicitly nullable field, not `NA`. Numeric inputs must be
finite. Empty objects can be represented as `setNames(list(), character())`.
Do not flatten nested reference datasets into data frames indiscriminately:
order, empty arrays and object keys can carry meaning.

`reference_datasets()` lists available datasets; `reference_dataset()` reads
one. `reference_cases()` and `reference_case()` expose frozen synthetic inputs
and expected outcomes. `parameter()` and its help page describe parameter
access. Retain resource and source hashes for audit trails.

Archived reporting definitions have their own license and attribution in
`documentation("THIRD_PARTY_NOTICES")`; the package license does not replace it.
