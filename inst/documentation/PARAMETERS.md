# Inspecting and overriding parameters

Choose the rule profile explicitly in `Context()`. Inspect available parameters
with `parameter()`; its help page shows the supported identifiers and schemas.
Parameters include correlations and catastrophe data, not just scalar factors.

Use `NumericOverride()` in a calculation context to change supported numbers.
Paths into arrays use zero-based indices, matching the reference contract.
Provide a reason and apply symmetric correlation edits consistently. Overrides
are local to the context: they do not modify installed resources or another
analyst's context. Results record override provenance and profile hashes.

An override documents an analyst choice; it does not establish supervisory
approval. Reporting filing-policy overrides are supplied through the reporting
interface, independently of calculation-context overrides. See the executable
examples in `?NumericOverride` and the reporting help pages before adapting one.
