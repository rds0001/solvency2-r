# Reference-based validation

The bundled suite covers 12,382 calculation cases and 227 linked calculation
portfolios, together with reporting reference datasets and interface tests.
Expected outcomes come from frozen reference resources, not the R output under
test. Numeric comparisons use case-specific tolerances; structured error codes,
units, statuses and provenance are checked separately.

Native numerical backend diagnostics and serialization hashes are not required
to be byte-identical across languages. Original resource provenance must remain
intact. A passing software test is not regulatory approval or validation of an
insurer's inputs, assumptions or model. Package-check results belong to the
specific tested version and environment.
