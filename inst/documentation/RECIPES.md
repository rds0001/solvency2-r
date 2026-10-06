# Practical workflow

1. Create a `Context()` with explicit dates, currency and rule profile.
2. Find a granular function using `api_catalogue()` and its R help page.
3. Start from its synthetic `reference_case()` and replace inputs with your own.
4. Inspect `value`, `unit`, `status`, `sources`, `details` and context in the result.
5. For linked calculations, use `Engine()` with explicit dependency references.
6. For reporting, select an archived framework and supply facts and bindings;
   inspect component checks and remaining obligations before using draft XML.

The installed getting-started vignette contains executable examples. Local
curve adapters separate input procurement from calculations. `export_resources()`
exports to a new directory when explicitly requested; ordinary calculations do
not write files or access the internet.
