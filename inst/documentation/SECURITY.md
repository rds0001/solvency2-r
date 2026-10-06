# Offline operation and safe inputs

The package has no download, telemetry or filing-submission service. Local input
adapters reject URLs. Resource export writes only when explicitly called.
Reporting expressions use a bounded domain-specific parser, not R code
evaluation; supported patterns use a bounded matcher. XML parsing disables
network access. Resource checksums detect changes but are not digital signatures.

Keep confidential portfolio inputs and outputs in your own controlled storage.
Report problems to riskdatascience@web.de using a minimal synthetic example,
package version and error code; do not send confidential customer records.
