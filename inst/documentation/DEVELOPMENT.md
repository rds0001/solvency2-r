# Developing the R package

Runtime code is native R and works offline. Native arithmetic dependencies are
declared in DESCRIPTION; Python is not a runtime dependency. The Golden Source
identity is recorded in the package resources. Private development scaffolding
and emission tools are not part of this distribution.

Keep frozen reference expectations independent of implementation changes.
Run focused tests for changed components, then the complete suite and package
check for a release. Build and install the produced source archive to check
resource availability outside the source directory. Preserve original-source,
decoded-JSON and packaged-file hashes as distinct provenance fields.

See the repository README for local build commands and system prerequisites.
Changing datasets also requires updating their inventory and license notices.
