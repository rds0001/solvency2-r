# Scope and boundaries

This R implementation follows the capabilities and datasets of the solvency2
Python 0.5.0 Golden Source. It does not construct an insurer's internal model,
procure market data, supply portfolio-specific actuarial assumptions or obtain
supervisory approvals. Profile dates and applicability guards remain explicit;
the package does not silently update regulations.

Reporting output is a draft instance plus component checks, not certification
or a submission service. Review the returned remaining obligations. Synthetic
market resources are examples, not current production inputs.

Native R optimizers and FFT routines replace Python numerical backends. Fits
are subject to the documented tolerances and convergence guards, not a general
proof of global optimality. Lattice quantile bounds and numerical diagnostics
must be interpreted according to the function contract.

R input/profile serialization hashes and native backend diagnostics need not
equal Python's bytes. Original source hashes remain separate from hashes of
the losslessly compressed R resources. XML serialization may likewise differ
while representing the same facts.
