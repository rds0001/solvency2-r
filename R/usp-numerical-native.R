# Annex XVII numerical methods. R's native optimisation/FFT replace SciPy/NumPy;
# numerical controls and backend provenance remain separate from legal inputs.
.s2_usp_evaluator <- function(ratios, volume_ratios) {
  centered <- ratios - ratios[[1L]]
  function(pair) {
    delta <- pair[[1L]]; gamma <- pair[[2L]]
    mixture <- if (delta == 0) volume_ratios else if (delta == 1) rep(0, length(ratios)) else {
      a <- log1p(-delta) + volume_ratios; b <- log(delta)
      pmax(a, b) + log1p(exp(-abs(a - b)))
    }
    exponent <- mixture + 2 * gamma
    variance <- pmax(exponent, 0) + log1p(exp(-abs(exponent)))
    if (any(!is.finite(variance) | variance <= 0)) stop("unresolved lognormal variance")
    change <- exp(-mixture) - exp(volume_ratios - mixture)
    sigmoid <- -expm1(-variance)
    weights <- min(variance) / variance
    if (any(weights <= 0)) stop("unresolved precision weights")
    mean <- sum(weights * centered) / sum(weights)
    average_variance <- min(variance) * (length(ratios) / sum(weights))
    residual <- centered - mean + (variance - average_variance) / 2
    standardized <- residual / sqrt(variance)
    derivative <- (1 + residual - standardized^2) / variance
    result <- list(value = sum(standardized^2 + log(variance)),
      gradient = c(sum(derivative * sigmoid * change), sum(derivative * 2 * sigmoid)))
    if (any(!is.finite(c(result$value, result$gradient)))) stop("unresolved likelihood or gradient")
    result
  }
}
.s2_usp_projected <- function(delta, gradient, count) {
  if (delta == 0) gradient[[1L]] <- min(gradient[[1L]], 0)
  if (delta == 1) gradient[[1L]] <- max(gradient[[1L]], 0)
  max(abs(gradient)) / count
}

.s2_usp_lattice <- function(mean_count, variance_count, severity_cv2, probability, size) {
  dispersion <- (variance_count - mean_count) / mean_count
  shape <- mean_count / dispersion
  extent <- 4 * (mean_count + sqrt((mean_count * severity_cv2 + variance_count) * probability / (1 - probability)))
  .s2_require(is.finite(extent) && extent > 0, "grid", "compound scale unresolved", "REVIEW_REQUIRED")
  step <- extent / size
  eta2 <- log1p(severity_cv2)
  .s2_require(is.finite(eta2) && eta2 > 0, "severity", "lognormal shape unresolved", "REVIEW_REQUIRED")
  severity_cdf <- c(0, stats::pnorm((log(seq_len(size) * step) + eta2 / 2) / sqrt(eta2)))
  floor_masses <- diff(severity_cdf)
  .s2_require(all(is.finite(floor_masses) & floor_masses >= 0), "severity", "severity probabilities unresolved", "REVIEW_REQUIRED")
  alias_bound <- 1e-12
  tilt <- exp(log(alias_bound) / size * (seq_len(size) - 1L))
  indices <- seq_len(size %/% 4L + 1L)
  roundoff <- 64 * .Machine$double.eps * (1 + mean_count) * log2(size) * cumsum(1 / tilt[indices])
  bounds <- list(NULL, NULL); runs <- list()
  for (i in 1:2) {
    masses <- if (i == 1L) floor_masses else c(0, head(floor_masses, -1L))
    generating <- stats::fft(masses * tilt)
    # R log1p does not accept complex values. log(1+z) is corrected by the
    # standard cancellation compensation, retaining small real/imaginary z.
    z <- dispersion * (1 - generating)
    one <- 1 + z
    logarithm <- log(one)
    nonzero <- Mod(one - 1) > 0
    logarithm[nonzero] <- logarithm[nonzero] * z[nonzero] / (one[nonzero] - 1)
    logarithm[!nonzero] <- z[!nonzero]
    transform <- exp(-shape * logarithm)
    distribution <- Re(stats::fft(transform, inverse = TRUE) / size)[indices] / tilt[indices]
    .s2_require(all(is.finite(distribution)), "fft", "nonfinite compound probabilities", "REVIEW_REQUIRED")
    negative <- pmax(-distribution, 0)
    .s2_require(all(negative <= roundoff), "fft", "negative masses exceed roundoff guard", "REVIEW_REQUIRED")
    clipped <- sum(negative)
    cdf <- cumsum(pmax(distribution, 0))
    guard <- roundoff + clipped + alias_bound
    crossed <- which((if (i == 1L) cdf + guard else cdf - guard) >= probability)
    index <- if (length(crossed)) crossed[[1L]] else NULL
    bounds[i] <- list(if (is.null(index)) NULL else (index - 1L) * step)
    runs[[i]] <- list(severity_rounding = if (i == 1L) "floor" else "ceil",
      quantile_index = if (is.null(index)) NULL else index - 1L,
      clipped_negative_mass = clipped,
      cdf_at_index = if (is.null(index)) NULL else cdf[[index]],
      cdf_guard_at_index = if (is.null(index)) NULL else guard[[index]])
  }
  list(bounds = bounds, details = list(grid_size = size, normalized_step = step,
    normalized_extent = extent, searched_grid_fraction = .25,
    exact_arithmetic_alias_bound = alias_bound, roundoff_guard_certified = FALSE, roundings = runs))
}

.s2_implementations <- c(.s2_implementations, list(
  usp_lognormal_fit = function(x, context) {
    .s2_reference(x$qualification_reference, "qualification_reference")
    basis <- .s2_native_usp_lognormal___method(x$method)
    iterations <- .s2_number(x$max_iterations, "max_iterations", 1)
    .s2_require(iterations == trunc(iterations), "max_iterations", "whole iteration count required")
    tolerance <- .s2_number(x$gradient_tolerance, "gradient_tolerance", 0, 1e-3)
    .s2_require(tolerance > 0, "gradient_tolerance", "positive numerical tolerance required")
    .s2_native_usp_lognormal___lognormal_components(x$losses, x$volumes, .5, 0, context)
    losses <- unlist(x$losses); volumes <- unlist(x$volumes)
    count <- length(losses)
    ratios <- log(losses) - log(volumes)
    centered <- ratios - ratios[[1L]]
    variance <- sum((centered - sum(centered) / count)^2) / count
    .s2_require(variance > 1e-28, "data", "constant or numerically unresolved log-ratios have no finite fit", "REVIEW_REQUIRED")
    log_mean <- log(max(volumes)) + log(sum(volumes / max(volumes)) / count)
    evaluate <- .s2_usp_evaluator(ratios, log_mean - log(volumes))
    initial_gamma <- (if (variance > 1) variance + log1p(-exp(-variance)) else log(expm1(variance))) / 2
    diagnostics <- list()
    equal_volumes <- all(volumes == volumes[[1L]])
    if (equal_volumes) {
      delta <- 0; gamma <- initial_gamma
      backend <- "analytic_equal_volumes"; version <- NULL
      evaluation <- evaluate(c(delta, gamma))
      objective <- evaluation$value
      projected <- .s2_usp_projected(delta, evaluation$gradient, count)
      .s2_require(projected <= tolerance, "fit", "analytic fit unresolved at requested tolerance", "REVIEW_REQUIRED")
    } else {
      backend <- "stats::optim/L-BFGS-B"; version <- as.character(getRversion())
      candidates <- list()
      for (boundary in list(NULL, 0, 1)) {
        for (start_delta in if (is.null(boundary)) c(0, .5, 1) else boundary) {
          for (offset in c(-1, 0, 1)) {
            start_gamma <- initial_gamma + offset
            point <- function(pair) if (is.null(boundary)) pair else c(boundary, pair[[1L]])
            result <- tryCatch({
              fit <- stats::optim(if (is.null(boundary)) c(start_delta, start_gamma) else start_gamma,
                fn = function(pair) evaluate(point(pair))$value,
                gr = function(pair) { g <- evaluate(point(pair))$gradient; if (is.null(boundary)) g else g[[2L]] },
                method = "L-BFGS-B", lower = if (is.null(boundary)) c(0, -Inf) else -Inf,
                upper = if (is.null(boundary)) c(1, Inf) else Inf,
                control = list(maxit = iterations, factr = 1e-14 / .Machine$double.eps, pgtol = tolerance * count * .1))
              fitted <- point(fit$par); evaluation <- evaluate(fitted)
              residual <- .s2_usp_projected(fitted[[1L]], evaluation$gradient, count)
              accepted <- fit$convergence == 0 && residual <= tolerance
              candidates[[length(candidates) + 1L]] <- list(value = evaluation$value, delta = fitted[[1L]],
                gamma = fitted[[2L]], residual = residual, accepted = accepted)
              list(boundary = boundary, start = list(start_delta, start_gamma),
                success = fit$convergence == 0, accepted = accepted,
                evaluations = unname(fit$counts[[1L]]), message = fit$message,
                objective = evaluation$value, projected_gradient_per_year = residual)
            }, error = function(error) list(boundary = boundary, start = list(start_delta, start_gamma),
              success = FALSE, accepted = FALSE, message = "numeric evaluation unresolved"))
            diagnostics[[length(diagnostics) + 1L]] <- result
          }
        }
      }
      accepted <- Filter(function(row) row$accepted, candidates)
      .s2_require(length(accepted) > 0L, "fit", "no converged fit satisfying projected-gradient tolerance", "REVIEW_REQUIRED")
      ordering <- order(vapply(accepted, `[[`, numeric(1), "value"), vapply(accepted, `[[`, numeric(1), "delta"),
        vapply(accepted, `[[`, numeric(1), "gamma"))
      best <- accepted[[ordering[[1L]]]]
      objective <- best$value; delta <- best$delta; gamma <- best$gamma; projected <- best$residual
      .s2_require(!any(vapply(candidates, function(row) row$value < objective - 1e-8 * (1 + abs(objective)), logical(1))),
        "fit", "lower unconverged candidate needs review", "REVIEW_REQUIRED")
    }
    result <- .s2_native_usp_lognormal___lognormal_components(x$losses, x$volumes, delta, gamma, context)
    details <- result[[2L]]
    .s2_require(abs(objective - details$objective) <= max(1e-10, 1e-10 * max(abs(objective), abs(details$objective))),
      "fit", "independent evaluation paths disagree", "REVIEW_REQUIRED")
    profile <- .s2_effective("profile.json", context)
    details <- .s2_merge(details, list(method = x$method, year_basis = basis,
      qualification_reference = x$qualification_reference, qualification_status = "EXTERNAL_UNVERIFIED",
      minimization_performed = TRUE, minimum_certified = FALSE, time_series_verified = FALSE,
      delta_identifiable = !equal_volumes, equal_volume_delta_convention = if (equal_volumes) 0 else NULL,
      optimizer = list(backend = backend, version = version, max_iterations = iterations,
        log_ratio_variance_resolution = 1e-28, gradient_tolerance = tolerance, gamma_bounds = NULL,
        projected_gradient_per_year = projected, runs = diagnostics),
      credibility_applied = FALSE, standard_parameter_replaced = FALSE, supervisory_approval_granted_by_engine = FALSE,
      scope = "numerically_fitted_B_C1_sigma_not_data_qualification_or_supervisory_approval",
      `_sources` = list(paste0(context$profile_id, ":", profile$sources$DR$sha256, ":annex-XVII-B-C"))))
    list(value = result[[1L]], details = details)
  },
  usp_revision_compound_quantile = function(x, context) {
    tolerance <- .s2_number(x$relative_tolerance, "relative_tolerance", 0, .1)
    .s2_require(tolerance > 0, "relative_tolerance", "positive numerical tolerance required")
    power <- .s2_number(x$max_grid_power, "max_grid_power", 10, 22)
    .s2_require(power == trunc(power), "max_grid_power", "whole grid exponent required")
    moments <- usp_revision_moments(x$annual_benefits, qualification_reference = x$qualification_reference, context = context)
    stats <- moments$details
    .s2_require(stats$negative_binomial_moments_nondegenerate && stats$lognormal_moments_nondegenerate,
      "distribution_moments", "nondegenerate negative-binomial and lognormal moments required", "REVIEW_REQUIRED")
    count_mean <- stats$mean_annual_count; count_variance <- stats$annual_count_stddev^2
    amount_mean <- stats$mean_positive_amount; severity_cv2 <- (stats$positive_amount_stddev / amount_mean)^2
    .s2_require(is.finite(count_variance) && count_variance > count_mean && is.finite(severity_cv2) && severity_cv2 > 0,
      "distribution_moments", "distribution parameters unresolved at numeric precision", "REVIEW_REQUIRED")
    dispersion <- (count_variance - count_mean) / count_mean; shape <- count_mean / dispersion
    .s2_require(is.finite(shape) && shape > 0 && is.finite(dispersion), "count_distribution", "negative-binomial parameters unresolved", "REVIEW_REQUIRED")
    probability <- .s2_number(.s2_scalar("usp_revision_quantile_probability", context), "quantile_probability", 0, 1)
    .s2_require(probability > 0 && probability < 1, "quantile_probability", "interior quantile probability required", "REVIEW_REQUIRED")
    zero_mass <- exp(-shape * log1p(dispersion))
    .s2_require(abs(zero_mass - probability) > 64 * .Machine$double.eps,
      "quantile_probability", "probability is numerically indistinguishable from the zero atom", "REVIEW_REQUIRED")
    history <- list(); backend <- list(method = "analytic_zero_atom", r_version = NULL)
    if (zero_mass > probability) lower <- upper <- width <- 0 else {
      backend <- list(method = "tilted_fft_floor_ceil_compound", implementation = "stats::fft/stats::pnorm", r_version = as.character(getRversion()))
      converged <- FALSE
      for (exponent in seq.int(10, power)) {
        lattice <- .s2_usp_lattice(count_mean, count_variance, severity_cv2, probability, 2^exponent)
        history[[length(history) + 1L]] <- lattice$details
        if (any(vapply(lattice$bounds, is.null, logical(1)))) next
        lower <- lattice$bounds[[1L]]; upper <- lattice$bounds[[2L]]
        .s2_require(lower >= 0 && lower <= upper && upper > 0, "quantile_interval", "rounding bounds crossed or unresolved", "REVIEW_REQUIRED")
        width <- (upper - lower) / upper
        if (width <= tolerance) { converged <- TRUE; break }
      }
      .s2_require(converged, "numerical_quantile", "quantile interval did not converge within grid budget", "REVIEW_REQUIRED")
      money <- function(value, field) {
        result <- value * amount_mean
        .s2_require(is.finite(result) && (value == 0 || result > 0), field, "quantile amount outside representable range", "REVIEW_REQUIRED")
        result
      }
      lower <- money(lower, "lower_quantile"); upper <- money(upper, "upper_quantile")
    }
    profile <- .s2_effective("profile.json", context)
    list(value = upper, details = list(method = "revision", quantile_probability = probability,
      quantile_interval = list(lower = lower, upper = upper, relative_width = width), returned_bound = "upper",
      mean_increase = moments$value, years = stats$years, mean_annual_count = count_mean,
      annual_count_variance = count_variance, mean_positive_amount = amount_mean,
      positive_amount_stddev = stats$positive_amount_stddev, negative_binomial_shape = shape,
      negative_binomial_success_probability = 1 / (1 + dispersion), probability_of_zero = zero_mass,
      relative_tolerance = tolerance, max_grid_power = power, numerical_tolerance_met = TRUE,
      backend = backend, grid_history = history, floating_point_error_formally_certified = FALSE,
      qualification_reference = x$qualification_reference, qualification_status = "EXTERNAL_UNVERIFIED",
      distribution_fit_verified = FALSE, independence_verified = FALSE, credibility_applied = FALSE,
      standard_parameter_replaced = FALSE, supervisory_approval_granted_by_engine = FALSE,
      scope = "prescribed_compound_quantile_numeric_upper_bracket_not_approved_USP",
      `_sources` = list(paste0(context$profile_id, ":", profile$sources$DR$sha256, ":annex-XVII-E"))))
  }
))
