#' Fit probit
#'
#' Fits a probit (or other transformations) to mortality data
#'
#' @param times A vector of times in which observations took place.
#' @param dead_experiment,dead_control Acummulated number of dead animals in the
#' treatment group and control group, respectively.
#' @param n_experiment  Total number of animals in the treatment group. The
#' default is the maximum number of `dead_experiment`.
#' @param n_control  Total number of animals in the control group. The default
#' is the same number of animals in treatment group.
#' @param trans_y A string describing how to transform the data. Options are
#' `"logit"`, `"probit"`,  `"cll"` (for the Complementary log-log transformation) and
#' `"identity"` (i.e. do not transform the data).
#' @param tol,max_iter Relative tolerance and maximum number of iterations
#' for the iterative fit.
#'
#'
#' @examples
#'
#' # Some hipothetical data
#' experiment <- data.frame(
#'   days = c(0L,1L,2L,3L,4L,5L,6L,7L,
#'            8L,9L,10L,11L,12L,13L,14L),
#'   control = c(0L,0L,0L,3L,3L,5L,5L,5L,
#'               5L,5L,6L,6L,6L,6L,6L),
#'   treatment_1 = c(0L,13L,23L,28L,29L,29L,30L,
#'                   30L,30L,30L,30L,30L,30L,30L,30L),
#'   treatment_2 = c(0L,10L,24L,28L,29L,29L,29L,
#'                   29L,30L,30L,30L,30L,30L,30L,30L))
#'
#'  model <- with(experiment, lt_fit(days, treatment_1, control))
#'
#'  lethal_time(model)
#'
#'  plot(model)
#'
#'
#' @references
#' Pro bit Analysis of Correlated Data: Multiple Observations Over TiIne at One Concentration
#' Code adapted from https://www.ars.usda.gov/pacific-west-area/parlier/sjvasc/cpq/docs/probit-download/
#'
#' @export
lt_fit <- function(times, dead_experiment, dead_control,
                   n_experiment = max(dead_experiment),
                   n_control = n_experiment,
                   trans_y = "logit", tol = 1e-3, max_iter = 50) {
  ncont <- n_control
  nexp <- n_experiment

  data <- data.frame(time = times,
                     acum_control = dead_control,
                     acum_experiment = dead_experiment)
  data$died_experiment <- c(0, diff(data$acum_experiment))

  # (* Delete data for time periods when no mortality occurred or in
  #    which all insects were dead.  Apply log10 transformation to time. *)

  data <- data[data$died_experiment != 0, ]
  m <- nrow(data)

  data$died_control <- c(0, diff(data$acum_control))
  data$p_died_control <- data$died_control/ncont
  data$p_died_experiment <- data$died_experiment/nexp

  data$time <- log10(data$time)
  # Abbott's formula to correct for control mortality
  data$p_died_experiment <- with(data, (p_died_experiment - p_died_control)/(1 - p_died_control))

  data$p_died_experiment <- with(data, ifelse(p_died_experiment <= 0, 0.5/(nexp +1), p_died_experiment))
  data$acum_p_dead_experiment <- cumsum(data$p_died_experiment)

  raw_data <- data.frame(time = 10^data$time,
                         p = data$acum_p_dead_experiment)

  # C matrix computes the covariance of the accumulated values

  C <- matrix(1, m, m)
  C[upper.tri(C)] <- 0

  covp <- with(data, covariance_p_corrected(p_died_control, p_died_experiment, ncont, nexp))
  covp <- C %*% covp %*% t(C)

  # Get transformation function
  #
  trans_fun <- switch(trans_y,
                      probit = trans_probit,
                      logit = trans_logit,
                      cll = trans_cll,
                      identity = trans_identity,  # this one does nothing! :D
                      stop("invalid trans_y"))

  # Transform proportion dead and covariance.

  z <- trans_fun$p_to_z(data$acum_p_dead_experiment)
  covz <- trans_fun$covp_to_covz(covp, data$acum_p_dead_experiment, nexp)

  # Fit linear model iteratively, refining the estimate for covz.
  old_model <- matrix(1, 1, nrow = 2)

  converged <- FALSE

  for (i in seq_len(max_iter)) {
    # Fit model
    model <- fit_linear(z, data$time,  covz)

    # Use predicted values to recompute covz
    pred_z <- predict_linear(model)
    p <- trans_fun$z_to_p(pred_z)
    p_dif <- c(p[1], diff(p))

    covp <- covariance_p(p_dif, nexp)
    covp <- C %*% covp %*% t(C)
    covz <- trans_fun$covp_to_covz(covp, p, nexp)

    # Check for convergence
    converged <- all(abs(old_model - model)/old_model < tol)

    if (converged) {
      break
    } else {
      old_model <- model
    }
  }

  # Compute difference between predicted and observed and compute the chi squared
  # statistic.
  # The cumulative number dead at each time is calculated from the proportions and
  # the number of the number of insects tested minus the number that died in the control
  # group (ncorr).
  pred_z <- predict_linear(model)
  pred_p <- trans_fun$z_to_p(pred_z)

  ncorr <- nexp*(1 - data$p_died_control)
  dpred <- pred_p*ncorr
  kpred <- c(dpred[1], diff(dpred))
  kobs <- data$p_died_experiment*ncorr
  obs_minus_pred <- kobs - kpred

  # The covariance matrix for the observed minus expected number dead is calculated
  # as ncorr squared times the covariance matrix of p.
  cov_omp <- ncorr^2*covp

  chisq_var <- obs_minus_pred %*% solve(cov_omp) %*% obs_minus_pred
  chisq_pval <- stats::pchisq(chisq_var, df = m - 2, lower.tail = FALSE)

  # If the Chi-square is significant, correct all variances used to
  # calculate confidence limits on LT levels by multiplying each variance by h.

  h <- ifelse(chisq_pval < 0.05, chisq_var/(m - 2), 1)

  model <- structure(
    list(coefficients = stats::setNames(c(model), c("(Intercept)", "time")),
         residuals = c(pred_z - z),
         rank = 2,
         fitted.values = pred_z,
         model = data.frame(time = 10^data$time,
                            z = z),
         raw_data = raw_data,
         terms = stats::terms(z ~ time),
         qr = qr(cbind("(Intercept)" = 1, data$time)),
         df.residual = m - 2,
         h = h,
         n = m,
         b1 = attr(model, "b1", TRUE),
         b2 = attr(model, "b2", TRUE),
         X = attr(model, "X", TRUE),
         cov = attr(model, "cov", TRUE),
         trans_fun = trans_fun),
    class = c("killtime_lt_fit", "lm"))


  return(model)
}


#' Linear model with matrix weight
#'
#' @param y independent variable
#' @param x dependent variable
#' @param cov covariance matrix
#'
fit_linear <- function(y, x, cov) {

  X <- cbind("(Intercept)" = 1, x)

  inv_covz <- solve(cov)
  b1 <- solve(t(X) %*% inv_covz %*% X)
  b2 <- b1 %*% t(X) %*% inv_covz
  model <- b2 %*% y

  attr(model, "cov") <- cov
  attr(model, "b1") <- b1
  attr(model, "b2") <- b2
  attr(model, "X") <- X
  attr(model, "x") <- x
  attr(model, "y") <- y

  model
}



predict_linear <- function(model) {
  X <- attr(model, "X", TRUE)
  X %*% model
}

#' Predit new values from a fit
#'
#'
#' @param object A model returned by [killtime::lt_fit].
#' @param newdata A vector of times. If omitted, the fitted values are used.
#' @param interval Whether to compute confidence interval.
#' @param alpha Confidence level for interval calculation.
#'
#'
#' @export
predict.killtime_lt_fit <- function(object,
                                    newdata = NULL,
                                    interval = FALSE,
                                    alpha = 0.05) {
  if (is.null(newdata)) {
    newdata <- object$model$time
  }

  newdata <- log10(newdata)

  X0 <- cbind("(Intercept)" = 1, newdata)
  pred <- X0 %*% as.matrix(object$coefficients)

  if (!interval) {
    pred <- object$trans_fun$z_to_p(pred)
    return(pred)
  }

  # from https://stats.stackexchange.com/a/370170
  y <- object$model$z
  b1 <- object$b1
  H <- object$X %*% object$b2
  I <- diag(1, nrow = nrow(H))

  omega <- object$cov
  s2 <- as.vector((t(y) %*% solve(omega) %*% (I - H) %*% y)/object$df.residual)

  t <- stats::qt(1- alpha/2, object$df.residual)
  h <- t*sqrt(s2*X0 %*% b1 %*% t(X0))

  h <- diag(h)

  low <- pred - h
  up <- pred + h

  list(fit = object$trans_fun$z_to_p(c(pred)),
       lwr = object$trans_fun$z_to_p(c(low)),
       upr = object$trans_fun$z_to_p(c(up)))

}



pretty_LT <- function(LT, p) {
  r <- ceiling(1/(LT$upr - LT$lwr))

  text <- paste0("LT", p*100,": ", round(LT$LT, r),
                 " (CI: ", round(LT$lwr, r), " \u2014 ",round(LT$upr, r), ")")
  text
}

#' @export
plot.killtime_lt_fit <- function(x, y, alpha = 0.05,
                                 p = 0.5,
                                 alpha_lt = 0.05,
                                 x_lab = "Time",
                                 y_lab = "% of dead animals\n(corrected by mortality in control group)",
                                 ...) {
  time <- fit <- time <- lwr <- upr <- NULL

  newtime <- seq(0.001, lethal_time(x, 0.99)$LT, length.out = 80)
  pred <- as.data.frame(stats::predict(x, newdata = newtime, interval = TRUE, alpha = alpha))
  pred$time <- newtime


  obs <- x$raw_data

  LT <- as.data.frame(lethal_time(x, p = p, alpha = alpha_lt))

  LD_text <- pretty_LT(LT, p)

  # LD_align <- ifelse( p > 0.5, 1.05, -0.05)
  # LD_x <- ifelse(p > 0.5, LT$lwr, LT$upr)

  LD_x <- ifelse(max(obs$time) - LT$upr > LT$lwr, LT$upr, LT$lwr)
  LD_align <- ifelse(max(obs$time) - LT$upr > LT$lwr,  -0.05, 1.05)


  ggplot2::ggplot(pred) +
    ggplot2::geom_ribbon(ggplot2::aes(time, fit, ymin = lwr, ymax = upr), fill = "#f4679d",
                         alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(time, fit), color = "#f4679d") +
    ggplot2::geom_errorbarh(data = LT, ggplot2::aes(y = p,  xmin = lwr, xmax = upr),
                            height = 0.05) +
    ggplot2::geom_point(data = LT, ggplot2::aes(y = p, x = LT), size = 2, shape = 18) +
    ggplot2::annotate("label", label = LD_text, y = p, x = LD_x,
                      hjust = LD_align, color = NA) +
    ggplot2::annotate("text", label = LD_text, y = p, x = LD_x,
                      hjust = LD_align, color = "black") +

    ggplot2::geom_point(data = obs, ggplot2::aes(time, p),
                        size = 2) +
    ggplot2::labs(x = x_lab,
                  y = y_lab) +
    ggplot2::scale_y_continuous(labels =  scales::percent_format()) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::coord_cartesian(xlim = c(0, max(obs$time)))



}

