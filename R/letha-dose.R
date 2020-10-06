#' Fits a logit
#'
#' @param dose Vector of doses for each experiment.
#' @param dead Vector of acummulated number of animals dead on each experiment.
#' @param n Total number of animals in each experimnet. By default is the maximum number
#' of dead animals of all experiments.
#' @param trans_y A string describing how to transform the data. Options are
#' `"logit"`, `"probit"` and `"cll"` (for the Complementary log-log transformation).
#' (Note that `"identiy"` is not supported).
#'
#' @examples
#'
#' data <- data.frame(dose = c(38,40,40,40,42,42,42,44,44,44,47,47,47),
#'                    dead = c(0,0,0,0,1,5,5,15,17,18,20,20,20))
#'
#' model <- with(data, ld_fit(dose, dead, n = 20))
#' lethal_dose(model, p = c(0.25, 0.5, .90))
#'
#' plot(model, p = c(0.25, 0.5, 0.9))
#'
#' @export
ld_fit <- function(dose, dead, n = max(dead), trans_y = "logit") {
  if (trans_y == "cll") {
    trans_y <- "cloglog"
  }

  y <- cbind(dead, n - dead)
  model <- stats::glm(y ~ dose, family = stats::binomial(link = trans_y))
  class(model) <- c("killtime_ld_fit", class(model))
  model$n <- n
  model
}



#' Compute lethal dose
#'
#' @param model A model returned by [killtime::ld_fit]
#' @param p Proportion of dead animals.
#' @param alpha Confidence level.
#'
#' @export
lethal_dose <- function(model, p = 0.5, alpha = 0.05) {
  ld <- MASS::dose.p(model, p = p)
  LD <- as.vector(ld)
  se <- as.vector(attr(ld, "SE", TRUE))
  t <- stats::qnorm(1 - alpha/2)

  list(LD = LD,
       upr = LD + se*t,
       lwr = LD - se*t)
}

#' @export
predict.killtime_ld_fit <- function(object,
                                    newdata = NULL,
                                    interval = FALSE,
                                    alpha = 0.05) {
  if (!is.null(newdata)) {
    newdata <- data.frame(dose = newdata)
  }

  pred <- stats::predict.glm(object, type = "link", se.fit = interval, newdata = newdata)

  if (!interval) {
    pred <- as.vector(stats::family(object)$linkinv(pred))
    return(pred)
  }
  t <- stats::qt(1 - alpha/2, object$df.residual)


  pred <- list(fit = pred$fit,
               lwr = pred$fit - pred$se.fit*t,
               upr = pred$fit + pred$se.fit*t)

  pred <- lapply(pred, function(x) stats::family(object)$linkinv(x))
  return(pred)
}


#' @export
plot.killtime_ld_fit <- function(x, y, alpha = 0.05,
                                 p = 0.5,
                                 alpha_ld = 0.05,
                                 x_lab = "Dose",
                                 y_lab = "% of dead animals",
                                 ...) {
  dead_experiment <- dose <- fit <- time <- lwr <- upr <- NULL
  newdose <- seq(min(x$model$dose), max(x$model$dose), length.out = 80)
  pred <- as.data.frame(stats::predict(x, newdata = newdose, interval = TRUE, alpha = alpha))
  pred$dose <- newdose

  obs <- data.frame(dose = x$model$dose,
                    dead_experiment = x$model$y[, 1]/x$n)

  LD <- as.data.frame(lethal_dose(x, p = p, alpha = alpha_ld))


  LD_text <- pretty_LD(LD, p)

  # LD_align <- ifelse( p > 0.5, 1.05, -0.05)
  # LD_x <- ifelse(p > 0.5, LT$lwr, LT$upr)

  LD_x <- ifelse(max(obs$dose) - LD$upr > LD$lwr, LD$upr, LD$lwr)
  LD_align <- ifelse(max(obs$dose) - LD$upr > LD$lwr,  -0.05, 1.05)

  ggplot2::ggplot(pred) +
    ggplot2::geom_ribbon(ggplot2::aes(dose, fit, ymin = lwr, ymax = upr), fill = "#f4679d",
                         alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(dose, fit), color = "#f4679d") +
    ggplot2::geom_errorbarh(data = LD, ggplot2::aes(y = p,  xmin = lwr, xmax = upr),
                            height = 0.05) +
    ggplot2::geom_point(data = LD, ggplot2::aes(y = p, x = LD), size = 2, shape = 18) +
    ggplot2::annotate("label", label = LD_text, y = p, x = LD_x,
                      hjust = LD_align, color = NA) +
    ggplot2::annotate("text", label = LD_text, y = p, x = LD_x,
                      hjust = LD_align, color = "black") +

    ggplot2::geom_point(data = obs, ggplot2::aes(dose, dead_experiment),
                        size = 2) +
    ggplot2::labs(x = x_lab,
                  y = y_lab) +
    ggplot2::scale_y_continuous(labels =  scales::percent_format()) +
    ggplot2::theme_minimal(base_size = 13)
}




pretty_LD <- function(LT, p) {
  r <- ceiling(1/(LT$upr - LT$lwr))

  text <- paste0("LD", p*100,": ", round(LT$LD, r),
                 " (CI: ", round(LT$lwr, r), " \u2014 ",round(LT$upr, r), ")")
  text
}

