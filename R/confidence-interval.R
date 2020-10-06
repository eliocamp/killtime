#' Computes Lethal Time and it's confidence interval
#'
#' Uses Fieller's theorem.
#'
#' @param model the linear model fitter with [fit_linear]
#' @param p the proportion of animals dead.
#' @param alpha confidence level
#'
#' @export
lethal_time <-  function(model, p = 0.5, alpha = 0.05) {
  trans_fun <- model$trans_fun
  p <- trans_fun$p_to_z(p)

  LT <- unname((p - model$coefficients[1])/model$coefficients[2])

  model_cov <- model$cov
  # h <- model$h
  h <- 1
  n <- model$n

  v11 <- c(model_cov[1, 1]*h)
  v22 <- c(model_cov[2, 2]*h)
  v12 <- c(model_cov[1, 2]*h)
  b <- unname(model$coefficients[2])

  r <- n - 2

  t_alpha <- qt(1 - alpha/2, df = r)

  g <- (t_alpha^2)*v22/(b^2)


  x11 <- LT + (v12/v22)
  x21 <- v11 + 2*LT*v12 + (LT^2)*v22
  x3 <- v11 - ((v12^2)/v22)
  y11 <- LT + (g*x11/(1-g))
  y21 <- t_alpha*sqrt(x21 - (g*x3))/(b*(1 - g))


  lower <- y11 - y21
  upper <- y11 + y21

  list(LT = 10^LT, lwr = 10^lower, upr = 10^upper)
}




