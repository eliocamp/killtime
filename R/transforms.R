#' Transformations
#'
#' @param p cummulative proportion of dead animals
#' @param z transformed p
#' @param covp covariance matrix of p
#' @param n number of total experimental animales.
#'
#'
#' @keywords internal
trans_probit <- list(
  p_to_z = function(p) {
    z <- qnorm(p)
    return(z)
  },

  z_to_p = function(z) {
    p <- pnorm(z)
    return(p)
  },

  covp_to_covz = function(covp, p, n) {
    z <- qnorm(p)

    phiz <- exp(-(z^2)/2)/sqrt(2*pi)
    Mphiz <- matrix(phiz, nrow = length(phiz), ncol = length(phiz))

    covz <- covp/(Mphiz*t(Mphiz))
    diag(covz) <- diag(covp)/phiz^2

    return(covz)
  }
)

#' @rdname trans_probit
trans_identity <- list(
  p_to_z = function(p) p,
  z_to_p = function(z) z,
  covp_to_covz = function(covp, p, n) covp
)

#' @rdname trans_probit
trans_logit <- list(
  p_to_z = function(p) {
    z <- log(p/(1 - p))
    return(z)
  },

  z_to_p = function(z) {
    p <- exp(z)/(1 + exp(z))
    return(p)
  },

  covp_to_covz = function(covp, p, n) {
    Mp <- matrix(p, nrow = length(p), ncol = length(p))

    covz <- 1/(Mp * (1 - Mp)) * 1/(t(Mp) * (1 - t(Mp))) * covp
    diag(covz) <- 1/(n*p*(1 - p))

    return(covz)
  }


)

#' @rdname trans_probit
trans_cll <- list(
  p_to_z = function(p) {
    z <- log(-log(1 - p))
    return(z)
  },

  z_to_p = function(z) {
    p <- 1 - exp(-exp(z))
    return(p)
  },

  covp_to_covz = function(covp, p, n) {
    Mp <- matrix(p, nrow = length(p), ncol = length(p))

    covz <- 1/(1 - Mp) * 1/(1 - t(Mp)) * 1/(-log(1 - Mp)) * 1/(-log(1 - t(Mp))) * covp

    diag(covz) <- p/(n*(1 - p)*log(1 - p)^2)
    return(covz)
  }
)
