#' Computes covariance of p
#' 
#' 
#' @param p proportion of animals that died at each observed time.
#' @param n total number of animals.
#' 
covariance_p <- function(p, n) {
  Mp <- matrix(p, nrow = length(p), ncol = length(p))
  
  covp <- -(Mp * t(Mp))
  diag(covp) <- p*(1 - p)
  
  return(covp/(n - 1))
}

#' covariance corrected
#' 
#' @param pcont,pexp proportion of animals died at each observed time in the control
#' and experimental group respectively.
#' @param ncont,nexp total number of animales in the control and 
#' experimental group respectively.
#' 
covariance_p_corrected <- function(pcont, pexp, ncont, nexp) {
  
  Mcont <- matrix(pcont, nrow = length(pcont), ncol = length(pcont))
  Mexp <- matrix(pexp, nrow = length(pexp), ncol = length(pexp))
  
  Mcont_t <- t(Mcont)
  Mexp_t <- t(Mexp)
  
  covariance <- -(1/(1 - Mcont) * 1/(1 - Mcont_t) * Mexp * Mexp_t/(nexp - 1) + 
                    (1 - Mexp)/(1 - Mcont)^2*(1 - Mexp_t)/(1  - Mcont_t)^2*Mcont*Mcont_t/(ncont - 1))
  
  variance <- (1/(1 - pcont))^2 * pexp*(1 - pexp)/(nexp - 1) + 
    (1 - pexp)^2/(1 - pcont)^4*pcont*(1 - pcont)/(ncont - 1)
  
  diag(covariance) <- variance
  covariance
}