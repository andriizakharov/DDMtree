#' getLL_DDM
#'
#' Returns the deviance (-2*log-likelihood) for the DDM model. Uses the "ddiffusion"
#' function from the "rtdists" package for the density of the DDM.
#'
#' @param pars The four patameters of the DDM to be fit.
#' a: threshold separation,
#' v: drift rate,
#' t0: non-decision time,
#' z: relative starting point (bias)
#' @param dat  The dataset to be modeled, with two outcome variables, "rt" and "response"
#' "Response" has to be a factor with values "upper" and "lower", or a numeric vector
#' with 1 = lower and 2 = upper.
#'
#' @return Deviance (-2*log-likelihood), a double.
#' @export --DELETE?
#'
#' @examples
#' deviance <- getLL_DDM(pars, dat)
getLL_DDM <- function(pars, dat) {
    densities <- rtdists::ddiffusion(dat$rt, dat$response,
                                     a=pars[1], v=pars[2], t0=pars[3], z=pars[4])
    return(-2*sum(log(densities)))
}
