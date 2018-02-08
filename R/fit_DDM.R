#' fit_DDM
#'
#' Fits the DDM model to the data. Uses R built-in optimization to minimize the deviance
#' (maximize the log-likelihood)
#'
#' @param LL_func Function used to calculate the deviance
#' @param dat Our dataset, with columns "rt" and "response". Response is either a factor
#' with values "upper" and "lower", or a numeric vector with 1=lower, 2=upper.
#' @param start_pars A vector of starting parameters for optimization. Must contain
#' four scalars, for a, v, to, and z - in that order!
#' @param optimizer The optimizer to be used (default is nlminb). Others TBD.
#' --TO BE EXTENDED
#'
#' @return A list with components:
#' par	The best set of parameters found.
#' objective	The value of objective corresponding to par.
#' convergence	An integer code. 0 indicates successful convergence.
#' message	A character string giving any additional information returned by the optimizer, or NULL. For details, see PORT documentation.
#' iterations	Number of iterations performed.
#' evaluations	Number of objective function and gradient function evaluations
#' @export --DELETE?
#'
#' @examples model_fit <- fit_DDM(getLL_DDM, dat, start_pars)
fit_DDM <- function(LL_func,
                    dat,
                    start_pars = c(runif(1, 0.5, 3),
                                            0.1,
                                            runif(1, 0, 0.5),
                                            0.2), # a, v, t0, z - in that order!
                    optimizer = "nlminb") {
    names(start_pars) <- c("a", "v", "t0", "z")

    if (optimizer == "nlminb") {
       fit <- nlminb(start_pars, LL_func, lower = 0, rt=dat$rt, response=dat$response)
    }
    # TBD: add other optimizers
    fit
}
