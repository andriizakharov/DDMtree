#' simdata
#'
#' simulate some data to test the tree on
#'
#' @param n_obs
#' @param n_vars
#' @param predictors
#' @param output
#'
#' @return a dataframe with multiple predictors and a single outcome variable
#' @export
#'
#' @examples
#' dat = simdata(500, 5)
#' head(dat)
simdata <- function(n_obs, n_vars, predictors = "factor", output = "cont") {
    mat = matrix(sample(c(1, 0),
                        size = n_obs * n_vars,
                        replace = TRUE),
                 nrow = n_obs,
                 ncol = n_vars)
    mat[, ncol(mat)] = abs(rnorm(n_obs, mean = 500, sd = 100))
    pred_names = c()
    for (i in 1:(n_vars-1)) pred_names = c(pred_names, paste0("pred_", i))
    colnames(mat) = c(pred_names, "out")
    return(as.data.frame(mat))
}
