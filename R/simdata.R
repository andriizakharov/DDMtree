#' simdata
#'
#' Simulate some data to test the tree on. The resulting dataframe will have
#' multiple factorial predictors (1/0), named "pred1", "pred2", ... and a
#' continuous outcome variable named "out".
#'
#' @param n_obs number of observations, i.e. number of rows in the dataframe
#' @param n_vars number of variables, i.e. total
#' number of columns in the dataframe (including the outcome!)
#' @param predictors predictor type ("factor" by default) -- TO BE EXTENDED!
#' @param output output variable type ("cont" by default) -- TO BE EXTENDED!
#'
#' @return a dataframe with multiple predictors and a single outcome variable
#' @export
#'
#' @examples
#' dat = simdata(n_obs = 500, n_vars = 5)
#' head(dat)
simdata <- function(n_obs, n_vars, predictors = "factor", output = "cont") {
    mat = matrix(sample(c(1, 0),
                        size = n_obs * n_vars,
                        replace = TRUE),
                 nrow = n_obs,
                 ncol = n_vars)
    mat[, ncol(mat)] = abs(rnorm(n_obs, mean = 500, sd = 100))
    pred_names = c()
    for (i in 1:(n_vars-1)) pred_names = c(pred_names, paste0("pred", i))
    colnames(mat) = c(pred_names, "out")
    return(as.data.frame(mat))
}
