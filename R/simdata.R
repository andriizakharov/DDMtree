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
simdata <- function(n_obs, n_vars, true_pars, predictors = "factor") {
    mat = matrix(sample(c(1, 0),
                        size = n_obs * n_vars,
                        replace = TRUE),
                 nrow = n_obs,
                 ncol = n_vars)
    rt_dist = rtdists::rdiffusion(n_obs,
                                  a = true_pars[1],
                                  v = true_pars[2],
                                  t0 = true_pars[3],
                                  z = true_pars[4])
    pred_names = c()
    for (i in 1:(n_vars)) pred_names = c(pred_names, paste0("pred", i))
    colnames(mat) = c(pred_names)
    mat = as.data.frame(mat)
    rt_dist[mat["pred1"] == 1, 1] = rt_dist[mat["pred1"] == 1, 1] + 0.25
    rt_dist[mat["pred2"] == 1, 1] = rt_dist[mat["pred2"] == 1, 1] + 0.5
    return(cbind(as.data.frame(mat), rt_dist))
}
