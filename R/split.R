#' split
#'
#' Recursively split the data on categorical predictors, fit the DDM model to
#' each split, evaluate the splits and record the values in nodes.
#'
#' @param pred A vector of zeroes and ones
#' @param out A vector of numeric or intreger values
#'
#' @return A matrix with two columns, the first being the values of the outcome
#' corresponding to the 0 value of the predictor, the second to 1.
#' @export --DELETE?
#'
#' @examples
#' split1 = split(pred1, out)
split <- function(dat) {
    results = vector(NA, length = ncol(dat)-2)

    f = fit_DDM(getLL_DDM, dat)
    total_dev = f$objective

    for (i in 1:ncol(dat)-2) {
        sub1 = subset(dat, i == 0, select = c(rt, response))
        sub2 = subset(dat, i == 1, select = c(rt, response))

        f1 = fit_DDM(getLL_DDM, sub1)
        f2 = fit_DDM(getLL_DDM, sub2)

        dev1 = f1$objective
        dev2 = f2$objective

        results[i] = total_dev - dev1 - dev2
    }
    imax = which.max(results)

    subset1 = subset(dat, dat[imax] == 0)
    subset2 = subset(dat, dat[imax] == 1)

    split(subset1)
    split(subset2)
}
# TBD: recording values into a suitable data structure (data.tree?)
