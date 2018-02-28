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
split <- function(dat, left = 1) {
    # fit model to full data
    f = fit_DDM(getLL_DDM, dat)

    # deviance from the full data
    total_dev = f$objective

    # keep track of which predictors should be considered for a split
    # (only those that have both 1 and 0 in them)
    preds = c()
    for (i in 1:(ncol(dat)-2)) {
        if (length(unique(dat[,i])) == 2) {
            preds = c(preds, i)
        }
    }
    print(preds)
    # if there are no such candidate predictors left, terminate splitting
    if (is.null(preds)) {
        tree <- list(params = NA, node_id = NA, param_names = "NA", model = f, lr =  NA, df = 1, N = 0)
        tree$caption <- "TERMINAL"
        return(tree)
    }

    # vector to store LR results of candidate splits
    results = rep(NA, ncol(dat)-2)

    for (i in preds) {

        # make a candidate split
        sub1 = subset(dat, dat[,i] == 0, select = c(rt, response))
        sub2 = subset(dat, dat[,i] == 1, select = c(rt, response))

        # fit models to both subsets of a split
        f1 = fit_DDM(getLL_DDM, sub1)
        f2 = fit_DDM(getLL_DDM, sub2)

        # get deviance for both subsets
        dev1 = f1$objective
        dev2 = f2$objective

        # calculate LR for this candidate split
        results[i] = total_dev + dev1 + dev2
    }

    # get the max LR for all candidate splits -> find the best split
    imax = which.max(results)

    # carry out the best split
    subset1 = subset(dat, dat[,imax] == 0)
    subset2 = subset(dat, dat[,imax] == 1)

    # construct a SEMtree class to store the data
    tree <- list(node_id = n0de_id_c0unt, params = f$par, param_names = names(f$par), lr = results[imax], N = nrow(dat), df = 1, model = f)
    class(tree) <- "semtree"
    if (left == 1) tree$caption <- paste(names(dat[imax]), "== 0")
    else tree$caption <- paste(names(dat[imax]), "== 1")
    n0de_id_c0unt <<- n0de_id_c0unt + 1

    # continue splitting recursively
    tree$left_child <- split(subset1)
    tree$right_child <- split(subset2, left = 0)

    return(tree)
}
