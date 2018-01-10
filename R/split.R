#' split
#'
#' Helper function to perform a split on a categorical predictor
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
split <- function(pred, out) {
    out_0 = out[pred==0]
    out_1 = out[pred==1]

    return(cbind(out_0, out_1))
}
