#' tree
#'
#' Builds the decision tree. Uses helper functions "split" and "eval_split" to
#' perform and evaluate splits.
#'
#' @param df
#'
#' @return --TBD
#' @export
#'
#' @examples --TBD
tree <- function(df) {
    n_row = nrow(df)
    n_col = ncol(df)
    preds = df[, -1]
    out = df[, n_col]


}
