#' eval_split
#'
#' Helper function to evaluate the current split based on a given evaluation
#' method (the default is least squares, "ls")
#'
#' @param split_df A matrix with two columns coming out of the "split" function
#' @param method A string giving the evaluation method (default is "ls")
#' --TO BE EXTENDED
#'
#' @return A named vector with the preferred tree node after evaluation.
#' The value gives the outcome in the node, the name is "0" or "1" depending on
#' which of the two nodes was selected.
#' @export --DELETE?
#'
#' @examples
#' pref_node = eval_split(split(pred1, out), method = "ls")
eval_split <- function(split_df, method = "ls") {
    eval_0 = sum((split_df[1]-mean(split_df[1]))**2)
    eval_1 = sum((split_df[2]-mean(split_df[2]))**2)
    minimum = min(eval_0, eval_1)
    names(minimum) = ifelse(minimum == eval_0, "0", "1")

    return(minimum)
}
