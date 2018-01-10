eval_split <- function(split_df, method = "ls") {
    eval_0 = sum((split_df[1]-mean(split_df[1]))**2)
    eval_1 = sum((split_df[2]-mean(split_df[2]))**2)
    minimum = min(eval_0, eval_1)
    names(minimum) = ifelse(minimum == eval_0, "0", "1")

    return(minimum)
}
