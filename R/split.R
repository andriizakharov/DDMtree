split <- function(pred, out) {
    out_0 = out[pred==0]
    out_1 = out[pred==1]

    return(cbind(out_0, out_1))
}
