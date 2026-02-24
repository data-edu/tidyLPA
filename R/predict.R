#' @method predict tidyProfile.mclust
#' @export
predict.tidyProfile.mclust <- function(object, ...){
    cl <- match.call()
    cl[[1]] <- quote(predict)
    cl[["object"]] <- object$model
    eval.parent(cl)
}
