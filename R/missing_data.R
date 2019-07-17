#' @title Apply single imputation to data
#' @description This function accommodates several methods for single imputation
#' of data. Currently, the following methods are defined:
#' \itemize{
#' \item{"imputeData"}{Applies the mclust native imputation function
#' \code{\link[mclust]{imputeData}}}
#' \item{"missForest"}{Applies non-parameteric, random-forest based data
#' imputation using \code{\link[missForest]{missForest}}. Radom forests can
#' accommodate any complex interactions and non-linear relations in the data. My
#' simulation studies indicate that this method is preferable to mclust's
#' \code{imputeData} (see examples).}
#' }
#' @param x A data.frame or matrix.
#' @param method Character. Imputation method to apply, Default: 'imputeData'
#' @return A data.frame
#' @author Caspar J. van Lissa
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(missForest)
#' library(mclust)
#'
#' dm <- 2
#' k <- 3
#' n <- 100
#' V <- 4
#'
#' # Example of one simulation
#' class <- sample.int(k, n, replace = TRUE)
#' dat <- matrix(rnorm(n*V, mean = (rep(class, each = V)-1)*dm), nrow  = n,
#'               ncol = V, byrow = TRUE)
#' results <- estimate_profiles(data.frame(dat), 1:5)
#' plot_profiles(results)
#' compare_solutions(results)
#'
#' # Simulation for parametric data (i.e., all assumptions of latent profile
#' # analysis met)
#' simulation <- replicate(100, {
#'     class <- sample.int(k, n, replace = TRUE)
#'     dat <- matrix(rnorm(n*V, mean = (rep(class, each = V)-1)*dm), nrow  = n,
#'                   ncol = V, byrow = TRUE)
#'
#'     d <- prodNA(dat)
#'
#'     d_mf <- missForest(d)$ximp
#'     m_mf <- Mclust(d_mf, G = 3, "EEI")
#'     d_im <- imputeData(d, verbose = FALSE)
#'     m_im <- Mclust(d_im, G = 3, "EEI")
#'
#'     class_tabl_mf <- sort(prop.table(table(class, m_mf$classification)),
#'                           decreasing = TRUE)[1:3]
#'     class_tabl_im <- sort(prop.table(table(class, m_im$classification)),
#'                           decreasing = TRUE)[1:3]
#'     c(sum(class_tabl_mf), sum(class_tabl_im))
#' })
#' # Performance on average
#' rowMeans(simulation)
#' # Performance SD
#' colSD(t(simulation))
#' # Plot shows slight advantage for missForest
#' plotdat <- data.frame(accuracy = as.vector(simulation), model =
#'                       rep(c("mf", "im"), n))
#' ggplot(plotdat, aes(x = accuracy, colour = model))+geom_density()
#'
#' # Simulation for real data (i.e., unknown whether assumptions are met)
#' simulation <- replicate(100, {
#'     d <- prodNA(iris[,1:4])
#'
#'     d_mf <- missForest(d)$ximp
#'     m_mf <- Mclust(d_mf, G = 3, "EEI")
#'     d_im <- imputeData(d, verbose = FALSE)
#'     m_im <- Mclust(d_im, G = 3, "EEI")
#'
#'     class_tabl_mf <- sort(prop.table(table(iris$Species,
#'                           m_mf$classification)), decreasing = TRUE)[1:3]
#'     class_tabl_im <- sort(prop.table(table(iris$Species,
#'                           m_im$classification)), decreasing = TRUE)[1:3]
#'     c(sum(class_tabl_mf), sum(class_tabl_im))
#' })
#'
#' # Performance on average
#' rowMeans(simulation)
#' # Performance SD
#' colSD(t(simulation))
#' # Plot shows slight advantage for missForest
#' plotdat <- data.frame(accuracy = as.vector(tmp),
#'                       model = rep(c("mf", "im"), n))
#' ggplot(plotdat, aes(x = accuracy, colour = model))+geom_density()
#' }
#' @rdname single_imputation
#' @export
single_imputation <- function(x, method = "imputeData"){
    if(all(complete.cases(x))) return(x)
    if(FALSE) missForest(x)
    imputed <- invisible(switch(method,
                      "imputeData" = do.call(imputeData, list(data = x, verbose = FALSE)),
                      "missForest" = do.call(missForest::missForest, list(xmis = as.matrix(x)))$ximp,
                      NULL))
    if(is.null(imputed)){
        stop("No method is currently defined for single imputation of data using '", method, "'.")
    }
    data.frame(imputed)
}
