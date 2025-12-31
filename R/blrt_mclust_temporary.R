# if(!packageVersion("mclust") == package_version("6.1.2")) stop()
# # Copy/pasted from package documentation:
# library(mclust)
# mod2 <- Mclust(iris[,1:4], G = 1:3)
# mclustBootstrapLRT(data = mod2$data, model = mod2$modelName, nboot = 20, maxG = mod2$G)
#
# # Change the model to EEI:
# mod3 <- Mclust(iris[,1:4], G = 1:3, modelNames = "EEI")
# blrt_mclust(data = mod3$data, model = mod3$modelName, nboot = 20, maxG = mod3$G)
#
# # Error in estepXXI(data = bootSample[, -1], parameters = param0, warn = FALSE) :
# # could not find function "estepXXI"
#

blrt_mclust <- function (data, modelName = NULL, nboot = 999, level = 0.05,
          maxG = NULL, verbose = FALSE, ...)
{
    maxG <- as.numeric(maxG)
    G <- seq.int(1, maxG + 1)

    BIC <- mclustBIC(data, G = G, modelNames = modelName, warn = FALSE,
                     verbose = FALSE, ...)
    bic <- BIC[, attr(BIC, "modelNames") == modelName]
    G <- G[!is.na(BIC)]
    if (length(G) == 0)
        stop(paste("no model", modelName, "can be fitted."))
    if (all(G == 1)) {
        warning("only 1-component model could be fitted. No LRT is performed!")
        return()
    }
    if (sum(is.na(bic)) > 0)
        warning("some model(s) could not be fitted!")
    obsLRTS <- p.value <- vector("numeric", length = max(G) -
                                     1)
    bootLRTS <- matrix(as.double(NA), nrow = nboot, ncol = max(G) -
                           1)
    g <- 0
    continue <- TRUE
    while (g < (max(G) - 1) & continue) {
        g <- g + 1
        Mod0 <- summary(BIC, data, G = g, modelNames = modelName)
        Mod1 <- summary(BIC, data, G = g + 1, modelNames = modelName)
        obsLRTS[g] <- 2 * (Mod1$loglik - Mod0$loglik)
        b <- 0
        while (b < nboot) {
            b <- b + 1
            bootSample <- sim(Mod0$modelName, Mod0$parameters,
                              n = Mod0$n)
            param0 <- em(data = bootSample[, -1], modelName = modelName,
                             parameters = Mod0$parameters, warn = FALSE,
                             ...)$parameters
            loglik0 <- estep(data = bootSample[, -1], modelName = modelName,
                                 parameters = param0, warn = FALSE, ...)$loglik

            param1 <- em(data = bootSample[, -1], modelName = modelName,
                         parameters = Mod1$parameters, warn = FALSE,
                         ...)$parameters
            loglik1 <- estep(data = bootSample[, -1], modelName = modelName,
                             parameters = param1, warn = FALSE, ...)$loglik
            LRTS <- 2 * (loglik1 - loglik0)
            if (is.na(LRTS)) {
                b <- b - 1
                (next)()
            }
            bootLRTS[b, g] <- LRTS
        }
        p.value[g] <- (1 + sum(bootLRTS[, g] >= obsLRTS[g]))/(nboot +
                                                                  1)
        if (is.null(maxG) & p.value[g] > level) {
            continue <- FALSE
        }
    }
    data.frame(G = 1:g, modelName = modelName, obs = obsLRTS[1:g], p.value = p.value[1:g])
}
