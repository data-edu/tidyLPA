check_solutions <- function(fits){
    if(any(fits$nmin < .1)) warning("Some profiles were assigned less than 10\\% of cases. Interpret the following solutions with care, and consider other solutions:\n  ",
                                    paste("Model", fits$Model[fits$nmin < .1], ", ", fits$Classes[fits$nmin < .1], "classes", collapse = "\n  "))
}

deprecated_arguments <- function(args, call = NULL){
    if(is.null(call)){
        call <- sys.call(-1)
    }
    call <- as.list(call)
    call_names <- names(call)[-1]
    function_name <- deparse(call[[1]])
    if (any(call_names %in% names(args))) {
        warning(
            "You are using a deprecated argument when calling '", function_name, "'. Check the documentation. Deprecated arguments are:\n  ",
            paste(paste0(names(args)[which(names(args) %in% call_names)], ": ", args[which(names(args) %in% call_names)]),
                  collapse = "\n  ")
            , call. = FALSE)
    }
}



#' Apply POMS-coding to data
#'
#' Takes in a data.frame, and applies POMS (proportion of of maximum)-coding to
#' the numeric columns.
#' @param data A data.frame.
#' @return A data.frame.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' data <- data.frame(a = c(1, 2, 2, 4, 1, 6),
#'                    b = c(6, 6, 3, 5, 3, 4),
#'                    c = c("a", "b", "b", "t", "f", "g"))
#' poms(data)
poms <- function(data){
    nums <- sapply(data, is.numeric)
    minscores <- sapply(data[, nums], min, na.rm=TRUE)
    data[, nums] <- sweep(data[, nums], 2, minscores, `-`)
    maxscores <- sapply(data[, nums], max, na.rm=TRUE)
    data[, nums] <- sweep(data[, nums], 2, maxscores, `/`)
    data
}

# Stirling's approximation for the SE of an SD
se_s_stirling <- function(s, n){
    s * sqrt(exp(1) * ((1-(1/n))^(n-1))-1)
}

# SE of an SD
se_s <- function(s, n){
    tryCatch({s * (gamma((n-1)/2)/gamma(n/2)) * sqrt(((n-1)/2)-(gamma(n/2)/gamma((n-1)/2))^2)},
             warning = function(x){se_s_stirling(s, n)})
}

syntax_class_specific <- function(mn, parameters){
    variances <- c("varying", "equal")[(mn%%2)+1]
    covariances <- c("zero", "equal", "varying")[ceiling((mn/2))]

    model_class_specific <- switch(variances,
        "equal" = label_parameters(paste0(paste(
            parameters, collapse = ";\n"
        ), ";")),
        "varying" = gsub("\\);", "{C}\\);",
                         label_parameters(paste0(
                             paste(parameters, collapse = ";  "), ";"
                         )))
    )
    cor_syntax <- paste(syntax_cor(parameters, parameters), collapse = "\n")
    cor_syntax <- switch(covariances,
        "equal" = label_parameters(cor_syntax),
        "varying" = gsub("\\);", "{C}\\);", label_parameters(cor_syntax)),
        "zero" = gsub(";", "@0;", cor_syntax)
    )

paste(model_class_specific, cor_syntax, sep = "\n\n")
}

# AHP-based model selection -----------------------------------------------
consistency_test <- function(m){
    lambda_max <- max(Re(eigen(m)$values))
    r <- dim(m)[1]
    if(abs(lambda_max - r) < 1e-12){
        return(TRUE)
    } else {
        CI <- (lambda_max - r)/(r-1)
        RI <- mean(replicate(100, {
            (max(Re(eigen(matrix(stats::runif(r*dim(m)[2]), ncol = r))$values))-r)/(r-1)
        }))
        CR <- CI/RI
        if(CR < .1){
            return(TRUE)
        } else {
            return(FALSE)
        }
    }
}

priority_vector <- function(m){
    eig <- eigen(m)
    ev <- Re(eig$vectors[, which.max(Re(eig$values))])
    ev/sum(ev)
}

#' @title Select best model using analytic hyrarchy process
#' @description Integrates information from several fit indices, and selects the best model.
#' @param fitindices A matrix or data.frame of fit indices, with colnames
#' corresponding to the indices named in \code{relative_importance}.
#' @param relative_importance A named numeric vector. Names should correspond to
#' columns in \code{fitindices}, and values represent the relative weight
#' assigned to the corresponding fit index. The default value corresponds to the
#' fit indices and weights assigned by Akogul and Erisoglu. To assign uniform
#' weights (i.e., each index is weighted equally), assign an equal value to all.
#' @return Numeric.
#' @details Many fit indices are available for model selection. Following
#' the procedure developed by Akogul and Erisoglu (2017), this function
#' integrates information from several fit indices, and selects the best model,
#' using Saaty's (1990) Analytic Hierarchy Process (AHP). Conceptually, the
#' process consists of the following steps:
#' \enumerate{
#' \item For each fit index, calculate the amount of support provided for each
#' model, relative to the other models.
#' \item From these comparisons, obtain a "priority vector" of the amount of
#' support for each model.
#' \item Compute a weighted average of the priority vectors for all fit indeces,
#' with weights based on a simulation study examining each fit index' ability to
#' recover the correct number of clusters (Akogul & Erisoglu, 2016).
#' \item Select the model with the highest weighted average priority.
#' }
#' @author Caspar J. van Lissa
#' @examples
#' iris[,1:4] %>%
#'   estimate_profiles(1:4) %>%
#'   get_fit() %>%
#'   AHP()
#' @export
AHP <- function(fitindices, relative_importance = c(AIC = 0.2323, AWE = 0.1129, BIC = 0.2525, CLC = 0.0922, KIC = 0.3101)){
    # Standardize
    relative_importance <- relative_importance/sum(relative_importance)
    fitindices <- fitindices[, names(relative_importance)]
    dm <- 1/(fitindices*10e-3)
    pc <- sapply(1:ncol(dm), function(x){dm[, x] %*% t(dm[, x])^-1}, simplify = "array")
    col_sums <- apply(pc, 3, colSums)

    if(any(!apply(pc, 3, consistency_test))){
        warning("Decision matrix was inconsistent; do not trust AHP results. Manually inspect results to determine the correct number of classes. See Saaty, 1990. DOI:10.1016/0377-2217(90)90057-I")
    }

    rivs <- apply(pc, 3, priority_vector)

    which.max(rivs %*% relative_importance)
}


# Model number ------------------------------------------------------------

colSD <- function(x){ sqrt(diag(stats::cov(x))) }

estimates <- function(model, ...){
    UseMethod("estimates")
}


estimates.mplus.model <- function(model){
  # Select means, variances, covariances of class-specific parameters; drop est_se
    df <- suppressWarnings(subset(model$parameters[["unstandardized"]], grepl("(^Means$|^Intercepts$|^Variances$|\\.WITH$)", model$parameters[["unstandardized"]]$paramHeader) & !is.na(as.numeric(model$parameters[["unstandardized"]]$LatentClass)), select = -5))
    # Extract original variable names

    varnames <- strsplit(model$input$variable$names, " ")[[1]]
    # Match original var names to names in $param column
    param_match <- sapply(df$param, pmatch, toupper(varnames))
    # Replace names in $param column
    df$param[!is.na(param_match)] <- varnames[param_match[!is.na(param_match)]]
    # Match original var names to names in $paramHeader column (remove any suffixes starting with ., like .WITH)
    param_match <- sapply(gsub("\\..*$", "", df$paramHeader), pmatch, toupper(varnames))
    # Replace these one at a time (necessary because of .WITH etc)
    for(i in 1:length(param_match)){
      if(is.na(param_match[i])) next
      df$paramHeader[i] <- gsub(names(param_match)[i], varnames[param_match[i]], df$paramHeader[i])
    }
    df <- subset(df, df$param %in% varnames)
    if(is.null(df)){return(NULL)}
    covariances <- grepl(".WITH$", df$paramHeader)
    df$param[covariances] <- paste(df$paramHeader[covariances], df$param[covariances], sep = ".")
    df$paramHeader[covariances] <- "Covariances"
    df$LatentClass <- as.integer(df$LatentClass)
    names(df) <- c("Category", "Parameter", "Estimate", "se", "p", "Class")
    df[!df$p == 999, ]
}


estimates.Mclust <- function(model){
    ses_mean <- apply(model$mclustBootstrap$mean, 3, colSD)
    ses_var <- apply(model$mclustBootstrap$variance, 4, function(x) {
        apply(x, 3, colSD)
    })

    var <- model$parameters$variance$sigma
    means <- model$parameters$mean

    if(!is.null(dim(means))){
        var_row <- paste(t(apply(var, 1, rownames))) # Get the rownames of var
        var_col <- paste(apply(var, 1, rownames)) # Colnames are transposed rownames
        covariances <- var_row != var_col
        var_col[covariances] <- paste(var_row[covariances], "WITH", var_col[covariances], sep = ".")
        var_row[covariances] <- "Covariances"
        var_row[!covariances] <- "Variances"
        var <- apply(var, 3, rbind)
    } else {
        if(!length(var) == length(means)){
            var <- rep(var, length(means))
        }
        var <- as.matrix(t(var))
        var_row <- "Variances"
        var_col <- colnames(model$data)[1]
        means <- as.matrix(t(means))
    }
    n_class <- ncol(var)
    var <- rbind(means, var)
    var_row <- c(rep("Means", nrow(means)), var_row)
    var_col <- c(rownames(means), var_col)
    not_estimated <- rowSums(var) == 0
    var <- as.vector(var)

    se <- as.vector(rbind(ses_mean, ses_var))

    df <- cbind(var_row, var_col, not_estimated)
    df <- cbind(df[rep(1:nrow(df), times = n_class), ], var, se)
    df <- df[!df[,3]=="TRUE", -3]
    df <- data.frame(df, stringsAsFactors = FALSE)
    df[, 3:4] <- lapply(df[, 3:4], as.numeric)
    df$p_value <- 2*pnorm(abs(df$var)/df$se, lower.tail = FALSE)
    df$Class <- rep(1:n_class, each = nrow(df)/n_class)
    row.names(df) <- NULL
    names(df) <- c("Category", "Parameter", "Estimate", "se", "p", "Class")
    df[!(duplicated(df$Estimate) & df$Category == "Covariances"), ]

}

#' @importFrom tidySEM table_results
estimates.MxModel <- function(model){
  # Select means, variances, covariances of class-specific parameters; drop est_se
  df <- tryCatch({
    table_results(model, columns = NULL)
  }, error = function(e){ return(NULL) })
  theclass <- suppressWarnings(as.integer(gsub("^class(\\d{0,})\\..+$", "\\1", df$openmx.label))) # Because only weights throws warning
  if(!isTRUE(length(theclass) == nrow(df))) theclass <- 1
  df$Class <- theclass

  #df$Category <- gsub("^(.+?)\\..*$", "\\1", df$label)
  df <- df[!is.na(df$Category), ]
  df <- df[df$Category %in% c("Variances", "Covariances", "Means"), ]
  df$col[df$Category == "Covariances"] <- paste(df$row[df$Category == "Covariances"], df$col[df$Category == "Covariances"], sep = ".")
  df$Parameter <- df$col
  row.names(df) <- NULL
  df$Estimate <- df$est
  df$p <- df$pvalue
  df <- df[!is.na(df$Class), ]
  df[order(df$Class, ordered(df$Category, levels = c("Means", "Variances", "Covariances"))), c("Category", "Parameter", "Estimate", "se", "p", "Class")]
}


# Conversion among number, title, and variance/covariance specific --------

get_title <- function(number){
    c(
        "Equal variances and covariances fixed to zero (model 1)",
        "Varying variances and covariances fixed to zero (model 2)",
        "Equal variances and equal covariances (model 3)",
        "Varying variances and equal covariances (model 4)",
        "Equal variances and varying covariances (model 5)",
        "Varying variances and varying covariances (model 6)"
    )[number]
}

get_modelname <- function(number){
    if(any(number %in% c(4, 5))) stop("Mclust does not allow for models with ", paste(tolower(get_title(number[which(number %in% c(4, 5))])), collapse = ", or "),".", call. = FALSE)
    c("EEI", "VVI", "EEE", "4", "5", "VVV")[number]
}

get_model_number <- function(variances, covariances){
    ((which(c("zero", "equal", "varying") == covariances)-1)*2)+which(c("equal", "varying") == variances)
}

number_to_varcov <- function(number){
  list(
    variances = c("varying", "equal")[(number %% 2)+1],
    covariances = c("zero", "equal", "varying")[ceiling(number / 2)]
  )
}


# Dynamic syntax generation -----------------------------------------------
syntax_cor <- function(x, y, all = TRUE){
    if(all){
        len <- length(x)
        #(1:len+(1:len-1)*len)

        drop <- unlist(sapply(1:len, function(x) {
            seq(from = (1 + (x - 1) * len),
                to = (x + (x - 1) * len))
        }))

        apply(expand.grid(x, " WITH ", y, ";")[-drop, c(3,2,1,4)], 1, paste, collapse = "")
    } else {
        paste(x, " WITH ", y, ";\n", collapse = "", sep = "")
    }
}

label_parameters <- function(syntax){
    if(grepl("\\(.*\\);", syntax)){
        if(grepl("([\\w\\(]+)\\K(\\d+)(?=[\\w\\);])", syntax, perl = TRUE)){
            #gsub("(?<=[\\w\\(]+)(\\d+)(?=[\\w\\);])", "", syntax, perl = TRUE)
            while(grepl("([\\w\\(]+)\\K(\\d+)(?=[\\w\\);])", syntax, perl = TRUE)){
                syntax <- gsub("([\\w\\(]+)\\K(\\d+)(?=[\\w\\);])", "", syntax, perl = TRUE)
            }
            syntax
        } else {
            gsub("\\s?\\(.+?\\)", "", syntax, perl = TRUE)
        }
    } else {
        syntax <- gsub("(\\w+)\\s+(o|O|w|W|b|B)(n|ith|y|N|ITH|Y)\\s+(\\w+)\\s{0,};", "\\1 \\2\\3 \\4 \\(\\1\\2\\4\\);", syntax)
        syntax <- gsub("\\[(.*?)\\]\\s{0,};", "\\[\\1\\] \\(m\\1\\);", syntax)
        gsub("(?<!\\w)(\\s{0,})(\\w+)\\s{0,};", "\\1\\2 \\(v\\2\\);", syntax, perl = TRUE)
    }
}


# Information criteria ----------------------------------------------------

calc_fitindices <- function(model, fitindices, ...){
  UseMethod("calc_fitindices", model)
}

calc_fitindices.Mclust <- function(model, fitindices, ...){
  # CAIC and BIC are much better than AIC, and slightly better than aBIC: https://www.statmodel.com/download/LCA_tech11_nylund_v83.pdf
  dots <- list(...)
  ll <- model$loglik
  if(is.null(dim(model$parameters$mean))){
    nvars <- 1
  } else {
    nvars <- nrow(model$parameters$mean)
  }

  nclass <- model$G
                # Prob     + means            + (co)vars, depending on model
  parameters <- (nclass-1) + (nvars * nclass) + switch(dots$modelname,
                                                       # One variance per variable,
                                                       "EEI" = nvars,
                                                       # One variance per variable per class
                                                       "V" = (nvars * nclass),
                                                       "VVI" = (nvars * nclass),
                                                       # Add covariances
                                                       "EEE" = nvars + (nvars * (nvars-1)),
                                                       "VVV" = (nvars * nclass) + ((nvars * (nvars-1)) * nclass),
                                                       nvars)# "E" partially matches EXPR, so use default option for "E" models.

  n <- model$n
  post_prob <- model$z
  class <- model$classification
  class_tab <- table(model$classification)
  if(length(class_tab) == ncol(post_prob)){
    prop_n <- range(prop.table(class_tab))
  } else {
    prop_n <- c(0, max(prop.table(class_tab)))
  }
  fits <- c(ifelse(ncol(post_prob) == 1, 1, 1 + (1/(nrow(post_prob)*log(ncol(post_prob))))*(sum(rowSums(post_prob * log(post_prob+1e-12))))),
            range(diag(classification_probs_mostlikely(post_prob, class))),
            prop_n,
            model$LRTS,
            model$LRTS_p
  )
  names(fits) <- c("Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_val", "BLRT_p")
  make_fitvector(ll = ll, parameters = parameters, n = n, postprob = post_prob, fits = fits)
}


mplus_as_list <- function(x){
  out <- switch(class(x)[1],
                mplus.model.list = x,
                mplus.model = list(Model_1 = x),
                mplusObject = list(Model_1 = x$results),
                model.list = lapply(x, `[[`, "results"),
                list = {
                  if(all(sapply(x, inherits, what = "mplusObject"))){
                    lapply(x, `[[`, "results")
                  } else {
                    if(all(sapply(x, inherits, what = "mplus.model"))){
                      x
                    } else {
                      stop("Not a list of Mplus models.")
                    }
                  }

                },
                stop("Not a list of Mplus models.")
  )
  if(is.null(names(out))){
    nms <- sapply(1:length(out), function(i){
      if(!is.null(out[[i]][["input"]][["title"]])){
        out[[i]][["input"]][["title"]]
      } else {
        paste0("Model ", i)
      }
    })
    names(out) <- nms
  } else {
    if(any(names(out) == "")){
      names(out)[which(names(out) == "")] <- paste0("Model ", which(names(out) == ""))
    }
  }
  out
}


one_mplus_model <- function(x){
  out <- switch(class(x)[1],
                mplus.model = x,
                mplusObject = x$results,
                model.list = {
                  if(length(x) == 1){
                    x[[1]][["results"]]
                  } else {
                    stop("Not a single Mplus model.")
                  }},
                list = {
                  if(length(x) == 1){
                    if(inherits(x, "mplusObject")){
                      x[["results"]]
                    } else {
                      if(inherits(x, "mplus.model")){
                        x
                      } else {
                        stop("Not a single Mplus model.")
                      }
                    }
                  }
                },
                mplus.model.list = {
                  if(length(x) == 1){
                    x
                  } else {
                    stop("Not a single Mplus model.")
                  }
                },
                stop("Not a single Mplus model.")
  )
  out
}


calc_fitindices.MplusObject <- function(model, fitindices, ...){
  model <- one_mplus_model(model)
  # CAIC and BIC are much better than AIC, and slightly better than aBIC: https://www.statmodel.com/download/LCA_tech11_nylund_v83.pdf
  ll <- model$summaries$LL
  parameters <- model$summaries$Parameters
  n <- model$summaries$Observations
  post_prob <- model$savedata[, grep("^CPROB", names(model$savedata))]
  fits <- c(ifelse(is.null(model$summaries$Entropy), 1, model$summaries$Entropy),
            tryCatch(range(diag(model$class_counts$classificationProbs.mostLikely)),
                     warning = function(x) {
                       c(NA, NA)
                     }),
            range(model$class_counts$mostLikely$proportion),
            ifelse(is.null(model$summaries$BLRT_2xLLDiff), NA, model$summaries$BLRT_2xLLDiff),
            ifelse(is.null(model$summaries$BLRT_PValue), NA, model$summaries$BLRT_PValue)
  )
  names(fits) <- c("Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_val", "BLRT_p")
  make_fitvector(ll = ll, parameters = parameters, n = n, postprob = post_prob, fits = fits)
}

calc_fitindices.mplus.model <- calc_fitindices.MplusObject

#' @importFrom utils getFromNamespace
#' @importFrom tidySEM class_prob
table_fit.mixture_list <- getFromNamespace("table_fit.mixture_list", "tidySEM")
calc_fitindices.MxModel <- function (model, fitindices, ...){
  sums <- do.call(table_fit.mixture_list, list(list(model)))
  ll <- sums$LL
  parameters <- sums$Parameters
  n <- sums$n
  all_probs <- class_prob(model)
  post_prob <- all_probs$individual
  fits <- data.frame()
  if(any(c("Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_val", "BLRT_p") %in% names(sums))){
    fits <- sums[c("Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_val", "BLRT_p")[which(c("Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_val", "BLRT_p") %in% names(sums))]]
  }
  if(any(!c("Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_val", "BLRT_p") %in% names(sums))){
    fits[1,c("Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_val", "BLRT_p")[which(!c("Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_val", "BLRT_p") %in% names(sums))]] <- NA_real_
  }
  fits <- fits[,c("Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_val", "BLRT_p")]
  make_fitvector(ll = ll, parameters = parameters, n = n, postprob = post_prob, fits = fits)
}

make_fitvector <- function(ll, parameters, n, postprob, fits){
  if(!is.data.frame(fits)) fits <- as.data.frame(t(fits))
  LogLik = ll
  AIC = -2*ll + (2*parameters)
  AWE = -2*(ll + unname(unlist(fits[1]))) + 2*parameters*(3/2 + log(n))
  BIC = -2*ll + (parameters * log(n))
  CAIC = -2*ll + (parameters * (log(n)+1))
  CLC = -2*ll + (2*unname(unlist(fits[1])))
  KIC = -2*ll + (3*(parameters + 1))
  SABIC = -2*ll + (parameters * log(((n+2)/24)))
  ICL = icl_default(postprob, BIC)
  cbind("LogLik" = LogLik,
    "parameters" = parameters,
    "n" = n,
    "AIC" = AIC,
    "AWE" = AWE,
    "BIC" = BIC,
    "CAIC" = CAIC,
    "CLC" = CLC,
    "KIC" = KIC,
    "SABIC" = SABIC,
    "ICL" = ICL,
    fits)
}

extract_postprob <- function(model){
  priors <- model$expectation$output$weights
  mix_names <- names(model@submodels)

  attr(model@submodels$class1, "fitfunction")$result
  liks <- sapply(mix_names, function(x){ attr(model@submodels[[x]], "fitfunction")$result })

  #First calculate posteriors:
  posteriors <- sweep(liks, 2, priors, "*")
  #Then calculate participants' marginal likelihoods (probably not be necessary):
  marglik <- rowSums(posteriors)
  # Divide posteriors by marginal likelihood
  posteriors <- posteriors/marglik
  # then normalize them into probabilities:
  posteriors/rowSums(posteriors)
}

classification_probs_mostlikely <- function (post_prob, class)
{
  if (is.null(dim(post_prob)))
    return(1)
  avg_probs <- avgprobs_mostlikely(post_prob, class)
  avg_probs[is.na(avg_probs)] <- 0
  C <- dim(post_prob)[2]
  N <- sapply(1:C, function(x) sum(class == x))
  tab <- mapply(function(this_row, this_col) {
    (avg_probs[this_row, this_col] * N[this_row])/(sum(avg_probs[,
                                                                 this_col] * N, na.rm = TRUE))
  }, this_row = rep(1:C, C), this_col = rep(1:C, each = C))
  matrix(tab, C, C, byrow = TRUE)
}

avgprobs_mostlikely <- function (post_prob, class)
{
  if (is.null(dim(post_prob)))
    return(1)
  t(sapply(1:ncol(post_prob), function(i) {
    colMeans(post_prob[class == i, , drop = FALSE])
  }))
}

icl.MxModel <- function (object, ...)
{

}




avgprobs_mostlikely <- function(post_prob, class){
    if(is.null(dim(post_prob))) return(1)
    t(sapply(1:ncol(post_prob), function(i){colMeans(post_prob[class == i, , drop = FALSE])}))
}

classification_probs_mostlikely <- function(post_prob, class){
    if(is.null(dim(post_prob))) return(1)
    avg_probs <- avgprobs_mostlikely(post_prob, class)
    avg_probs[is.na(avg_probs)] <- 0
    C <- dim(post_prob)[2]
    N <- sapply(1:C, function(x) sum(class == x))
    tab <- mapply(function(this_row, this_col){
        (avg_probs[this_row, this_col]*N[this_row])/(sum(avg_probs[ , this_col] * N, na.rm = TRUE))
    }, this_row = rep(1:C, C), this_col = rep(1:C, each = C))

    matrix(tab, C, C, byrow = TRUE)
}

# ICL method for mplus.model ----------------------------------------------

icl.mplus.model <- function(object, ...)
{
    n <- object$summaries$Observations
    z <- object$savedata[, grep("^CPROB", names(object$savedata))]
    if(!is.null(dim(z))){
        C <- z == apply(z, 1, max)
        (-1*object$summaries$BIC) + 2*sum(C * log(apply(z, 1, function(x){x[which.max(x)]})+1e-12))
    } else {
        (-1*object$summaries$BIC) + 2*sum(z * log(z))
    }
}

icl_default <- function(post_prob, BIC){
  tryCatch({
    if (!is.null(dim(post_prob))) {
      C <- post_prob == apply(post_prob, 1, max)
      (-1 * BIC) + 2 * sum(C * log(apply(post_prob,
                                                 1, function(x) {
                                                   x[which.max(x)]
                                                 }) + 1e-12))
    }
    else {
      (-1 * BIC) + 2 * sum(post_prob * log(post_prob))
    }
  }, error = function(e){ NA })
}
#LMR and ALMR indistinguishable from each other: https://www.tandfonline.com/doi/full/10.1080/10705511.2016.1169188?scroll=top&needAccess=true

# https://www.statmodel.com/download/LCA_tech11_nylund_v83.pdf

#' @title Lo-Mendell-Rubin likelihood ratio test
#' @description Implements the ad-hoc adjusted likelihood ratio test (LRT)
#' described in Formula 15 of Lo, Mendell, & Rubin (2001), or LMR LRT.
#' @param n Integer. Sample size
#' @param null_ll Numeric. Log-likelihood of the null model.
#' @param null_param Integer. Number of parameters of the null model.
#' @param null_classes Integer. Number of classes of the null model.
#' @param alt_ll Numeric. Log-likelihood of the alternative model.
#' @param alt_param Integer. Number of parameters of the alternative model.
#' @param alt_classes Integer. Number of classes of the alternative model.
#' @return A numeric vector containing the likelihood ratio LR, the ad-hoc
#' corrected LMR, degrees of freedom, and the LMR p-value.
#' @references Lo Y, Mendell NR, Rubin DB. Testing the number of components in a
#' normal mixture. Biometrika. 2001;88(3):767–778. doi:10.1093/biomet/88.3.767
#' @examples
#' calc_lrt(150L, -741.02, 8, 1, -488.91, 13, 2)
#' @rdname calc_lrt
#' @export
calc_lrt <- function(n, null_ll, null_param, null_classes, alt_ll, alt_param, alt_classes){
  args <- list(
    null = c(null_ll, null_param, n, null_classes),
    alt = c(alt_ll, alt_param, n, alt_classes)
  )
  do.call(calc_lrt_internal, args)
}

#' @importFrom stats pchisq
calc_lrt_internal <- function(null, alt){
  # c(ll, parameters, n, class)
  if(!alt[3] == null[3]){
    warning("Null and alt models estimated on different number of cases, and might not be nested. Used lowest number of cases.")
  }
  n <- min(c(alt[3], null[3]))
  if(!alt[4]>null[4]){
    stop("Alternative model does not have more classes than null model.")
  }
  lr_test_stat = 2 * (alt[1] - null[1])
  modlr_test_stat <- lr_test_stat / (1 + (((3 * alt[4] - 1) - (3 * null[4] - 1)) * log(n))^-1)
  df <- (alt[2] - null[2])
  lmrt_p <- pchisq(q = modlr_test_stat, df = df, lower.tail = FALSE)
  out <- c(lr = lr_test_stat, lmr_lr = modlr_test_stat, df = df, lmr_p = lmrt_p)
  class(out) <- c("LRT", class(out))
  out
}

#' @method print LRT
#' @export
print.LRT <- function(x,
                      digits = 3,
                      na.print = "",
                      ...) {
  cat("Lo-Mendell-Rubin ad-hoc adjusted likelihood ratio rest:\n\n")
  cat(
    "LR = ",
    formatC(x[["lr"]], digits = digits, format = "f"),
    ", LMR LR (df = ",
    as.integer(x[["df"]]),
    ") = ",
    formatC(x[["lmr_lr"]], digits = digits, format = "f"),
    ", p ",
    ifelse(x[["lmr_p"]] < 1/10^digits,
           paste0("< ", 1/10^digits),
           paste0("= ", formatC(x[["lmr_p"]], digits = digits, format = "f")))
  , sep = "")
}
