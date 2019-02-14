#' @title Convert Mplus output to object of class 'tidyLPA'
#' @description Takes a list of Mplus output files of class \code{modelList},
#' containing only mixture models with a single categorical latent variable, and
#' converts it to an object of class \code{tidyLPA}.
#' @param modelList A list of class \code{modelList}, as generated by
#' \code{\link[MplusAutomation]{readModels}}.
#' @return A list of class  \code{tidyLPA}.
#' @author Caspar J. van Lissa
#' @examples
#' \dontrun{
#' library(MplusAutomation)
#' createMixtures(classes = 1:4, filename_stem = "cars",
#'                model_overall = "wt ON drat;",
#'                model_class_specific = "wt;  qsec;",
#'                rdata = mtcars,
#'                usevariables = c("wt", "qsec", "drat"),
#'                OUTPUT = "standardized")
#' runModels(replaceOutfile = "modifiedDate")
#' cars_results <- readModels(filefilter = "cars")
#' results_tidyLPA <- as.tidyLPA(cars_results)
#' results_tidyLPA
#' plot(results_tidyLPA)
#' plot_profiles(results_tidyLPA) # Throws error; missing column 'Classes'
#' }
#' @rdname as.tidyLPA
#' @keywords internal

as.tidyLPA <- function(modelList) {
    # Check if mplusModel is of class mplus.model
    if (!(inherits(modelList, "mplus.model") |
          all(sapply(modelList, function(x) {
              inherits(x, "mplus.model")
          })))) {
        stop(
            "as.tidyLPA requires an object of class 'mplus.model' or a list of mplus.models as its first argument."
        )
    }
    if (inherits(modelList, "mplus.model")) {
        modelList <- list(Model_1 = modelList)
    }
    # Check if mplusModel is a mixture model
    mixtures <- sapply(modelList, function(x) {
        !is.null(x$input$analysis[["type"]])
    })
    mixtures[which(mixtures)] <-
        sapply(modelList[which(mixtures)], function(x) {
            grepl("mixture", tolower(x$input$analysis$type))
        })
    if (!any(mixtures))
        stop(
            "plot_profiles requires a list of mixture models, or one mixture model, as its first argument."
        )
    if (any(!mixtures))
        warning(
            "Some output files were excluded because they are not mixture models; specifically: ",
            paste(names(mixtures)[which(!mixtures)], collapse = ", "),
            call. = FALSE
        )
    # Remove models which are not type "mixture"
    modelList <- modelList[which(mixtures)]
    # Check if all models were run on the same dataset
    if (length(unique(sapply(modelList, function(x) {
        x$input$data$file
    }))) > 1) {
        stop("Models were not all run on the same data file.")
    }
    # Check if any models have missing columns (result of nonconvergence)
    missing_cols <-
        sapply(modelList, function(x) {
            length(names(x$parameters[["unstandardized"]]))
        })
    missing_cols <- which(missing_cols != max(missing_cols))
    if (length(missing_cols) > 0) {
        warning(
            "Some models had missing columns in the coefficients section. This likely indicates a convergence problem. These models were dropped: ",
            paste(names(modelList)[missing_cols], collapse = ", "),
            call. = FALSE
        )
        modelList <- modelList[-missing_cols]
    }
    # Prepare plot data
    # Get coefficients
    missing_coefficients <-
        which(sapply(modelList, function(x) {
            is.null(x$parameters[["unstandardized"]])
        }))
    if (length(missing_coefficients > 0)) {
        warning(
            "Some models were missing the unstandardized coefficients. Please request these coefficients from Mplus.",
            call. = FALSE,
            immediate. = TRUE
        )
        modelList <- modelList[-missing_coefficients]
    }
    if (length(modelList) < 1)
        stop("No models left to convert to tidyLPA.", call. = FALSE)

    # Try to figure out what kind of model it was
    # lapply(modelList, function(x){
    #     class_specific <- which(grepl("\\d%", x$input$model))
    # })

    model_numbers <-
        paste0("unknown_model_", as.numeric(factor(sapply(modelList, function(x) {
            end_first_class <- grep("#2%", x$input$model)
            end_first_class <-
                ifelse(length(end_first_class) == 0,
                       length(x$input$model),
                       end_first_class - 1)
            paste(x$input$model[1:end_first_class], collapse = "")
        }))))

    out_list <- lapply(modelList, function(x) {
        this_class <- nrow(x$class_counts$modelEstimated)
        this_model <- NA

        out <- list(model = x)
        out$fit <-
            c(Model = this_model,
              Classes = this_class,
              calc_fitindices(out$model))
        out$estimates <- estimates(out$model)
        out$estimates$Model <-
            paste0("Model ", this_model, ", ", this_class, " classes")
        out$dff <- out$model$savedata
        out$dff$model_number <- this_model
        out$dff$classes_number <- this_class
        out$dff <-
            out$dff[, c((ncol(out$dff) - 1), ncol(out$dff), 1:(ncol(out$dff) - 2))]
        #if(simplify) out$model <- NULL
        class(out) <-
            c("tidyProfile.mplus", "tidyProfile", "list")
        out
    })

    class(out_list) <- c("tidyLPA", "list")
    names(out_list) <-
        paste("model_",
              model_numbers,
              "_class_",
              sapply(out_list, function(x) {
                  x$fit["Classes"]
              }),
              sep = "")
    out_list
}
