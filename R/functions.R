# functions.R

select_create_profiles <- function(df, ...){
    if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    df <- tibble::as_tibble(df)
    df_ss <- dplyr::select(df, ...)
    cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep

    # cases_to_keep <- dplyr::data_frame(row_names = 1:nrow(df_ss),
    #                                    keep = cases_to_keep)

    d <- df_ss[cases_to_keep, ] # removes incomplete cases

    return(d)
}

#' Create profiles for a specific mclust model
#' @details Creates profiles (or estimates of the mixture components) for a specific mclust model in terms of the specific number of mixture components and the structure of the residual covariance matrix
#' @param df data.frame with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param n_profiles the number of profiles (or mixture components) to be estimated
#' @param model the mclust model to explore: 1 (varying means, equal variances, and residual covariances fixed to zero); 2 (varying means, equal variances and covariances; and 3 (varying means, variances, and covariances), in order from most to least constrained; run ?mclust::mclustModelNames() to see all of the possible models and their names / abbreviations)
#' @param to_return character string for either "tibble" or "mclust" if "tibble" is selected, then data with a column for profiles is returned; if "mclust" is selected, then output of class mclust is returned
#' @param return_posterior_probs TRUE or FALSE (only applicable if to_return == "tibble"); whether to include posterior probabilities in addition to the posterior profile classification; defaults to FALSE
#' @import mclust
#' @examples
#' \dontrun{
#' d <- pisaUSA15
#' d <- dplyr::sample_n(d, 200)
#' m3 <- create_profiles_mclust(d,
#'                              broad_interest, enjoyment, instrumental_mot, self_efficacy,
#'                              n_profiles = 3, to_return="tibble")
#' }
#' @return either a tibble or a ggplot2 plot of the BIC values for the explored models
#' @export

create_profiles_lpa <- function(df,
                                ...,
                                n_profiles,
                                model = 1,
                                to_return = "tibble",
                                return_posterior_probs = TRUE){

    d <- select_create_profiles(df, ...)

    if (model == 1) {
        model <- "EEI"
    } else if (model == 2) {
        model <- "EEE"
    } else if (model == 3) {
        model <- "VVV"
    } else if (model %in% c("E", "V", "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV", "X", "XII", "XXI", "XXX")) {
        model <- model
    } else {
        stop("Model name is not correctly specified: use 1, 2, or 3 (see ?create_profiles_lpa for descriptions) or one of the model names specified from mclustModelNames() from mclust")
    }

    model_print <- ifelse(model == "EEI", "varying means, equal variances, and residual covariances fixed to zero",
                          ifelse(model == "EEE", "varying means, equal variances and covariances",
                                 ifelse(model == "VVV", "varying means, variances, and covariances", model)))

    m <- mclust::Mclust(d, G = n_profiles, modelNames = model)

    message("Model with ", n_profiles, " profiles using the '", model_print, "' model.")

    AIC <- (2*m$df - 2*m$loglik)

    message("Model AIC is ", round(abs(as.vector(AIC)), 3))
    message("Model BIC is ", round(abs(as.vector(m$BIC)), 3))
    message("Model ICL is ", round(abs(as.vector(mclust::icl(m))), 3))

    dff <- as.data.frame(dplyr::bind_cols(d, profile = m$classification)) # replace with tibble

    if (return_posterior_probs == TRUE) {
        col <- 1 - round(m$uncertainty, 5)
        dff <- dplyr::bind_cols(dff, posterior_prob = col)
    }

    attributes(dff)$mclust_output <- m

    if (to_return == "tibble") {
        return(tibble::as_tibble(dff))
    } else if (to_return == "mclust") {
        return(attributes(dff)$mclust_output)
    }
}

#' Calculate centroids from an mclust model object
#' @details Extract the output of the mclust output from the function create_profiles() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @param x an object of class `Mclust`
#' @export

calculate_centroids_mclust <- function(x) {
    o <- tibble::rownames_to_column(as.data.frame(x$parameters$mean))
    names(o) <- c("Variable", paste0("Profile", 1:x$G))
    o
}

# proc_df <- dff %>%
#     dplyr::mutate_at(vars(-classification), scale) %>%
#     dplyr::group_by(classification) %>%
#     dplyr::summarize_all(funs(mean)) %>%
#     dplyr::mutate(classification = paste0("Profile ", 1:n_profiles)) %>%
#     dplyr::mutate_at(vars(-classification), function(x) round(x, 3)) %>%
#     dplyr::rename(profile = classification)

#' Extract mclust output from the function create_profiles()
#' @details Extract the output of the mclust output from the function create_profiles() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @param x an object of class `Mclust`
#' @export

extract_mclust_output <- function(x) {
    attributes(x)$mclust_output
}

#' Extract mclust classifications from the function create_profiles()
#' @details Extract the classifications, in the form of posterior probabilties, for specific observations from the mclust output from the function create_profiles() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @param x an object of class `Mclust`
#' @export

extract_mclust_classifications <- function(x) {
    attributes(x)$mclust_output$classification
}

#' Extract mclust classifications from the function create_profiles()
#' @details Extract the classifications, in the form of posterior probabilties, for specific observations from the mclust output from the function create_profiles() so that posterior probabilities for specific observations, statistics related to the estimation, and other output can be viewed
#' @param x an object of class `Mclust`
#' @export

extract_mclust_classification_certainty <- function(x) {
    1 - attributes(x)$mclust_output$uncertainty
}

#' Calculate summary statistics for mclust output
#' @details groups by profile and calculates the mean
#' @param x output from create_profiles_mclust() when to_return is set to TRUE
#' @import dplyr
#' @importFrom magrittr %>%
#' @export

summary_mclust <- function(x) {
    x %>%
        group_by(profile) %>%
        summarize_all(mean)
}

#' Bootstrap the likelihood-ratio test statistic for mixture components
#' @details Bootstrap the p-values for the likelihood-ratio test statistic for the number of mixture components for an mclust model
#' @param df data.frame with two or more columns with continuous variables
#' @param ... additional arguments to be passed to mclustBootstrapLRT()
#' @param model_names names of one or more models ?run mclust::mclustModelNames() to see all of the possible models and their names / abbreviations)
#' @export

bootstrap_LRT_mclust <- function(df, model_names, ...) {
    if (length(model_names) == 1) {
        mclust::mclustBootstrapLRT(data = df, modelName = model_names, ...)
    } else if (length(model_names) > 1) {
        purrr::map(model_names, ~ mclust::mclustBootstrapLRT(data = df, modelName = .))
    }
}

#' Extract mclust variances
#' @details Extract the variances and covariances
#' @param x an object of class `Mclust`
#' @param profile_n the number of profiles
#' @export

extract_variance <- function(x, profile_n) {
    x$parameters$variance$sigma[, , profile_n] %>%
        diag() %>%
        dplyr::as_tibble() %>%
        dplyr::rename(est = value) %>%
        tibble::rownames_to_column("var_name") %>%
        dplyr::mutate(param_name = "Variances") %>%
        dplyr::mutate(class = paste0("class_", profile_n),
                      est = round(est, 3)) %>%
        dplyr::select(param_name, var_name, class, est)
}

#' Extract mclust covariance
#' @details Extract the covariances
#' @param x an object of class `Mclust`
#' @param profile_n the number of profiles
#' @import dplyr
#' @export

extract_covariance <- function(x, profile_n) {
    x$parameters$variance$sigma[, , profile_n] %>%
        as.data.frame() %>%
        rownames_to_column("param_name") %>%
        as.tibble() %>%
        gather(key, val, -param_name) %>%
        rename(var_name = key, est = val) %>%
        mutate(param_name = toupper(str_sub(param_name, start = 1L, end = 8L)),
               param_name = paste0(param_name, ".WITH"),
               param_name = str_replace(param_name, "\\.", "_"),
               var_name = toupper(var_name),
               var_name = str_replace(var_name, "\\.", "_"),
               var_name = str_sub(var_name, start = 1L, end = 10L))
}

#' Plot profile centroids
#' @details Plot the centroids for tibble or mclust output from create_profiles_lpa()
#' @param d output from create_profiles_mclust()
#' @param to_center whether to center the data before plotting
#' @param to_scale whether to scale the data before plotting
#' @param plot_what whether to plot tibble or mclust output from create_profiles_lpa(); defaults to tibble
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
#'
#

plot_profiles_lpa <- function(d, to_center = F, to_scale = F, plot_what = "tibble") {

    if (plot_what == "tibble") {

        d %>%
            dplyr::mutate_at(vars(-profile), scale, center = to_center, scale = to_scale) %>%
            group_by(profile) %>%
            summarize_all(mean) %>%
            tidyr::gather(key, val, -profile) %>%
            ggplot(aes(x = profile, y = val, fill = key)) +
            geom_col(position = "dodge") +
            theme_bw() +
            scale_fill_brewer("", type = "qual", palette=6)

    } else if (plot_what == "mclust") {

    }

}

#' Extract mclust summary statistics
#' @details Extract the log likelihood, BIC, and entropy statistics
#' @param x an object of class `Mclust`
#' @export

extract_mclust_summary <- function(x) {
    data.frame(LL = round(x$loglik, 3),
               BIC = round(x$bic * -1, 3),
               Entropy = round(1 - mean(round(x$uncertainty, 3)), 3))
}


#' Explore BIC of mclust models
#' @details Explore the BIC values of a range of models in terms of a) the structure of the residual covariance matrix and b) the number of mixture components (or profiles)
#' @param n_profiles_range a vector with the range of the number of mixture components to explore; defaults to 1 through 9 (1:9)
#' @param statistic what statistic to plot; BIC or ICL are presently available as options
#' @param return_table logical (TRUE or FALSE) for whether to return a table of the output intsead of a plot; defaults to FALSE
#' @inheritParams create_profiles_lpa
#' @return a ggplot2 plot of the BIC values for the explored models
#' @import mclust
#' @examples
#' d <- iris
#' compare_models_lpa(d, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#' @export

compare_models_lpa <- function(df, ..., n_profiles_range = 1:9, model = c(1, 2, 3), statistic = "BIC", return_table = FALSE) {

    d <- select_create_profiles(df, ...)

    model <- dplyr::case_when(
        model == 1 ~ "EEI",
        model == 2 ~ "EEE",
        model == 3 ~ "VVV",
        TRUE ~ as.character(model)
    )

    if (statistic == "BIC") {
        x <- mclust::mclustBIC(d, G = n_profiles_range, modelNames = model)
    } else if (statistic == "ICL") {
        x <- mclust::mclustICL(d, G = n_profiles_range, modelNames = model_names)
    } else {
        stop("This statistic cannot presently be computed")
    }

    y <- x %>%
        as.data.frame.matrix() %>%
        tibble::rownames_to_column("n_profiles") %>%
        dplyr::rename(`Constrained variance, fixed covariance` = EEI,
                      `Constrained variance, constrained covariance` = EEE,
                      `Freed variance, freed covariance` = VVV)

    to_plot <- y %>%
        tidyr::gather(`Covariance matrix structure`, val, -n_profiles) %>%
        dplyr::mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`),
                      val = abs(val)) # this is to make the BIC values positive (to align with more common formula / interpretation of BIC)

    to_plot$`Covariance matrix structure` <- forcats::fct_relevel(to_plot$`Covariance matrix structure`,
                                                                  "Constrained variance, fixed covariance",
                                                                  "Constrained variance, constrained covariance",
                                                                  "Freed variance, freed covariance")


    if(return_table == TRUE) {
        return(to_plot)
    }

    ggplot2::ggplot(to_plot, ggplot2::aes(x = n_profiles, y = val, color = `Covariance matrix structure`, group = `Covariance matrix structure`)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::ylab(paste0(statistic, " (smaller value is better)"))

}
