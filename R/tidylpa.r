#' tidyLPA: Functionality to carry out Latent Profile Analysis in R
#'
#' Latent Profile Analysis (LPA) is a statistical modeling approach for
#' estimating distinct profiles, or groups, of variables. In the social
#' sciences and in educational research, these profiles could represent, for
#' example, how different youth experience dimensions of being engaged (i.e.,
#' cognitively, behaviorally, and affectively) at the same time.
#'
#' tidyLPA provides the functionality to carry out LPA in R. In particular,
#' tidyLPA provides functionality to specify different models that determine
#' whether and how different parameters (i.e., means, variances, and
#' covariances) are estimated and to specify (and compare solutions for) the
#' number of profiles to estimate.
#'
#' @docType package
#' @name tidyLPA
#' @importFrom dplyr sample_n everything bind_rows bind_cols arrange
#' mutate mutate_at filter case_when rename tbl_df select count semi_join
#' contains vars group_by summarize_all funs left_join mutate_all row_number
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 ggplot aes aes_string geom_line geom_point xlab ylab
#' theme_bw geom_col geom_errorbar theme element_text scale_x_discrete
#' position_dodge scale_fill_brewer position_jitterdodge scale_alpha_continuous
#' geom_vline
#' @importFrom magrittr "%>%"
#' @importFrom mclust Mclust MclustBootstrap mclustBootstrapLRT mclustBIC
#' mclustICL priorControl icl
#' @importFrom purrr map map_chr map_lgl map_df
#' @importFrom readr write_rds write_lines read_lines
#' @importFrom rlang .data
#' @importFrom stats complete.cases sd reshape quantile qnorm
#' @importFrom stringr str_c str_extract str_detect str_trim str_replace
#' str_sub str_split
#' @importFrom tibble tibble rownames_to_column as_tibble
#' @importFrom tidyr gather spread
#' @importFrom utils capture.output globalVariables write.table
NULL
