
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tidyLPA)](https://cran.r-project.org/package=tidyLPA)
[![](https://cranlogs.r-pkg.org/badges/tidyLPA)](https://cran.r-project.org/package=tidyLPA)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.00978/status.svg)](https://doi.org/10.21105/joss.00978)
[![R-CMD-check](https://github.com/data-edu/tidyLPA/workflows/R-CMD-check/badge.svg)](https://github.com/data-edu/tidyLPA/actions)
[![test-coverage](https://github.com/data-edu/tidyLPA/workflows/test-coverage/badge.svg)](https://github.com/data-edu/tidyLPA/actions)

## Background

Latent Profile Analysis (LPA) is a statistical modeling approach for
estimating distinct profiles, or groups, of variables. In the social
sciences and in educational research, these profiles could represent,
for example, how different youth experience dimensions of being engaged
(i.e., cognitively, behaviorally, and affectively) at the same time.

tidyLPA provides the functionality to carry out LPA in R. In particular,
tidyLPA provides functionality to specify different models that
determine whether and how different parameters (i.e., means, variances,
and covariances) are estimated and to specify (and compare solutions
for) the number of profiles to estimate. The package is designed and
documented to be easy to use, especially for beginners to LPA, but with
fine-grained options available for estimating models and evaluating
specific output as part of more complex analyses.

## Installation

You can install tidyLPA from CRAN with:

``` r
install.packages("tidyLPA")
```

You can also install the development version of tidyLPA from GitHub
with:

``` r
install.packages("devtools")
devtools::install_github("data-edu/tidyLPA")
```

## Examples

### Mclust

Here is a brief example using the built-in `pisaUSA15` data set and
variables for broad interest, enjoyment, and self-efficacy. Note that we
first type the name of the data frame, followed by the unquoted names of
the variables used to create the profiles. We also specify the number of
profiles and the model. See `?estimate_profiles` for more details.

In these examples, we pass the results of one function to the next by
*piping* (using the `%>%` operator, loaded from the `dplyr` package). We
pass the data to a function that selects relevant variables, and then to
`estimate_profiles`:

``` r
library(tidyLPA)
library(dplyr)
```

``` r
pisaUSA15[1:100, ] %>%
  select(broad_interest, enjoyment, self_efficacy) %>%
  single_imputation() %>%
  estimate_profiles(3)
#> tidyLPA analysis using mclust: 
#> 
#>  Model Classes AIC    BIC    Entropy prob_min prob_max n_min n_max BLRT_p
#>  1     3       629.72 666.20 0.80    0.84     0.95     0.04  0.67  0.01
```

A simple summary of the analysis is printed to the console (and its
posterior probability).

The resulting object can be further passed down a pipeline to other
functions, such as:

-   `plot_profiles()`
-   `compare_solutions()`
-   `get_data()`
-   `get_fit()`

This is the “tidy” part, in that the function can be embedded in a tidy
analysis pipeline.

### Mplus

We can use MPlus simply by changing the package argument for
`estimate_profiles()` to `"MplusAutomation"` (please note that *MPlus
must be installed on your computer for this functionality to work*):

``` r
pisaUSA15[1:100, ] %>%
  select(broad_interest, enjoyment, self_efficacy) %>%
  single_imputation() %>%
  estimate_profiles(3, package = "MplusAutomation")
```

# Learning More

To learn more, we highly recommend the following:

-   Browse the tidyLPA [website](https://data-edu.github.io/tidyLPA/)
    (especially check out the Reference page to see more about other
    functions)
-   *Read the Introduction to tidyLPA*
    [vignette](https://data-edu.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html),
    which has much more information on the models that can be specified
    with tidyLPA and on additional functionality

## Citing tidyLPA

> Rosenberg, J. M., Beymer, P. N., Anderson, D. J., Van Lissa, C. J., &
> Schmidt, J. A. (2018). tidyLPA: An R Package to Easily Carry Out
> Latent Profile Analysis (LPA) Using Open-Source or Commercial
> Software. *Journal of Open Source Software, 3*(30), 978,
> <https://doi.org/10.21105/joss.00978>

## Contributing and Contact Information

One of the easiest but also most important ways to contribute is to post
a question or to provide feedback. Both positive *and* negative feedback
is welcome and helpful. You can get in touch by:

-   Sending a message via <tidylpa@googlegroups.com> or view the [the
    tidyLPA group page](https://groups.google.com/forum/#!forum/tidylpa)

-   Filing an issue on GitHub
    [here](https://github.com/data-edu/tidyLPA)

Contributions are also welcome via by making pull requests (PR),
e.g. through [this page on
GitHub](https://github.com/data-edu/tidyLPA/pulls).

It may be easier if you first file an issue outlining what you will do
in the PR. You can also reach out via the methods described above.

## Contributor Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://link.springer.com/article/10.1007/s41686-019-00030-5).
By participating in this project you agree to abide by its terms.

## Acknowledgments

This material is based upon work supported by the National Science
Foundation under *Grant No.: DRL\#1661064*. Any opinions, findings,
conclusions, or recommendations expressed in this material are those of
the authors and do not reflect the views of the National Science
Foundation.
