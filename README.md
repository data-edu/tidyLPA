
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyLPA
=======

The goal of `tidyLPA` is to provide tools to make it easier to use the `R` package [MCLUST](http://www.stat.washington.edu/mclust/) for Latent Profile Analysis analyses.

Installation
------------

You can install tidyLPA from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("jrosen48/tidyLPA")
```

Example
=======

Here is a brief example using the built-in `pisaUSA15` dataset and variables for broad interest, enjoyment, and self-efficacy. See `?create_profiles_lpa` for more details.

Also, please view the [Introduction to tidyLPA vignette](https://jrosen48.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html) for more information.

``` r
library(tidyLPA)
```

``` r
devtools::load_all(".")
#> Loading tidyLPA
```

``` r
library(dplyr, warn.conflicts = FALSE)
d <- pisaUSA15[1:100, ]
m3 <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 2)
#> Fit varying means, equal variances and covariances (Model 2) model with 3 profiles.
#> LogLik is 281.206
#> AIC is 596.412
#> CAIC is 656.648
#> BIC is 639.648
#> SABIC is 585.979
#> ICL is 669.316
#> Entropy is 0.865
plot_profiles_lpa(m3)
```

![](README-unnamed-chunk-4-1.png)
