
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyLPA
=======

The goal of `tidyLPA` is to provide tools to make it easier to use the `R` package [MCLUST](http://www.stat.washington.edu/mclust/) for Latent Profile Analysis analyses.

This is a sister-project to [prcr](https://github.com/jrosen48/prcr), for two-step cluster analysis. tidyLPA (which, again, is an interface to the `MCLUST` package) has been benchmarked to MPlus, at least for a simple dataset (the [iris dataset](https://en.wikipedia.org/wiki/Iris_flower_data_set)). You can find the results of that benchmarking, which showed the results to be nearly, identical, [here](https://jrosen48.github.io/blog/comparing-mplus-and-mclust-output/).

Installation
------------

You can install tidyLPA from github with:

``` r
# install.packages("devtools")
devtools::install_github("jrosen48/tidymixmod")
```

Example
=======

Here is a brief example using the built-in `pisaUSA15` dataset and variables for broad interest, enjoyment, and self-efficacy. See `?create_profiles_lpa` for more details.

Also, please view the [Introduction to tidyLPA vignette](https://jrosen48.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html) for more information.

``` r
library(tidyLPA)
```

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
d <- pisaUSA15
d <- sample_frac(d, .5)
m3 <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 2)
#> Model with 3 profiles using the 'varying means, equal variances and covariances' model.
#> AIC is 16106.59
#> BIC is 16206.787
#> ICL is 16698.022
#> Entropy is 0.92087
m3
#> # A tibble: 2,681 x 5
#>    broad_interest enjoyment self_efficacy profile posterior_prob
#>             <dbl>     <dbl>         <dbl>   <dbl>          <dbl>
#>  1            2.2       2.8         1.500       2        0.98343
#>  2            2.6       2.4         2.375       2        0.99106
#>  3            2.8       2.8         2.500       2        0.99162
#>  4            2.2       2.8         1.000       2        0.98168
#>  5            2.4       3.6         2.875       3        0.75493
#>  6            1.8       2.5         2.000       2        0.99541
#>  7            3.2       3.0         2.000       2        0.97364
#>  8            1.4       2.2         2.750       2        0.98097
#>  9            3.2       3.0         1.000       2        0.96800
#> 10            3.4       3.2         2.125       2        0.91580
#> # ... with 2,671 more rows
```
