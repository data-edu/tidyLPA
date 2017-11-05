
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
#> AIC is 16299.539
#> BIC is 16399.806
#> ICL is 18859.778
#> Entropy is 0.64094
m3
#> # A tibble: 2,692 x 5
#>    broad_interest enjoyment self_efficacy profile posterior_prob
#>             <dbl>     <dbl>         <dbl>   <dbl>          <dbl>
#>  1           2.00       1.0      3.000000       1        0.96493
#>  2           2.40       3.6      2.750000       3        0.60204
#>  3           2.40       3.0      2.750000       3        0.65147
#>  4           2.80       2.6      1.500000       3        0.64193
#>  5           2.25       2.0      2.125000       3        0.46955
#>  6           3.00       3.0      2.000000       3        0.63512
#>  7           2.20       3.0      3.857143       3        0.65411
#>  8           1.00       1.0      2.142857       1        0.99028
#>  9           3.20       3.0      2.000000       3        0.62924
#> 10           2.50       3.0      2.000000       3        0.64958
#> # ... with 2,682 more rows
plot_profiles_lpa(m3, to_center = TRUE)
```

![](README-unnamed-chunk-3-1.png)
