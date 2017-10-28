
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyLPA
=======

The goal of `tidyLPA` is to provide tools to make it easier to use the `R` package [MCLUST](http://www.stat.washington.edu/mclust/) for Latent Profile Analysis analyses.

This is a sister-project to [prcr](https://github.com/jrosen48/prcr), for two-step cluster analysis. tidyLPA (which, again, is an interface to the `MCLUST` package) has been benchmarked to MPlus, at least for a simple dataset (the [iris dataset](https://en.wikipedia.org/wiki/Iris_flower_data_set)). You can find the results of that benchmarking, which showed the results to be nearly, identical, [here](https://jrosen48.github.io/blog/comparing-mplus-and-mclust-output/).

Example
=======

Here is a brief example using the built-in `pisaUSA15` dataset and variables for broad interest, enjoyment, and self-efficacy. See `?create_profiles_lpa` for more details and view the `Introduction to tidyLPA` vignette for more information.

``` r
library(tidyLPA)
```

``` r
d <- pisaUSA15
m3 <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 4, model = 2)
#> Model with 4 profiles using the 'varying means, equal variances and covariances' model.
#> AIC is 32204.043
#> BIC is 32342.423
#> ICL is 34776.451
#> Entropy is 0.80708
m3
#> # A tibble: 5,375 x 5
#>    broad_interest enjoyment self_efficacy profile posterior_prob
#>  *          <dbl>     <dbl>         <dbl>   <dbl>          <dbl>
#>  1            3.8       4.0         1.000       1        0.96454
#>  2            3.0       3.0         2.750       2        0.75999
#>  3            1.8       2.8         3.375       2        0.81157
#>  4            1.4       1.0         2.750       3        0.99951
#>  5            1.8       2.2         2.000       2        0.87555
#>  6            1.6       1.6         1.875       3        0.88153
#>  7            3.0       3.8         2.250       1        0.91827
#>  8            2.6       2.2         2.000       2        0.83843
#>  9            1.0       2.8         2.625       2        0.80674
#> 10            2.2       2.0         1.750       2        0.71589
#> # ... with 5,365 more rows
plot_profiles_lpa(m3, to_center = TRUE)
```

![](README-unnamed-chunk-3-1.png)
