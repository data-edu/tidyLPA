
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
m3 <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 4, model = 2)
#> Model with 4 profiles using the 'varying means, equal variances and covariances' model.
#> AIC is 15110.863
#> BIC is 15234.659
#> ICL is 15429.776
#> Entropy is 0.9677
m3
#> # A tibble: 2,684 x 5
#>    broad_interest enjoyment self_efficacy profile posterior_prob
#>  *          <dbl>     <dbl>         <dbl>   <dbl>          <dbl>
#>  1            3.2       4.0      2.000000       1        0.99999
#>  2            2.6       3.0      2.000000       2        0.99990
#>  3            2.4       3.0      2.000000       2        0.99989
#>  4            1.0       1.0      1.428571       3        1.00000
#>  5            3.2       3.0      2.500000       2        0.99993
#>  6            2.8       2.8      1.750000       2        0.99756
#>  7            3.0       3.0      2.000000       2        0.99991
#>  8            3.0       3.8      2.750000       1        0.99955
#>  9            3.0       3.0      2.000000       2        0.99991
#> 10            2.8       2.6      2.500000       2        0.92567
#> # ... with 2,674 more rows
plot_profiles_lpa(m3, to_center = TRUE)
```

![](README-unnamed-chunk-3-1.png)
