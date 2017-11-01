
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyLPA
=======

The goal of `tidyLPA` is to provide tools to make it easier to use the `R` package [MCLUST](http://www.stat.washington.edu/mclust/) for Latent Profile Analysis analyses.

This is a sister-project to [prcr](https://github.com/jrosen48/prcr), for two-step cluster analysis. tidyLPA (which, again, is an interface to the `MCLUST` package) has been benchmarked to MPlus, at least for a simple dataset (the [iris dataset](https://en.wikipedia.org/wiki/Iris_flower_data_set)). You can find the results of that benchmarking, which showed the results to be nearly, identical, [here](https://jrosen48.github.io/blog/comparing-mplus-and-mclust-output/).

Example
=======

Here is a brief example using the built-in `pisaUSA15` dataset and variables for broad interest, enjoyment, and self-efficacy. See `?create_profiles_lpa` for more details and view the [Introduction to tidyLPA vignette](https://jrosen48.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html) for more information.

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
#> AIC is 16366.576
#> BIC is 16466.805
#> ICL is 19038.138
#> Entropy is 0.63806
m3
#> # A tibble: 2,686 x 5
#>    broad_interest enjoyment self_efficacy profile posterior_prob
#>  *          <dbl>     <dbl>         <dbl>   <dbl>          <dbl>
#>  1            3.0       3.0         1.000       1        0.65860
#>  2            2.8       3.4         1.500       1        0.62543
#>  3            2.4       3.0         3.125       1        0.46379
#>  4            2.0       1.0         2.250       2        0.87012
#>  5            2.0       2.0         2.500       2        0.59641
#>  6            1.8       2.0         2.375       2        0.73886
#>  7            1.0       1.0         4.000       2        0.99391
#>  8            3.4       2.0         2.000       1        0.65632
#>  9            3.4       3.0         2.000       1        0.69110
#> 10            3.2       3.8         2.500       1        0.67175
#> # ... with 2,676 more rows
plot_profiles_lpa(m3, to_center = TRUE)
```

![](README-unnamed-chunk-3-1.png)
