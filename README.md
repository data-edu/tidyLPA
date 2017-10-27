
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyLPA
=======

The goal of `tidyLPA` is to provide tools to make it easier to use the `R` package [MCLUST](http://www.stat.washington.edu/mclust/) for Latent Profile Analysis analyses.

This is a sister-project to [prcr](https://github.com/jrosen48/prcr), for two-step cluster analysis. tidyLPA (which, again, is an interface to the `MCLUST` package) has been benchmarked to MPlus, at least for a simple dataset (the [iris dataset](https://en.wikipedia.org/wiki/Iris_flower_data_set)). You can find the results of that benchmarking, which showed the results to be nearly, identical, [here](https://jrosen48.github.io/blog/comparing-mplus-and-mclust-output/).

Example
=======

First, we can explore the Bayesian Information Criteria (BIC) or the Integrated Complete-data Likelihood (ICL) values, using the `explore_models_clust()` function:

``` r
library(tidyLPA)
```

Using the built-in `pisaUSA15` dataset (using just 200 observations for illustrative purposes) and variables for broad interest, enjoyment, and self-efficacy, we can quickly explore a three profile solution:

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
d <- sample_n(pisaUSA15, 200)
m3 <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 2)
#> Model with 3 profiles using the 'varying means, equal variances and covariances' model.
#> AIC is 1177.4
#> BIC is 1232.51
#> ICL is 1263.251
#> Entropy is 0.93032
plot_profiles_lpa(m3, to_center = TRUE)
```

![](README-unnamed-chunk-4-1.png)

We can also extract the posterior probabilities by setting `return_posterior_probs` to `TRUE`:

``` r
m3 <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 2, return_posterior_probs = TRUE)
#> Model with 3 profiles using the 'varying means, equal variances and covariances' model.
#> AIC is 1177.4
#> BIC is 1232.51
#> ICL is 1263.251
#> Entropy is 0.93032
m3
#> # A tibble: 189 x 5
#>    broad_interest enjoyment self_efficacy profile posterior_prob
#>  *          <dbl>     <dbl>         <dbl>   <dbl>          <dbl>
#>  1            3.4       3.0      1.750000       1        0.99836
#>  2            2.8       3.4      2.125000       1        0.98890
#>  3            1.4       2.0      2.375000       2        0.98077
#>  4            3.2       3.0      1.750000       1        0.99643
#>  5            3.0       3.0      1.375000       1        0.99416
#>  6            3.4       3.8      2.000000       1        0.99956
#>  7            2.6       2.8      1.666667       1        0.94932
#>  8            3.4       2.8      1.625000       1        0.99782
#>  9            3.2       3.0      2.625000       1        0.99276
#> 10            3.4       3.0      1.375000       1        0.99880
#> # ... with 179 more rows
```

See `?create_profiles_lpa` for a description of the models; model `2` as specified in this example is for a model with varying means but equal variances and covariances across profiles.

We can also explore a range of models (here using the built-in `iris` dataset, as this function takes longer to run with larger datasets):

``` r
d <- iris
compare_models_lpa(d, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
```

![](README-unnamed-chunk-6-1.png)
