
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

Using the built-in `pisaUSA15` dataset and variables for broad interest, enjoyment, and self-efficacy, we can quickly explore a three profile solution:

``` r
d <- pisaUSA15
m3 <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 2)
#> Model with 3 profiles using the 'varying means, equal variances and covariances' model.
#> AIC is 32198.704
#> BIC is 32310.725
#> ICL is 33168.746
#> Entropy is 0.93051
plot_profiles_lpa(m3, to_center = TRUE)
```

![](README-unnamed-chunk-4-1.png)

We can also extract the posterior probabilities by setting `return_posterior_probs` to `TRUE`:

``` r
m3 <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 2, return_posterior_probs = TRUE)
#> Model with 3 profiles using the 'varying means, equal variances and covariances' model.
#> AIC is 32198.704
#> BIC is 32310.725
#> ICL is 33168.746
#> Entropy is 0.93051
m3
#> # A tibble: 5,375 x 5
#>    broad_interest enjoyment self_efficacy profile posterior_prob
#>  *          <dbl>     <dbl>         <dbl>   <dbl>          <dbl>
#>  1            3.8       4.0         1.000       1        0.96594
#>  2            3.0       3.0         2.750       2        0.97333
#>  3            1.8       2.8         3.375       2        0.98003
#>  4            1.4       1.0         2.750       3        0.99949
#>  5            1.8       2.2         2.000       2        0.98567
#>  6            1.6       1.6         1.875       3        0.87652
#>  7            3.0       3.8         2.250       1        0.92046
#>  8            2.6       2.2         2.000       2        0.95714
#>  9            1.0       2.8         2.625       2        0.94357
#> 10            2.2       2.0         1.750       2        0.80086
#> # ... with 5,365 more rows
```

See `?create_profiles_lpa` for a description of the models; model `2` as specified in this example is for a model with varying means but equal variances and covariances across profiles.

We can also explore a range of models (here using the built-in `iris` dataset, as this function takes longer to run with larger datasets):

``` r
d <- iris
compare_models_lpa(d, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
```

![](README-unnamed-chunk-6-1.png)
