
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
m3 <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 1)
#> Model with 3 profiles using the 'varying means, equal variances, and residual covariances fixed to zero' model.
#> AIC is 1167.482
#> BIC is 1212.792
#> ICL is 1285.864
#> Entropy is 0.83877
plot_profiles_lpa(m3, to_center = TRUE)
```

![](README-unnamed-chunk-4-1.png)

See `?create_profiles_lpa` for a description of the models; model `2` as specified in this example is for a model with varying means across profiles, but equal variances across profiles, and residual covariances fixed to zero.

We could specify other models:

``` r
m3i <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 2)
#> Model with 3 profiles using the 'varying means, equal variances and covariances' model.
#> AIC is 1132.2
#> BIC is 1187.22
#> ICL is 1196.975
#> Entropy is 0.978
m3ii <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 3)
#> Model with 3 profiles using the 'varying means, variances, and covariances' model.
#> AIC is 1159.533
#> BIC is 1253.39
#> ICL is 1302.717
#> Entropy is 0.88996
```

We can also extract the posterior probabilities by setting `return_posterior_probs` to `TRUE`:

``` r
m3 <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 2, return_posterior_probs = TRUE)
#> Model with 3 profiles using the 'varying means, equal variances and covariances' model.
#> AIC is 1132.2
#> BIC is 1187.22
#> ICL is 1196.975
#> Entropy is 0.978
m3
#> # A tibble: 188 x 5
#>    broad_interest enjoyment self_efficacy profile posterior_prob
#>  *          <dbl>     <dbl>         <dbl>   <dbl>          <dbl>
#>  1            3.2       3.0         2.125       1        0.99986
#>  2            1.0       1.0         2.750       2        0.99890
#>  3            2.0       2.2         2.625       1        0.95880
#>  4            4.0       3.0         2.000       1        0.99737
#>  5            1.0       1.0         2.125       2        0.99972
#>  6            1.2       1.0         2.500       2        0.99934
#>  7            2.8       2.8         1.375       1        0.99623
#>  8            3.8       3.2         1.250       1        0.99983
#>  9            4.0       3.2         1.000       1        0.99970
#> 10            2.8       3.0         1.500       1        0.99946
#> # ... with 178 more rows
```

We can also explore a range of models (here using the built-in `iris` dataset, as this function takes longer to run with larger datasets):

``` r
d <- iris
compare_models_lpa(d, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
```

![](README-unnamed-chunk-7-1.png)
