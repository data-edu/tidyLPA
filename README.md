tidymixmod
==========

The goal of tidymixmod is to provide tools to make it easier to use the `R` package `MCLUST` for Latent Profile Analysis analyses.

This is a sister-project to [prcr](https://github.com/jrosen48/prcr), for two-step cluster analysis, and may be merged with it in the future.

A candidate feature or change is to provide an interface to the general latent variable model software `OpenMx` to carry out Latent Profile Analysis (LPA) and Latent Class Analysis (LCA), as while presently `MCLUST` provides great functionality, it does not work with categorical variables, and therefore cannot be used to carry out LCA. Moreover, some more fine-grained specifications for how observed variables' residuals covary can be specified in `OpenMx` (i.e., specific correlations between specific variables' residuals).

Example
=======

First, we can explore the Bayesian Information Criteria (BIC) or the Integrated Complete-data Likelihood (ICL) values, using the `explore_models_clust()` function:

    ## Loading tidymixmod

``` r
library(tidymixmod)
```

Using the built-in `pisaUSA15` dataset and variables for broad interest, instrumental motivation, and self-efficacy, we can quickly explore a three profile solution:

``` r
d <- pisaUSA15
m3 <- create_profiles_mclust(d, broad_interest, instrumental_mot, self_efficacy, n_profiles = 3, to_return = "tibble")
```

    ## Fit model with 3 profiles using the 'constrained variance' model.

    ## Model BIC is 34636.654

``` r
summary_mclust(m3)
```

    ## # A tibble: 3 x 4
    ##   profile broad_interest instrumental_mot self_efficacy
    ##     <dbl>          <dbl>            <dbl>         <dbl>
    ## 1       1       3.576766         1.458378      1.575340
    ## 2       2       1.400114         2.529300      2.628835
    ## 3       3       2.719294         2.121035      2.159403

``` r
plot_profiles_mclust(m3)
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

We can also explore a range of models:

``` r
explore_models_mclust(d, broad_interest, instrumental_mot, self_efficacy)
```

    ## # A tibble: 5,367 x 3
    ##    broad_interest instrumental_mot self_efficacy
    ##             <dbl>            <dbl>         <dbl>
    ##  1            3.8             2.00         1.000
    ##  2            3.0             2.50         2.750
    ##  3            1.8             3.50         3.375
    ##  4            1.4             2.75         2.750
    ##  5            1.8             2.00         2.000
    ##  6            1.6             2.75         1.875
    ##  7            3.0             1.25         2.250
    ##  8            2.6             2.00         2.000
    ##  9            1.0             1.00         2.625
    ## 10            2.2             1.00         1.750
    ## # ... with 5,357 more rows

    ## Warning: Removed 7 rows containing missing values (geom_path).

    ## Warning: Removed 7 rows containing missing values (geom_point).

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)
