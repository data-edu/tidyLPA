
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/jrosen48/tidyLPA.svg?branch=master)](https://travis-ci.org/jrosen48/tidyLPA)

Background
----------

Latent Profile Analysis (LPA) is a statistical modeling approach for estimating distinct profiles, or groups, of variables. In the social sciences and in educational research, these profiles could represent, for example, how different youth experience dimensions of being engaged (i.e., cognitively, behaviorally, and affectively) at the same time.

tidyLPA provides the functionality to carry out LPA in R. In particular, tidyLPA provides functionality to specify different models that determine whether and how different parameters (i.e., means, variances, and covariances) are estimated and to specify (and compare solutions for) the number of profiles to estimate.

Installation
------------

You can install tidyLPA (version 0.1.2) from CRAN with:

``` r
install.packages("tidyLPA")
```

You can also install the in-development version of tidyLPA from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("jrosen48/tidyLPA")
```

Example
-------

Here is a brief example using the built-in `pisaUSA15` dataset and variables for broad interest, enjoyment, and self-efficacy. Note that we first type the name of the data frame, followed by the unquoted names of the variables used to create the profiles. We also specify the number of profiles and the model. See `?estimate_profiles` for more details.

``` r
library(tidyLPA)
```

``` r
d <- pisaUSA15[1:100, ]

estimate_profiles(d, 
                  broad_interest, enjoyment, self_efficacy, 
                  n_profiles = 3, 
                  model = 2)
#> Fit varying means, equal variances and covariances (Model 2) model with 3 profiles.
#> LogLik is 279.692
#> BIC is 636.62
#> Entropy is 0.798
#> # A tibble: 94 x 5
#>    broad_interest enjoyment self_efficacy profile posterior_prob
#>             <dbl>     <dbl>         <dbl> <fct>            <dbl>
#>  1           3.80      4.00          1.00 1                0.976
#>  2           3.00      3.00          2.75 2                0.847
#>  3           1.80      2.80          3.38 2                0.982
#>  4           1.40      1.00          2.75 3                0.963
#>  5           1.80      2.20          2.00 3                0.824
#>  6           1.60      1.60          1.88 3                0.960
#>  7           3.00      3.80          2.25 1                0.847
#>  8           2.60      2.20          2.00 3                0.704
#>  9           1.00      2.80          2.62 3                0.584
#> 10           2.20      2.00          1.75 3                0.861
#> # ... with 84 more rows
```

See the output is simply a data frame with the profile (and its posterior probability) and the variables used to create the profiles (this is the "tidy" part, in that the function takes and returns a data frame).

In addition to the number of profiles (specified with the `n_profiles` argument), the model is important. The `model` argument allows for four models to be specified:

-   Varying means, equal variances, and covariances fixed to 0 (model 1)
-   Varying means, equal variances, and equal covariances (model 2)
-   Varying means, varying variances, and covariances fixed to 0 (model 3)
-   Varying means, varying variances, and varying covariances (model 6)

Two additional models can be fit using functions that provide an interface to the MPlus software. More information on the models can be found in the [vignette](https://jrosen48.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html).

We can plot the profiles with by *piping* (using the `%>%` operator, loaded from the `dplyr` package) the output to `plot_profiles()`.

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

estimate_profiles(d, 
                  broad_interest, enjoyment, self_efficacy, 
                  n_profiles = 3, 
                  model = 2) %>% 
    plot_profiles(to_center = TRUE)
#> Fit varying means, equal variances and covariances (Model 2) model with 3 profiles.
#> LogLik is 279.692
#> BIC is 636.62
#> Entropy is 0.798
```

![](man/figures/README-unnamed-chunk-5-1.png)

More information
----------------

To learn more:

-   Browse the tidyLPA [website](https://jrosen48.github.io/tidyLPA/) (especially check out the Reference page to see more about other functions)

-   *Read the Introduction to tidyLPA* [vignette](https://jrosen48.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html), which has much more information on the models that can be specified with tidyLPA and on additional functionality

Contact
-------

As tidyLPA is at an early stage of its development, issues should be expected. If you have any questions or feedback, please do not hesitate to get in touch:

-   By [email (jrosen@msu.edu)](mailto:jrosen@msu.edu)
-   By [Twitter](http://twitter.com/jrosenberg6432)
-   Through filing an issue on GitHub [here](https://github.com/jrosen48/tidyLPA)

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
