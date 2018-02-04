
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyLPA
=======

[![Travis build status](https://travis-ci.org/jrosen48/tidyLPA.svg?branch=master)](https://travis-ci.org/jrosen48/tidyLPA) [![CRAN status](http://www.r-pkg.org/badges/version/tidyLPA)](https://cran.r-project.org/package=tidyLPA).

tidyLPA provides the functionality to carry out Latent Profile Analysis (LPA). LPA is a statistical modeling approach for estimating parameters (i.e., the means, variances, and covariances) distinct profiles of groups of variables. Note that tidyLPA is still at the beta stage! Please report any bugs at <https://github.com/jrosen48/tidyLPA> or send an email to <jrosen@msu.edu>.

Installation
------------

You can install tidyLPA from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("jrosen48/tidyLPA")
```

Example
-------

Here is a brief example using the built-in `pisaUSA15` dataset and variables for broad interest, enjoyment, and self-efficacy. See `?estimate_profiles` for more details.

``` r
library(tidyLPA)
#> tidyLPA provides the functionality to carry out Latent Profile Analysis. Note that tidyLPA is still at the beta stage! 
#> Please report any bugs at https://github.com/jrosen48/tidyLPA or send an email to jrosen@msu.edu.
```

``` r
d <- pisaUSA15[1:100, ]

m3 <- estimate_profiles(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 2)
#> Fit varying means, equal variances and covariances (Model 2) model with 3 profiles.
#> LogLik is 279.692
#> AIC is 593.384
#> CAIC is 653.62
#> BIC is 636.62
#> SABIC is 582.951
#> ICL is 681.845
#> Entropy is 0.798

plot_profiles(m3, to_center = TRUE)
```

![](man/figures/README-unnamed-chunk-3-1.png)

To compare a range of profile solutions, use `compare_solutions()`

``` r
compare_solutions(d, broad_interest, enjoyment, self_efficacy)
```

![](man/figures/README-unnamed-chunk-4-1.png)

Learn more
----------

To learn more:

-   Please check out the tidyLPA [website](https://jrosen48.github.io/tidyLPA/)

-   In particular, the Introduction to tidyLPA [vignette]((https://jrosen48.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html)) has in-depth information and a brief walked-through example

Contact
-------

As tidyLPA is at an early stage of its development, issues should be expected. If you have any questions or feedback, please do not hesitate to get in touch:

-   By [email](mailto:jrosen@msu.edu)
-   By [Twitter](http://twitter.com/jrosenberg6432)
-   Through filing an issue on GitHub [here](https://github.com/jrosen48/tidyLPA)

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
