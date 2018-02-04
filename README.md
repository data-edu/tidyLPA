
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyLPA
=======

[![Travis build status](https://travis-ci.org/jrosen48/tidyLPA.svg?branch=master)](https://travis-ci.org/jrosen48/tidyLPA)

tidyLPA provides the functionality to carry out Latent Profile Analysis. Note that tidyLPA is still at the beta stage! Please report any bugs at <https://github.com/jrosen48/tidyLPA> or send an email to <jrosen@msu.edu>.

Installation
------------

You can install tidyLPA from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("jrosen48/tidyLPA")
```

Example
-------

Here is a brief example using the built-in `pisaUSA15` dataset and variables for broad interest, enjoyment, and self-efficacy. See `?create_profiles_lpa` for more details.

``` r
library(tidyLPA)
#> tidyLPA provides the functionality to carry out Latent Profile Analysis. Note that tidyLPA is still at the beta stage! 
#> Please report any bugs at https://github.com/jrosen48/tidyLPA or send an email to jrosen@msu.edu.
```

``` r
d <- pisaUSA15[1:100, ]

m3 <- estimate_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 2)
#> Fit varying means, equal variances and covariances (Model 2) model with 3 profiles.
#> LogLik is 279.692
#> AIC is 593.384
#> CAIC is 653.62
#> BIC is 636.62
#> SABIC is 582.951
#> ICL is 681.845
#> Entropy is 0.798

plot_profiles_lpa(m3, to_center = TRUE)
```

![](README-unnamed-chunk-3-1.png)

Learn more
==========

Please check out the [tidyLPA website](https://jrosen48.github.io/tidyLPA/) to learn more.

In particular, the [Intro to tidyLPA vignette](https://jrosen48.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html) has more in-depth information.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

Contact
=======

If you have any questions or feedback, please do not hesitate to get in touch:

-   By [email](mailto:jrosen@msu.edu)
-   By [Twitter](http://twitter.com/jrosenberg6432)
-   Through filing an issue on GitHub [here](https://github.com/jrosen48/tidyLPA).
