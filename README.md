
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyLPA
=======

[![Travis build status](https://travis-ci.org/jrosen48/tidymixmod.svg?branch=master)](https://travis-ci.org/jrosen48/tidymixmod)

tidyLPA provides the functionality to carry out Latent Profile Analysis. Note that tidyLPA is still at the beta stage! Please report any bugs at <https://github.com/jrosen48/tidyLPA> or send an email to <jrosen@msu.edu>.

Installation
------------

You can install tidyLPA from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("jrosen48/tidyLPA")
```

Example
=======

Here is a brief example using the built-in `pisaUSA15` dataset and variables for broad interest, enjoyment, and self-efficacy. See `?create_profiles_lpa` for more details.

Also, please view the [Introduction to tidyLPA vignette](https://jrosen48.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html) for more information.

``` r
library(tidyLPA)
```

    #> Loading tidyLPA
    #> tidyLPA provides the functionality to carry out Latent Profile Analysis. Note that tidyLPA is still at the beta stage! 
    #> Please report any bugs at https://github.com/jrosen48/tidyLPA or send an email to jrosen@msu.edu.

``` r
library(dplyr, warn.conflicts = FALSE)
d <- pisaUSA15[1:100, ]
m3 <- create_profiles_lpa(d, broad_interest, enjoyment, self_efficacy, n_profiles = 3, model = 2)
#> Fit varying means, equal variances and covariances (Model 2) model with 3 profiles.
#> LogLik is 279.692
#> AIC is 593.384
#> CAIC is 653.62
#> BIC is 636.62
#> SABIC is 582.951
#> ICL is 681.845
#> Entropy is 0.798
plot_profiles_lpa(m3)
```

![](README-unnamed-chunk-4-1.png)
