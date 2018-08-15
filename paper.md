---
title: tidyLPA
tags:
  - R
  - mixture modeling
  - Latent Profile Analysis
  - MPlus
authors:
  - name: Joshua M. Rosenberg
    orcid: ""
    affiliation: "1
affiliations:
 - name: University of Tennessee, Knoxville
   index: 1
date: 14 August 2018
bibliography: paper.bib
---

# tidyLPA: Easily carry out latent profile analysis

Researchers are often interested in whether there are different groups on the basis of some measures, such as youths' experience dimensions of being engaged (i.e., cognitively, behaviorally, and affectively) in educational settings. Latent Profile Analysis (LPA) is a statistical method for finding such groups, or *profiles* of continuous measured variables [@pastor_et_al_2007]. LPA is a specific type of general mixture model [@harring_et_al_2016]. **tidyLPA** provides functionality to specify different models that determine whether and how different parameters (i.e., means, variances, and covariances) are estimated and to specify (and compare solutions for) the number of profiles to estimate. 

This package makes it easier to carry out LPA. As LPA is only one type of mixture model, we do not expect it to replace the functionality of tools that allow for the estimation of more sophisticated. Nevertheless, this package provides a convenient way to carry out LPA using both open-source and commercial software. In doing so, it allows for researchers with and without access to MPlus to carry out this common statistical method. 

## A "tidy" user-interface 

The package follows a "tidy" approach (that used in the [**tidyverse** collection of R packages](https://www.tidyverse.org/)) in that input and output are both a data.frame (specifically its modified version, a tibble) that can be used to create plots or can be used in subsequent analyses and that the *pipe* operator, `%>%`, can be used to compose functions. Moreover, the package is designed and documented to be easy to use, especially for beginners (but also to provide options for finer-grained choices for estimating the model and for viewing more specific aspects of the LPA output).

## Functionality is provided through both open-source and commercial software

This R package provides an interface to two different tools for carrying out LPA, one the open-source **mclust** R package [@scrucca_et_al_2017] and the other the commercial **MPlus** [@muthen_et_al_2017] software (via the **MplusAutomation** R package [hallquist_et_al_2018]). Both the open-source and commercial tools allow for the specification of four model parameterizations, which range from very restricted to variable means, variances, and covariances being freely-estimated across profiles. The packages are benchmarked to one another; the benchmarks are checked when **tidyLPA** is deployed through automated tests.

The two primary functions are `compare_solutions()`, for comparing a wide range of fit indices, likelihood ratio tests, and other statistics (e.g., the entropy statistic) associated with a range of model parameterizations and the number of profiles, and `estimate_profiles()`, which allows for easily interpreting the output and investigating the profile solution when used in conjunction with the `plot_profiles()` function. All three functions use **mclust**; corresponding functions with `_mplus()` appended use **MPlus**. 

# References
