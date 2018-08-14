---
output:
  pdf_document: default
  html_document: default
---
# paper.md

---
title: 'tidyLPA'
tags:
  - R
  - mixture modeling
  - Latent Profile Analysis
  - MPlus
authors:
  - name: Joshua M. Rosenberg
    orcid: ""
    affiliation: "1
  - name: Author 2
    orcid: 0000-0000-0000-0000
    affiliation: 2
  - name: Author 3
    orcid: 0000-0000-0000-0000
    affiliation: 3
affiliations:
 - name: University of Tennessee, Knoxville
   index: 1
 - name: Institution 2
   index: 2
date: 13 August 2018
bibliography: paper.bib
---

# tidyLPA: Easily carry out latent profile analysis

Researchers are often interested in whether there are different groups on the basis of some measures, such as youths' experience dimensions of being engaged (i.e., cognitively, behaviorally, and affectively) in educational settings. Latent Profile Analysis (LPA) is a statistical method for finding such groups, or *profiles* of continuous measured variables [@pastor_et_al_2007]. LPA is a specific type of general mixture model [@harring_et_al_2016]. **tidyLPA** provides functionality to specify different models that determine whether and how different parameters (i.e., means, variances, and covariances) are estimated and to specify (and compare solutions for) the number of profiles to estimate. 

This R package provides an interface to two different tools for carrying out LPA, one the open-source **mclust** R package [@scrucca_et_al_2017] and the other the commercial **MPlus** [@muthen_et_al_2017] software (via the **MplusAutomation** R package [hallquist_et_al_2018]). Both the open-source and commercial tools allow for the specification of four model parameterizations, which range from very restricted to variable means, variances, and covariances being freely-estimated across profiles. 

The package follows a "tidy" approach (that used in the [**tidyverse** collection of R packages](https://www.tidyverse.org/)) in that input and output are both a data.frame (specifically its modified version, a tibble) that can be used to create plots or can be used in subsequent analyses and that the *pipe* operator, `%>%`, can be used to compose functions. Moreover, the package is designed and documented to be easy to use, especially for beginners (but also to provide options for finer-grained choices for estimating the model and for viewing more specific aspects of the LPA output).

The two primary functions are `compare_solutions()`, for comparing fit indices and other statistics associated with a range of model parameterizations and the number of profiles, and `estimate_profiles()`, which allows for easily interpreting the output and investigating the profile solution when used in conjunction with the `plot_profiles()` function. All three functions use **mclust**; corresponding functions with `_mplus()` appended use **MPlus**.

# References
