tidyLPA
================
14 August 2018

tidyLPA: Easily carry out latent profile analysis
=================================================

Researchers are often interested in identifying homogeneous subgroups within heterogeneous samples on the basis of a set of measures, such as dimensions of youths' engagement in educational settings (i.e., cognitively, behaviorally, and affectively). Latent Profile Analysis (LPA) is a statistical method for identifying such groups, or *latent profiles*, and is a special case of the general mixture model where all measured variables are continuous (Harring and Hodis 2016; Pastor et al. 2007). The **tidyLPA** package allows users to specify different models that determine whether and how different parameters (i.e., means, variances, and covariances) are estimated, and to specify and compare different solutions based on the number of profiles extracted.

The aim of the **tidyLPA** package is to provide a simple interface for conducting and evaluating LPA models. Given that LPA is only one type of mixture model, we do not expect it to replace the more general functionality of other tools that allow for the estimation of wider range of models. Nevertheless, this package provides convenient methods for conducting LPA using both open-source and commercial software, while aligning with a widely used coding framework (i.e., *tidy* data, described more below). In doing so, **tidyLPA** allows researchers with and without access to proprietary tools, such as MPlus, to conduct LPA.

A *tidy* user-interface
-----------------------

The input for *tidyLPA* assumes a tidy data structure (see Wickham and others 2014), and all output are returned in a tidy from, which aligns with the broad array of tools within the [**tidyverse**](https://www.tidyverse.org/) collection of R packages. The data can be efficiently used to create plots, explore model results, or used in subsequent analyses. The interface is also designed to work efficiently with the *pipe* operator, `%>%`, and **dplyr** helper functions can be used to select variables (e.g., `data %>% tidyLPA::estimate_profiles(dplyr::starts_with())`). The package is designed and documented to be easy to use, especially for beginners, but with finer-grained options available for estimating models and for evaluating specific output.

Functionality through both open-source and commercial software
--------------------------------------------------------------

The *tidyLPA* package provides an interface to two different tools for estimating models, one from the open-source **mclust** R package (Scrucca et al. 2017) and the other the commercial **MPlus** (L. Muthen and Muthen 2017) software (via the **MplusAutomation** R package \[hallquist\_et\_al\_2018\]). Both the open-source and commercial tools allow for the specification of four model parameterizations, which range from very restricted to means, variances, and covariances being freely-estimated across profiles. The packages are benchmarked to one another; the benchmarks are checked when **tidyLPA** is deployed through automated tests.

The two primary functions in the package are `estimate_profiles()` and `compare_solutions()`, with the former used to estimate a given model and the latter used evaluate differences in the fit of alternative models and number of profiles extracted. The `estimate_profiles()` function returns the predicted probability of membership in each probability for each case in the dataset, and allows for simple interpretation of the model output, particularly when combined with the `plot_profiles()` function, which displays the mean values (and their standard errors) on each measure for each profile.

The `compare_solutions()` function fits a wide range of models and returns various fit indices, including likelihood ratio tests and other statistics (e.g., entropy) for each parameterizations. All three functions use **mclust**; corresponding functions with `_mplus()` appended use the **MPlus** software.

References
==========

Harring, Jeffrey R, and Flaviu A Hodis. 2016. “Mixture Modeling: Applications in Educational Psychology.” *Educational Psychologist* 51 (3-4). Taylor & Francis: 354–67.

Muthen, LK, and BO Muthen. 2017. “Mplus User’s Guide (8th Ed.).” *Muthen & Muthen*.

Pastor, Dena A, Kenneth E Barron, BJ Miller, and Susan L Davis. 2007. “A Latent Profile Analysis of College Students’ Achievement Goal Orientation.” *Contemporary Educational Psychology* 32 (1). Elsevier: 8–47.

Scrucca, Luca, Michael Fop, Thomas Brendan Murphy, and Adrian E. Raftery. 2017. “Mclust 5: Clustering, Classification and Density Estimation Using Gaussian Finite Mixture Models.” *The R Journal* 8 (1): 205–33. <https://journal.r-project.org/archive/2017/RJ-2017-008/RJ-2017-008.pdf>.

Wickham, Hadley, and others. 2014. “Tidy Data.” *Journal of Statistical Software* 59 (10). Foundation for Open Access Statistics: 1–23.
