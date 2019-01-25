# tidyLPA 0.2.4

* minor bug fixes
* last update before transitioning to development version presently at https://github.com/cjvanlissa/tidyLPA
* thank you to Qingyao Z. and @frigoli for correcting an error in the vignette and to others who submitted issues/identified bugs

# tidyLPA 0.2.3

* update to plot_profiles_mplus()
* add option to specify latent vars
* add message/vignette for major changes to come

# tidyLPA 0.2.2

* patch minor issue with model specification

# tidyLPA 0.2.1

* minor changes and bug fixes 
* addition of new reference to JOSS paper

# tidyLPA 0.2.0

## Major breaking change:

* change how models are specified; instead of using the model argument, whether and how the variances are covariances are estimated are passed the the variances and covariances argument; there are details in the readme and vignette and if a model argument is passed to a function, a message is returned describing how to specify the model using the variances and covariances arguments

## Major change

* change the compare_solutions_mplus() functions Mplus to still allow for the specification of six models, but to use the same four as compare_solutions() (which uses the mclust package, not MPlus) by default

# Minor changes: 

* improve NAMESPACE documentation
* add option to return original data frame for functions that use MPlus
* add option to use missing data for functions that use MPlus
* add tidylpa@googlegroups.com mailing list address to README as preferred contact method 
* remove deprecated function (to extract key statistics from an MPlus model)
* make it so that a data frame with fit and other statistics is returned by default from compare_solutions_mplus()
* added new values to the statistics returned by compare_solutions_mplus():
    - the cell size (the number of observations associated with each profile)
    - the number of times the log-likelihood was replicated, based on the number of optimization steps
    - Approximate Weight of Evidence (AWE) criterion
    - the number of parameters estimated
* remove the messages about the software being in beta
* how the Mplus syntax is generated was substantially changed/improved; thanks @gbiele

## Bug fixes

* change include_LMR argument to include VLMR
* remove scale_fill_brewer("", type = "qual", palette = "Set3") so that solutions with larger numbers of profiles may be plotted
* fix issue where lines longer than 90 characters (i.e., when there are many variables) cause an error

# tidyLPA 0.1.3

* improve plot_profiles() plots, including plotting bootstrapped standard when mclust output is directly used (thanks @cjvanlissa) & updated vignette with example of this
* improve output from compare_solutions_mplus (thanks @DJAnderson07)
* add function, extract_LL_mplus(), to extract log-likelihoods from models fit witsah estimate_profiles_mplus()
* update documentation for pisaUSA15 dataset
* improve compare_solutions_mplus() so it more reliably handles errors
* improve vignette (thanks @oreojo for suggestion to mention that this package works best for continuous variables)
* add URLs for package and bug reports to DESCRIPTION
* add C.J. van Lissa and Daniel John Anderson as contributors

# tidyLPA 0.1.2

Fix: 

* Specify version 0.7 of MplusAutomation in Imports to address error

Minor updates: 

* Update README and Vignette
* Update function names to include MPlus
* Export %>% from magrittr (so it does not need to be loaded from dplyr)
* Correct name of title for vignette
* Update function names
* Change output of estimate_profiles_mplus() to be returned with return(), instead of with invisible()

# tidyLPA 0.1.1

* Added a `NEWS.md` file to track changes to the package.
