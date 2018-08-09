# tidyLPA 0.2.0

## Major change:

* change the Mplus functions to allow for the specification of four common models, instead of six (with two very esoteric models)

# Minor changes: 

* improve NAMESPACE documentation
* add option to return original data frame for functions that use MPlus
* add option to use missing data for functions that use MPlus
* add tidylpa@googlegroups.com mailing list address to README as preferred contact method 
* remove deprecated function (to extract key statistics from an MPlus model)
* make it so that a data frame with fit and other statistics is returned by default from compare_solutions_mplus()
* added two new values to the statistics returned by compare_solutions_mplus(), a) the cell size (the number of observations associated with each profile) and b) the number of times the log-likelihood was replicated, based on the number of optimization steps
* remove the messages about the software being in beta

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
