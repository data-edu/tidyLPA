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
