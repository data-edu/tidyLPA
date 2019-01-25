# helpers.R
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("tidyLPA has received a major update, with a much easier workflow and improved functionality. However, you might have to update old syntax to account for the new workflow. See vignette('introduction-to-major-changes') for details!")
}
