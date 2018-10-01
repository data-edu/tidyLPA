# mplusavailable.R

#' Check whether Mplus can be found
#'
#' This is a simple utility to check whether Mplus can be found.
#' Returns 0 if Mplus command can be found by the system.
#' If \code{silent = FALSE}, prints a message to the user
#' to help suggest what to do.
#'
#' Note: This is copied directly from the MplusAutomation
#' repository. This is because it is only available for the
#' development version. When it is part of an update, it will
#' be imported from MplusAutomation.
#'
#' @param silent A logical whether to print a message or not.
#'   Defaults to \code{TRUE} for silent operation.
#' @return The status of finding Mplus. Per unix conventions,
#' status 0 indicates Mplus was found (0 problems) and
#' status 1 indicates that Mplus was not found.
#' @author Joshua Wiley
#' @keywords interface
#' @export
#' @examples
#'
#' mplusAvailable(silent = TRUE)
#' mplusAvailable(silent = FALSE)
mplusAvailable <- function(silent = TRUE) {
    os <- .Platform$OS.type
    if (identical(os, "windows")) {
        res <- system2("where", args = "mplus.exe",
                       stdout = FALSE, stderr = FALSE)
        note <- paste0(
            "Mplus is either not installed or could not be found\n",
            "Try installing Mplus or if Mplus is already installed, \n",
            "make sure it can be found on your PATH, try\n\n",
            "Windows 10 and Windows 8:\n",
            " (1) In Search, search for and then select: System (Control Panel)\n",
            " (2) Click the Advanced system settings link.\n",
            " (3) Click Environment Variables ...\n",
            " (4) In the Edit System Variable (or New System Variable ) window,\n",
            " (5) specify the value of the PATH environment variable...\n",
            " (6) Close and reopen R and run:\n\n",
            "mplusAvailable(silent=FALSE)",
            "\n")
    }
    if (identical(os, "unix")) {
        res <- system2("type", args = "mplus",
                       stdout = FALSE, stderr = FALSE)
        note <-     paste0(
            "Mplus is either not installed or could not be found\n",
            "Try installing Mplus or if it already is installed,\n",
            "making sure it can be found by adding it to your PATH or adding a symlink\n\n",
            "To see directories on your PATH, From a terminal, run:\n\n",
            "echo $PATH",
            "\n\nthen try something along these lines:\n\n",
            "sudo ln -s /path/to/mplus/on/your/system /directory/on/your/PATH",
            "\n")
    }

    if (!silent) message(c("Mplus is installed and can be found", note)[res+1])
    return(invisible(res))
}
