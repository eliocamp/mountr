
#' Checks system dependencies
#'
#'
#'
#' @export
check_system <- function() {
  lapply(c("sshfs", "ssh"), .check_system)
}

# nocov
.check_system <- function(package) {
  out <- suppressWarnings(system(paste0("which ", package), intern = TRUE))

  if (length(out) == 0) {
    stop("System pacakge ", package," not installed.")
  }
}

.sys_type <- function() {
  return(Sys.info()["sysname"])
}

