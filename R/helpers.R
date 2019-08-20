
.check_system <- function(package) {
  out <- suppressWarnings(system(paste0("which ", package), intern = TRUE))

  if (length(out) == 0) {
    stop("Pacakge ", package," not installed.")
  }
}
