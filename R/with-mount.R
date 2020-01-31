#' Evaluate expressions with mounted directory
#'
#' SSHFS can be a bit finicky and fail when reading lots of files. It can be safer, then,
#' to mount and unmount the folder each time you read each file.
#'
#' @param data a mount object returned by [sshfs_mount()]
#' @param expr any R expression thet will be evaluated.
#' @param ... ignored
#'
#' @details
#' The function mounts the remote folder, evaluates the expresion and then unmounts.
#' The `expr` can access the locally-mounted folder with the `.folder` variable.
#'
#' @examples
#' \dontrun{
#' mount <- sshfs_mount("user", "remote.server.com", "~/data_folder", "~/loca_folder")
#' bici <- with(mount, read.csv(file.path(.folder, "some_file.csv")))
#' }
#' @export
with.sshfs_mount_point <- function(data, expr, ...) {
  on.exit(
    data$unmount()
  )

  data$mount()
  x <- eval(substitute(expr), envir = list(.folder = data$.folder))
  return(x)
}


