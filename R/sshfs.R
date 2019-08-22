#' Opens up a ssh tunnel
#'
#' @param user username
#' @param tunnel_server tunnel server
#' @param remote_server remote server
#' @param local_port local port to use for tunneling
#' @param remote_port remote port used for ssh
#' @param tunnel tunnel object returned by [tunnel_open]
#'
#' @export
#' @rdname tunnel
tunnel_open <- function(user, tunnel_server, remote_server, local_port = 4567, remote_port = 22) {
  tunnel$new(user = user, tunnel_server = tunnel_server, remote_server = remote_server,
             local_port = local_port, remote_port = remote_port)
}

#' @export
#' @rdname tunnel
tunnel_close <- function(tunnel) {
  tunnel$finalize()
}

tunnel <- R6::R6Class("sshfs_tunnel", list(
  initialize = function(user, tunnel_server, remote_server, local_port = 4567, remote_port = 22) {
    ok <-  FALSE
    on.exit(
      # Kill the process if something went wrong
      if (!ok) {
        process$kill()
      }
    )

    command <- paste0("ssh ", user, "@", tunnel_server, " -L ", local_port, ":", remote_server, ":", remote_port," -N")
    process <- callr::r_bg(function(command) system(command), args = list(command = command))

    self$user <- user
    self$tunnel_server <- tunnel_server
    self$remote_server <- remote_server
    self$remote_port <- remote_port
    self$local_port <- local_port
    self$process <- process

    ok <-  TRUE
    return(invisible(self))
  },

  finalize = function() {
    self$process$kill()
  },

  user = NA,
  tunnel_server = NA,
  remote_server = NA,
  remote_port = 22,
  local_port = 4567,
  process = NA
))


#' Mounts and unmounts a remote folder
#'
#' @param user user
#' @param remote_server remote server or tunnel object returned by [tunnel_open]
#' @param remote_folder server's folder to be mounted
#' @param local_folder local folder where to mount
#' @param mount_point mount_point object returned from [sshfs_mount]
#' @param permission either "r" or "rw"
#' @param extra_args character string of extra arguments passed to `sshfh`.
#'
#' @examples
#' \dontrun{
#' mount <- sshfs_mount(user = "myself",
#'                      remote_server = "server.domain",
#'                      remote_folder = "~/data_folder",
#'                      local_folder = "./DATA)
#'
#' # Now you can access your files with
#' list.files(mount$folder)
#'
#' # Unmount when done
#' sshfs_unmount(mount)
#'
#' # If you need to tunnel over another server, first create the
#' # tunnel and then pass the object as a `remote_server`
#' tunnel <- tunnel_open(user = "myself",
#'                       tunnel_server = "portal.domain",
#'                       remote_server = "server.domain")
#'
#' mount <- sshfs_mount(user = "myself",
#'                      remote_server = tunnel,
#'                      remote_folder = "~/data_folder",
#'                      local_folder = "./DATA)
#'
#' # When done, unmount and close the tunnel.
#' sshfs_unmount(mount)
#' tunnel_close(tunnel)
#'
#' }
#'
#' @export
#' @rdname sshfs
sshfs_mount <- function(user, remote_server, remote_folder, local_folder, permission = c("r", "rw"),
                        extra_args= "") {
  mount <- sshfs_mount_point$new(user = user,
                                 remote_server = remote_server,
                                 remote_folder = remote_folder,
                                 local_folder = local_folder,
                                 permission = permission,
                                 extra_args = extra_args)
  return(mount)
}

#' @rdname sshfs
#' @export
sshfs_unmount <- function(mount_point) {
  mount_point$finalize()
}


sshfs_mount_point <- R6::R6Class("sshfh_mount_point", list(
  initialize  = function(user, remote_server, remote_folder, local_folder, permission = "r",
                         extra_args = "") {

    os <- .sys_type()

    unmount_command <- switch (os,
                               "Darwin" = "umount",
                               "Windows" = stop("mountr only works in unix platforms."),
                               "fusermount -u"
    )

    .check_system("sshfs")
    .check_system("ssh")


    if (dir.exists(local_folder)) {
      if (is.empty(local_folder)) {
        stop(local_folder, " already exists and is not empty.")
      }
    } else {
      dir.create(local_folder, showWarnings = FALSE, recursive = TRUE)
    }

    if (inherits(remote_server, "sshfs_tunnel")) {
      local_port <- remote_server$local_port
      remote_server <- "localhost"
      port_spec <- paste0("-p ", local_port)
    } else {
      port_spec <- ""
    }
    command <- paste0("sshfs ", port_spec, " ", user, "@", remote_server, ":", remote_folder, " ", local_folder)

    if (permission[1] == "r") {
      command <- paste0(command, " -o default_permissions ")
    }

    command <- paste0(command, " ", extra_args)

    system(command, intern = TRUE)

    self$folder <- local_folder



    self$unmount_command <- paste0(unmount_command, " ", local_folder)
    return(invisible(self))
  },

  finalize = function() {
    if (file.exists(self$folder)) {
      out <- system(self$unmount_command, intern = TRUE, ignore.stdout = TRUE)
      if (is.null(attr(out, "status"))) {
        if (is.empty(self$folder)) {
          file.remove(self$folder)
        }
      } else {
        stop("Unmounting operation failed")
      }
    }
    return(invisible(self))
  },

  show = function() {
    command <- paste0("xdg-open ", self$folder)
    callr::r_bg(function(command) system(command), args = list(command = command))

    return(invisible(self))
  },

  folder = NA,
  unmount_command = NA
))



is.empty <- function(folder) {
  if (file.exists(folder)) {
    files <- list.files(path = local_folder, all.files = TRUE)
    files <- files[!(files %in% c(".", ".."))]
  } else {
    stop(folder, " does not exists.")
  }

  length(files) == 0
}
