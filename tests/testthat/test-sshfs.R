context("sshfs")

environ <- readRenviron("test_environ.Renviron")

if (!environ) {
  stop("Can't access test_environment")
}

user <- Sys.getenv("user")
server = Sys.getenv("server")
local_folder = Sys.getenv("local_folder")
remote_folder = Sys.getenv("remote_folder")


test_that("sshfs_mount creates mountopints", {
  mount <- sshfs_mount(user, server, remote_folder, local_folder)
  expect_true(inherits(mount, "sshfs_mount_point"))

  mount$mount()
  expect_true(mount$mounted)
  expect_true(file.exists(mount$.folder))

  mount$mount()
  expect_true(mount$mounted)
  expect_true(file.exists(mount$.folder))

  expect_warning(file.create(file.path(mount$.folder, "something")), "Permission denied")

  expect_s3_class(read.csv(file.path(mount$.folder, "estaciones.csv")), "data.frame")

  mount$unmount()
  expect_true(!mount$mounted)
  expect_true(!file.exists(mount$.folder))

  mount$mount()
  sshfs_unmount(mount)
  expect_true(!mount$mounted)
  expect_true(!file.exists(mount$.folder))

  dir.create(mount$.folder)
  expect_error(mount$mount())
  unlink(mount$.folder, recursive = TRUE)

  mount <- sshfs_mount(user, server, remote_folder, local_folder, permission = "w")
  mount$mount()
  expect_true(file.create(file.path(mount$.folder, "test_file")))
  expect_true(file.remove(file.path(mount$.folder, "test_file")))
  mount$unmount()
})


test_that("with works", {
  mount <- sshfs_mount(user, server, remote_folder, local_folder)
  mount$unmount()
  expect_true(!file.exists(mount$.folder))

  expect_s3_class(with(mount, read.csv(file.path(.folder, "estaciones.csv"))), "data.frame")

  expect_true(!mount$mounted)
  expect_true(!file.exists(mount$.folder))
})

context("tunnel")
