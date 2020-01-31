
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mountr

<!-- badges: start -->

<!-- badges: end -->

mountr is a barebones package that allows you to mount remote folders
using `sshfs`.

## Installation

`sshfs` only works on unix platforms and requires `sshfs`

You can install the developement version of `mountr` from GitHub with:

``` r
remotes::install_github("eliocamp/mountr")
```
## How to use it

To mount a remote fonder locally, you have to have set the ssh keys previously. It is recommended to save the mount_point in a variable. 

``` r
mount <- mountr::sshfs_mount("my_remote_user", "remote_server", "path_to_remote_folder", "path_to_local_folder")
mount$mount()
```

This function also has a `permission` argument to change permissions from **read only mode** to **read and write mode**. Use this very carefully.

To mount a remote folder through a tunneling server you have to create the tunnel first and past it to the mount function.

``` r
tunnel <- mountr::tunnel_open("my_remote_user", "tunnel_server", "remote_server")

mount <- mountr::sshfs_mount("my_remote_user", tunnel, "path_to_remote_folder", "path_to_local_folder")
mount$mount()
```

To unmount the remote folder use: 

``` r
mount$unmount()
```
