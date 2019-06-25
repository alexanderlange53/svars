.onUnload <- function(libpath) {
  library.dynam.unload("svars", libpath)
  invisible(NULL)
}
