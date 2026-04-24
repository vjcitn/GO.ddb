
# R/zzz.R

.cache <- NULL

.onLoad <- function(libname, pkgname) {
  .cache <<- new.env(parent = emptyenv())
}

.onUnload <- function(libpath) {
  cache <- tryCatch(
    get(".cache", envir = asNamespace("GO.ddb"), inherits = FALSE),
    error = function(e) NULL
  )

  if (!is.null(cache)) {
    if (exists("con", envir = cache, inherits = FALSE)) {
      con <- get("con", envir = cache, inherits = FALSE)
      if (DBI::dbIsValid(con))
        try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
    }
    rm(list = ls(cache, all.names = TRUE), envir = cache)
  }
}
