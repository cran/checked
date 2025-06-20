#' @import cli
NULL

base_pkgs <- function() {
  c("R", utils::installed.packages(priority = "base")[, "Package"])
}

.callr <- as.list(getNamespace("callr"), all.names = TRUE)[c(
  "default_load_hook"
)]

.tools <- as.list(getNamespace("tools"), all.names = TRUE)[c(
  ".split_dependencies"
)]

replace_with_map <- function(x, value, replacement) {
  m <- match(x, value)
  x[which(!is.na(m))] <- replacement[m[!is.na(m)]]
  x
}

is_package_installed <- function(pkg, lib.loc) {  # nolint object_name_linter
  path <- find.package(pkg, lib.loc = lib.loc, quiet = TRUE)
  length(path) > 0
}

hash_alias <- function(x) {
  paste0(c("hash", as.character(charToRaw(x))), collapse = "")
}

dir_create <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
}

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

drlapply <- function(...) {
  do.call(rbind, lapply(...))
}

drmapply <- function(...) {
  do.call(rbind, mapply(..., USE.NAMES = FALSE, SIMPLIFY = FALSE))
}

uulist <- function(...) unique(as.character(unlist(...)))
vcapply <- function(...) vapply(..., FUN.VALUE = character(1L))
vlapply <- function(...) vapply(..., FUN.VALUE = logical(1L))
viapply <- function(...) vapply(..., FUN.VALUE = integer(1L))
vnapply <- function(...) vapply(..., FUN.VALUE = numeric(1L))
