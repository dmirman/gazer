# edfR is not a dependency of gazer (it is not on CRAN and has stopped
# working on many systems). The legacy .edf readers look it up only when they
# are called; convert .edf files to .asc and use read_asc() instead.
edf_fun <- function(name) {
  pkg <- "edfR"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Reading .edf files needs the edfR package, which is not installed. ",
         "Convert .edf files to .asc with SR Research's edf2asc and use read_asc() instead.",
         call. = FALSE)
  }
  getExportedValue(pkg, name)
}
