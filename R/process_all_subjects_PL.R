#' Process all Pupil Labs subjects in a root directory
#'
#' @description
#' Recursively finds immediate subfolders, runs \code{\link{read_pl_subject}} on each,
#' writes per-subject CSVs and a combined CSV, and invisibly returns the combined tibble.
#'
#' @param root_dir Directory containing one folder per subject.
#' @param output_dir Directory for outputs. Defaults to \code{file.path(root_dir, "processed")}.
#' @param ... Extra arguments forwarded to \code{\link{read_pl_subject}} (e.g., start_mode, start_messages).
#'
#' @return Invisibly returns a combined tibble of all processed subjects.
#' @export
#'
#' @examples
#' \dontrun{
#' process_all_subjects("/path/Timeseries",
#'                      start_mode = "exact",
#'                      start_messages = c("trial-started-light", "start-dark"))
#' }
#'
#' @import dplyr
#' @importFrom readr write_csv
process_all_subjects_PL <- function(root_dir,
                                 output_dir = file.path(root_dir, "processed"),
                                 ...) {
  stopifnot(dir.exists(root_dir))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  subdirs <- list.dirs(root_dir, full.names = TRUE, recursive = FALSE)
  subdirs <- subdirs[subdirs != root_dir]

  message("Found ", length(subdirs), " subject folder(s). Processing...")

  all_list <- list()
  for (sdir in subdirs) {
    sid <- basename(sdir)
    out_file <- file.path(output_dir, paste0(sid, "_processed.csv"))
    message("Processing: ", sid)

    df <- try(parse_pl(sdir, sid, ...), silent = TRUE)
    if (inherits(df, "try-error")) {
      warning("Skipping ", sid)
      next
    }

    readr::write_csv(df, out_file, na = "")
    message("  \u2714 Saved: ", out_file)
    all_list[[sid]] <- df
  }

  all_combined <- dplyr::bind_rows(all_list)
  combined_file <- file.path(output_dir, "all_subjects_processed.csv")
  readr::write_csv(all_combined, combined_file, na = "")
  message("\u2714 Combined file saved: ", combined_file)

  invisible(all_combined)
}
