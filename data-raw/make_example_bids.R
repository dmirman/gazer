# Builds inst/extdata/bids_eyetrack/, the example BIDS eye-tracking dataset
# (BEP020) used in the BIDS vignette, from inst/extdata/PN010BWS.asc.gz (see
# make_example_asc.R): 6 trials of participant PN010, binocular, 1000 Hz.
# Each eye is one recording (recording-eye1 = left, recording-eye2 = right):
#   *_physio.tsv.gz        samples, no header (columns in the JSON sidecar)
#   *_physioevents.tsv.gz  messages and blinks (onset in tracker ms)
#
# Usage (from the package root): Rscript data-raw/make_example_bids.R
asc <- "inst/extdata/PN010BWS.asc.gz"
root <- "inst/extdata/bids_eyetrack"
sub_label <- "PN010"; task <- "bws"
out <- file.path(root, paste0("sub-", sub_label), "eye")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

jsonlite::write_json(list(Name = "gazeR example: Black-and-White Switch task (PN010, 6 trials)",
                          BIDSVersion = "1.10.0", DatasetType = "raw",
                          License = "CC-BY-4.0",
                          ReferencesAndLinks = list("https://osf.io/7k9tr/")),
                     file.path(root, "dataset_description.json"), auto_unbox = TRUE, pretty = TRUE)

d <- gazer::read_asc(asc, eye = "both", keep_messages = TRUE)
msgs <- attr(d, "messages")
eb <- grep("^EBLINK", readLines(asc), value = TRUE)
write_tsv_gz <- function(x, file) {
  con <- gzfile(file, "w")
  utils::write.table(x, con, sep = "\t", row.names = FALSE, col.names = FALSE, na = "n/a", quote = FALSE)
  close(con)
}
for (k in 1:2) {
  side <- c("L", "R")[k]; nm <- c("left", "right")[k]
  stem <- file.path(out, sprintf("sub-%s_task-%s_recording-eye%d", sub_label, task, k))
  phys <- data.frame(timestamp = d$tracker_time, x_coordinate = d[[paste0("x_", nm)]],
                     y_coordinate = d[[paste0("y_", nm)]], pupil_size = d[[paste0("pupil_", nm)]])
  write_tsv_gz(phys, paste0(stem, "_physio.tsv.gz"))
  jsonlite::write_json(list(SamplingFrequency = 1000, StartTime = 0, Columns = names(phys),
                            PhysioType = "eyetrack", RecordedEye = nm,
                            SampleCoordinateSystem = "gaze-on-screen", Manufacturer = "SR-Research",
                            EnvironmentCoordinates = "top-left",
                            timestamp = list(Description = "Tracker timestamp", Units = "ms"),
                            x_coordinate = list(Units = "pixel"), y_coordinate = list(Units = "pixel"),
                            pupil_size = list(Description = "Pupil area", Units = "arbitrary")),
                       paste0(stem, "_physio.json"), auto_unbox = TRUE, pretty = TRUE)
  b <- eb[startsWith(eb, paste0("EBLINK ", side, " "))]
  p <- strsplit(trimws(substring(b, 10)), "[[:space:]]+")
  st <- as.numeric(vapply(p, `[`, "", 1)); en <- as.numeric(vapply(p, `[`, "", 2))
  bl <- data.frame(onset = st, duration = (en - st) / 1000, trial_type = "blink", message = NA)
  mm <- data.frame(onset = msgs$tracker_time, duration = NA, trial_type = NA, message = msgs$text)
  ev <- rbind(mm, bl)
  write_tsv_gz(ev[order(ev$onset), ], paste0(stem, "_physioevents.tsv.gz"))
  jsonlite::write_json(list(Columns = names(ev), OnsetSource = "timestamp", PhysioType = "eyetrack",
                            onset = list(Units = "ms"), duration = list(Units = "s")),
                       paste0(stem, "_physioevents.json"), auto_unbox = TRUE, pretty = TRUE)
}
