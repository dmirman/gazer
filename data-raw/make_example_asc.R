# Builds inst/extdata/PN010BWS.asc.gz, the example EyeLink .asc file used in
# the ASC vignette: the first 6 trials of participant PN010 from the
# Black-and-White Switch task in "Dynamics of the Pupillary Consensual
# Response" (public OSF project https://osf.io/7k9tr/). Binocular, 1000 Hz,
# pupil area. The original header path is replaced with the file name.
#
# Usage: Rscript data-raw/make_example_asc.R path/to/PN010BWS.asc
args <- commandArgs(TRUE)
src <- if (length(args)) args[1] else "PN010BWS.asc"
n_trials <- 6

lines <- readLines(src, warn = FALSE)
lines[1] <- sub("CONVERTED FROM .*?/([^/]+\\.edf)", "CONVERTED FROM \\1", lines[1], perl = TRUE)

# Keep everything up to and including the TRIAL_RESULT of trial `n_trials`
results <- grep("^MSG\\s+\\d+\\s+TRIAL_RESULT", lines)
keep <- lines[seq_len(results[n_trials])]

con <- gzfile("inst/extdata/PN010BWS.asc.gz", "w")
writeLines(keep, con)
close(con)
