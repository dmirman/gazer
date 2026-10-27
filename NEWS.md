# gazer 0.2.11

* New `read_bids_eyetrack()`: reads BIDS eye-tracking data (BEP020
  `*_physio.tsv.gz` recordings with their JSON sidecars and
  `*_physioevents.tsv.gz` events) into the same layout as `read_asc()`.
  Participants come from the `sub`/`ses`/`task`/`run` entities; left- and
  right-eye recordings are merged by timestamp (`eye = "average"`, `"left"`,
  `"right"` or `"both"`); event messages are attached to samples and blink
  events become `blink`. Each run is one trial unless `trial_start` names
  the messages that start a trial.
  Timestamps in s, ms, us or ns are converted to ms, so BIDS conversions
  from EyeLink, Tobii and Pupil Labs all work; the sidecars' sampling rate,
  manufacturer and pupil units are returned in `attr(x, "bids_info")`.
* New vignette "Reading BIDS eye-tracking data with gazeR", with an example
  BIDS dataset in `inst/extdata/bids_eyetrack` (built by
  `data-raw/make_example_bids.R` from `PN010BWS.asc.gz`).
* jsonlite is now imported.

# gazer 0.2.10

* New `resample_gaze()`: brings each participant's data to a target sampling
  rate. Higher rates are downsampled by averaging consecutive samples (a bin
  with any missing sample stays missing; blink flags and every event message
  are kept). Lower rates are upsampled with `upsample_gaze()`, with new
  samples interpolated from the recorded samples either side. At the same
  rate, trials are put on a uniform grid with dropped samples inserted as
  missing.

* `baseline_correction_pupil_msg()` and `baseline_correction_pupil()` gain
  `baseline_stat = c("median", "mean")`, choosing whether the baseline is
  the median (default, unchanged) or the mean of the baseline period. Mathôt
  & Vilotijević (2022) recommend a per-trial subtractive baseline using the
  mean of a short baseline period.

# gazer 0.2.9

* README: updated install instructions (r-universe) and a summary of the
  October 2026 changes and bug fixes.

* `smooth_interpolate_pupil()`: smoothing (moving average or Hanning) is now
  done **within each trial**. Before, it ran over the whole data frame, so
  the first and last samples of each trial were blended with the
  neighbouring trial (or the next participant): about n/2 samples at each
  edge for the moving average and 5 for the 11-point Hanning window.
  Interpolation was already per trial. Results change slightly near trial
  edges, which can affect baselines measured at the start of a trial.
* `smooth_interpolate_pupil()`: four combinations that always failed now
  work (cubic interpolation with `extendblinks = FALSE`), and two linear
  interpolate-first combinations no longer pipe their result into
  `return()`. `na.locf()` fills no longer drop leading values, which could
  make columns the wrong length.
* `hanning_filter()` no longer errors on an all-missing vector.

# gazer 0.2.8

* lme4 is now optional (Suggests instead of Imports). It is only used by the
  model helpers `get_ranef()` and `get_pvalues(method = "KR")`, which now
  call `lme4::ranef()`/`lme4::fixef()` explicitly (previously they only
  worked if lme4 was attached) and say how to install it if missing.
* `get_ranef()` also failed unless tidyr was attached (`separate()` wasn't
  namespaced); fixed.

# gazer 0.2.7

* saccades is now optional (Suggests instead of Imports), so gazeR installs
  with CRAN packages only. It is used only by `blink_detect()`, which now
  checks for it and points to `detect_blinks_noise()` or `read_asc()`'s
  blink column as alternatives. `parse_edf()` no longer installs or loads
  saccades (it called `remotes::install_github()` on every run but never
  used it).

# gazer 0.2.6

* `upsample_gaze()` fixes:
  - It failed unless purrr was attached (`map_dfr()` wasn't imported).
  - It filled *every* missing value downward in all non-pupil/gaze
    columns, including the original rows, so a sparse `message` column was
    copied onto every following sample (one participant's 84 event messages
    became 162,497). Original rows are now returned unchanged. Inserted rows
    copy metadata from the previous original sample, but event columns
    (new `event_cols` argument, default `"message"`) stay empty.
  - `up_sampled` is now `TRUE` only for inserted rows (it was `TRUE` for all
    rows).

* The ASC vignette has a new section on `read_asc()` (reading files, trial
  variables and messages, binocular options, several participants, sampling
  rate checks) and a note on the `parse_asc()` calibration-line issue that
  produced negative and duplicated times. The vignette now builds: chunks
  that need data not shipped with gazeR are no longer run.
* New example file `inst/extdata/PN010BWS.asc.gz`: the first 6 trials of one
  participant from the public OSF project "Dynamics of the Pupillary
  Consensual Response" (https://osf.io/7k9tr/), binocular at 1000 Hz. See
  `data-raw/make_example_asc.R`.

# gazer 0.2.5

## New features

* `read_asc()` reads an EyeLink `.asc` file into gazeR's long format (one
  row per sample) in a few vectorised passes, about 40× faster than
  `parse_asc()` on a 1000 Hz binocular recording, with identical samples.
  Unlike `parse_asc()` and `find_messages_asc()`, it takes any file path (no
  folder-per-subject layout), writes nothing to disk, and returns:
  - event messages attached to the first sample at or after them, with
    Experiment Builder offsets applied;
  - one column per Experiment Builder `TRIAL_VAR`;
  - blink flags from the tracker's `SBLINK`/`EBLINK` events;
  - `NA` for missing pupil and gaze (instead of `0` and `1e8`);
  - the left eye, right eye, their average, or both (`eye`);
  - the sampling rate, recorded eyes and pupil type in
    `attr(x, "asc_info")`.

## Bug fixes

* `smooth_interpolate_pupil()` with `extendblinks = TRUE`, `type = "cubic"`,
  `step.first = "interp"` and `filter = "Hann"` applied the Hanning filter to
  the un-interpolated signal (`extendpupil`) instead of the cubic-interpolated
  one. The cubic interpolation was discarded: gaps were filled linearly by
  `hanning_filter()`, and the samples around each blink were distorted.
  Results from this combination will change. Other combinations are
  unaffected.

* `lowpass_filter()` produced large edge artifacts. `signal::filtfilt()`
  pads with zeros, so both ends of the filtered signal were pulled towards 0
  (e.g. a trace around 1000 started near 500 and ended near 60). The signal
  is now mean-centred and padded by odd reflection before filtering, then
  trimmed and restored. Filtered values will change, mostly at the start and
  end of each trial, so re-run analyses that used `lowpass_filter()`.

## Other

* The shiny gazeR app (<https://github.com/jgeller112/shiny-gazeR>) provides
  a point-and-click interface to gazeR's pupil pre-processing pipeline.
