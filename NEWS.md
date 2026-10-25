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
