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
