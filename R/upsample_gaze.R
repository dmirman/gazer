#' Up-sample gaze and pupil data
#'
#' Increase the sampling frequency to `target_hz` Hz by inserting additional rows.
#' Missing values in gaze and pupil data will be preserved for later interpolation.
#'
#' Original rows are returned unchanged. Each inserted row takes its other
#' columns (e.g. condition, trial variables) from the original sample before
#' it, except `pupil_cols`/`gaze_cols` (left `NA` for interpolation) and
#' `event_cols` (left `NA`, so each event message still appears once).
#'
#' @param x A dataframe containing gaze and pupil data with columns: `subject`, `trial`, and `time`.
#' @param pupil_cols Character vector of pupil diameter column names.
#' @param gaze_cols Character vector of gaze position column names.
#' @param target_hz Target sampling frequency (default is 1000 Hz).
#' @param upsample_pupil Logical; if `TRUE`, pupil data will also be upsampled.
#' @param event_cols Character vector of event/message columns that should stay
#'   empty on inserted rows (default `"message"`). Columns not present are ignored.
#' @return A dataframe with up-sampled time points and an `up_sampled` column
#'   (`TRUE` for inserted rows, `FALSE` for original ones).
#' @importFrom purrr map_dfr
#' @export

upsample_gaze <- function(x,
                          pupil_cols = c("Pupil_Diameter"),
                          gaze_cols = c("x_pred_normalised", "y_pred_normalised"),
                          target_hz = 1000,
                          upsample_pupil = TRUE,
                          event_cols = c("message")) {
  # Ensure time is an integer
  x <- x %>% mutate(time = as.integer(time))

  # Compute the correct time step (1ms for 1000Hz, 2ms for 500Hz, etc.)
  time_step <- round(1000 / target_hz)

  # Determine which columns should NOT be forward-filled
  non_fill_cols <- if (upsample_pupil) c(pupil_cols, gaze_cols) else gaze_cols

  # Process each Subject & Trial separately and upsample
  upsampled_data <- x %>%
    group_by(subject, trial) %>%  # Ensure processing per subject-trial
    group_split() %>%  # Split the data into subject-trial groups
    map_dfr(~ {
      min_time <- min(.x$time, na.rm = TRUE)
      max_time <- max(.x$time, na.rm = TRUE)
      orig_times <- .x$time

      # Generate strictly spaced time points for upsampling
      time_seq <- tibble(subject = unique(.x$subject),
                         trial = unique(.x$trial),
                         time = seq.int(from = min_time, to = max_time, by = time_step))

      # Merge with original data, ensuring all time points are present
      .x <- full_join(.x, time_seq, by = c("subject", "trial", "time")) %>%
        arrange(subject, trial, time)

      # Inserted rows copy metadata from the previous original row; original
      # rows are left as they are (filling every NA down would also spread
      # event messages and other sparse values across the trial)
      inserted <- !(.x$time %in% orig_times)
      last_orig <- cummax(ifelse(inserted, 0L, seq_len(nrow(.x))))
      take <- inserted & last_orig > 0
      copy_cols <- setdiff(names(.x), c(non_fill_cols, event_cols, "subject", "trial", "time"))
      for (col in copy_cols) .x[[col]][take] <- .x[[col]][last_orig[take]]
      .x$up_sampled <- inserted

      return(.x)
    })

  # Ensure 'Subject' column is at the beginning
  upsampled_data <- upsampled_data %>%
    relocate(subject, .before = "trial") %>%
    arrange(subject, trial, time)

  return(upsampled_data)
}
