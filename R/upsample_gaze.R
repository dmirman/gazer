#' Up-sample gaze and pupil data
#'
#' Increase the sampling frequency to `target_hz` Hz by inserting additional rows.
#' Missing values in gaze and pupil data will be preserved for later interpolation.
#'
#' @param x A dataframe containing gaze and pupil data with columns: `subject`, `trial`, and `time`.
#' @param pupil_cols Character vector of pupil diameter column names.
#' @param gaze_cols Character vector of gaze position column names.
#' @param target_hz Target sampling frequency (default is 1000 Hz).
#' @param upsample_pupil Logical; if `TRUE`, pupil data will also be upsampled.
#' @return A dataframe with up-sampled time points and an `up_sampled` column.
#' @export

upsample_gaze <- function(x,
                          pupil_cols = c("Pupil_Diameter"),
                          gaze_cols = c("x_pred_normalised", "y_pred_normalised"),
                          target_hz = 1000,
                          upsample_pupil = TRUE) {
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

      # Generate strictly spaced time points for upsampling
      time_seq <- tibble(subject = unique(.x$subject),
                         trial = unique(.x$trial),
                         time = seq.int(from = min_time, to = max_time, by = time_step))

      # Merge with original data, ensuring all time points are present
      .x <- full_join(.x, time_seq, by = c("subject", "trial", "time")) %>%
        arrange(subject, trial, time)

      # Forward-fill missing metadata (excluding pupil & gaze columns)
      .x <- .x %>%
        fill(-any_of(non_fill_cols), .direction = "down") %>%
        mutate(up_sampled = TRUE)

      return(.x)
    })

  # Ensure 'Subject' column is at the beginning
  upsampled_data <- upsampled_data %>%
    relocate(subject, .before = "trial") %>%
    arrange(subject, trial, time)

  return(upsampled_data)
}
