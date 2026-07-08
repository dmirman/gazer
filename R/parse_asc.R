#' Take asc files and extract relevant data
#' Does not merge data with messages for fixation data, obtains sample report and puts time in ms
#' This code was written by Dr. Holger Mitterer
#' @param dirList list if asc files
#' @param homeDir directory of edf files
#' @param overwriteBlinks set to false
#' @param cutPreview number of ms to cut from the start of each trial (default 0)
#' @param binocular set to TRUE to return both left and right eye samples
#'   (x/y/pupil for each eye plus per-eye blink flags); FALSE (default) returns
#'   a single (monocular) eye.
#' @importFrom data.table fwrite
#' @export

parse_asc <- function(
  dirList,
  homeDir = "./",
  overwriteBlinks = FALSE,
  cutPreview = 0,
  binocular = FALSE
) {
  check.numeric <- function(x) !is.na(suppressWarnings(as.numeric(x)))

  for (myDir in dirList) {
    myPP <- myDir
    cat("\nworking on:", myDir, "\n")
    myID <- regmatches(myDir, gregexpr("[0-9]+", myDir)) %>% unlist()
    myDir <- paste0(homeDir, myDir)

    hasFile <- dir(myDir, pattern = "\\.asc$")
    if (length(hasFile) != 1) {
      cat("Warning: No asc file found in directory:", myDir, "\n")
      next
    }

    myFile <- file.path(myDir, paste0(myPP, ".asc"))
    myData <- read.table(myFile, fill = TRUE, header = FALSE)
    messages <- subset(myData, V1 == "MSG")

    # Adjust MSG timestamps: when V3 is a numeric offset, subtract it and
    # shift the message text left (same logic as original).
    thirdIsNumber <- check.numeric(messages$V3)
    for (i in seq_len(nrow(messages))) {
      temp <- messages[i, ]
      if (thirdIsNumber[i]) {
        messages[i, 2] <- as.numeric(temp$V2) - as.numeric(temp$V3)
        messages[i, 3] <- messages[i, 4]
      }
    }

    messageOverview <- sort(table(messages$V3), decreasing = TRUE)
    varDF <- subset(messages, V4 == "TRIAL_VAR")
    varOverview <- sort(table(varDF$V5))
    write.table(
      messageOverview,
      file.path(myDir, paste0("overview_", myID, ".txt")),
      row.names = FALSE
    )
    write.table(
      varOverview,
      file.path(myDir, paste0("Var_overview_", myID, ".txt")),
      row.names = FALSE
    )

    trialNumber <- messageOverview["TRIALID"]
    # Fix 1: build path directly — no gsub regex on the full path string.
    myOutFile <- file.path(myDir, paste0(myPP, "_eye.csv"))
    is_data_point <- grepl("^\\d+$", myData$V1)
    start_recordings <- as.numeric(subset(messages, V3 == "!MODE")$V2)

    if (length(start_recordings) != trialNumber) {
      stop(cat(
        "Wrong number of start of recordings!!\n",
        trialNumber,
        "trials, but",
        length(start_recordings),
        "start of recordings\n"
      ))
    }

    trial <- 0
    # Shared buffers (left eye / monocular)
    time <- x <- y <- pupil <- NULL
    # Binocular-only buffers
    xr <- yr <- pr <- bl <- br <- NULL
    in_blink_L <- in_blink_R <- FALSE

    for (i in seq_len(nrow(myData))) {
      thisRow <- myData[i, ]
      v1 <- as.character(thisRow$V1)

      # ── Trial start ─────────────────────────────────────────────────────────
      if (thisRow$V3 == "TRIALID") {
        trial <- trial + 1
        zero_time <- start_recordings[trial]
        in_blink_L <- in_blink_R <- FALSE
        cat("\ntrial", trial, "by", myPP)
        if (trial == 1) {
          header <- if (binocular) {
            "time,trial,ID,subject,x_left,y_left,pupil_left,x_right,y_right,pupil_right,blink_left,blink_right\n"
          } else {
            "time,trial,ID,subject,pupil,x,y\n"
          }
          cat(header, file = myOutFile)
        }
      }

      # ── Blink tracking (binocular only) ─────────────────────────────────────
      if (binocular) {
        if (v1 == "SBLINK" && !is.na(thisRow$V2)) {
          if (thisRow$V2 == "L") {
            in_blink_L <- TRUE
          }
          if (thisRow$V2 == "R") in_blink_R <- TRUE
        }
        if (v1 == "EBLINK" && !is.na(thisRow$V2)) {
          if (thisRow$V2 == "L") {
            in_blink_L <- FALSE
          }
          if (thisRow$V2 == "R") in_blink_R <- FALSE
        }
      }

      # ── Trial end: flush buffers ─────────────────────────────────────────────
      if (thisRow$V3 == "TRIAL_RESULT") {
        x <- suppressWarnings(as.numeric(x))
        x[is.na(x)] <- 1e8
        y <- suppressWarnings(as.numeric(y))
        y[is.na(y)] <- 1e8
        pupil <- as.numeric(pupil)

        if (binocular) {
          xr_n <- suppressWarnings(as.numeric(xr))
          xr_n[is.na(xr_n)] <- 1e8
          yr_n <- suppressWarnings(as.numeric(yr))
          yr_n[is.na(yr_n)] <- 1e8
          pr_n <- as.numeric(pr)

          if (overwriteBlinks) {
            # Left eye — identical logic to original monocular overwrite
            nObs <- length(x)
            if (x[1] > 10000) {
              s_i <- 2
              while ((x[s_i] > 10000) & (s_i < nObs)) {
                s_i <- s_i + 1
              }
              for (s_j in s_i:1) {
                x[s_j + 1] <- x[s_j + 1]
                y[s_j + 1] <- y[s_j + 1]
                pupil[s_j + 1] <- pupil[s_j + 1]
              }
            }
            for (s_i in 2:nObs) {
              if (x[s_i] > 10000) {
                x[s_i] <- x[s_i - 1]
                y[s_i] <- y[s_i - 1]
                pupil[s_i] <- pupil[s_i - 1]
              }
            }
            # Right eye — same logic applied independently
            if (xr_n[1] > 10000) {
              s_i <- 2
              while ((xr_n[s_i] > 10000) & (s_i < nObs)) {
                s_i <- s_i + 1
              }
              for (s_j in s_i:1) {
                xr_n[s_j + 1] <- xr_n[s_j + 1]
                yr_n[s_j + 1] <- yr_n[s_j + 1]
                pr_n[s_j + 1] <- pr_n[s_j + 1]
              }
            }
            for (s_i in 2:nObs) {
              if (xr_n[s_i] > 10000) {
                xr_n[s_i] <- xr_n[s_i - 1]
                yr_n[s_i] <- yr_n[s_i - 1]
                pr_n[s_i] <- pr_n[s_i - 1]
              }
            }
          }

          thisTrial <- data.frame(
            time = time,
            trial = trial,
            ID = myID,
            subject = myPP,
            x_left = x,
            y_left = y,
            pupil_left = pupil,
            x_right = xr_n,
            y_right = yr_n,
            pupil_right = pr_n,
            blink_left = as.integer(bl),
            blink_right = as.integer(br)
          )
        } else {
          # ── Original monocular path — unchanged ──────────────────────────────
          if (overwriteBlinks) {
            nObs <- length(x)
            if (x[1] > 10000) {
              s_i <- 2
              while ((x[s_i] > 10000) & (s_i < nObs)) {
                s_i <- s_i + 1
              }
              for (s_j in s_i:1) {
                x[s_j + 1] <- x[s_j + 1]
                y[s_j + 1] <- y[s_j + 1]
                pupil[s_j + 1] <- pupil[s_j + 1]
              }
            }
            for (s_i in 2:nObs) {
              if (x[s_i] > 10000) {
                x[s_i] <- x[s_i - 1]
                y[s_i] <- y[s_i - 1]
                pupil[s_i] <- pupil[s_i - 1]
              }
            }
            thisTrial <- data.frame(
              time,
              trial,
              ID = myID,
              subject = myPP,
              pupil,
              x,
              y
            )
          } else {
            thisTrial <- data.frame(
              time,
              trial,
              ID = myID,
              subject = myPP,
              pupil,
              x,
              y
            )
          }
        }

        if (cutPreview > 0) {
          thisTrial <- subset(thisTrial, time > cutPreview)
        }
        fwrite(
          thisTrial,
          file = myOutFile,
          sep = ",",
          append = TRUE,
          row.names = FALSE,
          col.names = FALSE
        )

        time <- x <- y <- pupil <- NULL
        xr <- yr <- pr <- bl <- br <- NULL
      }

      # ── Accumulate sample ────────────────────────────────────────────────────
      if (is_data_point[i]) {
        time <- c(time, as.numeric(thisRow$V1) - zero_time)
        x <- c(x, thisRow$V2)
        y <- c(y, thisRow$V3)
        pupil <- c(pupil, thisRow$V4)
        if (binocular) {
          xr <- c(xr, as.character(thisRow$V5))
          yr <- c(yr, as.character(thisRow$V6))
          pr <- c(pr, as.character(thisRow$V7))
          bl <- c(bl, in_blink_L)
          br <- c(br, in_blink_R)
        }
      }
    }
  }
}
