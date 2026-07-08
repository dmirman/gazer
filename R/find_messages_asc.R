#' Take asc files and return important events
#' Collect trial variables for your experiment.
#' This code was written by Dr. Holger Mitterer
#' @param dirList dir of edf files
#' @param homeDir dir of files
#' @param time2extract  a list of strings that match message texts for which only the time should be retained.
#' @param msg2extract A list of messages to extract fully based on the first word of that message output will be string variable
#' @param vars2extract A list of Trial variables used by Experiment Builder that should be extracted value only, with column names set by the variable name
#' @param nOfTrials number of trials to extract; -1 for all
#' @param binocular set to TRUE when the recording is binocular. Message
#'   extraction is eye-independent, so this only keeps the interface symmetric
#'   with \code{\link{parse_asc}}; the extracted output is identical either way.
#' @importFrom data.table fwrite
#' @importFrom tidyr pivot_longer any_of
#' @export

find_messages_asc <- function(
  dirList,
  homeDir = "./",
  time2extract = NULL,
  msg2extract = NULL,
  vars2extract = NULL,
  nOfTrials = -1,
  binocular = FALSE
) {
  check.numeric <- function(x) !is.na(suppressWarnings(as.numeric(x)))

  for (myDir in dirList) {
    myPP <- myDir
    myDir1 <- paste0(homeDir, myDir)
    myID <- regmatches(myDir, gregexpr("[0-9]+", myDir)) %>% unlist()

    hasFile <- dir(myDir1, pattern = "\\.asc$")
    if (length(hasFile) != 1) {
      cat("Warning: No asc file found in directory:", myDir, "\n")
      cat("Full path tried:", myDir1, "\n")
      next
    }

    myFile <- file.path(myDir1, paste0(myPP, ".asc"))
    myData <- read.table(myFile, fill = TRUE, header = FALSE)
    messages <- subset(myData, V1 == "MSG")

    trialStarts <- grep("TRIALID", messages$V3)
    trialEnds <- grep("TRIAL_RESULT", messages$V3)

    if (nOfTrials < 0) {
      nOfTrials <- length(trialStarts)
    } else if (nOfTrials != length(trialStarts)) {
      stop(paste(
        "Problem with asc file:",
        myFile,
        "\nfound",
        length(trialStarts),
        "TRIAL_ID for",
        nOfTrials,
        "trials\n"
      ))
    }
    if (length(trialEnds) != length(trialStarts)) {
      stop(paste(
        "Problem with asc file:",
        myFile,
        "\nfound",
        length(trialStarts),
        "TRIAL_ID for",
        nOfTrials,
        "trials\n"
      ))
    }

    allVars <- c(msg2extract, time2extract, vars2extract, "TRIAL_RESULT")
    msg.df <- data.frame(matrix(ncol = length(allVars), nrow = nOfTrials))
    colnames(msg.df) <- allVars

    # Adjust MSG timestamps (same logic as parse_asc)
    thirdIsNumber <- check.numeric(messages$V3)
    for (i in seq_len(nrow(messages))) {
      temp <- messages[i, ]
      if (thirdIsNumber[i]) {
        messages[i, 2] <- as.numeric(temp$V2) - as.numeric(temp$V3)
        messages[i, 3] <- messages[i, 4]
      }
    }

    # Fix 1: build path directly
    myOutFile <- file.path(myDir1, paste0(myPP, "_messages.csv"))
    start_recordings <- as.numeric(subset(messages, V3 == "!MODE")$V2)

    if (length(start_recordings) != nOfTrials) {
      stop(cat(
        "Wrong number of start of recordings!!\n",
        nOfTrials,
        "trials, but",
        length(start_recordings),
        "start of recordings\n"
      ))
    }

    for (t in seq_len(nOfTrials)) {
      zero_time <- start_recordings[t]
      thisTrial <- messages[trialStarts[t]:trialEnds[t], ]
      thisVars <- subset(thisTrial, V4 == "TRIAL_VAR")

      trialResultLine <- subset(thisTrial, V3 == "TRIAL_RESULT")
      msg.df[t, "TRIAL_RESULT"] <- trialResultLine$V4

      # msg2extract: keep full message text prefixed by its timestamp
      for (v in seq_along(msg2extract)) {
        toSearch <- msg2extract[v]
        thisMSG <- subset(thisTrial, V3 == toSearch)
        if (nrow(thisMSG) > 0) {
          # Fix 2: guard
          msgTime <- as.numeric(thisMSG$V2) - zero_time
          restMessage <- paste(thisMSG[1, 4:ncol(thisMSG)], collapse = " ")
          restMessage <- gsub(" NA", "", restMessage)
          while (grepl("  ", restMessage)) {
            restMessage <- gsub("  ", " ", restMessage)
          }
          msg.df[t, toSearch] <- paste(msgTime, restMessage)
        }
      }

      # time2extract: keep only the normalised timestamp
      for (v in seq_along(time2extract)) {
        toSearch <- time2extract[v]
        thisMSG <- subset(thisTrial, V3 == toSearch)
        if (nrow(thisMSG) > 0) {
          # Fix 2: guard
          msg.df[t, toSearch] <- as.numeric(thisMSG$V2) - zero_time
        }
      }

      # vars2extract: Experiment Builder TRIAL_VAR values
      for (v in seq_along(vars2extract)) {
        toSearch <- vars2extract[v]
        thisMSG <- subset(thisVars, V5 == toSearch)
        if (nrow(thisMSG) > 0) {
          # Fix 2: guard
          restMessage <- paste(thisMSG[1, 6:ncol(thisMSG)], collapse = " ")
          restMessage <- gsub(" NA", "", restMessage)
          while (grepl("  ", restMessage)) {
            restMessage <- gsub("  ", " ", restMessage)
          }
          msg.df[t, toSearch] <- restMessage
        }
      }
    }

    # Append trial index column and reorder to front (same as original)
    msg.df$trial <- seq_len(nOfTrials)
    lastCol <- ncol(msg.df)
    msg.df <- msg.df[, c(lastCol, seq_len(lastCol - 1))]

    # Pivot time2extract columns to long format: one row per message event,
    # with a single 'message' column (the marker name) and 'time' column.
    # vars2extract and msg2extract columns are repeated for every message row
    # of that trial so the file stays self-contained.
    if (!is.null(time2extract)) {
      msg.df <- msg.df %>%
        pivot_longer(
          cols = any_of(time2extract),
          names_to = "message",
          values_to = "time"
        ) %>%
        filter(!is.na(time)) %>%
        arrange(trial, time) %>%
        select(trial, message, time, everything())
    }

    fwrite(msg.df, myOutFile, row.names = FALSE)
    cat("\nprocessed messages from", myDir, "\n")
  }
}
