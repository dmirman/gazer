#' Take asc files and extract relevant data
#' Merges eye data (from parse_asc) with message data (from find_messages_asc)
#' This code was written by Dr. Holger Mitterer
#' @param dirList list if asc files
#' @param homeDir directory of edf files
#' @param dataType type of eye data being merged. "fixation" (default) merges
#'   eye and message data by trial only, matching every message row for a
#'   trial to every eye row for that trial. "sample" merges by trial and time
#'   so that per-sample eye rows line up with message rows at matching
#'   timestamps (requires \code{_messages.csv} to have a \code{time} column,
#'   i.e. \code{find_messages_asc} was called with \code{time2extract}).
#' @export


merge_asc_files <- function(dirList, homeDir = "./", dataType = c("fixation", "sample"))
{
  library(data.table)
  dataType <- match.arg(dataType)
  mergeBy <- if (dataType == "sample") c("trial", "time") else "trial"
  merged = 0
  notMerged  = 0
  for (myDir in dirList)
  {
    myEyeFile = paste0(homeDir, myDir, "/", myDir, "_eye.csv")
    myMsgFile = paste0(homeDir, myDir, "/", myDir, "_messages.csv")
    eyeOK = file.exists(myEyeFile)
    msgOK = file.exists(myMsgFile)
    if (eyeOK & msgOK){
      cat("\n merging files for", myDir)
      eyeData = fread(myEyeFile)
      msgData = fread(myMsgFile)
      missingCols = setdiff(mergeBy, intersect(names(eyeData), names(msgData)))
      if (length(missingCols) > 0) {
        stop("Cannot merge by ", paste(mergeBy, collapse = ", "), " for ", myDir,
             ": missing column(s) ", paste(missingCols, collapse = ", "),
             " in eye or message data.")
      }
      combined = merge(eyeData, msgData, by = mergeBy)
      myOutFile = gsub("_eye.csv","_combined.csv",myEyeFile)
      data.table::fwrite(combined, myOutFile)
      merged = merged + 1
    }else{
      notMerged = notMerged + 1
      eyeFileMsg = ifelse(eyeOK, "eye data found\n", paste("eye data missing\n", myEyeFile, "does not exist.\n"))
      msgFileMsg = ifelse(msgOK, "MSG data found\n", paste("MSG data missing\n", myMsgFile, "does not exist.\n"))
      cat("\n!!!!!!!!problem with: ", myDir, "\n", eyeFileMsg, msgFileMsg)
    }
  }
  cat("\n##########################\nMerging Report:\n", merged, "merged\n",notMerged, "with missing files\n")
}


