#' Take asc files and extract relevant data
#' Does not merge data with messages for fixation data, obtains sample report and puts time in ms
#' This code was written by Dr. Holger Mitterer
#' @param dirList list if asc files
#' @param homeDir directory of edf files
#' @export


merge_asc_files <- function(dirList, homeDir = "./")
{
  library(data.table)
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
      combined = merge(eyeData, msgData, by = "trial")
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


