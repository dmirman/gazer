#' Take EDF files and return message events to extract behav data
#' creates pupil column that is ambigious to whether you sampled from left eye, right eye, or both (takes the average)
#'@import dplyr
#'
#
# Collect trial variables for your experiment. 

#' @param file_list directory to edf files
#' @param output.dir directory to save new cleaned files
#' @export
#' 
#' 
#' 
#'
parse_edf_messages <- function (file_list, output.dir) { # make sure you file
  
  library(edfR)#use edfR to read in the edf files
  
  subs <- length(file_list)
  for (sub in 1:subs) {
    subject = basename(file_list[sub]) # get id from file
    event_messages = edf.messages.c(file_list[sub]) # get trial ID messages
    event_messages$subject <- rep(subject, length(event_messages$msg)) # add id so it is easier to extract events per subject

    subOutData <- file.path(output_dir, paste(file_list[sub], "_messages.csv", sep="")) # save file
    write.table(event_messages, file = subOutData, append = FALSE, sep = ",",
                row.names = FALSE, col.names = TRUE)
  }
}


