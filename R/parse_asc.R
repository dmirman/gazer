#' Take asc files and extract relevant data
#' Does not merge data with messages for fixation data, obtains sample report and puts time in ms
#' This code was written by Dr. Holger Mitterer
#' @param dirList list if asc files
#' @param homeDir directory of edf files
#' @param overwriteBlinks set to false
#' @param cutPreview number of ms to cut from the start of each trial (default 0)
#' @export

parse_asc <- function(dirList, homeDir = "./", overwriteBlinks = FALSE, cutPreview = 0) {
  for (myDir in dirList)
  {
    myPP = myDir
    cat("\nworking on:", myDir, "\n")
    #extract numbers out of participant ID
    myID =regmatches(myDir,gregexpr('[0-9]+',myDir)) %>% unlist()
    myDir = paste0(homeDir, myDir)
    hasFile = dir(myDir, pattern = ".asc")
    if (length(hasFile) != 1)
    {
      cat("Warning: No asc file found in directory: ", myDir, "\n")

    }else
    {
      #read in file ----
      myFile = paste0(myDir, "/", myPP, ".asc")
      myData = read.table(myFile, fill= T, header = F)
      messages = subset(myData, V1 == "MSG")


      #correct messages with a time offset ----

      #need a warningless function that checks whether it can be converted to a number
      check.numeric <- function(x)
      {
        x_num = suppressWarnings(as.numeric(x))
        x_isNum = !is.na(x_num)
      }

      thirdIsNumber = check.numeric(messages$V3)
      for (i in 1:nrow(messages))
      {
        temp = messages[i,]
        if (thirdIsNumber[i])
        {
          messages[i,2] = as.numeric(temp$V2) - as.numeric(temp$V3)
          messages[i,3] = messages[i,4]
        }
      }

      #generate an overview of the messsages and trial variables present ----
      messageOverview = sort(table(messages$V3), decreasing = T)
      varDF = subset(messages, V4 == "TRIAL_VAR")
      varOverview = sort(table(varDF$V5))
      write.table(messageOverview, paste0(myDir, "/", "overview_", myID,".txt"), row.names = F)
      write.table(varOverview, paste0(myDir, "/", "Var_overview_", myID,".txt"), row.names = F)

      trialNumber = messageOverview["TRIALID"]

      #start making VWP data file ----
      #header is time	trial	ID	subject	pupil	x	y
      myOutFile = gsub(".asc","_vwp.csv",myFile)

      #initialize variables
      time = NULL
      x = NULL
      y = NULL
      time = NULL
      pupil = NULL

      #is the first variable a number?
      is_data_point = grepl("^\\d+$",myData$V1)

      #get the start recording times of all trials
      start_recordings_DF = subset(messages, V3 == "!MODE")
      start_recordings = as.numeric(start_recordings_DF$V2)
      if (length(start_recordings) != trialNumber)
      {
        err_message = cat("Wrong number of start of recordings!!\n", trialNumber, " trials,
                      but ", length(start_recordings), "start of recordings\n")
        stop(err_message)
      }
      trial = 0
      #go through the whole file line byline
      for (i in 1:nrow(myData))
      {
        thisRow = myData[i, ]
        if (thisRow$V3 == "TRIALID")
        {
          #if TRIALID, start a new trial
          trial = trial + 1
          zero_time = start_recordings[trial]
          cat("\ntrial", trial, "by", myPP)
          #add header for first trial
          if (trial == 1){
            cat("time,trial,ID,subject,pupil,x,y\n", file = myOutFile)
          }
        }
        if (thisRow$V3 == "TRIAL_RESULT")
        {
          #wrap up trial if it is trial result
          x = suppressWarnings(as.numeric(x))
          y = suppressWarnings(as.numeric(y))
          x[is.na(x)] = 1e+08
          y[is.na(y)] = 1e+08
          #replace missing values by 10.000.000 as in the edf converted data
          pupil = as.numeric(pupil)
          #overwrite blinks option ----
          if (overwriteBlinks){
            nObs = length(x)
            #make sure trial starts with a valid data point
            if (x[1] > 10000){
              s_i = 2
              while ((x[s_i] > 10000) & (s_i < nObs))
              { s_i = s_i + 1 }
              for (s_j in s_i:1)
              {
                x[s_j+1] = x[s_j+1]
                y[s_j+1] = y[s_j+1]
                pupil[s_j+1] = pupil[s_j+1]
              }
            }
            #now correct forward
            for (s_i in 2:nObs)
            {
              if (x[s_i] > 10000)
              {
                x[s_i] = x[s_i-1]
                y[s_i] = y[s_i-1]
                pupil[s_i] = pupil[s_i-1]
              }
            }
            thisTrial  = data.frame(time, trial, ID = myID, subject = myPP, pupil, x, y)
          }else #end overwrite blinks option
          {
            thisTrial  = data.frame(time, trial, ID = myID, subject = myPP, pupil, x, y)
          }
          if (cutPreview > 0)
          { thisTrial = subset(thisTrial, time > cutPreview) }
          fwrite(thisTrial, file= myOutFile, sep=",", append=TRUE , row.names=FALSE, col.names=FALSE)
          time = NULL
          x = NULL
          y = NULL
          time = NULL
          pupil = NULL
        }
        if (is_data_point[i])
        {
          time = c(time,as.numeric(thisRow$V1) - zero_time)
          x = c(x, thisRow$V2)
          y = c(y, thisRow$V3)
          pupil = c(pupil,thisRow$V4)
        }
      }#end of loop through lines of asc file
    }
  }#end loop for directories

}
