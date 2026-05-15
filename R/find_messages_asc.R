#' Take asc files and return important events
#' Collect trial variables for your experiment.
#' #' This code was written by Dr. Holger Mitterer
#'@param dirList dir of edf files
#'@param homeDir dir of files
#'@param time2extract  a list of strings that match message texts for which only the time should be retained.
#'@param msg2extract A list of messages to extract fully based on the first word of that message output will be string variable
#'@param vars2extract A list of Trial variables used by Experiment Builder that should be extracted value only, with column names set by the variable name
#'@param nOfTrials number of trials to extract; -1 for all
#'@export


find_messages_asc <- function(dirList, homeDir = "./",
                              time2extract = NULL,
                              msg2extract = NULL,
                              vars2extract = NULL,
                              nOfTrials = -1
)
{
  for (myDir in dirList)
  {
    myPP= myDir
    myDir1 = paste0(homeDir,myDir)
    myID =regmatches(myDir,gregexpr('[0-9]+',myDir)) %>% unlist()

    hasFile = dir(myDir1, pattern = "asc")
    if (length(hasFile) != 1)
    {
      cat("Warning: No asc file found in directory: ", myDir, "\n")
      cat("Full path tried: ", myDir1, "\n")

    }else
    {
      #read in file ----
      myFile = paste0(myDir1, "/", myPP, ".asc")
      myData = read.table(myFile, fill= T, header = F)
      messages = subset(myData, V1 == "MSG")

      #parse by trial
      trialStarts    = grep("TRIALID", messages$V3)
      trialEnds      = grep("TRIAL_RESULT", messages$V3)




      #check presence of all critical messages ----
      if (nOfTrials < 0)
      {     nOfTrials = length(trialStarts)   } else
      {
        if (nOfTrials != length(trialStarts))
        {
          errMSG = paste("Problem with asc file:", myFile, "\nfound", length(trialStarts), "TRIAL_ID\n",
                         "for", nOfTrials, "trials\n")
          stop(errMSG)
        }
      }

      if (length(trialEnds) != length(trialStarts))
      {
        errMSG = paste("Problem with asc file:", myFile, "\nfound", length(trialStarts), "TRIAL_ID\n",
                       "for", nOfTrials, "trials\n")
        stop(errMSG)
      }

      #prepare data frame----
      allVars = c(msg2extract, time2extract, vars2extract, "TRIAL_RESULT")
      msg.df =  data.frame(matrix(ncol = length(allVars), nrow = nOfTrials))
      colnames(msg.df) = allVars


      #correct messages with a time offset ----
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


      #start making message data file ----
      #header is time	trial	ID	subject	pupil	x	y
      myOutFile = gsub(".asc","_messages.csv",myFile)

      #initialize variables



      #get the start recording times of all trials
      start_recordings_DF = subset(messages, V3 == "!MODE")
      start_recordings = as.numeric(start_recordings_DF$V2)
      if (length(start_recordings) != nOfTrials)
      {
        err_message = cat("Wrong number of start of recordings!!\n", trialNumber, " trials,
                      but\n", length(start_recordings), "start of recordings\n")
        stop(err_message)
      }
      trial = 0
      for (t in 1:nOfTrials)
      {
        zero_time = start_recordings[t]
        thisTrial = messages[ trialStarts[t]:trialEnds[t],  ]
        thisVars  = subset(thisTrial, V4 == "TRIAL_VAR")
        trialResultLine = subset(thisTrial, V3 == "TRIAL_RESULT")
        msg.df[t, "TRIAL_RESULT"] = trialResultLine$V4

        #full messsages to extract ----
        for (v in 1:length(msg2extract))
        {
          toSearch = msg2extract[v]
          thisMSG = subset(thisTrial, V3 == toSearch)
          msgTime = as.numeric(thisMSG$V2) - zero_time
          restMessage = paste(thisMSG[1,4:ncol(thisMSG)], collapse = " ")
          restMessage = gsub(" NA","",restMessage)
          while(grepl("  ", restMessage))
          {
            restMessage = gsub("  ", " ", restMessage)
          }
          msg.df[t,msg2extract[v]] = paste(msgTime, restMessage)
        }
        for (v in 1:length(time2extract))
        {
          toSearch = time2extract[v]
          thisMSG = subset(thisTrial, V3 == toSearch)
          msgTime = as.numeric(thisMSG$V2) - zero_time
          msg.df[t,time2extract[v]] = msgTime
        }
        for (v in 1:length(vars2extract))
        {
          toSearch = vars2extract[v]
          thisMSG = subset(thisVars, V5 == toSearch)
          restMessage = paste(thisMSG[1,6:ncol(thisMSG)], collapse = " ")
          restMessage = gsub(" NA","",restMessage)
          while(grepl("  ", restMessage))
          {
            restMessage = gsub("  ", " ", restMessage)
          }
          msg.df[t,vars2extract[v]] = restMessage
        }
      }
      msg.df$trial = 1:nOfTrials
      lastCol = ncol(msg.df)
      msg.df2 = msg.df[  , c(lastCol, 1:(lastCol -1))]
      fwrite(msg.df2, myOutFile, row.names = F)
      cat("\nprocessed messaged from ", myDir, "\n")
    }
  }
}
