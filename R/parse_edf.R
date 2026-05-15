#' Take EDF files and return data that is in format usable for gazeR
#' for pupil merges the sample data with the message data (merges on nearest values (millisecond diff between SR and this function)
#' puts time in ms
#' adds subject variable column
#' cleans up the column names
#' creates pupil column that is ambigious as to whether you sampled from left eye, right eye, or both (takes the average)
#' For fixation data, obtains sample report and puts time in ms
#'@import dplyr
#'@importFrom data.table as.data.table
#' @param file_list directory to edf files
#' @param output.dir directory to save new cleaned files
#' @param type include whether you want to parse edf pupil data (pupil) or vwp (fixations)
#' @export

parse_edf <- function (file_list, output_dir, type="pupil") {

  library(edfR)#use edfR to read in the edf files
  remotes::install_github("tmalsburg/saccades/saccades")
  library(saccades)
  library(data.table)

  if (type=="pupil") {
    
    subs <- length(file_list)

    for (sub in 1:subs) {

      subject = basename(file_list[sub])
      all=edf.trials(file_list[sub], samples=TRUE)
      #samps_all <- edf.batch(file_list[sub], samples = TRUE, do.plot=FALSE)

      msg<-all$messages

      msg<- msg %>%
        dplyr::rename(trial="eyetrial", time="sttime") %>%
        distinct(time, .keep_all = TRUE)


      samp <- all$samples

      samp <- samp %>%
        rowwise() %>%
        dplyr::mutate(pup=mean(c(paL,paR),na.rm=TRUE), x=mean(c(gxL,gxR),na.rm=TRUE), y=mean(c(gyL, gyR),na.rm=TRUE)) %>%
        dplyr::rename(trial="eyetrial") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(x=ifelse(is.na(x),  1e+08, x), y=ifelse(is.na(y),  1e+08, y))

      blinks_merge <- blink_detect(samp)

      blinks <- blinks_merge %>%
        dplyr::group_by(grp = cumsum(!is.na(startend))) %>%
        dplyr::mutate(Label = replace(startend, first(startend) == 'start', 'start')) %>% #extends the start message forward until end message
        dplyr::ungroup() %>%
        # label blinks as 1
        dplyr::select(trial, time, x, y, pup, Label, -grp)
      # use rolling merge from DT

      blinks <-as.data.table(blinks)
      msg<-as.data.table(msg)
      DT_mesg <- msg[blinks, on=c("trial", "time"), roll=5]

      #SR edfs are a nightmare. This makes it so messages are alined with closest values
      get_msg <- DT_mesg %>%
        group_by(trial, message) %>%
        top_n(n=1, wt=desc(time))

      dat_samp_msg <- dplyr::full_join(blinks, get_msg, by=c("time", "trial", "x", "y", "pup", "Label"))

      dat_samp_msg1 <- dat_samp_msg %>%
        dplyr::mutate(blink=ifelse(!is.na(Label), 1, 0), pupil=ifelse(blink==1 | pup==0, NA, pup))%>%
        dplyr::filter(trial!="NA") %>% # get rid non-trial values
        dplyr::group_by(trial) %>%
        dplyr::mutate(time=time-time[1], subject=subject)%>%
        dplyr::ungroup()%>%
        dplyr::select(subject, time, trial, pupil, x, y, trial, blink, message, -Label)

      dat_samp_msg2 <- dat_samp_msg1 %>%
        group_by(subject) %>%
        distinct(trial, time, .keep_all=TRUE) %>%
        ungroup()


      subOutData <- file.path(output_dir, paste(file_list[sub], "_raw_pupil.csv", sep="")) # save file
      write.table(dat_samp_msg2, file = subOutData, append = FALSE, sep = ",",
                  row.names = FALSE, col.names = TRUE)
    }
  }

  if (type=="vwp") {

    subs <- length(file_list)

    for (sub in 1:subs) {

      subject = basename(file_list[sub])
      #samps_all <- edf.trials(file_list[sub], samples=TRUE)
      samps_all <- edf.batch(file_list[sub], samples = TRUE, do.plot=FALSE)

      samp <- samps_all[[1]][["samples"]]


      df_samp = samp %>% group_by(eyetrial) %>%
        dplyr::mutate(time=time-time[1], subject=subject) %>%
        dplyr::rowwise()%>%
        dplyr::mutate(pupil=mean(c(paL,paR),na.rm=TRUE), x=mean(c(gxL,gxR),na.rm=TRUE), y=mean(c(gyL, gyR),na.rm=TRUE)) %>%
        dplyr::select(-blink, -fixation, -saccade, -gxL, -gyL, -gxR, -gyR, -paR, -paL) %>%
        ungroup() %>%
        dplyr::mutate(x=ifelse(is.na(x), 1e+08, x), y=ifelse(is.na(y), 1e+08, y)) %>%
        #for some experiments there are NAs in the x, y coordinates which throws off blink detection
        dplyr::rename(trial="eyetrial")

      subOutData <- file.path(output_dir, paste(file_list[sub], "_raw_vwp.csv", sep="")) # save file
      write.table(df_samp, file = subOutData, append = FALSE, sep = ",",
                  row.names = FALSE, col.names = TRUE)

    }

  }
}


