#' Read EyeDataViewer fixation report
#'
#' Reads in a fixation report created by EyeLink's EyeDataViewer application
#'
#' @param filename string containing full filename of fixation report
#' @param screen_size size of the screen in pixels. Used to compute track quality diagnostics. Defaults to c(1024, 768)
#' @param name_changes set of variable name pairs for improving variable names. Should be formatted as named character vector with new names as values and old names as names; see plyr::rename()
#' @param plot_fix_scatter logical indicating whether a scatterplot of fixations should be created
#' @param pages outputs a plots 4 x 3 and displays the first page. Change number to see other pages if you have more Ps 
#' @export
#' @return gaze data frame containing fixation report
read_fixation_report <- function(filename, screen_size=c(1024, 768),
                               name_changes=c(Subject = "RECORDING_SESSION_LABEL",
                                              ACC = "StimSlide.ACC", RT = "StimSlide.RT",
                                              TargetLoc = "CorrectPort"),
                               plot_fix_scatter=TRUE, pages=1) {
  gaze <- read.table(filename, header=TRUE,sep="\t", na.strings=".")
  #simplify some other column names
  gaze <- dplyr::rename(gaze, !!!name_changes)
  gaze$Subject <- as.factor(gaze$Subject)

  #convert CorrectPort into TargetLocation by taking just the integer at the end of the line
  gaze$TargetLocation <- as.factor(substr(gaze$TargetLoc, 6, 6))

  if(plot_fix_scatter){
    # get calibration diagnostics
    calibGoodness <- get_gaze_diagnostics(gaze)
    g3 <- merge(gaze, calibGoodness)
    #make a strip text label containing subject ID and calibration diagnostics
    g3$sText <- with(g3, paste("ID:", Subject,": NonFix=", signif(nonFixTime, 2),
                               ", OOB=", signif(oob_prop, 2), sep=""))
    #make the scatterplots
    nSubj <- n_distinct(g3$Subject) #number of subjects
    nc <- 4     #4 columns
    nr <- ceiling(nSubj/nc) #number of rows

    sp <- ggplot(g3, aes(CURRENT_FIX_X, CURRENT_FIX_Y,
                         size=sqrt(CURRENT_FIX_DURATION))) +
    facet_wrap(~ sText, scales="free", ncol=nc) + 
      geom_point(alpha=0.5) +
      
      annotate("rect", xmin=0, ymin=0,
               xmax=screen_size[1], ymax=screen_size[2], color="red", fill=NA) + 
      theme_bw()+ 
      theme(axis.title.y=element_text(size = 14, face="bold"), axis.title.x = element_text(size=14, face="bold"), axis.text.x=element_text(size = 12, face="bold"), axis.text.y=element_text(size=12, face="bold"), legend.position = "bottom")
    print(sp)
    ggsave(paste0("fixation_scatterplots_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"), sp, width = nc*3, height = nr*3) 
    #3
    #show them
  }
  return(gaze)
}
