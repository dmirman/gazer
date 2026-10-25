#' Take pupil file and detect blinks
#' merge blinks with sample report 

#' Requires the saccades package (`remotes::install_github("tmalsburg/saccades/saccades")`).
#' @param samp pupil data file
#' @return  merged data file with blinks detected
#' @export
#' 
blink_detect <- function (samp) {

  if (!requireNamespace("saccades", quietly = TRUE)) {
    stop("blink_detect() needs the saccades package. Install it with ",
         "remotes::install_github(\"tmalsburg/saccades/saccades\"), or use ",
         "detect_blinks_noise() or the blink column from read_asc(), which don't need it.",
         call. = FALSE)
  }

  blk <- saccades::detect.fixations(samp) # get blinks using the saccades algo from saccades package

  blk1 <- blk %>%
  dplyr::filter(event=="blink") %>%
  tidyr::gather(data=., key="startend", value="time", start:end) # gather all the blinks

  blk_merge <- merge(samp, blk1, by=c("trial", "time", "x", "y"), all=TRUE)
  
  return(blk_merge)
}
  
  
  