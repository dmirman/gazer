#' Assign coordinates to areas of interest
#'
#' Takes a data frame of gaze positions (or other locations), plus screen size and aoi size (or location), and computes the area of interest (AOI) for each location. Defaults assume standard four-corner design.
#'
#' @param gaze data frame containing positions
#' @param screen_size size of the screen in pixels. Defaults to c(1024, 768) and assumes reversed vertical (i.e., (0,0) is top left).
#' @param aoi_size size of AOIs in pixels. Defaults to a c(400, 300) width-height pair and assumes AOIs are in screen corners. AOIs will be coded numerically 1 to 4 in reading order (left to right, top to bottom), with 0 as center location.
#' @param aoi_loc location of rectangular AOIs. Use as alternative to aoi_size for non-corner AOIs. Each AOI location should be a separate row in a data frame that has variables xmin, xmax, ymin, and ymax. Assumes reversed vertical (i.e., (0,0) is top left). AOIs will be coded numerically in row order.
#' @param X name of variable containing X coordinates. Defaults to "CURRENT_FIX_X"
#' @param Y name of variable containing Y coordinates. Defaults to "CURRENT_FIX_Y"
#' @return Original gaze data frame with AOI column added. Non-AOI and off-screen gazes are marked NA.
#' @export
assign_aoi <- function(gaze, screen_size=c(1024, 768), aoi_size=c(400, 300), aoi_loc=NULL, X="CURRENT_FIX_X", Y="CURRENT_FIX_Y") {

  #create AOI variable in gaze data frame
  gaze$AOI <- NA

  #standard case: using aoi_size (not aoi_loc) for each corner
  if(is.null(aoi_loc)){
    #image location 1: top left
    ind1 <- (gaze[,X] < aoi_size[1]) & (gaze[,Y] < aoi_size[2])
    gaze$AOI[ind1] <- 1
    #image location 2: top right
    ind2 <- (gaze[,X] > (screen_size[1]-aoi_size[1])) & (gaze[,Y] < aoi_size[2])
    gaze$AOI[ind2] <- 2
    #image location 3: bottom left
    ind3 <- (gaze[,X] < aoi_size[1]) & (gaze[,Y] > (screen_size[2]-aoi_size[2]))
    gaze$AOI[ind3] <- 3
    #image location 4: bottom right
    ind4 <- (gaze[,X] > (screen_size[1]-aoi_size[1])) & (gaze[,Y] > (screen_size[2]-aoi_size[2]))
    gaze$AOI[ind4] <- 4
    #center location
    ind0 <- (gaze[,X] > aoi_size[1]) & (gaze[,X] < (screen_size[1]-aoi_size[1])) &
      (gaze[,Y] > aoi_size[2]) & (gaze[,Y] < (screen_size[2]-aoi_size[2]))
    gaze$AOI[ind0] <- 0 #factor value of 0 becomes 1 when converted to integer
  }
  #alternative parsing based on aoi_loc
  else {
    #loop over AOI locations
    for(i in 1:nrow(aoi_loc)){
      #find all gazes in this AOI
      ind <- (gaze[,X] > aoi_loc$xmin[i]) & (gaze[,X] < aoi_loc$xmax[i]) &
        (gaze[,Y] > aoi_loc$ymin[i]) & (gaze[,Y] < aoi_loc$ymax[i])
      #store AOI index
      gaze$AOI[ind] <- i
    }
  }

  return(gaze)
}
