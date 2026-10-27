#' Compare groups on a set of variables.
#' 
#' For each numeric variable in a data frame, computes number of observations, mean, and standard deviation, for each group, and t- and p-values for the t-test comparing the groups.
#'  
#' @param dat data frame
#' @param Group string name of grouping variable
#' @return Returns a data frame with rows for each numeric variable in the input data frame (plus N, for number of observations) and columns for mean, SD, t-value, and p-value.
#' @examples
#' \dontrun{
#' compare_groups(SND, "SemNear_Cond")
#' }
compare_groups <- function(dat, Group){
  tval <- pval <- num <- NA
  for (i in 1:ncol(dat)){
    num[i] <- is.numeric(dat[,i]) #figure out which columns are numeric
    #run t-tests for them
    if(is.numeric(dat[,i])){
      x <- t.test(dat[,i] ~ dat[,Group], data=dat)
      tval[i] <- x$statistic
      pval[i] <- format.pval(x$p.value, digits=3, eps=0.0001)
    }     
  }
  tt <- data.frame(t=tval[num], p=pval[num])
  #get means and SD
  M <- aggregate(dat[, num], list(dat[,Group]), mean, na.rm=T)
  Mx <- melt(M, id="Group.1")
  Mg <- dcast(Mx, variable ~ Group.1)
  SD <- aggregate(dat[, num], list(dat[,Group]), sd, na.rm=T)
  SDx <- melt(SD, id="Group.1")
  g <- merge(Mg, dcast(SDx, variable ~ Group.1), by="variable", sort=F, suffixes = c(".M",".SD"))
  
  ##get group sizes
  GroupSize <- data.frame("N", t(aggregate(dat[, Group], list(dat[, Group]), length)[,2]), NA, NA)
  colnames(GroupSize) <- colnames(g)
  
  #combine for output
  GroupCompare <- cbind(rbind(GroupSize, g), rbind(c(NA, NA), tt))
  return(GroupCompare)
}
#In the future: expand to multi-group comparisons, possibly include a test-type input variable
