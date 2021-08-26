  removeAndLOCF <- function(TimeSeries,remove0=F,removeNeg=F,removeInf=T,forward=T,backward=T,setNeg0=F,smooth=F,spar=1000){
    if (remove0) {
      TimeSeries[TimeSeries==0] <- NA
    }
    if (removeNeg) {
      TimeSeries[TimeSeries<0] <- NA
    }
    if (removeInf) {
      TimeSeries[is.infinite(TimeSeries)] <- NA
    }
    if (forward) {
      TimeSeries <- na.locf(TimeSeries,na.rm = FALSE)
    }
    if (backward) {
      TimeSeries <- na.locf(TimeSeries,na.rm = FALSE, fromLast = TRUE)
    }
    if (setNeg0) {
      TimeSeries[TimeSeries<0] <- 0
    }
    if (smooth) {
      TimeSeries <- weSmooth(TimeSeries,spar = spar)
      return(TimeSeries$ysmth)
    }else{
      return(TimeSeries)
    }
  }
