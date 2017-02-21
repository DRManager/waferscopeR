#' Wafer statistics: mean, 3s and range non-uniformity (NU)
#'
#' Takes a dataframe from wafer list and returns wafer statistics, including mean, 3 sigma and range nonuniformity
#' @param wafer dataframe from the wafer list (output of read.kla_f5)
#' @return Wafer statistics: mean, 3-sigma and range NU
#' @export
#'
#' @importFrom stats sd loess predict

wafer_stat <- function (wafer){

  wafer.raw <- data.frame(x = round(wafer[,"X"], 1), y = round(wafer[,"Y"], 1), z = round(wafer[,"ThK"], 1))

  #wafer statistics
  wafer.mean <- round(mean(wafer.raw$z, na.rm = T),1)
  wafer.3sigma <- round(3*sd(wafer.raw$z, na.rm = T),1)
  percent_3sigma <- paste(round(100*wafer.3sigma/wafer.mean, 1), "%", sep="")
  wafer.range <- round(max(wafer.raw$z, na.rm = T)-min(wafer.raw$z, na.rm = T),1)
  percent_range <- paste(round(100*wafer.range/wafer.mean, 1), "%", sep="")
  wafer.stat <- paste("Mean: ", wafer.mean, 'A ', '3-sigma: ', wafer.3sigma, 'A (',
                      percent_3sigma,') ', 'Range: ', wafer.range, 'A (', percent_range, ')', sep = '')

  #radial component
  wafer.raw$r <- round(sqrt(wafer.raw[,1]^2+wafer.raw[,2]^2),1)
  r.unique <- unique(wafer.raw$r)
  wafer.radial <- data.frame()

  for (i in 1:length(r.unique)){

    wafer.samer <- subset(wafer.raw, wafer.raw$r == r.unique[i])

    wafer.samer$zmean <- round(mean(wafer.samer$z),1)

    wafer.radial <- rbind(wafer.radial, wafer.samer)

  }

  #wafer stat - radial
  radial.3sigma <- round(3*sd(wafer.radial$zmean, na.rm = T),1)
  radial.percent_3sigma <- paste(round(100*radial.3sigma/wafer.mean, 1), "%", sep="")
  radial.range <- round(max(wafer.radial$zmean, na.rm = T)-min(wafer.radial$zmean, na.rm = T),1)
  radial.percent_range <- paste(round(100*radial.range/wafer.mean, 1), "%", sep="")
  radial.stat <- paste('3-sigma: ', radial.3sigma, 'A (',
                       radial.percent_3sigma,') ', 'Range: ', radial.range, 'A (', radial.percent_range, ')', sep = '')

  #wafer stat - residual
  resi <- wafer.radial
  resi$z <- wafer.radial$z - wafer.radial$zmean

  resi.3sigma <- round(3*sd(resi$z, na.rm = T),1)
  resi.range <- round(max(resi$z, na.rm = T)-min(resi$z, na.rm = T),1)
  resi.stat <- paste('3-sigma: ', resi.3sigma, 'A ', 'Range: ', resi.range, 'A ', sep = '')


wafer.summary <- list(mean.stat=wafer.stat, radial.stat=radial.stat, resi.stat=resi.stat)

return(wafer.summary)

}

