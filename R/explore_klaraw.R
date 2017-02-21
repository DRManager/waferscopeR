#' Explore wafer profile in each slot in input files and output boxplot or summary
#'
#' A quick exploration of wafer profile can be done with KLA raw data files using this function. It outputs a boxplot or a summary of the input file.
#' @param file KLA raw data file of wafer profile measurement
#' @param output specify output type: '\code{boxplot}': show a boxplot, '\code{summary}': show a summary
#' @return A boxplot or a summary of the wafer profiles in the input file
#' @export

explore_klaraw <-function (file, output="boxplot") {

  ox.run1 <- read_kla_f5(file)

  #Combine for box plot
  ox.run1.all <- do.call("rbind", ox.run1)
  ox.run1.all$id <- rep(names(ox.run1), sapply(ox.run1, nrow))
  colnames (ox.run1.all) <- c("ThK","GOF","X","Y","slot")

  if (output=="boxplot") {
    # order the slots
    ox.run1.all$slot <-factor(ox.run1.all$slot, levels=ox.run1.all[order(as.numeric(gsub("slot", "",ox.run1.all$slot)), decreasing = T),"slot"])

    #boxplot
    ggplot(ox.run1.all,aes(x=slot, y=ThK, color=slot)) + geom_boxplot() + ylab("Oxide thickness (A)")
  } else if (output=="summary") {

    ox.run1.all.summary <- dplyr::group_by(ox.run1.all,slot)
    Thk.summary <-dplyr::summarise(ox.run1.all.summary,mean = mean(ThK), perc_3sigma = 3*sd(ThK)/mean*100,perc_range=(max(ThK)-min(ThK))/mean(ThK)*100)

    #round the decimals to 2

    head(Thk.summary[,c("slot","mean","perc_3sigma","perc_range")])

  }

}
