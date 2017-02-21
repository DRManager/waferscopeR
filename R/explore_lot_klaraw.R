#' Explore pre and post measurement of each slot in input files
#'
#' A quick exploration of wafer maps can be done with KLA raw data files using this function. It compares the pre and post measurement of each slot in the file.
#' @param pre KLA raw data file of pre-process measurement
#' @param post KLA raw data file of post-process measurement
#' @return the pre, post and delta thickness and a table with uniformity statistics
#' @export

explore_lot_klaraw <-function (pre, post) {

  pre.wafer <-read_kla_f5(pre)
  #combine for box plot
  pre.wafer.all <- do.call("rbind", pre.wafer)
  #strip of slot and add number
  pre.wafer.all$id <- rep(gsub("slot", "", names(pre.wafer)), sapply(pre.wafer, nrow))
  colnames (pre.wafer.all) <- c("ThK","GOF","X","Y","slot")

  #generate a lot summary
  pre.wafer.summary <- dplyr::group_by(pre.wafer.all,slot)
  dplyr::summarise(pre.wafer.summary,mean = mean(ThK), sigma3 = 3*sd(ThK))
  dplyr::mutate(pre.wafer.summary,split="pre")

  pre.plot<- ggplot(pre.wafer.all,aes(x=slot,y=ThK)) + geom_boxplot() +ylab("Thickness(A)") +ggtitle("Pre measurement")

  post.wafer <-read_kla_f5(post)
  #combine for box plot
  post.wafer.all <- do.call("rbind", post.wafer)
  post.wafer.all$id <- rep(gsub("slot", "", names(post.wafer)), sapply(post.wafer, nrow))
  colnames (post.wafer.all) <- c("ThK","GOF","X","Y","slot")

  #generate a lot summary
  post.wafer.summary <- dplyr::group_by(post.wafer.all,slot)
  dplyr::summarise(post.wafer.summary,mean = mean(ThK), sigma3 = 3*sd(ThK))
  dplyr::mutate(post.wafer.summary,split="pre")

  post.plot<- ggplot(post.wafer.all,aes(x=slot,y=ThK)) + geom_boxplot() + ylab("Thickness(A)") + ggtitle("Post Measurement")

#delta
  delta.wafer.all <- dplyr::left_join(pre.wafer.all, post.wafer.all, by=c("X"="X","Y"="Y","slot"="slot"))

  delta.wafer.all$ThK <- delta.wafer.all$ThK.y - delta.wafer.all$ThK.x
#delta map plots
  delta.plot<- ggplot(delta.wafer.all,aes(x=slot,y=ThK)) + geom_boxplot() +ylab("Thickness(A)") +ggtitle("Delta measurement")
#delta summary
  delta.wafer.summary <- dplyr::group_by(delta.wafer.all,slot)
  delta.table <-dplyr::summarise(delta.wafer.summary,mean = mean(ThK), perc_3sigma = 3*sd(ThK)/mean*100,perc_range=(max(ThK)-min(ThK))/mean(ThK)*100)

  #round the decimals to 2
  delta.table <- format(round(delta.table[,2:4], 2), nsmall = 2)

  table <-gridExtra::tableGrob(head(delta.table[,c("mean","perc_3sigma","perc_range")]))

    #combine summary for ggplot
#  lots.summary <- rbind(pre.wafer.summary,post.wafer.summary)

# plot.summary <-ggplot(lots.summary, aes(x=slot, y=mean, fill=split)) +
#    geom_bar(stat = "identity",position="dodge") + ylab("Film ThK(A)")+ ggtitle("Summary")

  #Arrange the graphic outputs
  #easyGgplot2::ggplot2.multiplot(pre.plot, post.plot, delta.plot,table, col=2)
  gridExtra::grid.arrange(pre.plot, post.plot, delta.plot,table, ncol=2)
}
