#' Compare wafer maps
#'
#' Takes wafer inputs in X,Y,z format and compare contour map, boxplot and radial profile
#' @param wafer1 thickness profile of wafer 1
#' @param wafer2 thickness profile of wafer 2
#' @param output '\code{compare}': show countour of each wafer, display boxplot and radial plots,  '\code{delta}': show countour of each wafer, display delta map and radial plots,
#' @return Plots for wafer map comparison
#' @export
#'
#' @import ggplot2
#' @import dplyr


# output options: 1. comapare to show countour, boxplot and radial plots; 2. delta map to show three maps
compare_wafer <-function (wafer1, wafer2, output = "compare") {
  #Change wafer 1 & 2 col names

  colnames(wafer1) <- c("X", "Y", "ThK")
  colnames(wafer2) <- c("X", "Y", "ThK")

  if (output == "compare") {
    #Coutour maps
    wafer1.contour <- map_wafer(wafer1, wlabel = "wafer1")
    wafer2.contour <- map_wafer(wafer2, wlabel = "wafer2")

    #Combine wafers for boxplot and radial plots
    wafer1$split <- "wafer1"
    wafer2$split <- "wafer2"
    wafer.combined <- rbind(wafer1, wafer2)
    wafer.combined$r <- sqrt(wafer.combined$X^2 +wafer.combined$Y^2)

    #Boxplots
    plot.boxplot <- ggplot(wafer.combined, aes(x=split, y=ThK, fill=split)) + scale_fill_manual(values = c("red","blue")) + geom_boxplot() + ylab("Thickness (A)")

    #Radial plots
    plot.radial <- ggplot(wafer.combined, aes(x=r, y=ThK, color=split)) + scale_color_manual(values = c("red","blue")) + geom_point(size=2) + ylab("Thickness (A)") + xlab("Wafer Radius (mm)") + theme(legend.position = c(0.2, 0.35))

    gridExtra::grid.arrange(wafer1.contour,plot.boxplot,wafer2.contour,plot.radial)
  } else {
    #Coutour maps
    wafer1.contour <-map_wafer(wafer1, wlabel = "wafer1")
    wafer2.contour <-map_wafer(wafer2, wlabel = "wafer2")

    #Calculate delta
    colnames(wafer1) <- c("X", "Y", "wafer1")
    colnames(wafer2) <- c("X", "Y", "wafer2")
    delta.wafer <- dplyr::left_join(wafer1,wafer2, by =c("X","Y"))
    delta.wafer <- dplyr::mutate(delta.wafer, ThK = wafer2-wafer1)

    #Contour map
    delta.contour <-map_wafer(delta.wafer[,c("X","Y","ThK")], wlabel="Delta Map")

    delta.wafer$r <- sqrt(delta.wafer$X^2 +delta.wafer$Y^2)
    plot.radial <- ggplot(delta.wafer, aes(x=r, y=ThK)) + geom_point(size=2) + ylab("Thickness (A)") + xlab("Wafer Radius (mm)")

    gridExtra::grid.arrange(wafer1.contour,delta.contour,wafer2.contour,plot.radial )
  }

}



