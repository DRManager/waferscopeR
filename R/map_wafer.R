#' Plot wafer contour maps
#'
#' Given thickness profile of a wafer, this function plots wafer maps including radial and residual decomposition
#' @param wafer dataframe from the wafer list (output of read.kla_f5, or a self-imported file obeys the same data structure)
#' @param resolution takes value of '\code{high}', '\code{normal}', '\code{low}', '\code{rapid}'
#' @param point '\code{on} (\code{off})': display (hide) point label,
#' @param maps '\code{mean}' shows the mean map only, '\code{decompose}' all the radial and residual maps, '\code{all}' includes all above three maps and radial spatial distribution
#' @param wlabel label of the output plot
#' @return coresponding wafer map(s)
#' @export
#' @import grDevices

#plot wafer maps with the input of wafer data
map_wafer <- function(wafer, resolution="normal", point="off",maps="mean", wlabel ="Wafer Map"){
  #pass wafer data as data frame with x,y,thickness and/or id ->thickness, GOF, X,Y
  #calcuate mean, radial, residual
  #output four charts: mean map, radial distribution, radial and residual maps

#colnames(wafer) <- c("X","Y","ThK")

  # resolution 2: normal, 1: high, 3: low, 5: ultra low
  step <- switch(resolution,
                 "high" = 1,
                 "normal" =2,
                 "low" = 3,
                 "rapid" =5)

  #create a data.frame with x,y,z columns
  wafer.raw <- data.frame(x = round(wafer[,"X"], 1), y = round(wafer[,"Y"], 1), z = round(wafer[,"ThK"], 1))

  if (maps == "mean") {
    #wafer statistics
    wafer.mean <- round(mean(wafer.raw$z, na.rm = T),1)
    wafer.3sigma <- round(3*sd(wafer.raw$z, na.rm = T),1)
    percent_3sigma <- paste(round(100*wafer.3sigma/wafer.mean, 1), "%", sep="")
    wafer.range <- round(max(wafer.raw$z, na.rm = T)-min(wafer.raw$z, na.rm = T),1)
    percent_range <- paste(round(100*wafer.range/wafer.mean, 1), "%", sep="")
    wafer.stat <- paste("Mean: ", wafer.mean, 'A \n', '3s: ', wafer.3sigma, 'A (',
                        percent_3sigma,') \n', 'Range: ', wafer.range, 'A (', percent_range, ')', sep = '')

    #model to smoothen data
    wafer.loess <- loess(z ~ x*y, data = wafer.raw, degree = 2, span = 0.25)

    #expand grid, -150:150 -> 300 mm square
    wafer.square <- expand.grid(list(x = seq(-150, 150, step), y = seq(-150, 150, step)))

    #predict z values based on the model
    z <- predict(wafer.loess, newdata = wafer.square)
    wafer.square$z <- round(as.numeric(z),2)


    #make a subset for 300 mm wafer
    wafer.round <- subset(wafer.square, x^2+y^2 <= 150^2)

    #color for surface plot
    jet.colors <-   # function from grDevices package
      colorRampPalette(c('#FF00FF',"#00007F", "blue", "#007FFF", "cyan",
                         "#7FFF7F", "yellow", "#FF7F00", "red"))
    colorzjet <- jet.colors(20)  # 100 separate color


    #for text on map
    name.hori <- wafer.raw[wafer.raw$y != 0,]
    name.vert <- wafer.raw[wafer.raw$y == 0,]

    #mean map
    if (point =="off") {
      #mean map
      plot.mean <- ggplot()+
        geom_tile(data = wafer.round, aes(x = wafer.round$x,y = wafer.round$y,fill=wafer.round$z))+
        xlab(wafer.stat) + ylab('Y(mm)')+ggtitle(wlabel)+
        theme(axis.line=element_blank(),axis.text.y=element_blank(), axis.text.x=element_blank(),
              axis.ticks=element_blank(),
              axis.title.y=element_blank(),
              axis.title=element_text(size=rel(1)),plot.title = element_text(size=rel(1)),
              panel.background = element_rect(fill = "transparent"), panel.grid.minor=element_blank()
        )+ scale_fill_gradientn(name = 'Etch Amount (A)', colours=colorzjet) + coord_fixed(ratio=1)+
        annotate("path", x=150*cos(seq(0,2*pi,length.out=1000)), y=150*sin(seq(0,2*pi,length.out=1000)), size = 1)


    } else{

          plot.mean <- ggplot()+
        geom_tile(data = wafer.round, aes(x = wafer.round$x,y = wafer.round$y,fill=wafer.round$z))+
        xlab(wafer.stat) +
        ylab('Y(mm)')+ggtitle(wlabel)+
        theme(axis.line=element_blank(),axis.text.y=element_blank(), axis.text.x=element_blank(),
              axis.ticks=element_blank(),
              axis.title.y=element_blank(),
              axis.title=element_text(size=rel(1)),plot.title = element_text(size=rel(1)),
              panel.background = element_rect(fill = "transparent"), panel.grid.minor=element_blank()
        )+
        scale_fill_gradientn(name = 'Etch Amount (A)', colours=colorzjet) + coord_fixed(ratio=1)+
        geom_point(data = wafer.raw, aes(x = wafer.raw$x, y = wafer.raw$y, label=as.character(wafer.raw$z)), size = 2)+
        geom_text(aes(x = name.hori$x, y = name.hori$y, label=as.character(name.hori$z)),
                  size = 3,hjust=0, vjust=0, nudge_x = 0.5, angle = 0)+
        geom_text(aes(x = name.vert$x, y = name.vert$y, label=as.character(name.vert$z)),
                  size = 3,hjust=0, vjust=0,nudge_y = 0.5, angle = 60)+
        annotate("path", x=150*cos(seq(0,2*pi,length.out=1000)), y=150*sin(seq(0,2*pi,length.out=1000)), size = 1.5)

    }

   return(plot.mean) # mean wafer map only
  }

  else {

    #wafer statistics
    wafer.mean <- round(mean(wafer.raw$z, na.rm = T),1)
    wafer.3sigma <- round(3*sd(wafer.raw$z, na.rm = T),1)
    percent_3sigma <- paste(round(100*wafer.3sigma/wafer.mean, 1), "%", sep="")
    wafer.range <- round(max(wafer.raw$z, na.rm = T)-min(wafer.raw$z, na.rm = T),1)
    percent_range <- paste(round(100*wafer.range/wafer.mean, 1), "%", sep="")
    wafer.stat <- paste("Mean: ", wafer.mean, 'A \n', '3s: ', wafer.3sigma, 'A (',
                        percent_3sigma,') \n', 'Range: ', wafer.range, 'A (', percent_range, ')', sep = '')

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
    radial.stat <- paste('3s: ', radial.3sigma, 'A (',
                         radial.percent_3sigma,') \n', 'Range: ', radial.range, 'A (', radial.percent_range, ')', sep = '')

    #wafer stat - residual
    resi <- wafer.radial
    resi$z <- wafer.radial$z - wafer.radial$zmean

    resi.3sigma <- round(3*sd(resi$z, na.rm = T),1)
    resi.range <- round(max(resi$z, na.rm = T)-min(resi$z, na.rm = T),1)
    resi.stat <- paste('3s: ', resi.3sigma, 'A ',
                       ' \n', 'Range: ', resi.range, 'A ', sep = '')



    #model to smoothen data
    wafer.loess <- loess(z ~ x*y, data = wafer.raw, degree = 2, span = 0.25)
    wafer.radial.loess <- loess(zmean ~ x*y, data = wafer.radial, degree = 2, span = 0.25)

    #expand grid, -150:150 -> 300 mm square
    wafer.square <- expand.grid(list(x = seq(-150, 150, step), y = seq(-150, 150, step)))
    wafer.radial.square <- expand.grid(list(x = seq(-150, 150, step), y = seq(-150, 150, step)))

    #predict z values based on the model
    z <- predict(wafer.loess, newdata = wafer.square)
    wafer.square$z <- round(as.numeric(z),2)

    zmean <- predict(wafer.radial.loess, newdata = wafer.radial.square)
    wafer.radial.square$zmean <- round(as.numeric(zmean),2)
    wafer.radial.square$r <- round(sqrt(wafer.radial.square$x^2+wafer.radial.square$y^2),0)

    #make a subset for 300 mm wafer
    wafer.round <- subset(wafer.square, x^2+y^2 <= 150^2)
    wafer.radial.round <- subset(wafer.radial.square, x^2+y^2 <= 150^2)



    radial.test <- wafer.radial.round
    r.unique <- unique(radial.test$r)
    test <- data.frame()
    wafer.radial.avg <- data.frame()

    for (i in 1:length(r.unique)){

      wafer.samer <- subset(radial.test, radial.test$r == r.unique[i])

      xy.unique <- unique(abs(wafer.samer$x))
      x.unique <- xy.unique[xy.unique <= round(r.unique[i]/sqrt(2),0)]

      for (j in 1:length(x.unique)){

        test <- wafer.samer[abs(wafer.samer$x) == x.unique[j] | abs(wafer.samer$y) == x.unique[j],]
        zmeanmean <- mean(test$zmean, na.rm = T)

        wafer.samer[abs(wafer.samer$x) == x.unique[j] | abs(wafer.samer$y) == x.unique[j],]$zmean <- zmeanmean

      }

      radial.test[radial.test$r == r.unique[i],]$zmean <- wafer.samer$zmean

    }

    wafer.radial.round <- radial.test


    #residual

    residual <- data.frame(x = wafer.round$x, y = wafer.round$y, z= wafer.round$z - wafer.radial.round$zmean)




    #color for surface plot
    jet.colors <-   # function from grDevices package
      colorRampPalette(c('#FF00FF',"#00007F", "blue", "#007FFF", "cyan",
                         "#7FFF7F", "yellow", "#FF7F00", "red"))
    colorzjet <- jet.colors(20)  # 100 separate color


    #for text on map
    name.hori <- wafer.raw[wafer.raw$y != 0,]
    name.vert <- wafer.raw[wafer.raw$y == 0,]

    #mean map
    if (point =="off") {
      #mean map
      plot.mean <- ggplot()+
        geom_tile(data = wafer.round, aes(x = wafer.round$x,y = wafer.round$y,fill=wafer.round$z))+
        xlab(wafer.stat) + ylab('Y(mm)')+ggtitle(wlabel)+
        theme(axis.line=element_blank(),axis.text.y=element_blank(), axis.text.x=element_blank(),
              axis.ticks=element_blank(),
              axis.title.y=element_blank(),
              axis.title=element_text(size=rel(1)),plot.title = element_text(size = rel(1)),
              panel.background = element_rect(fill = "transparent"), panel.grid.minor=element_blank()
        )+ scale_fill_gradientn(name = 'Etch Amount (A)', colours=colorzjet) + coord_fixed(ratio=1)+
        annotate("path", x=150*cos(seq(0,2*pi,length.out=1000)), y=150*sin(seq(0,2*pi,length.out=1000)), size = 1)


    } else{


      plot.mean <- ggplot()+
        geom_tile(data = wafer.round, aes(x = wafer.round$x,y = wafer.round$y,fill=wafer.round$z))+
        xlab(wafer.stat) +
        ylab('Y(mm)')+ggtitle(wlabel)+
        theme(axis.line=element_blank(),axis.text.y=element_blank(), axis.text.x=element_blank(),
              axis.ticks=element_blank(),
              axis.title.y=element_blank(),
              axis.title=element_text(size=rel(1)),plot.title = element_text(size = rel(1)),
              panel.background = element_rect(fill = "transparent"), panel.grid.minor=element_blank()
        )+
        scale_fill_gradientn(name = 'Etch Amount (A)', colours=colorzjet) + coord_fixed(ratio=1)+
        geom_point(data = wafer.raw, aes(x = wafer.raw$x, y = wafer.raw$y, label=as.character(wafer.raw$z),size=0.5), size = 2)+
        geom_text(aes(x = name.hori$x, y = name.hori$y, label=as.character(name.hori$z)),
                  size = 2,hjust=0, vjust=0, nudge_x = 1, angle = 0)+
        geom_text(aes(x = name.vert$x, y = name.vert$y, label=as.character(name.vert$z)),
                  size = 3,hjust=0, vjust=0,nudge_y = 0.5, angle = 0)+
        annotate("path", x=150*cos(seq(0,2*pi,length.out=1000)), y=150*sin(seq(0,2*pi,length.out=1000)), size = 1.5)

    }

    # radial
    plot.radial <- ggplot(wafer.radial.round, aes(wafer.radial.round$x,wafer.radial.round$y,z=wafer.radial.round$zmean))+
      geom_tile(aes(fill=zmean))+
      xlab(radial.stat) + ylab('Y(mm)')+ggtitle('Radial Average')+
      theme(axis.line=element_blank(),axis.text.y=element_blank(), axis.text.x=element_blank(),
            axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            axis.title=element_text(size=rel(1)),plot.title = element_text(size = rel(1)),
            panel.background = element_rect(fill = "transparent"), panel.grid.minor=element_blank()
      )+ scale_fill_gradientn(name = 'Etch Amount (A)', colours=colorzjet) + coord_fixed(ratio=1)+
      annotate("path", x=150*cos(seq(0,2*pi,length.out=1000)), y=150*sin(seq(0,2*pi,length.out=1000)), size = 1.5)

    # residual
    plot.residual <- ggplot(residual, aes(residual$x, residual$y, z=residual$z))+
      geom_tile(aes(fill=z))+
      xlab(resi.stat) + ylab('Y(mm)')+ggtitle('Residuals')+
      theme(axis.line=element_blank(),axis.text.y=element_blank(), axis.text.x=element_blank(),
            axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            axis.title=element_text(size=rel(1)),plot.title = element_text(size = rel(1)),
            panel.background = element_rect(fill = "transparent"), panel.grid.minor=element_blank()
      )+ scale_fill_gradientn(name = 'Etch Amount (A)', colours=colorzjet) + coord_fixed(ratio=1)+
      annotate("path", x=150*cos(seq(0,2*pi,length.out=1000)), y=150*sin(seq(0,2*pi,length.out=1000)), size = 1.5)


    # Radial distribution
    plot.distribution <- ggplot(wafer.raw, aes(r,z)) + geom_point(size=2, color="blue") +
      xlab("Wafer Radius (mm)") + ylab("Etch Amount (A)") + xlim(0,150) + ggtitle ("Radial Distribution")

 # Arrange the output with gridExtra package
    gridExtra::grid.arrange(plot.mean, plot.radial,plot.distribution,plot.residual)
    #return(plist)
  }




}
