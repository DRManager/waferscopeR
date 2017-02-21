#' Read from raw data file from KLA-Tencor metrology tools
#'
#' Takes raw datafile from KLA metrology machine and returns a wafer data in lists
#' @param filename Path of the raw metrology datafile
#' @return A list of wafer profiles
#' @export
#'
#' @import utils
#' @import methods

read_kla_f5 <- function(filename){

  # find the max number of columns in the file
  no_of_col <- max(count.fields(filename, sep = ','))

  #first read the file into wafer.test
  wafer.test <- read.csv(filename, header = FALSE, sep = ",",
                         col.names = paste0("V",seq_len(no_of_col)), fill = T,stringsAsFactors = F)

  #find locations of each slot and number of slots
  slot_location <- which(wafer.test[,1]=='SLOT')
  slot.wafer <- list()
  slot.name <- vector()

  if (length(slot_location) != 0){

    x_location <- numeric()

    #for each slot, put the data and colname into the list slot.wafer
    for (i in 1:length(slot_location)){

      x_location[i] <- max(which(wafer.test[slot_location[i]+17,]=='Value'))+1

      # The last wafer
      if (i > (length(slot_location)-1)){

        wafer.slot <- data.frame(wafer.test[(slot_location[i]+18):nrow(wafer.test),2:(x_location[i]+1)])

        # Covert all the column into numeric
        indx <- sapply(wafer.slot, is.character)
        wafer.slot[indx] <- lapply(wafer.slot[indx], function(x) as.numeric(as.character(x)))

        colnames(wafer.slot) <- c( wafer.test[slot_location[i]+8,2:(x_location[i]-1)], 'X','Y')

        #data frame name
        slot.name[i] <-paste0("slot",wafer.test[slot_location[i],2])

        slot.wafer[[i]]<-wafer.slot


      } else #begin collection of wafer

      {
        wafer.slot <- data.frame(wafer.test[(slot_location[i]+18):(slot_location[i+1]-5),2:(x_location[i]+1)])

        colnames(wafer.slot) <- c( wafer.test[slot_location[i]+8,2:(x_location[i]-1)], 'X','Y')

        # Covert all the column into numeric
        indx <- sapply(wafer.slot, is.character)
        wafer.slot[indx] <- lapply(wafer.slot[indx], function(x) as.numeric(as.character(x)))

        slot.name[i] <-paste0("slot",wafer.test[slot_location[i],2])

        slot.wafer[[i]]<-wafer.slot
      }


    }
    names(slot.wafer) <-slot.name

    return(slot.wafer)

  } else {

    comment_location <- which(wafer.test[,1]=='[END COMMENT]')

    wafer.slot <- cbind(data.frame(wafer.test[(comment_location+3):nrow(wafer.test), 3]),data.frame(wafer.test[(comment_location+3):nrow(wafer.test), 1:2]))

    colnames(wafer.slot) <- c( wafer.test[3,1], 'X','Y')

    # Covert all the column into numeric
    indx <- sapply(wafer.slot, is.character)
    wafer.slot[indx] <- lapply(wafer.slot[indx], function(x) as.numeric(as.character(x)))


    slot.wafer[[1]]<-wafer.slot

    return(slot.wafer)
  }


}



