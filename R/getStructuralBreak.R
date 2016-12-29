# This internal function provides a numerical value for the structural break based on time input by user

getStructuralBreak <- function(SB, dateVector = NULL, start = NULL, end = NULL,
                                   frequency = NULL, format = NULL){

if(is.null(dateVector) & is.null(start) & is.null(end)){

  stop("Please provide either a valid number of observation or proper date specifications")
}

if(is.null(dateVector)){

       dateVector =  seq(as.Date(start, format = format), as.Date(end, format = format),  frequency)
  }

    return(which(grepl(as.Date(SB, format = format), dateVector)))
  }

