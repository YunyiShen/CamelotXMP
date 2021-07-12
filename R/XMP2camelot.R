#' read XMP metadata and write it as camelot flavored data.frame
#' @description This function will read photos metadata, convert it into a camelot report as good as we could, then write the camelot flavored data frame to the report_path
#' @details Please check the package exifr for instructions on how to install needed dependencies, especially see ?exifr::read_exif, Make sure that exiftool are proper configed, check documentation in exifr and exiftool https://exiftool.org/.
#' @param photo_path where to find the files
#' @param report_path path to save the camelot metadata
#' @param report_name a string, the base file name of the reports, default the date
#' @param maxentries maximum entries in one csv file, default 65535
#' @param extensions photo extensions, default c("JPG","JPEG","PNG"), case insensitive
#' @param progress whether to have a progress bar, default true
#' @param verbos whether to report during the progress
#' @return a bool return on whether there are errors, also with warning and error log if there is any
#' @export
#'

xmp2camelot <- function(photo_path, report_path, report_name = paste0( "xmp2camelot-",Sys.Date()),
                           maxentries = 65535,
                           extensions = c("JPG","JPEG","PNG"),
                           progress = TRUE,
                           verbos = TRUE){
  pattern <- paste0(extensions,"$")
  pattern <- paste(pattern, collapse = "|")

  files <- list.files(path = photo_path, pattern = pattern,
                      full.names = TRUE, recursive = TRUE,
                      ignore.case = TRUE)

  n_files <- length(files)

  if(verbos){
    cat(n_files,
        "photos found in the directory, there will be",
        ceiling(n_files/maxentries),"report(s) generated\n\n")
  }

  if(progress){
    cat("progress:\n")
    pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                         max = n_files, # Maximum value of the progress bar
                         style = 3,
                         char = "=")   # Character used to create the bar
  }

  i_report <- 1 # the report
  error_file <- c()
  error_message <- c()

  getinfo <- tryCatch(xmp2camelot_helper(files[1]),
                      error = function(e){e$message})

  if( "character" %in% class(getinfo)){
    stop(paste0("the very first file failed with error:\n",getinfo,"\nplease check"))
  }

  temp <- getinfo[-1,]
  curr_report_name <- paste(report_path,paste0(report_name,"-r1.csv"),sep = "/")

  for (i in 1:n_files) {
    getinfo <- tryCatch(xmp2camelot_helper(files[i]),
                        error = function(e){e$message})

    if( "character" %in% class(getinfo)){
      error_file <- c(error_file, files[i])
      error_message <- c(error_message, getinfo)
    }
    else{
      temp <- rbind(temp,getinfo)
    }


    if(progress){
      setTxtProgressBar(pb, i)
    }
    write.csv(temp, curr_report_name, row.names = FALSE)
    if(nrow(temp) >= maxentries){

      temp <- camelotheader
      i_report <- i_report + 1
      curr_report_name <- paste(report_path,paste0(report_name,"-r",i_report,".csv"),sep = "/")
    }
    errors <- length(error_file)>0
    if(errors){
      errlog <- data.frame(photo = error_file, message = error_message)
      errorlog_file <- paste0(report_path,"/errlog.csv")
      write.csv(errlog, errorlog_file, row.names = FALSE)
      #warning("There are ", length(error_file), " file(s) failed to generate the metadata, check the error log at\n", errorlog_file)
    }
  }


  if(progress){
    close(pb)
  }


  errors <- length(error_file)>0
  if(errors){
    errlog <- data.frame(photo = error_file, message = error_message)
    errorlog_file <- paste0(report_path,"/errlog.csv")
    write.csv(errlog, errorlog_file, row.names = FALSE)
    warning("There are ", length(error_file), " file(s) failed to generate the metadata, check the error log at\n", errorlog_file)
  }

  return(!errors)

}


xmp2camelot_helper <- function(thefile){
  exifinfo <- read_exif(thefile)
  if(is.null(exifinfo$Subject)){
    stop("no tag in the photo")
  }
  xmpinfo <- exifinfo$Subject[[1]]
  temp <- fromJSON(xmpinfo)
  return(temp)
}

