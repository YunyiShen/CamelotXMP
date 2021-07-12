#' write camelot entries to XMP via exifr
#' @description This function read a camelot data.frame and write the metadata into XMP part of the Jpeg file as a JSON string
#' @details Make sure that exiftool are proper configed, check documentation in exifr and exiftool https://exiftool.org/. Especially when on Windows, you might need to install Perl. The tag written is xmp-dc:Subject, see exiftool's documentation for detail of the tag https://exiftool.org/TagNames/XMP.html
#' @param camelot data.frame, the camelot data with at least Absolute.Path column
#' @param errorlog_path string, the direction to store the error log (if there is any), default is the working directory
#' @param delete_original boolean, the exiftool will automatically creat a copy of the original photo end with "_original", do you want to delete it? The default is not to delete it (FALSE)
#' @param progress boolean, whether to have a progress bar
#' @param verbos boolean, whether to be verbos
#' @return boolean, whether we encounter errors
#' @export

camelot2xmp <- function(camelot, errorlog_path  = getwd(),
                        delete_original = FALSE, progress = TRUE, verbos = TRUE){
  if(!"data.frame" %in% class(camelot)){
    stop("Camelot should be a data.frame\n")
  }

  if(!"Absolute.Path" %in% colnames(camelot)){
    stop("The camelot data.frame should at least have the 'Absolute.Path' column\n")
  }

  n_files <- nrow(camelot)
  if(verbos){
    cat(n_files,
        "photos found in camelot metadata\n")
    if(delete_original){
      cat("Original photos will be deleted\n")
    }
  }

  if(progress){
    cat("progress:\n")
    pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                         max = n_files, # Maximum value of the progress bar
                         style = 3,
                         char = "=")   # Character used to create the bar
  }

  error_file <- c()
  error_message <- c()

  for(i in 1:n_files){
    getinfo <- tryCatch(camelot2XMP_helper(camelot[i,], delete_original),
                        error = function(e){e$message})

    if( "character" %in% class(getinfo)){
      if(regexpr("ExifTool not properly configured", getinfo)!=-1){
        stop(getinfo)
      }
      error_file <- c(error_file, camelot$Absolute.Path[i])
      error_message <- c(error_message, getinfo)
    }

    errors <- length(error_file)>0
    if(errors){
      errlog <- data.frame(photo = error_file, message = error_message)
      errorlog_file <- paste0(errorlog_path,"/errlog.csv")
      write.csv(errlog, errorlog_file)
      #warning("There are ", length(error_file), " file(s) failed to generate the metadata, check the error log at\n", errorlog_file)
    }


    if(progress){
      setTxtProgressBar(pb, i)
    }

  }
  if(progress){
    close(pb)
  }

  errors <- length(error_file)>0
  if(errors){
    errlog <- data.frame(photo = error_file, message = error_message)
    errorlog_file <- paste0(errorlog_path,"/errlog.csv")
    write.csv(errlog, errorlog_file)
    warning("There are ", length(error_file), " file(s) failed to generate the metadata, check the error log at\n", errorlog_file)
  }

  return(!errors)
}


camelot2XMP_helper <- function(camelot_entries,delete_original){
  json_string <- toJSON(camelot_entries, na = "string")
  file_name <- camelot_entries$Absolute.Path
  if(!file.exists(file_name)){
    stop("file does not exist, check the path")
  }
  exiftoolstring <- paste0("-xmp-dc:Subject='", json_string, "'")
  tt <- exiftool_call(exiftoolstring,quiet = TRUE, fnames = file_name, stdout = FALSE)
  if(tt!=0){
    stop("exiftools failed with unknown reason, check manually")
  }
  if(delete_original){
    tt1 <- file.remove(paste0(file_name,"_original"))
  }
  return(tt)
}


