


#' Convert bridge lables to camelot metadata
#'
#' @description This function will read photos metadata, convert it into a camelot report as good as we could
#' @details Please check the package exifr for instructions on how to install needed dependencies, especially see ?exifr::read_exif
#' @param photo_path where to find the files
#' @param report_path path to save the camelot metadata
#' @param report_name a string, the base file name of the reports, default the date
#' @param maxentries maximum entries in one csv file, default 65535
#' @param extensions photo extensions, default c("JPG","JPEG","PNG"), case insensitive
#' @param progress whether to have a progress bar, default true
#' @param verbos whether to report during the progress
#' @return a bool return on whether there are errors, also with warning and error log if there is any

#lapply(list.files("./data", full.names = T), load) # this is needed if package not installed

bridge2camelot <- function(photo_path, report_path, report_name = paste0( "bridge2camelot-",Sys.Date()),
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
  temp <- camelotheader
  curr_report_name <- paste(report_path,paste0(report_name,"-r1.csv"),sep = "/")

  for (i in 1:n_files) {
    getinfo <- tryCatch(bridge2camelot_helper(files[i]),
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

    if(nrow(temp) >= maxentries){
      write.csv(temp, curr_report_name)
      temp <- camelotheader
      i_report <- i_report + 1
      curr_report_name <- paste(report_path,paste0(report_name,"-r",i_report,".csv"),sep = "/")
    }
  }

  if(nrow(temp)>0){
    write.csv(temp, curr_report_name)
  }

  if(progress){
    close(pb)
  }


  errors <- length(error_file)>0
  if(errors){
    errlog <- data.frame(photo = error_file, message = error_message)
    errorlog_file <- paste0(report_path,"/errlog.csv")
    write.csv(errlog, errorlog_file)
    warning("There are ", length(error_file), " file(s) failed to generate the metadata, check the error log at\n", errorlog_file)
  }

  return(!errors)

}


bridge2camelot_helper <- function(thefile){
  temp <- camelotheader
  temp <- rbind(temp,rep(NA,78))
  colnames(temp) <- colnames(camelotheader)
  exifinfo <- read_exif(thefile)
  xmpinfo <- exifinfo$Subject[[1]]

  any_match <- colnames(temp) %in% colnames(exifinfo)
  any_match <- colnames(temp)[any_match]
  temp[,any_match] <- exifinfo[,any_match]

  sitee <- regexpr("CN*", xmpinfo)!=-1
  if(sum(sitee)>1){
    stop("multiple matched site names")
  }
  temp[,"Camera.Name"] <- xmpinfo[sitee]


  island_list_local <- paste0(island_list,"$")
  island_list_local <- paste0(island_list_local, collapse = "|")
  islandd <- regexpr(island_list_local, xmpinfo)!=-1
  if(sum(islandd)>1){
    stop("multiple matched island names")
  }
  temp[,"Site.Name"] <- xmpinfo[islandd]


  spp_list_local <- paste0(spp_list, "$")
  spp_list_local <- paste0(spp_list_local, collapse = "|")
  sppp <- regexpr(spp_list_local, xmpinfo) != -1

  if(sum(sppp)>1){
    stop("multiple matched species name, this is possible, please check by hand")
  }

  temp[,"Species"] <- xmpinfo[sppp]

  xmprest <- xmpinfo[(!sppp) & (!islandd) & (!sitee)]
  if(length(xmprest)>0){
    temp[,"Notes"] <- paste(xmprest,collapse = " ")
  }

  temp[,"Absolute.Path"] <- exifinfo[,"SourceFile"]
  datetime <- as.POSIXlt.character(exifinfo$ModifyDate, format = "%Y:%m:%d %H:%M:%OS", tz = "CDT")
  datetime <- sub(" CDT","", datetime)
  temp[,"Date.Time"] <- datetime

  return(temp)
}

