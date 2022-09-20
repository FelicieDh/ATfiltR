#' @title loadSolo
#'
#' @description Loads the data for use in findSolo()
#'
#'
#' @param detection.folder The name of the folder containing the detection data
#'
#' @return The data loaded for findSolo()
#'
#' @examples
#' \dontrun{
#' # Before running the function, you must be in an R project that contains
#' your detections folder and data folder, your data must be compiled via
#' compileData(), and could (should) have already been filtered by wWindow()
#' findSolo(detection.folder="Detections", save.solo=T, save=T, delay = 1)
#' }
#' @export
#'
#' @importFrom here here
#' @import crayon
#' @importFrom data.table shift
#'
#' @importFrom utils View read.table write.table head
#'
#'


############################################################################
############################################################################
############################ SOLO LOAD #####################################
############################################################################
############################################################################


loadSolo<-function(detection.folder="Detections"){

  cat("\n",crayon::bold$underline$blue("Step 1: Loading your compiled detections from the compileData() function, possibly filtered via wWindow()..."))
  cat("\n")
  cat("\n")

  start<-Sys.time()

  ##############################################
  ########## Defining the objects ##############
  ##############################################


  these.animals<-NA ##all the tags that you have in the detections data
  Solo.number<-NA ##number of rows with Solo detections

  ######################################################
  ########## Loading ATfiltR_data.1 or .2 ##############
  ######################################################


  if(!exists("ATfiltR_data.2")){
    path=here::here(detection.folder)
    files <- dir(path, pattern ="^ATfiltR_data.*\\.RData$")

    if (length(files[which(is.na(as.numeric(substr(files, 14, 14))))]) >0) { ##checking for entries that aren't valid
      files<-files[-which(is.na(as.numeric(substr(files, 14, 14))))]}

    if (length(files[which(is.na(as.Date(substr(files, 16, 25))))]) >0) { ##checking for entries that aren't valid
      files<-files[-which(is.na(as.Date(substr(files, 16, 25))))]}


    if (length(files) == 0){
      cat("No compiled detections found in your environment or Detections folder... Please compile your detections using compileData() first. You could (should) also pre-filter it with wWindow()", " \n")
      stop("Run compileData() first...")

    } else{

      if (length(files[which(as.numeric(substr(files, 14, 14)) >2)]) >0){
        files<-files[-which(as.numeric(substr(files, 14, 14)) >2)]

        if (length(files) == 0){
          cat("We only found detection files that had already been through findSolo() or speedCheck() in your Detections folder... Please re-compile your detections using compileData() first. You could (should) also pre-filter it with wWindow()", " \n")
          stop("Run compileData() first...")
        }
      }

      files.2<-files[which.max(as.numeric(substr(files, 14, 14)))]
      cat("Compiled detections found in your environment. Loading the most recent (and most up-to-date) compilation:", crayon::cyan(paste(files.2[which.min(as.numeric(difftime(Sys.Date(),as.Date(substr(files.2, 16, 25)))))]), " \n"))
      ATfiltR_data.2<-loadRData(here::here(detection.folder, files.2[which.min(difftime(Sys.Date(),as.Date(substr(files.2, 16, 25))))]))
    }
  }

  cat("\n")
  cat(crayon::bold("Compiled detections found! Let's do this!", " \n"))

  cat("Ordering the data chronologically...", " \n")

  ATfiltR_data.2<-ATfiltR_data.2[order(ATfiltR_data.2$Date.and.Time),]
  ATfiltR_data.2<<-ATfiltR_data.2

}
