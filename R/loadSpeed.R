#' @title loadSpeed
#'
#' @description Loads the data for use in speedCheck()
#'
#'
#' @param detection.folder The name of the folder containing the detection data
#'
#' @return The data loaded for speedCheck()
#'
#' @examples
#' \dontrun{
#' # Before running the function, you must be in an R project that contains
#' your detections folder and data folder, your data must be compiled via
#' compileData(), and have been filtered by wWindow(). It should (but doesnt have to) also have gone through findSolo()
#' findSolo(detection.folder="Detections", save.solo=T, save=T, delay = 1)
#' }
#' @export
#'
#' @importFrom here here
#' @import crayon
#' @import data.table
#'
#' @importFrom utils View read.table write.table head
#'
#'


############################################################################
############################################################################
############################ SOLO LOAD #####################################
############################################################################
############################################################################


loadSpeed<-function(detection.folder="Detections"){

  cat("\n",crayon::bold$underline$blue("Step 1: Loading your compiled detections from the compileData() function, possibly filtered via wWindow()..."))
  cat("\n")
  cat("\n")



  ######################################################
  ########## Loading ATfiltR_data.3 ##############
  ######################################################


  if(!exists("ATfiltR_data.3")){
    path=here::here(detection.folder)
    files <- dir(path, pattern ="^ATfiltR_data.3.*\\.RData$")


    if (length(files[which(is.na(as.numeric(substr(files, 14, 14))))]) >0) { ##checking for entries that aren't valid
      files<-files[-which(is.na(as.numeric(substr(files, 14, 14))))]}

    if (length(files[which(is.na(as.Date(substr(files, 16, 25))))]) >0) { ##checking for entries that aren't valid
      files<-files[-which(is.na(as.Date(substr(files, 16, 25))))]}

    if (length(files) == 0){
      cat("No valid compiled detections found in your environment or Detections folder... Please compile your detections using compileData() first andrun them through findSolo().", " \n")
      stop("Run compileData(), wWindow() and findSolo() first...")
    } else {
      if (length(files[which(as.numeric(substr(files, 14, 14)) >3)]) >0){
        files<-files[-which(as.numeric(substr(files, 14, 14)) >3)]}

      if (length(files[which(as.numeric(substr(files, 14, 14)) ==1)]) >0){
        files<-files[-which(as.numeric(substr(files, 14, 14)) ==1)]}

        if (length(files) == 0){
          cat("We only found detection files that had already been through findSolo() or speedCheck() in your Detections folder... Please re-compile your detections using compileData() first. You could (should) also pre-filter it with wWindow()", " \n")
          stop("Run compileData() first...")
        }

      files.2<-files[which.max(as.numeric(substr(files, 14, 14)))]
      cat("Compiled detections found in your environment. Loading the most recent (and most up-to-date) compilation:", crayon::cyan(paste(files.2[which.min(as.numeric(difftime(Sys.Date(),as.Date(substr(files.2, 16, 25)))))]), " \n"))
      ATfiltR_data.3<-loadRData(here::here(detection.folder, files.2[which.min(difftime(Sys.Date(),as.Date(substr(files.2, 16, 25))))]))
  }}
  cat(" \n")
  cat(crayon::bold("Compiled detections found! Let's do this!", " \n"))
  ##Preparing the datetime as a POSIX and ordering the dataframe
  cat("Ordering the data chronologically...", " \n")

  data.table::setkey(ATfiltR_data.3, Date.and.Time)


  ATfiltR_data.3<<-ATfiltR_data.3
  cat("\n")
}
