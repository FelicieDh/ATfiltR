#' @title findSolo
#'
#' @description Finds and filters out the detections that are alone within a defined time window (delay).
#'
#'
#' @param detection.folder The name of the folder containing the detection data
#' @param save.solo Should the solitary detections be saved in the detections folder? (TRUE or FALSE)
#' @param save Should the data be saved in your detections folder (TRUE or FALSE)
#' if FALSE the filtered detections will be in your R environment only
#' @param delay (hours) defines solitary detections: detections that are recorded *delay* hours after the previous one and *delay* hours before the subsequent one are considered solitary.
#'
#' @return A data frame object that contains the detections from your tags, wihtout solitary detections, another that contains the solitary detections
#'
#' @examples
#' \dontrun{
#' # Before running the function, you must be in an R project that contains your detections folder and data folder, your data must be compiled via compileData(), and could (should) have already been filtered by wWindow()
#' findSolo(detection.folder="Detections", save.solo=T, save=T, delay = 1)
#' }
#' @export
#'
#' @importFrom here here
#' @import crayon
#'
#' @importFrom utils View read.table write.table
#'
#'
#'


############################################################################
############################################################################
############################ FIND SOLO #####################################
############################################################################
############################################################################


findSolo<-function(detection.folder="Detections", save.solo=T, save=T, delay = 1){


  cat("\n","\n",crayon::bold$yellow("ATfiltR findSolo(): Find detections that are solitary within a certain time-span."))
  cat("\n")
  cat("\n","Bring me Solo, and the Wookie. They will suffer for this outrage!"," \n")
  cat("\n")
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
    files <- dir(path, pattern ="ATfiltR_data")

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
      ATfiltR_data.2<-read.table(here::here(detection.folder, files[which.min(difftime(Sys.Date(),as.Date(substr(files.2, 16, 25))))]), sep=",", header=T)
    }
  }

  cat("\n")
  cat(crayon::bold("Compiled detections found! Let's do this!", " \n"))

  cat("Formatting the Date.and.Time column as a POSIXct object and ordering the data chronologically...", " \n")

  ATfiltR_data.2$Date.and.Time<-as.POSIXct(ATfiltR_data.2$Date.and.Time, format="%Y-%m-%d %H:%M:%S")
  ATfiltR_data.2<-ATfiltR_data.2[order(ATfiltR_data.2$Date.and.Time),]
  ATfiltR_data.2<<-ATfiltR_data.2




  ATfiltR_data.2$Time.after<-NA
  ATfiltR_data.2$Time.before<-NA
  ATfiltR_data.2$Time.after<-as.POSIXct(ATfiltR_data.2$Time.after, format="%Y-%m-%d %H:%M:%S")
  ATfiltR_data.2$Time.before<-as.POSIXct(ATfiltR_data.2$Time.before, format="%Y-%m-%d %H:%M:%S")


  cat("\n","\n",crayon::bold$underline$blue("Step 2: Calculating the delay between consecutive detections and defining solitary detections..."))
  cat("\n")
  cat("\n")
  these.animals<-unique(ATfiltR_data.2$ID)
  for (i in 1:length(these.animals)){
    if (nrow(ATfiltR_data.2[which(ATfiltR_data.2$ID == these.animals[i]),])<2) {
      next
    } else {
      ATfiltR_data.2[which(ATfiltR_data.2$ID == these.animals[i])[1:(nrow(ATfiltR_data.2[which(ATfiltR_data.2$ID == these.animals[i]),])-1)],"Time.after"]<-
        ATfiltR_data.2[which(ATfiltR_data.2$ID == these.animals[i])[2:(nrow(ATfiltR_data.2[which(ATfiltR_data.2$ID == these.animals[i]),]))],"Date.and.Time"]

      ATfiltR_data.2[which(ATfiltR_data.2$ID == these.animals[i])[2:(nrow(ATfiltR_data.2[which(ATfiltR_data.2$ID == these.animals[i]),]))],"Time.before"]<-
        ATfiltR_data.2[which(ATfiltR_data.2$ID == these.animals[i])[1:(nrow(ATfiltR_data.2[which(ATfiltR_data.2$ID == these.animals[i]),])-1)],"Date.and.Time"]

    }
    cat(crayon::bold("Calculating the lag between detections for each animal:", i, "/",length(these.animals), " \r"))
  }

  ATfiltR_data.2$Lag.before<-as.numeric(difftime(ATfiltR_data.2$Date.and.Time, ATfiltR_data.2$Time.before, units="hours")) ##calculate the lags
  ATfiltR_data.2$Lag.after<-as.numeric(difftime(ATfiltR_data.2$Time.after, ATfiltR_data.2$Date.and.Time, units="hours")) ##calculate the lags

  ATfiltR_data.2$Solo<-NA

  ATfiltR_data.2[which(ATfiltR_data.2$Lag.after > delay & ATfiltR_data.2$Lag.before > delay), "Solo"]<-"Yes" ## When the previous and next detections are before and after our delay, the detection is considered a solo
  ATfiltR_data.2[which(is.na(ATfiltR_data.2$Lag.after) & ATfiltR_data.2$Lag.before > delay), "Solo"]<-"Yes" ##same for the first and last row of each individual
  ATfiltR_data.2[which(ATfiltR_data.2$Lag.after > delay & is.na(ATfiltR_data.2$Lag.before)), "Solo"]<-"Yes"##same for the first and last row of each individual
  Solo.number<-nrow(ATfiltR_data.2[which(ATfiltR_data.2$Solo=="Yes"),])

  cat("\n")
  cat("You chose a delay of", crayon::cyan(delay), ". Detections thats are at least", crayon::cyan(delay),"hours after the preceding one,
      and", crayon::cyan(delay),"hours before the next one will be declared as solitary ones!"," \n")
  cat("(Note: This results in an effective time window of ", crayon::cyan(delay*2)," hours)")
  cat("\n")
  cat("\n")
  cat(crayon::bold("There are"), crayon::bold$cyan(Solo.number), crayon::bold("Solo detections in your data, we did not find any Wookies...", " \n"))

  if(Solo.number>0){

    solo.detections<-ATfiltR_data.2[which(ATfiltR_data.2$Solo=="Yes"),]

    ATfiltR_data.3<-ATfiltR_data.2[-which(ATfiltR_data.2$Solo=="Yes"),]



    solo.detections<<-solo.detections
    ATfiltR_data.3$Solo<-"No"
    ATfiltR_data.3<<-ATfiltR_data.3

    if (save.solo==TRUE){
      cat("\n")
      cat("You chose to save the solo detections!", " \n")
      write.table(solo.detections, here::here(detection.folder, paste0("ATfilter_solo_", Sys.Date(),".txt")), sep=",", row.names=F)
      cat(crayon::bold("File saved in your detections folder under"), crayon::bold$cyan(paste0("ATfilter_solo_", Sys.Date(),".txt")), crayon::bold$cyan(" \n"))}

    if (save==TRUE){
      cat("\n")
      cat("You chose to save the detections after filtering out the solitary detections!", " \n")
      write.table(ATfiltR_data.3, here::here(detection.folder, paste0("ATfiltR_data.3_", Sys.Date(),".txt")), sep=",", row.names=F)
      cat(crayon::bold("File saved in your detections folder under"), crayon::bold$cyan(paste0("ATfiltR_data.3_", Sys.Date(),".txt")), crayon::bold(" \n"))}

  } else if (Solo.number == 0) {

    ATfiltR_data.2$Solo<-"No"
    ATfiltR_data.3<<-ATfiltR_data.2


    if (save==TRUE){
      cat("\n")
      cat("There weren't any solitary detections: Saving the compiled file without removing any detections...", " \n")
      write.table(ATfiltR_data.3, here::here(detection.folder, paste0("ATfiltR_data.3_", Sys.Date(),".txt")), sep=",", row.names=F)
      cat(crayon::bold("File saved in your detections folder under"), crayon::bold$cyan(paste0("ATfiltR_data.3_", Sys.Date(),".txt")), crayon::bold(" \n"))}
  }
  cat("\n")
  cat("\n")
  cat(crayon::bold$yellow("End of process for the Solo detection filtering. This took approximately", paste(round(difftime(Sys.time(), start, units="min"))), "minutes!"," \n"))



} ##end of findSolo function

