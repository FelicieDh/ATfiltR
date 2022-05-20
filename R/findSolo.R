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
#' @param per.receiver If TRUE (default) the solitary detections are considered solitary when they occur alone on a given receiver (Similarly to Kessel et al. 2014). If FALSE they are considered solitary
#' when they occur alone across the whole array.
#'
#'
#' @return A data frame object that contains the detections from your tags, wihtout solitary detections, another that contains the solitary detections
#'
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
#' @importFrom data.table shift
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


findSolo<-function(detection.folder="Detections", save.solo=T, save=T, per.receiver=T, delay = 0.5){


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





  cat("\n","\n",crayon::bold$underline$blue("Step 2: Calculating the delay between consecutive detections and defining solitary detections..."))
  cat("\n")
  cat("\n")
  these.animals<-unique(ATfiltR_data.2$ID)
  for (i in 1:length(these.animals)){
    if (ATfiltR_data.2[ID == these.animals[i],.N]<2) {
      next
    } else {


      ATfiltR_data.2[ID == these.animals[i] &
                       !Station.name %in% c("Implantation", "Active", "Inactive"), Time.after := shift(ATfiltR_data.2[ID == these.animals[i] &
                                                                                                                        !Station.name %in% c("Implantation", "Active", "Inactive"),Date.and.Time], 1, type="lead")]
      ATfiltR_data.2[ID == these.animals[i] &
                       !Station.name %in% c("Implantation", "Active", "Inactive"), Time.before := shift(ATfiltR_data.2[ID == these.animals[i] &
                                                                                                                         !Station.name %in% c("Implantation", "Active", "Inactive"),Date.and.Time], 1)]
      if (per.receiver==T){
        receiverlist<-unique(ATfiltR_data.2[ID == these.animals[i] &
                                              !Station.name %in% c("Implantation", "Active", "Inactive"), Station.name])
        if (length(receiverlist)>0){
        for (j in 1:length(receiverlist)){
          ATfiltR_data.2[ID == these.animals[i] &
                           Station.name == receiverlist[j],
                         Time.after.pR := shift(ATfiltR_data.2[ID == these.animals[i] &
                                                                 Station.name == receiverlist[j],Date.and.Time], 1, type="lead")]
          ATfiltR_data.2[ID == these.animals[i] &
                           Station.name == receiverlist[j],
                         Time.before.pR := shift(ATfiltR_data.2[ID == these.animals[i] &
                                                                  Station.name == receiverlist[j],Date.and.Time], 1)]
        }
        }
      }
    }

    cat(crayon::bold("Calculating the lag between detections for each animal:", i, "/",length(these.animals), " \r"))
  }


  ATfiltR_data.2[, Lag.before := as.numeric(difftime(ATfiltR_data.2$Date.and.Time, ATfiltR_data.2$Time.before, units="hours"))]
  ATfiltR_data.2[, Lag.after := as.numeric(difftime(ATfiltR_data.2$Time.after, ATfiltR_data.2$Date.and.Time, units="hours"))]

  if (per.receiver==T){

    ATfiltR_data.2[, Lag.before.pR := as.numeric(difftime(ATfiltR_data.2$Date.and.Time, ATfiltR_data.2$Time.before.pR, units="hours"))]
    ATfiltR_data.2[, Lag.after.pR := as.numeric(difftime(ATfiltR_data.2$Time.after, ATfiltR_data.2$Date.and.Time.pR, units="hours"))]

    ATfiltR_data.2[, Solo := "No"]

    ATfiltR_data.2[Lag.after.pR > delay & Lag.before.pR > delay, Solo := "Yes"] ## When the previous and next detections are before and after our delay, the detection is considered a solo
    ATfiltR_data.2[is.na(Lag.after.pR) & Lag.before.pR > delay, Solo := "Yes"]##same for the first and last row of each individual
    ATfiltR_data.2[is.na(Lag.before.pR) & Lag.after.pR > delay, Solo := "Yes"]##same for the first and last row of each individual
    ATfiltR_data.2[is.na(Lag.before.pR) & is.na(Lag.after.pR), Solo := "Yes"]##same if these is no lag on the same receivers
  } else {

    ATfiltR_data.2[, Solo := "No"]

    ATfiltR_data.2[Lag.after > delay & Lag.before > delay, Solo := "Yes"] ## When the previous and next detections are before and after our delay, the detection is considered a solo
    ATfiltR_data.2[is.na(Lag.after) & Lag.before > delay, Solo := "Yes"]##same for the first and last row of each individual
    ATfiltR_data.2[is.na(Lag.before) & Lag.after > delay, Solo := "Yes"]##same for the first and last row of each individual
  }



  Solo.number<-ATfiltR_data.2[Solo == "Yes" , .N]

  cat("\n")
  cat("You chose a delay of", crayon::cyan(delay), ". Detections thats are at least", crayon::cyan(delay),"hours after the preceding one,
      and", crayon::cyan(delay),"hours before the next one will be declared as solitary ones!"," \n")
  cat("(Note: This results in an effective time window of ", crayon::cyan(delay*2)," hours)","\n")
  if (per.receiver==T){
    cat( crayon::yellow("Detection delay is applied on a given receiver, as you chose per.receiver=TRUE")," \n")
  }
  cat("\n")
  cat("\n")
  cat(crayon::bold("There are"), crayon::bold$cyan(Solo.number), crayon::bold("Solo detections in your data, we did not find any Wookies...", " \n"))

  if(Solo.number>0){

    solo.detections<-ATfiltR_data.2[Solo=="Yes",]

    ATfiltR_data.3<-ATfiltR_data.2[Solo == "No",]



    solo.detections<<-solo.detections

    ATfiltR_data.3<<-ATfiltR_data.3

    if (save.solo==TRUE){
      cat("\n")
      cat("You chose to save the solo detections!", " \n")
      save(solo.detections, file=here::here(detection.folder, paste0("ATfilter_solo_", Sys.Date(),".RData")))
      cat(crayon::bold("File saved in your detections folder under"), crayon::bold$cyan(paste0("ATfilter_solo_", Sys.Date(),".RData")), crayon::bold$cyan(" \n"))}

    if (save==TRUE){
      cat("\n")
      cat("You chose to save the detections after filtering out the solitary detections!", " \n")
      save(ATfiltR_data.3, file=here::here(detection.folder, paste0("ATfiltR_data.3_", Sys.Date(),".RData")))
      cat(crayon::bold("File saved in your detections folder under"), crayon::bold$cyan(paste0("ATfiltR_data.3_", Sys.Date(),".RData")), crayon::bold(" \n"))}

  } else if (Solo.number == 0) {

    ATfiltR_data.2$Solo<-"No"
    ATfiltR_data.3<<-ATfiltR_data.2


    if (save==TRUE){
      cat("\n")
      cat("There weren't any solitary detections: Saving the compiled file without removing any detections...", " \n")
      save(ATfiltR_data.3, file=here::here(detection.folder, paste0("ATfiltR_data.3_", Sys.Date(),".RData")))
      cat(crayon::bold("File saved in your detections folder under"), crayon::bold$cyan(paste0("ATfiltR_data.3_", Sys.Date(),".txt")), crayon::bold(" \n"))}
  }
  cat("\n")
  cat("\n")
  cat(crayon::bold$yellow("End of process for the Solo detection filtering. This took approximately", paste(round(difftime(Sys.time(), start, units="min"))), "minutes!"," \n"))



} ##end of findSolo function
