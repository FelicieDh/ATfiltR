#' @title toActel
#'
#' @description Converts your ATFiltR files for use into actel
#'
#'
#' @param detection.folder The name of the folder containing the detection data
#' @param data.folder The name of the folder containing auxillary data (e.g. spatial data). Can be se same af detection.folder
#' @param target.folder the name of the folder in which to store the data for use in actel
#'
#' @return Your detection, animal, spatial and deployment data prepared in the actel format and saved in your target folder
#'
#'
#' @examples
#' \dontrun{
#' # Before running the function, you must be in an R project that contains
#' your detections folder and data folder.
#' # You must have processed your data with at least compileData() and
#' withinWindow()
#' }
#'
#' @export
#'
#' @importFrom here here
#' @import crayon
#' @import data.table
#'
#' @importFrom utils View read.table write.table head
#'
#'
#'


############################################################################
############################################################################
############################# to Actel #####################################
############################################################################
############################################################################

toActel<-function(detection.folder="Detections", data.folder="Data",
                  target.folder="Actel"){


  cat("\n","\n",crayon::bold$yellow("ATfiltR toActel(): formatting your ATfiltR files for use in pacake actel."))
  cat("\n")
  cat("\n","\n",crayon::bold$underline$blue("Step 1: Chose the detections data that you want to pimp..."))
  cat("\n")
  cat("\n","This is an interactive process, we are going to need you to answer a few questions.")
  cat("\n")
  cat("\n","We suggest answers between brackets. So [y] means that you need to press on the y key of your keyboard.")
  cat("\n","To validate answers, just press [enter]")
  cat("\n")
  cat("\n")

  if(exists("atfiltr.detections")){
    cat( " \n")
    stop("You already have a data object called 'atfiltr.detections' loaded in your environment... Rename it so we may use this name in our manipulations...")
  }

  file.toact <- dir(here::here(detection.folder), pattern ="ATfiltR_data")

  if(length(file.toact)==0){

    cat("\n",crayon::bold("No ATfiltR data files found in your detection folder..."),"\n")

    break

  } else {

    repeat{ ##r1 for deployment data

      print(data.frame(Number=1:length(dir(here::here(detection.folder), pattern=c("ATfiltR_data"))),File=dir(here::here(detection.folder), pattern=c("ATfiltR_data"))))

      cat("\n",crayon::bold("
      Here are all the ATfiltR data files we found in your detections folder:
            Please enter the number of the file that you would like to prepare for actel"))

      cat("\n","(Note: If you can't see the file you wanted, you might be giving us a wrong detections.folder, check that your detections.folder is correct)")

      toact<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if(!is.na(suppressWarnings(as.numeric(toact))) & suppressWarnings(as.numeric(toact)) %in% 1:length(dir(here::here(detection.folder), pattern=c("ATfiltR_data")))) { ##check validity of depl

        cat("\n","(Ok, we are loading the file of your choice. This might take a moment...)")

        assign('atfiltr.detections', get(load(here::here(detection.folder,dir(here::here(detection.folder), pattern=c("ATfiltR_data"))[as.numeric(toact)]))))

        atfiltr.detections<<-atfiltr.detections
        cat("\n")

        if(exists("atfiltr.detections")){

          print(head(atfiltr.detections)) }

        repeat{
          cat("\n",crayon::bold("Is this the right file? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break} ##end of check validity of depl
      } ##end of r1 bracket

    }
  }
  cat("\n","\n",crayon::bold$underline$blue("Step 2: We format your ATfiltR detections into an actel.detection file..."))
  cat("\n")
  cat("\n","We use the numeric values in your ID column as a Transmitter. This is done because actel does not have a solution to handle tags that are retrieved and redeployed on different animals.
      By using the animal ID as a transmitter number, we now allow actel to consider animals separately even if they share the same transmitter.
      Your original transmitter data is stored in the column 'True.transmitter'.")

  colnames(atfiltr.detections)[colnames(atfiltr.detections)=="File"]<-"Source.file"
  colnames(atfiltr.detections)[colnames(atfiltr.detections)=="Transmitter"]<-"True.transmitter"

  if(!"Sensor.Value" %in% colnames(atfiltr.detections)){atfiltr.detections$Sensor.Value<-NA}
  if(!"Sensor.Unit" %in% colnames(atfiltr.detections)){atfiltr.detections$Sensor.Unit<-NA}

  atfiltr.detections$Transmitter<-as.numeric(gsub("\\D", "", atfiltr.detections$ID))

  colnames(atfiltr.detections)[colnames(atfiltr.detections)=="Date.and.Time"]<-"Timestamp"
  if(!"CodeSpace" %in% colnames(atfiltr.detections)){atfiltr.detections$CodeSpace<-NA}

  if (any(is.na(atfiltr.detections$Receiver))){atfiltr.detections<-atfiltr.detections[-which(is.na(atfiltr.detections$Receiver)),]}

  atfiltr.detections$Receiver<-as.factor(atfiltr.detections$Receiver)
  atfiltr.detections$CodeSpace<-as.factor(atfiltr.detections$CodeSpace)


  path=here::here(target.folder)
  actel.detections <- list(detections = atfiltr.detections, timestamp = Sys.time())

  actel.detections[["detections"]]$Receiver<-as.factor(actel.detections[["detections"]]$Receiver)
  actel.detections[["detections"]]$Transmitter<-as.factor(actel.detections[["detections"]]$Transmitter)

  save(actel.detections, file = ifelse(file_test("-d", path),
                                       paste0(path, "/actel.detections.RData"), "actel.detections.RData"))


  cat("\n","\n",crayon::bold$underline$blue("Step 3: We load and format your ATfiltR data files for use in actel..."))
  cat("\n")
  cat("\n","Note that we are only formatting your files for the most basic use of actel. We do not make a spatial.txt file, nor do we consider different release site for the fish  or make your distance matrix.
      Your files will only contain the columns that are strictly necessary for actel::explore().
      This is a good starting point and you can add whatever else you need to the files we compile.")
  cat("\n")

  file.deployment <- dir(here::here(data.folder), pattern ="ATfiltR_deployment")
  file.animal <- dir(here::here(data.folder), pattern ="ATfiltR_animal")
  file.spatial <- dir(here::here(data.folder), pattern ="ATfiltR_spatial")


  if (length(file.deployment) == 0) { stop("No ATfiltR_deployment file found in your data folder... ")}
  if (length(file.animal) == 0) { stop("No ATfiltR_animal file found in your data folder... ")}
  if (length(file.spatial) == 0) { stop("No ATfiltR_spatial file found in your data folder... ")}


  cat("\n",crayon::bold("Loading the deployment file"))

  deployment<-read.table(here::here(data.folder,file.deployment), sep=",", header=T)

  deployment$Start<-lubridate::parse_date_time(deployment$Start, c("Ymd HMS", "ymd HMS","dmy HMS", "dmY HMS"), truncated = 3)
  deployment$Stop<-lubridate::parse_date_time(deployment$Stop, c("Ymd HMS", "ymd HMS","dmy HMS", "dmY HMS"), truncated = 3)

  deployment$Start<-as.POSIXct(deployment$Start, format="%Y-%m-%d %H:%M:%S")
  deployment$Stop<-as.POSIXct(deployment$Stop, format="%Y-%m-%d %H:%M:%S")

  write.table(deployment, here::here(target.folder, "deployments.csv"), sep=",", row.names=F)

  cat("\n",crayon::bold("Loading the animal file"))

  animal<-read.table(here::here(data.folder,file.animal), sep=",", header=T)
  animal$Signal<-as.numeric(gsub("\\D", "", animal$ID))

  animal$Date<-lubridate::parse_date_time(animal$Date, c("Ymd HMS", "ymd HMS","dmy HMS", "dmY HMS"), truncated = 3)

  animal$Release.date<-format(as.POSIXct(animal$Date, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S")

  animal<-animal[-which(duplicated(animal$Signal)),]

  write.table(animal, here::here(target.folder, "biometrics.csv"), sep=",", row.names=F)


  cat("\n",crayon::bold("Loading the spatial file"))

  spatial<-read.table(here::here(data.folder,file.spatial), sep=",", header=T)

  if ("Array" %in% colnames(spatial)){
    cat("\n","An 'Array' column was found in your spatial file, we will keep it as it is.")
  } else {
    spatial$Array<-"ATfiltR-array"
  }

  if ("Type" %in% colnames(spatial) & "Hydrophone" %in% spatial$Type){
    cat("\n","A valid 'Tpye' column was found in your spatial file, we will keep it as it is.")
  } else {
    spatial$Type<-"Hydrophone"
  }

  spatial<-spatial[which(spatial$Station.name %in% unique(atfiltr.detections$Station.name)),]

  write.table(spatial, here::here(target.folder, "spatial.csv"), sep=",", row.names=F)


  cat("\n")
  cat("\n")
  cat(crayon::bold$yellow("End of process. You should find actel.detections.RData, biometrics.csv, deployment.csv and spatial.csv in your target folder !"))
  cat("\n",paste0("Do not forget to set your working directory to your target folder. setwd(\"",paste0(here::here(target.folder)),"\")"), "\n")
  cat("Then you can directly run actel::explore(tz = 'Europe/London'), or whatever your timezone is.", "\n")
  cat("When actel asks:", crayon::yellow("Reuse processed detections?(y/n)"), "type", crayon::yellow("y"), ", we have prepared detections for this.")
}
