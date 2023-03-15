#' @title wWindow
#'
#' @description Finds and filters out the detections that are outside of the deployment window (deployment of tags and/orr of receivers).
#'
#'
#' @param detection.folder The name of the folder containing the detection data
#' @param data.folder The name of the folder containing auxillary data (e.g. spatial data). Can be se same af detection.folder
#' @param sep.type The column separator in your data (e.g. ","). All dataframes that you plan on using should have the same separator
#' @param save.out.of.deployment Should the data collected outside of the deployment window be saved in the detections folder? (TRUE or FALSE)
#' if FALSE the compiled detections will be in your R environment only
#' @param save.unknown.tags Should the data from tags that aren't in your animal data (and from your own tags but outside of the deployment window) be saved in the detections folder? (TRUE or FALSE)
#' @param discard.first How many of the first hours after deploying a tag should be discarded? (e.g. 24 = the first 24h will be discarded) (if save.unknown.tags is TRUE the discarded data will be saved in the unknown tags file)
#' @param save Should the data be saved in your detections folder (TRUE or FALSE)
#' if FALSE the filtered detections will be in your R environment only
#'
#' @return A data frame object that contains the detections from your tags, collected during a deployment window, another that contains the detections outside of receiver deployment, and a third containing detections from unknown tags or tags pinging outside of their deployment window
#'
#' @examples
#' \dontrun{
#' # Before running the function, you must be in an R project that contains
#' your detections folder and data folder our data must be compiled via
#' compileData()
#' wWindow(detection.folder="Detections", data.folder="Data", sep.type=",",
#' save.out.of.deployment=F,
#' save.unknown.tags=T, discard.first=24, save=T)
#' }
#' @export
#'
#' @importFrom here here
#' @import crayon
#' @importFrom lubridate parse_date_time
#' @importFrom data.table as.data.table
#'
#' @importFrom utils View read.table write.table head
#' @importFrom stats na.omit
#'
#'




################################################################################
################################################################################
############################ WITHIN WINDOW #####################################
################################################################################
################################################################################


wWindow<-function(detection.folder="Detections", data.folder="Data", sep.type=",", save.out.of.deployment=F,
                  save.unknown.tags=T, discard.first=24, save=T){

  cat("\n","\n",crayon::bold$yellow("ATfiltR wWindow(): checking detections within your deployment window."))
  cat("\n")
  cat("\n","\n",crayon::bold$underline$blue("Step 1: Loading your compiled detections from the compileData() function..."))
  cat("\n")
  cat("\n")
  start<-Sys.time()

  ##############################################
  ########## Defining the objects ##############
  ##############################################

  depl<-NA ## file containing deployment data
  start.d<-NA ## start of deployment
  stop.d<-NA ## end of deployment
  file.dep<-NA ##a premade Deployment file
  receiv<-NA ##Column where receiver data is
  stat.d<-NA ##column with station name in deployment df
  file.spat<-NA ##a premade Spatial file
  spat<-NA ## file containing spatial data
  lon<-NA ## longitude column
  lat<-NA ## latitude column
  range.cat<-NA ##Column where receiver data is
  file.animal<-NA ##a premade animal file
  anim<-NA ## file containing animal data
  trans<-NA ## teansmitter column
  ID<-NA ## ID column
  anim.date<-NA ## Date column in animal data
  tag.stat<-NA ##tag.status column
  iter.tag<-NA ##iterations to change the tag status column
  to.transfer<-NA ##the columns to take from animal a put in detections


  ################################################
  ########## Loading ATfiltR_data.1 ##############
  ################################################

  if(!exists("ATfiltR_data.1")){
    path=here::here(detection.folder)
    files <- dir(path, pattern ="^ATfiltR_data.1.*\\.RData$")

    if (length(files) == 0){
      cat("No compiled detections found in your environment or Detections folder... Please compile your detections using compileData() first.", " \n")
      stop("Run compileData() first...")
    } else{
      cat("Compiled detections found in your environment. Loading the most recent compilation:", crayon::cyan(paste(files[which.min(difftime(Sys.Date(),as.Date(substr(files, 16, 25))))]), " \n"))
      load(here::here(detection.folder, files[which.min(difftime(Sys.Date(),as.Date(substr(files, 16, 25))))]))

    }
  }
  cat(" \n")
  cat(crayon::bold("Compiled detections found! Let's do this!", " \n"))
  ##Preparing the datetime as a POSIX and ordering the dataframe
  cat("Ordering the data chronologically...", " \n")

  ATfiltR_data.1<-ATfiltR_data.1[order(ATfiltR_data.1$Date.and.Time),]
  cat(" \n")
  cat("Let's find your deployment, spatial and animal data...", " \n")


  #################################################
  ########## Loading deployment data ##############
  #################################################

  cat("\n","\n",crayon::bold$underline$blue("Step 2: Loading your deployment data..."))
  cat("\n")
  cat("\n","This is an interactive process, we might need you to answer a few questions.")
  cat("\n")
  cat("\n","We suggest answers between brackets. So [y] means that you need to press on the y key of your keyboard.")
  cat("\n","To validate answers, just press [enter]")
  cat("\n")
  cat("\n")


  file.dep <- dir(here::here(data.folder), pattern ="ATfiltR_deployment")

  if(length(file.dep)>0){

    cat("\n",crayon::bold("We found an ATfiltR deployment data file, you don't have anything to do, we are loading it..."),"\n")

    deployment<-read.table(here::here(data.folder,file.dep), sep=",", header=T)

  } else {

    print(data.frame(Number=1:length(dir(here::here(data.folder), pattern=c(".txt|.csv"))),File=dir(here::here(data.folder), pattern=c(".txt|.csv"))))

    repeat{ ##r1 for deployment data
      cat("\n",crayon::bold("No ATfiltR deployment file found,
                        Please enter the number of the file that contains your deployment data (see choices above)"))

      cat("\n","(Note: If you can't see it, you might be giving us a wrong data.folder,
      check that your data.folder is correct and that your deployment data is in .csv or .txt format)")

      depl<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if(!is.na(suppressWarnings(as.numeric(depl))) & suppressWarnings(as.numeric(depl)) %in% 1:length(dir(here::here(data.folder), pattern=c(".txt|.csv")))) { ##check validity of depl

        deployment<-read.table(here::here(data.folder,dir(here::here(data.folder), pattern=c(".txt|.csv"))[as.numeric(depl)]), sep=sep.type, header=T)

        cat("\n")
        print(head(deployment))


        repeat{
          cat("\n",crayon::bold("Is this the right file? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} ##end of check validity of depl
    } ##end of r1 bracket



    repeat{ ##r2 ask for start stop
      cat("\n",crayon::bold("Please enter the column number that contains the date and time for the BEGINNING of the deployment info [column number]"))
      start.d<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      cat("\n",crayon::bold("Please enter the column number that contains the date and time for the END of the deployment info [column number]"))
      stop.d<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if ((suppressWarnings(as.numeric(start.d)) %in% 1:as.numeric(ncol(deployment))) & (suppressWarnings(as.numeric(stop.d)) %in% 1:as.numeric(ncol(deployment)))){ ##check validity of start and stop

        print(deployment[1,c(as.numeric(start.d),as.numeric(stop.d))])


        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} ##end of check validity of start and stop
    } ##end of r2 bracket


    if(length(colnames(deployment)[which(colnames(deployment) %in% c("Start","Stop"))])>0){ ##this makes sure there arent any other colummns called start stop... It also renames the columns if they are the right ones, but we take care of it later
      colnames(deployment)[which(colnames(deployment) %in% c("Start","Stop"))]<-paste0("x.", colnames(deployment)[which(colnames(deployment) %in% c("Start","Stop"))])
    }

    colnames(deployment)[c(as.numeric(start.d),as.numeric(stop.d))]<-c("Start","Stop")

    print(head(deployment))



    repeat{ ##r4 to check for receiver ID
      cat("\n",crayon::bold("Please enter the column number that contains the receiver ID data [column number]"))
      cat("\n","(Note: This must match the format of the data you indicated in the detections file,
      See compileData())")

      receiv<-scan("",what="numeric",nmax=1,fill=T, quiet=T)
      if(suppressWarnings(as.numeric(receiv)) %in% 1:ncol(deployment)){

        print(deployment[1,c(as.numeric(receiv))])

        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} #end of receiv validity check
    } ##end of r4 bracket


    if(length(colnames(deployment)[which(colnames(deployment) %in% c("Receiver"))])>0){ ##this makes sure there arent any other colummns called Receiver... It also renames the columns if they are the right ones, but we take care of it later
      colnames(deployment)[which(colnames(deployment) %in% c("Receiver"))]<-paste0("x.", colnames(deployment)[which(colnames(deployment) %in% c("Receiver"))])
    }

    colnames(deployment)[c(as.numeric(receiv))]<-"Receiver"
    deployment$Receiver<-as.character(deployment$Receiver)
    print(head(deployment))




    repeat{ ##r6 to check for station name
      cat("\n",crayon::bold("Please enter the column number that contains the station name data [column number]"))
      cat("\n","(Note: This must match the format of the station name data in your spatial file)")

      stat.d<-scan("",what="numeric",nmax=1,fill=T, quiet=T)
      if(suppressWarnings(as.numeric(receiv)) %in% 1:ncol(deployment)){

        print(deployment[1,c(as.numeric(stat.d))])

        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} #end of stat.d valididty check
    } ##end of r6 bracket

    if(length(colnames(deployment)[which(colnames(deployment) %in% c("Station.name"))])>0){ ##this makes sure there arent any other colummns called Station.name... It also renames the columns if they are the right ones, but we take care of it later
      colnames(deployment)[which(colnames(deployment) %in% c("Station.name"))]<-paste0("x.", colnames(deployment)[which(colnames(deployment) %in% c("Station.name"))])
    }

    colnames(deployment)[c(as.numeric(stat.d))]<-"Station.name"
    deployment$Station.name<-as.character(deployment$Station.name)

  } ##end of ifelse deployment is already made



  deployment$Start<-lubridate::parse_date_time(deployment$Start, c("Ymd HMS", "ymd HMS","dmy HMS", "dmY HMS"), truncated = 3)
  deployment$Stop<-lubridate::parse_date_time(deployment$Stop, c("Ymd HMS", "ymd HMS","dmy HMS", "dmY HMS"), truncated = 3)

  deployment$Start<-as.POSIXct(deployment$Start, format="%Y-%m-%d %H:%M:%S")
  deployment$Stop<-as.POSIXct(deployment$Stop, format="%Y-%m-%d %H:%M:%S")


  print(head(deployment))



  if(length(file.dep)==0){
    repeat{ ##repeat 6a to save
      cat(" \n")
      cat(crayon::yellow("Here is your deployment data, does it look good ? [y]es or [n]o."))
      cat(" \n","Note: If one or more of your columns had a name that we use in the package we added 'x.' to its name to prevent errors)", " \n")
      cat(" \n")

      check<-scan("",what="character",nmax=1,fill=T, quiet=T)

      if (length(check)==1 & check %in% c("y","n")){
        if (check == "y"){
          cat("\n",crayon::bold("All good, the deployment data is saved as"),crayon::bold$cyan("'ATfiltR_deployment.csv'"), crayon::bold(" in your data folder so we can use it automatically later.", " \n"))
          write.table(deployment, here::here(data.folder, "ATfiltR_deployment.csv"), sep=",", row.names=F)
          break
        } else {
          cat("\n",crayon::bold("Sorry this did not work out! Aborting the process so you can start again!", " \n"))
          stop("Deployment data creation did not work properly...")
        }
      }
    }
  }

  deployment<<-deployment

  ##############################################
  ########## Loading spatial data ##############
  ##############################################

  cat("\n","\n",crayon::bold$underline$blue("Step 3: Loading your spatial data..."))
  cat("\n")
  cat("\n")

  file.spat <- dir(here::here(data.folder), pattern ="ATfiltR_spatial")

  if(length(file.spat)>0){

    cat("\n",crayon::bold("We found an ATfiltR spatial data file, you don't have anything to do, we are loading it..."))

    spatial<-read.table(here::here(data.folder,file.spat), sep=",", header=T)

  } else {

    print(data.frame(Number=1:length(dir(here::here(data.folder), pattern=c(".txt|.csv"))),File=dir(here::here(data.folder), pattern=c(".txt|.csv"))))

    repeat{ ##r8 for spatial data
      cat("\n",crayon::bold("No ATfiltR spatial file found,
                        Please enter the number of the file that contains your spatial data (see choices above)"))

      cat("\n","(Note: If you can't see it, you might be giving us a wrong data.folder,
      check that your data.folder is correct and that your spatial data is in .csv or .txt format)")

      spat<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if(!is.na(suppressWarnings(as.numeric(spat))) & suppressWarnings(as.numeric(spat)) %in% 1:length(dir(here::here(data.folder), pattern=c(".txt|.csv")))) { ##check validity of spat
        spatial<-read.table(here::here(data.folder,dir(here::here(data.folder), pattern=c(".txt|.csv"))[as.numeric(spat)]), sep=sep.type, header=T)

        print(head(spatial))


        repeat{
          cat("\n",crayon::bold("Is this the right file? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} ##end of check validity of spat
    } ##end of r8 bracket


    repeat{ ##r9 ask for longitude latitude
      cat("\n",crayon::bold("Please enter the column number that contains the Longitude info [column number]"))
      lon<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      cat("\n",crayon::bold("Please enter the column number that contains the Latitude info [column number]"))
      lat<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if ((suppressWarnings(as.numeric(lon)) %in% 1:as.numeric(ncol(spatial))) & (suppressWarnings(as.numeric(lat)) %in% 1:as.numeric(ncol(spatial)))){ ##check validity of lon and lat

        print(spatial[1,c(as.numeric(lon),as.numeric(lat))])

        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} ##end of check validity of lon and lat
    } ##end of r9 bracket


    if(length(colnames(spatial)[which(colnames(spatial) %in% c("Longitude","Latitude"))])>0){ ##this makes sure there arent any other colummns called Station.name... It also renames the columns if they are the right ones, but we take care of it later
      colnames(spatial)[which(colnames(spatial) %in% c("Longitude","Latitude"))]<-paste0("x.", colnames(spatial)[which(colnames(spatial) %in% c("Longitude","Latitude"))])
    }

    colnames(spatial)[c(as.numeric(lon))]<-"Longitude"
    colnames(spatial)[c(as.numeric(lat))]<-"Latitude"

    print(head(spatial))



    repeat{ ##r11 to check for receiver ID
      cat("\n",crayon::bold("Please enter the column number that contains the station name data [column number]"))
      cat("\n","(Note: This must match the format of the station name data in your deployment file)")

      stat.name<-scan("",what="numeric",nmax=1,fill=T, quiet=T)
      if(suppressWarnings(as.numeric(stat.name)) %in% 1:ncol(spatial)){

        print(spatial[1,c(as.numeric(stat.name))])

        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} #end of receiv valididty check
    } ##end of r11 bracket


    if(length(colnames(spatial)[which(colnames(spatial) %in% c("Station.name"))])>0){ ##this makes sure there arent any other colummns called Station.name... It also renames the columns if they are the right ones, but we take care of it later
      colnames(spatial)[which(colnames(spatial) %in% c("Station.name"))]<-paste0("x.", colnames(spatial)[which(colnames(spatial) %in% c("Station.name"))])
    }

    colnames(spatial)[c(as.numeric(stat.name))]<-"Station.name"
    spatial$Station.name<-as.character(spatial$Station.name)
    print(head(spatial))



    repeat{ ##r13 for the range category
      cat("\n",crayon::bold("Did you specify a range category column? If yes, enter the column number [column number], else enter [n]o"))

      range.cat<-scan("",what="character",nmax=1,fill=T, quiet=T)

      if (length(range.cat)==1 & (range.cat=="n")){

        if(length(colnames(spatial)[which(colnames(spatial) %in% c("Range.category"))])>0){ ##this makes sure there arent any other colummns called Range.category... It also renames the columns if they are the right ones, but we take care of it later
          colnames(spatial)[which(colnames(spatial) %in% c("Range.category"))]<-paste0("x.", colnames(spatial)[which(colnames(spatial) %in% c("Range.category"))])
        }
        spatial$Range.category<-"All"
        break

      } else if (length(range.cat)==1 & suppressWarnings(as.numeric(range.cat)) %in% 1:ncol(spatial)){

        print(spatial[1,c(as.numeric(range.cat))])

        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} #end of checking for the validity of the range cat

    } #end of r13bracket


    if(length(colnames(spatial)[which(colnames(spatial) %in% c("Range.category"))])>0){ ##this makes sure there arent any other colummns called Range.category... It also renames the columns if they are the right ones, but we take care of it later
      colnames(spatial)[which(colnames(spatial) %in% c("Range.category"))]<-paste0("x.", colnames(spatial)[which(colnames(spatial) %in% c("Range.category"))])
    }

    colnames(spatial)[c(as.numeric(range.cat))]<-"Range.category"
    spatial$Range.category<-as.character(spatial$Range.category)


  } ##end of ifelse spatial is already made



  print(head(spatial))


  if(length(file.spat)==0){
    repeat{ ##repeat 6a to save
      cat(" \n")
      cat(crayon::yellow("Here is your spatial data, does it look good ? [y]es or [n]o."))
      cat(" \n","Note: If one or more of your columns had a name that we use in the package we added 'x.' to its name to prevent errors)", " \n")

      cat(" \n")

      check<-scan("",what="character",nmax=1,fill=T, quiet=T)

      if (length(check)==1 & check %in% c("y","n")){
        if (check == "y"){
          cat("\n",crayon::bold("All good, the spatial data is saved as"), crayon::bold$cyan("'ATfiltR_spatial.csv'"), crayon::bold(" in your data folder so we can use it automatically later.", " \n"))
          write.table(spatial, here::here(data.folder, "ATfiltR_spatial.csv"), sep=",", row.names=F)
          break
        } else {
          cat("\n",crayon::bold("Sorry this did not work out! Aborting the process so you can start again!", " \n"))
          stop("Spatial data creation did not work properly...")
        }
      }
    }
  }

  spatial<<-spatial


  #############################################
  ########## Loading animal data ##############
  #############################################



  cat("\n","\n",crayon::bold$underline$blue("Step 4: Loading your animal data..."))
  cat("\n")
  cat("\n")

  file.animal <- dir(here::here(data.folder), pattern ="ATfiltR_animal")

  if(length(file.animal)>0){

    cat("\n",crayon::bold("We found an ATfiltR animal data file, you don't have anything to do, we are loading it..."))

    animal<-read.table(here::here(data.folder,file.animal), sep=",", header=T)

  } else {

    print(data.frame(Number=1:length(dir(here::here(data.folder), pattern=c(".txt|.csv"))),File=dir(here::here(data.folder), pattern=c(".txt|.csv"))))

    repeat{ ##r15 for animal data
      cat("\n",crayon::bold("No ATfiltR animal file found,
                        Please enter the number of the file that contains your animal data (see choices above)"))

      cat("\n","(Note: If you can't see it, you might be giving us a wrong data.folder,
      check that your data.folder is correct and that your animal data is in .csv or .txt format)")

      anim<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if(!is.na(suppressWarnings(as.numeric(anim))) & suppressWarnings(as.numeric(anim)) %in% 1:length(dir(here::here(data.folder), pattern=c(".txt|.csv")))) { ##check validity of spat
        animal<-read.table(here::here(data.folder,dir(here::here(data.folder), pattern=c(".txt|.csv"))[as.numeric(anim)]), sep=sep.type, header=T)

        print(head(animal))

        repeat{
          cat("\n",crayon::bold("Is this the right file? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} ##end of check validity of anim
    } ##end of r15 bracket




    repeat{ ##r16 ask for Transmitter
      cat("\n",crayon::bold("Please enter the column number that contains the Transmitter info [column number]"))
      cat("(Note: this has to match the transmitter info you indicated in your detection data)")
      trans<-scan("",what="numeric",nmax=1,fill=T, quiet=T)


      if ((suppressWarnings(as.numeric(trans)) %in% 1:as.numeric(ncol(animal)))){ ##check validity of  trans

        print(animal[1,c(as.numeric(trans))])

        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} ##end of check validity of trans
    } ##end of r16 bracket

    if(length(colnames(animal)[which(colnames(animal) %in% c("Transmitter"))])>0){ ##this makes sure there arent any other colummns called Transmitter... It also renames the columns if they are the right ones, but we take care of it later
      colnames(animal)[which(colnames(animal) %in% c("Transmitter"))]<-paste0("x.", colnames(animal)[which(colnames(animal) %in% c("Transmitter"))])
    }

    colnames(animal)[c(as.numeric(trans))]<-"Transmitter"
    animal$Transmitter<-as.character(animal$Transmitter)
    print(head(animal))




    repeat{ ##r18 ask for ID
      cat("\n",crayon::bold("Please enter the column number that contains the animal ID [column number]"))
      ID<-scan("",what="numeric",nmax=1,fill=T, quiet=T)


      if ((suppressWarnings(as.numeric(ID)) %in% 1:as.numeric(ncol(animal)))){ ##check validity of ID

        print(animal[1,c(as.numeric(ID))])

        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} ##end of check validity of ID
    } ##end of r18 bracket

    if(length(colnames(animal)[which(colnames(animal) %in% c("ID"))])>0){ ##this makes sure there arent any other colummns called ID... It also renames the columns if they are the right ones, but we take care of it later
      colnames(animal)[which(colnames(animal) %in% c("ID"))]<-paste0("x.", colnames(animal)[which(colnames(animal) %in% c("ID"))])
    }

    colnames(animal)[c(as.numeric(ID))]<-"ID"
    animal$ID<-as.character(animal$ID)

    print(head(animal))



    repeat{ ##r20 ask for anim.date
      cat("\n",crayon::bold("Please enter the column number that contains the Date info [column number]"))
      cat("\n","(Note: It can be in a simple date format or a date and time format, both will work)")
      anim.date<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if ((suppressWarnings(as.numeric(anim.date)) %in% 1:as.numeric(ncol(animal)))){ ##check validity of  anim.date

        print(animal[1,c(as.numeric(anim.date))])

        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} ##end of check validity of anim.date
    } ##end of r20 bracket


    if(length(colnames(animal)[which(colnames(animal) %in% c("Date"))])>0){ ##this makes sure there arent any other colummns called Date... It also renames the columns if they are the right ones, but we take care of it later
      colnames(animal)[which(colnames(animal) %in% c("Date"))]<-paste0("x.", colnames(animal)[which(colnames(animal) %in% c("Date"))])
    }

    colnames(animal)[c(as.numeric(anim.date))]<-"Date"

    print(head(animal))




    repeat{ ##r22 ask for tag status
      cat("\n",crayon::bold("Is there a column with the tag status ? If yes, enter the column number [column number], else enter [n]o"))
      cat("\n","\n","(Reminder: the tag status column allows us to distinguish the start of the tag
        and the subsequent recaptures of the animal. If you don't have a tag status column, we will
        consider that every row in you animal data frame is an start row (i.e., contains
        data on the animal on the day of tagging))")
      cat("\n","\n","(Note: the tag status column has 5 possible entries. 'Pre-TagStart' for captures before the animal is tagged,
      'TagStart' for when the animal is tagged. 'TagActive' for when the animal is resighted or recaptured and released alive with the transmitter.
        'TagStop' for when the animal is resighted or recaptured and the transmitter and/or the animal removed, 'Post-TagStop' for potential recpatures after the tag stop date.
        If your dataset is a little different we will help you change it to fit the package...)")


      tag.stat<-scan("",what="character",nmax=1,fill=T, quiet=T)


      if (length(check)==1 & tag.stat=="n"){
        cat("\n",crayon::bold("No tag status column. Creating one, with every row being 'TagStart'."))

        if(length(colnames(animal)[which(colnames(animal) %in% c("Tag.status"))])>0){ ##this makes sure there arent any other colummns called Date... It also renames the columns if they are the right ones, but we take care of it later
          colnames(animal)[which(colnames(animal) %in% c("Tag.status"))]<-paste0("x.", colnames(animal)[which(colnames(animal) %in% c("Tag.status"))])
        }
        animal$Tag.status<-"TagStart"
        tag.stat<-as.numeric(ncol(animal))

        break ##break r22
      } else if (length(check)==1 & suppressWarnings(as.numeric(tag.stat)) %in% 1:as.numeric(ncol(animal))){

        print(animal[1,c(as.numeric(tag.stat))])


        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} #end of validity check

    } ##end of r22 bracket


    if (any(!c(unique(as.character(animal[,c(as.numeric(tag.stat))]))) %in% c("Pre-TagStart","TagStart","TagActive","TagStop","Post-TagStop"))){
      cat("\n",crayon::bold("The content of the column is not 'Pre-TagStart','TagStart','TagActive','TagStop' or 'Post-TagStop'"))
      cat("\n","We will show you the contents of your column, for each of them, please indicate if they correspond to",
          "\n",crayon::bold("[1] Pre-TagStart"), "(Data collected before the animal is tagged)",
          "\n",crayon::bold("[2] TagStart"), "(Data on the day the animal is tagged), each animal you tagged MUST have an implantation!",
          "\n",crayon::bold("[3] TagActive"), "(A recapture or resigthing of the animal, released alive and with the transmitter)",
          "\n",crayon::bold("[4] TagStop"), "(A recapture or resigthing of the animal where the transmitter and/or the animal was removed, or the known end date of the tracking",
          "\n",crayon::bold("[5] Post-TagStop"), "(A recapture or resigthing of the animal after the transmitter was removed or turned off")

      if(length(colnames(animal)[which(colnames(animal) %in% c("Tag.status"))])>0){ ##this makes sure there arent any other colummns called Date... It also renames the columns if they are the right ones, but we take care of it later
        colnames(animal)[which(colnames(animal) %in% c("Tag.status"))]<-paste0("x.", colnames(animal)[which(colnames(animal) %in% c("Tag.status"))])

        for (i in 1:length(unique(animal[,c(as.numeric(tag.stat))]))){
          repeat{
            cat("\n",crayon::bold$yellow(unique(animal[,c(as.numeric(tag.stat))])[i]))

            cat("\n",crayon::bold("[1] Pre-TagStart, [2] TagStart, [3] TagActive, [4] TagStop or [5] Post-TagStop ?"))

            iter.tag<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

            if (length(iter.tag)==1 & suppressWarnings(as.numeric(iter.tag)) %in% c(1,2,3)){
              animal[which(animal[,c(as.numeric(tag.stat))] == unique(animal[,c(as.numeric(tag.stat))])[i]), "Tag.status"] <- c("Pre-TagStart","TagStart","TagActive","TagStop","Post-TagStop")[as.numeric(iter.tag)]
              break
            }
          }
        }
      }
    } else {

      if(length(colnames(animal)[which(colnames(animal) %in% c("Tag.status"))])>0){ ##this makes sure there arent any other colummns called Date... It also renames the columns if they are the right ones, but we take care of it later
        colnames(animal)[which(colnames(animal) %in% c("Tag.status"))]<-paste0("x.", colnames(animal)[which(colnames(animal) %in% c("Tag.status"))])
      }

      colnames(animal)[c(as.numeric(tag.stat))]<-"Tag.status"

    }


    repeat{ ##r18 ask for Locatiom
      cat("\n",crayon::bold("Is there a column corresponding to the identity of the location (correponding to the locations in the distance matrix for speed calculations), if yes, enter the [column number], otherwise enter [n]o"))
      loc<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if (length(loc)==1 & loc=="n"){break}

      if (length(loc)==1 & (suppressWarnings(as.numeric(ID)) %in% 1:as.numeric(ncol(animal)))){ ##check validity of ID

        print(animal[1,c(as.numeric(loc))])

        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} ##end of check validity of ID
    } ##end of r18 bracket

    if(length(colnames(animal)[which(colnames(animal) %in% c("Location"))])>0){ ##this makes sure there arent any other colummns called ID... It also renames the columns if they are the right ones, but we take care of it later
      colnames(animal)[which(colnames(animal) %in% c("Location"))]<-paste0("x.", colnames(animal)[which(colnames(animal) %in% c("Location"))])
    }

    if (loc=="n"){
      animal$Location<-"ATfiltR_animal"
    } else {
    colnames(animal)[c(as.numeric(loc))]<-"Location"}
    animal$Location<-as.character(animal$Location)

    print(head(animal))

  }  ##end of ifelse animal is already made



  animal$Date<-lubridate::parse_date_time(animal$Date, c("Ymd HMS", "ymd HMS","dmy HMS", "dmY HMS"), truncated = 3)

  animal$Date<-as.POSIXct(animal$Date, format="%Y-%m-%d %H:%M:%S")

  animal<-animal[order(animal$Date),]

  cat("\n")
  print(head(animal))


  if(length(file.animal)==0){
    repeat{ ##repeat 6a to save
      cat(" \n")
      cat(crayon::yellow("Here is your animal data, does it look good ? [y]es or [n]o."))
      cat(" \n","Note: If one or more of your columns had a name that we use in the package we added 'x.' to its name to prevent errors)", " \n")
      cat(" \n")

      check<-scan("",what="character",nmax=1,fill=T, quiet=T)

      if (length(check)==1 & check %in% c("y","n")){
        if (check == "y"){
          cat("\n",crayon::bold("All good, the animal data is saved as"), crayon::bold$cyan("'ATfiltR_animal.csv'"), crayon::bold("in your data folder so we can use it automatically later.", " \n"))
          write.table(animal, here::here(data.folder, "ATfiltR_animal.csv"), sep=",", row.names=F)
          break
        } else {
          cat("\n",crayon::bold("Sorry this did not work out! Aborting the process so you can start again!", " \n"))
          stop("Animal data creation did not work properly...")
        }
      }
    }
  }

  animal<<-animal


  #########################################################################################
  ########## Checking the data that is within the receiver deployment window ##############
  #########################################################################################



  cat("\n","\n",crayon::bold$underline$blue("Step 5: Filtering the detections that are outside of the receiver deployment window..."))
  cat("\n")
  cat("\n")


  for (i in 1:nrow(deployment)){

    ATfiltR_data.1[Receiver == deployment[i,"Receiver"] &
                     Date.and.Time > deployment[i,"Start"] &
                     Date.and.Time < deployment[i,"Stop"],`:=` (Station.name = deployment[i,"Station.name"],
                                                                Latitude = spatial[which(spatial$Station.name==deployment[i,"Station.name"]), c("Latitude")],
                                                                Longitude = spatial[which(spatial$Station.name==deployment[i,"Station.name"]), c("Longitude")],
                                                                Range.category = spatial[which(spatial$Station.name == deployment[i,"Station.name"]), "Range.category"])]


    cat(crayon::bold("Checking deployment window and attributing spatial coordinates:"), crayon::bold$cyan(i), crayon::bold("/",nrow(deployment), " \r"))
  }
  cat("\n")
  cat("\n")
  out.of.deployment<<-ATfiltR_data.1[which(is.na(ATfiltR_data.1$Station.name)),]
  cat(crayon::cyan(nrow(out.of.deployment)), "detections out of the deployment window... (out of ",crayon::cyan(nrow(ATfiltR_data.1)),")", " \n")

  if (save.out.of.deployment==TRUE){
    cat("Saving the data obtained outside of the deployment window...", " \n")
    save(out.of.deployment, file=here::here(detection.folder, paste0("ATfiltR_out.of.deployment_", Sys.Date(),".Rdata")))
    cat(crayon::bold("File saved in your Detections folder under"), crayon::bold$cyan(paste0("out.of.deployment_", Sys.Date(),".txt")), " \n")}


  if (nrow(out.of.deployment)>0){
    cat(crayon::bold("Removing the data obtained outside of the deployment window...", " \n"))
    #ATfiltR_data.1<-ATfiltR_data.1[-which(is.na(ATfiltR_data.1$Station.name)),]
    ATfiltR_data.1<-na.omit(ATfiltR_data.1, cols="Station.name")}




  #########################################################################################
  ########## Checking the data that is within the tag deployment window ##############
  #########################################################################################


  cat("\n","\n",crayon::bold$underline$blue("Step 6: Filtering the detections that are outside of the transmitter deployment window..."))
  cat("\n")
  cat("\n")

  repeat{ ##r20 ask for anim.date
    cat("\n",crayon::bold("Is there any data that you would like to transfer to the detection data?
                          Enter [column numbers] one by one then validate with [enter][enter]
                          Or just type [enter][enter] if you don't want any extra data transfered..."))
    cat("\n","(Note: The animal ID is automatically transferred. 'Date', 'Longitude', 'Latitude', 'Transmitter' are already inherently part of the detections data.
    We strongly recommend to tranfer any length data you will want to use for speed based filtering later)")
    cat("\n")
    cat("\n")
    print(data.frame(Column.number=which(!colnames(animal) %in% c("ID","Transmitter","Longitude","Latitude","Date","Location")),
                     Column.name=colnames(animal)[which(!colnames(animal) %in% c("ID","Transmitter","Longitude","Latitude","Date","Location"))]))

    to.transfer<-scan("",what="numeric",nmax=ncol(animal),fill=T, quiet=T)

    if (length(to.transfer) == 0){break}

    if (all(suppressWarnings(as.numeric(to.transfer)) %in% which(!colnames(animal) %in% c("ID","Transmitter","Longitude","Latitude","Date","Location")))){

      cat(colnames(animal)[as.numeric(to.transfer)])

      repeat{
        cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
        check<-scan("",what="character",nmax=1,fill=T, quiet=T)

        if (length(check)==1 & check %in% c("y","n")){
          break
        }
      }

      if (check=="y"){
        to.transfer<-as.numeric(to.transfer)
        break}
    }
  }


  repeat{
    cat("\n")
    cat("\n",crayon::bold("Should the animal data become data points in your detections? [y]es or [n]o"))
    cat("\n","Every row in the animal data will be used to create detections.
        The newly created detections will inherit the 'Date', 'Longitude', 'Latitude', 'Transmitter', 'ID', and all other data you chose to transfer,
        but the 'Station.name' will be the 'Location'")

    check<-scan("",what="character",nmax=1,fill=T, quiet=T)

    if (length(check)==1 & check %in% c("y","n")){
      break
    }
  }

  if (check=="y"){
    new.detect<-TRUE
  } else {new.detect<-FALSE }



  for (i in 1:nrow(animal)){

    if (animal[i,"Tag.status"]=="Pre-TagStart"){
      ATfiltR_data.1[Transmitter == animal[i,"Transmitter"] &
                       Date.and.Time >= animal[i,"Date"],ID := NA]

      if (length(to.transfer) > 0){
        ATfiltR_data.1[Transmitter == animal[i,"Transmitter"] &
                         Date.and.Time >= animal[i,"Date"], colnames(animal)[to.transfer] := animal[i,to.transfer]]
      }

      else if (animal[i,"Tag.status"]=="TagStart"){
      ATfiltR_data.1[Transmitter == animal[i,"Transmitter"] &
                       Date.and.Time >= animal[i,"Date"]+24*3600,ID := animal[i,"ID"]]

      if (length(to.transfer) > 0){
        ATfiltR_data.1[Transmitter == animal[i,"Transmitter"] &
                         Date.and.Time >= animal[i,"Date"]+24*3600, colnames(animal)[to.transfer] := animal[i,to.transfer]]
      }

    }
    else if (animal[i,"Tag.status"]=="TagActive"){
      ATfiltR_data.1[Transmitter == animal[i,"Transmitter"] &
                       Date.and.Time >= animal[i,"Date"],ID := animal[i,"ID"]]

      if (length(to.transfer) > 0){
        ATfiltR_data.1[Transmitter == animal[i,"Transmitter"] &
                         Date.and.Time >= animal[i,"Date"], colnames(animal)[to.transfer] := animal[i,to.transfer]]}

    } else if (animal[i,"Tag.status"]=="TagStop"){
      ATfiltR_data.1[Transmitter == animal[i,"Transmitter"] &
                       Date.and.Time == animal[i,"Date"],ID := animal[i,"ID"]]
      ATfiltR_data.1[Transmitter == animal[i,"Transmitter"] &
                       Date.and.Time > animal[i,"Date"],ID := NA]

      if (length(to.transfer) > 0){
        ATfiltR_data.1[Transmitter == animal[i,"Transmitter"] &
                         Date.and.Time == animal[i,"Date"], colnames(animal)[to.transfer] := animal[i,to.transfer]]
      }

    }
    }
    cat(crayon::bold("Attributing tagged animals to their respective detections:"), crayon::bold$cyan(i), crayon::bold("/", nrow(animal), " \r"))
  }
  cat("\n")
  cat("\n")
  cat("Out of", crayon::cyan(length(unique(animal$ID))), "animals tagged, only", crayon::cyan(length(unique(ATfiltR_data.1$ID))), "produced data...", " \n")


  if (new.detect==TRUE){
    cat("\n")
    cat("\n")
    cat(crayon::bold("Adding the animal data as data.points in your detections."))
    adding<-animal[which(animal$ID %in% unique(ATfiltR_data.1$ID)),]
    colnames(adding)[which(colnames(adding)=="Date")]<-"Date.and.Time"
    colnames(adding)[which(colnames(adding)=="Location")]<-"Station.name"
    adding<-adding[,which(colnames(adding) %in% colnames(ATfiltR_data.1))]
    adding[,colnames(ATfiltR_data.1)[-which(colnames(ATfiltR_data.1) %in% colnames(adding))]]<-NA
    adding<-adding[,colnames(ATfiltR_data.1)]
    adding<-data.table::as.data.table(adding)

    ATfiltR_data.1<-rbind(adding,ATfiltR_data.1)

    ATfiltR_data.1<-ATfiltR_data.1[order(ATfiltR_data.1$Date.and.Time),]
  }



  unknown.tags<<-ATfiltR_data.1[!Transmitter %in% unique(animal$Transmitter),]
  cat("\n")
  cat("\n")
  cat(crayon::cyan(nrow(unknown.tags)), "detections from unknown tags...", " \n")

  if (save.unknown.tags==TRUE){
    cat("You indicated 'save.unknown.tags=T'. Saving the data from tags that don't belong to you...", " \n")
    save(unknown.tags, file=here::here(detection.folder, paste0("ATfiltR_unknown.tags_", Sys.Date(),".RData")))
    cat(crayon::bold("File saved in your Detections folder under"), crayon::cyan$bold(paste0("ATfiltR_unknown.tags_", Sys.Date(),".RData"), " \n"))}
  cat("\n")
  cat("\n")
  cat("Removing the unknown tags...", " \n")

  if (nrow(unknown.tags)>0){
    ATfiltR_data.1<-ATfiltR_data.1[Transmitter %in% unique(animal$Transmitter),]}
  ATfiltR_data.1$within.window<-"Yes"

  ATfiltR_data.2<<-ATfiltR_data.1

  if (save==TRUE){
    cat("\n")
    cat("\n")
    cat("You indicated 'save=T'. Saving the compiled file after within window filtering...", " \n")
    save(ATfiltR_data.2, file=here::here(detection.folder, paste0("ATfiltR_data.2_", Sys.Date(),".RData")))
    cat(crayon::bold("File saved in your Detections folder under"), crayon::cyan$bold(paste0("ATfiltR_data.2_", Sys.Date(),".RData"), " \n"))}


  cat("\n")
  cat("\n")
  cat(crayon::bold$yellow("End of process for the within window filtering. The filtered detections are in your R environment under ATfiltR_data.2 !"))
  cat(crayon::bold$yellow("This took approximately", paste(round(difftime(Sys.time(), start, units="min"))), "minutes!"," \n"))

} ##end of function
