#' @title speedCheck
#'
#' @description Filters out detections that occur faster than the animal's speed (user provided)
#'
#'
#' @param detection.folder The name of the folder containing the detection data
#' @param data.folder The name of the folder containing auxillary data (e.g. spatial data). Can be se same af detection.folder
#' @param receiver.range Range of the receivers in meters (if the range is the same for all receivers and the whole study duration). Keep as NA if your range data is stored in a separate file (i.e. if you have different ranges for your stations).
#' @param base The base of the equation you use for speed (in m/s). For instance if the speed is calculated is speed=2*body_length^0.015, the base is 2. If the speed is the same for all fish, for instance 10m/s, the base is 10.
#' @param factor The column name for the data to use in the speed calculation. For instance is speed=2*body_length^0.015, indicate the name of the column that contains the necessary bodylength data, in quotation marks.
#' If the speed doesn't depend on data in your dataset, keep as NA.
#' @param exponent The exponent for the factor you indicated in your speed calculation. For instance for speed=2*body_length^0.015, expononent is equal to 0.015
#' @param max.distance Maximum distance (in meters) that an animal may move, and beyond which the detections should be removed. Default: NA
#' @param save.speedy Should the detections that happened too fast be saved in the detections folder? (TRUE or FALSE)
#' @param save Should the data be saved in your detections folder (TRUE or FALSE)
#'
#' @return A data frame object that contains the detections from your tags, filtered for speed, and another that contains the detections that were found to be occur too fast according to the user inputed speed.
#'
#' @examples
#' \dontrun{
#' # Before running the function, you must be in an R project that contains
#' your detections folder and data folder your data must be compiled via
#' compileData(), and filteres within withinWindow() and findSolo()
#' speedCheck(detection.folder="Detections", data.folder="Data",
#' receiver.range=NA, base=3600, factor=NA, exponent=NA, max.distance=NA,
#' save.speedy=TRUE, save=TRUE)
#' }
#' @export
#'
#' @importFrom here here
#' @import crayon
#' @importFrom lubridate parse_date_time
#' @importFrom data.table as.data.table
#'
#' @importFrom utils View read.table write.table head
#'
#'


############################################################################
############################################################################
############################ SPEED CHECK ###################################
############################################################################
############################################################################

speedCheck<-function(detection.folder="Detections", data.folder="Data",
                     receiver.range=NA, base=1000, factor=NA, exponent=NA,
                     max.distance=NA, save.speedy=TRUE, save=TRUE, project=TRUE,
                     data.file="data", ID.col="ID", DateTime.col="Date.and.Time",
                     Station.col="Station.name"){

  cat("\n","\n",crayon::bold$yellow("ATfiltR speedCheck(): filtering detections that happen at an unreasonable speed."))
  cat("\n")

  start<-Sys.time()

  ##############################################
  ########## Defining the objects ##############
  ##############################################

  range.df<-NA #the file that contains range data
  ran<-NA #the column that contains the range data
  t.step<-NA #column with the time step
  r.cat<-NA #column with the range category

  ################################################
  ########## Loading ATfiltR_data.3 ##############
  ################################################


if (project==T){
  loadSpeed(detection.folder=detection.folder)
}  else {
  cat("\n",crayon::bold$underline$blue("Step 1: You are working outside of a project (project=F), let's check that your data is formatted properly..."))
  cat("\n")
  cat("\n")

  start<-Sys.time()

  ATfiltR_data.3<-as.data.frame(get(data.file))

  ATfiltR_data.3[,"ID"] <- as.character(ATfiltR_data.3[,which(colnames(ATfiltR_data.3)==ID.col )])

  ATfiltR_data.3[,"Date.and.Time"] <- as.character(ATfiltR_data.3[,which(colnames(ATfiltR_data.3)==DateTime.col) ])

  ATfiltR_data.3[,"Date.and.Time"] <- lubridate::parse_date_time(ATfiltR_data.3$Date.and.Time, c("Ymd HMS", "ymd HMS","dmy HMS", "dmY HMS"), truncated = 3)
  ATfiltR_data.3[,"Date.and.Time"] <- as.POSIXct(ATfiltR_data.3$Date.and.Time, format="%Y-%m-%d %H:%M:%S")

  ATfiltR_data.3[,"Station.name"] <- as.character(ATfiltR_data.3[,which(colnames(ATfiltR_data.2)==Station.col) ])


  cat("Ordering the data chronologically...", " \n")

  ATfiltR_data.3<-ATfiltR_data.3[order(ATfiltR_data.3$Date.and.Time),]
  ATfiltR_data.3<-data.table(ATfiltR_data.3)
  ATfiltR_data.3<<-ATfiltR_data.3
}


  ################################################
  ########## Getting the range data ##############
  ################################################

  cat("\n","\n",crayon::bold$underline$blue("Step 2: Preparing the range data and distance matrix..."))
  cat("\n")
  cat("\n")


  if(!is.na(suppressWarnings(as.numeric(receiver.range)))) {
    cat("Range of", crayon::cyan(receiver.range),"meters, as per user command. If this is not correct, [escape] and start again.", " \n")
    Category<-"All"
    Range.m<-receiver.range
    Time.step<-"All"
    range<-as.data.frame(cbind(Category, Range.m, Time.step))

    ATfiltR_data.3$Range.category<-"All"


  } else {
    if (project==T){
    print(data.frame(Number=1:length(dir(here::here(data.folder), pattern=c(".txt|.csv"))),File=dir(here::here(data.folder), pattern=c(".txt|.csv"))))
    } else {
      print(data.frame(Number=1:length(dir(getwd(), pattern=c(".txt|.csv"))),File=dir(getwd(), pattern=c(".txt|.csv"))))
}
    repeat{ ##r1 for range data
      cat("\n",crayon::bold(" Please enter the number of the file that contains your range data (see choices above)"))

      cat("\n","(Note: If you can't see it, you might be giving us a wrong data.folder,
      check that the data.folder is correct and that your range data is in .csv or .txt format)")

      range.df<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if(!is.na(suppressWarnings(as.numeric(range.df))) & suppressWarnings(as.numeric(range.df)) %in% 1:length(dir(here::here(data.folder), pattern=c(".txt|.csv")))) { ##check validity of depl

        if(project==T){
        range<-read.table(here::here(data.folder,dir(here::here(data.folder), pattern=c(".txt|.csv"))[as.numeric(range.df)]), sep=",", header=T)
        }else{
        range<-read.table(paste0(getwd(),"/",dir(getwd(), pattern=c(".txt|.csv"))[as.numeric(range.df)]), sep=",", header=T)
}
        cat("\n")
        print(head(range))


        repeat{
          cat("\n",crayon::bold("Is this the right file? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }
        if (check=="y"){break}} ##end of check validity of depl
    } ##end of r1 bracket



    ######################################
    ########## Range column ##############
    ######################################


    repeat{ ##r2 to get the range
      cat("\n",crayon::bold("Please enter the column number that contains the range [column number]"))

      ran<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if (is.na(ran)){


      }
      if(suppressWarnings(as.numeric(ran)) %in% 1:ncol(range)){

        print(range[1,c(as.numeric(ran))])

        repeat{
          cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){break}} #end of receiv validity check
    } ##end of r4 bracket

    repeat {
      cat("\n",crayon::bold("Is the range in meter? [y]es or [n]o"))
      check<-scan("",what="character",nmax=1,fill=T, quiet=T)
      if (length(check)==1 & check %in% c("y","n")){
        break
      }

    }

    if(check=="n"){

      repeat {
        cat("\n",crayon::bold("Please enter a multiplier (numeric) to convert your range to meters. For instance if your data is in km, enter [1000], if it is in cm, enter [0.01]"))
        mult<-scan("",what="numeric",nmax=1,fill=T, quiet=T)
        if (length(check)==1 & !is.na(suppressWarnings(as.numeric(mult)))){

          print(range[1,c(as.numeric(ran))]*mult)

          repeat{
            cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
            check1<-scan("",what="character",nmax=1,fill=T, quiet=T)

            if (length(check1)==1 & check1 %in% c("y","n")){
              break
            }
          }

          if (check1=="y"){break}
        }

      }
    } ##end of if check ==n

    if(length(colnames(range)[which(colnames(range) %in% c("range.m"))])>0){ ##this makes sure there arent any other colummns called Receiver... It also renames the columns if they are the right ones, but we take care of it later
      colnames(range)[which(colnames(range) %in% c("range.m"))]<-paste0("x.", colnames(range)[which(colnames(range) %in% c("Range.m"))])
    }

    colnames(range)[c(as.numeric(ran))]<-"Range.m"

    print(head(range))




    ##########################################
    ########## Time step column ##############
    ##########################################


    repeat{ ##r4 to get the time step
      cat("\n",crayon::bold("Please enter the column number that contains the time step [column number], if there are no time steps, just press [enter][enter]"))

      t.step<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if (is.na(t.step)){

        if(length(colnames(range)[which(colnames(range) %in% c("Time.step"))])>0){ ##this makes sure there arent any other colummns called Receiver... It also renames the columns if they are the right ones, but we take care of it later
          colnames(range)[which(colnames(range) %in% c("Time.step"))]<-paste0("x.", colnames(range)[which(colnames(range) %in% c("Time.step"))])
        }

        range$Time.step<-"All"

        cat("\n","No time, we use the same range for the whole data.")

        break

      } else {

        if(suppressWarnings(as.numeric(t.step)) %in% 1:ncol(range)){

          print(range[1,c(as.numeric(t.step))])

          repeat{
            cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
            check<-scan("",what="character",nmax=1,fill=T, quiet=T)

            if (length(check)==1 & check %in% c("y","n")){
              break
            }
          }

          if (check=="y"){
            if(length(colnames(range)[which(colnames(range) %in% c("Time.step"))])>0){ ##this makes sure there arent any other colummns called Receiver... It also renames the columns if they are the right ones, but we take care of it later
              colnames(range)[which(colnames(range) %in% c("Time.step"))]<-paste0("x.", colnames(range)[which(colnames(range) %in% c("Time.step"))])
            }

            colnames(range)[c(as.numeric(t.step))]<-"Time.step"

            break}} #end of receiv validity check
      } #end of if is na time step
    } ##end of repeat for time step

    print(head(range))

    if (range$Time.step[1] !="All"){ ## find out the format if the time.step isn't ALL

      repeat{ ##r4 to get the time step
        cat("\n",crayon::bold("In what format is your time step?
                            We use the standard R date formats, so for instance, if the data is '10-2020' for october 2020, the format is ['%m-%Y']"))
        cat("\n","(Note: the time format must be indicated between quotation marks)")
        time.format<-scan("",what="character",nmax=1,fill=T, quiet=T)
        if(length(time.format)==1){

          print(time.format)

          repeat{
            cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
            check<-scan("",what="character",nmax=1,fill=T, quiet=T)

            if (length(check)==1 & check %in% c("y","n")){
              break
            }
          }

          if (check=="y"){break}} #end of receiv validity check
      } ##end of r4 bracket

    } ##end of if it is "all"



    #########################################
    ########## Category column ##############
    #########################################



    repeat{ ##r4 to get the range category
      cat("\n",crayon::bold("Please enter the column number that contains the range category [column number], if there are no time steps, just press [enter][enter]"))

      r.cat<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if (is.na(r.cat)){

        if(length(colnames(range)[which(colnames(range) %in% c("Category"))])>0){ ##this makes sure there arent any other colummns called Receiver... It also renames the columns if they are the right ones, but we take care of it later
          colnames(range)[which(colnames(range) %in% c("Category"))]<-paste0("x.", colnames(range)[which(colnames(range) %in% c("Category"))])
        }

        range$Category<-"All"

        cat("\n","No time.")

        break

      } else {
        if(suppressWarnings(as.numeric(r.cat)) %in% 1:ncol(range)){

          print(range[1,c(as.numeric(r.cat))])

          repeat{
            cat("\n",crayon::bold("Is this right? [y]es or [n]o"))
            check<-scan("",what="character",nmax=1,fill=T, quiet=T)

            if (length(check)==1 & check %in% c("y","n")){
              break
            }
          }

          if (check=="y"){break}} #end of receiv validity check
      } ##end of if else
    }##end of r4 bracket

    if(length(colnames(range)[which(colnames(range) %in% c("Category"))])>0){ ##this makes sure there arent any other colummns called Receiver... It also renames the columns if they are the right ones, but we take care of it later
      colnames(range)[which(colnames(range) %in% c("Category"))]<-paste0("x.", colnames(range)[which(colnames(range) %in% c("Category"))])
    }

    colnames(range)[c(as.numeric(r.cat))]<-"Category"

    print(head(range))


  } ##end of if else to make the range data

  cat("\n","Attributing the time step...", "\n")
  if (range$Time.step[1] == "All"){
    ATfiltR_data.3[,Time.step := "All"]
  } else {
    ATfiltR_data.3[,Time.step := as.character(format(ATfiltR_data.3[,"Date.and.Time"], time.format))]
  }




  #####################################################
  ########## Loading the distance matrix ##############
  #####################################################

  cat("Loading and preparing the distance matrix...", "\n")

  if(project==T){
  print(data.frame(Number=1:length(dir(here::here(data.folder), pattern=c(".txt|.csv"))),File=dir(here::here(data.folder), pattern=c(".txt|.csv"))))
  }else{
  print(data.frame(Number=1:length(dir(getwd(), pattern=c(".txt|.csv"))),File=dir(getwd(), pattern=c(".txt|.csv"))))
  }
  repeat{ ##r1 for range data
    cat("\n",crayon::bold(" Please enter the number of the file that contains your distance matrix (see choices above)"))

    cat("\n","(Note: If you can't see it, you might be giving us a wrong data.folder,
      check that the data.folder is correct and that your distance matrix is in .csv or .txt format)")

    cat("\n","(Note 2: If you don't have a distance matrix, you can manually create it or use distancesMatrix() from the package 'actel',
        making sure to set actel=F and saving it using write.table() with row.names=T. See ?distancesMatrix for more info (after having installed actel))")

    distmat<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

    if(!is.na(suppressWarnings(as.numeric(distmat))) & suppressWarnings(as.numeric(distmat)) %in% 1:length(dir(here::here(data.folder), pattern=c(".txt|.csv")))) { ##check validity of depl
if (project==T){
      distances<-read.table(here::here(data.folder,dir(here::here(data.folder), pattern=c(".txt|.csv"))[as.numeric(distmat)]), sep=",", header=T, check.names = FALSE)
} else{
  distances<-read.table(paste0(getwd(),"/",dir(getwd(), pattern=c(".txt|.csv"))[as.numeric(distmat)]), sep=",", header=T, check.names = FALSE)
}

      cat("\n")
      distances<-as.matrix(distances)
      print(head(distances))


      repeat{
        cat("\n",crayon::bold("Is this the right file? [y]es or [n]o"))
        check<-scan("",what="character",nmax=1,fill=T, quiet=T)

        if (length(check)==1 & check %in% c("y","n")){
          break
        }
      }

      if (check=="y"){

        distances[lower.tri(distances, diag = TRUE)]<-NA
        distances<-as.matrix(distances)
        dist.long<-data.table(
          row = rep(row.names(distances), ncol(distances)),
          col = rep(colnames(distances), each = nrow(distances)),
          value = as.vector(distances)
        )
        dist.long<-dist.long[-which(is.na(dist.long[,3])),]

        break}} ##end of check validity of depl

  } ##end of r1 bracket


  cat("Calculating swimming speeds... in meter per second", "\n")

  if (is.na(factor) & is.na(exponent)){
    ATfiltR_data.3[,Swim.speed := eval(base)]

  } else if (!is.na(factor)){
    cat("You indicated that speed is to be calculated based on the animal's size in the column:", factor, "\n")
    print(ATfiltR_data.3[1,eval(factor), with=FALSE])

    repeat{
      cat("Please indicate by how much this needs to be multiplied for your results to be in meters", "\n")
      cat("Note: if the size you indicated is in m, indicate [1], if it is in cm, indicate [0.01], if it is in mm, indicate [0.001], etc.",  "\n")
      cat("Note 2: In some instances, formulas in papers take the body size in mm, to calculate speed in m/h, in this case, you should leave your bodysize in mm...",  "\n")

      convert<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

      if(!is.na(suppressWarnings(as.numeric(convert)))) { ##check validity of comvert

        cat("\n")
        print(ATfiltR_data.3[1,eval(factor), with=FALSE])
        print(ATfiltR_data.3[1,eval(factor), with=FALSE]*as.numeric(convert))


        repeat{
          cat("\n",crayon::bold("Here is your column before and after conversion. Is it correct (in meters)? [y]es or [n]o"))
          check<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check)==1 & check %in% c("y","n")){
            break
          }
        }

        if (check=="y"){
          ATfiltR_data.3[,BL.calc := ATfiltR_data.3[,eval(factor), with=FALSE]*as.numeric(convert)]
          break}} ##end of check validity of depl

    } ##end of r1 bracket


    if (is.na(exponent)) {
      ATfiltR_data.3[,Swim.speed := eval(base)*ATfiltR_data.3[,BL.calc]]
    } else {
      ATfiltR_data.3[,Swim.speed := eval(base)*(ATfiltR_data.3[,BL.calc]^eval(exponent))]
    }
  }


  if (!is.na(factor)){
    repeat {
      cat(crayon::bold("Speed calculated: Please make sure these results seem to make sense", "\n"))

      cat("For a fish of bodylength", crayon::cyan(paste(ATfiltR_data.3[1,BL.calc])), " the speed in meters per hour was calculated at:", crayon::cyan(ATfiltR_data.3[1,Swim.speed]*3600), "\n", "\n")

      cat("Is this reasonable? [y]es or [n]o")

      cat(crayon::italic("(Note: If it isn't we will abort the calculations, so you can correct your formula)"), "\n")

      check<-scan("",what="character",nmax=1,fill=T, quiet=T)

      if (length(check)==1 & check %in% c("y","n")){
        break
      }

    }

    if (check=="n"){stop("Speeds do not make sense, please double check your formula")}
  }

  ##FILTERING##
  these.animals<-unique(ATfiltR_data.3$ID)
  reduced.dist<-dist.long
  reduced.range<-range
  ATfiltR_data.3[,Swim.speed :=(ATfiltR_data.3[,Swim.speed]*3600)]
  ATfiltR_speedy.data<-data.table::data.table(a=NA, b=NA)
  t<-0

  # ATfiltR_data.3[, Previous.station := character()]
  # ATfiltR_data.3[, Previous.range.category := character()]
  ignore.stations<-unique(ATfiltR_data.3$Station.name)[-which(unique(ATfiltR_data.3$Station.name) %in% colnames(distances))]

  if (length(ignore.stations) == 0){
    ignore.stations<-"Padfoot"
  } else {
    cat("\n","Stations", crayon::cyan(paste(ignore.stations)), "will be ignored for the speed calculation, as they do not appear in the distance matrix", "\n")
  }

  cat("\n")

  ATfiltR_data.3[, Range.here := 500]
  ATfiltR_data.3[, Range.previous := 500]

  repeat{


    if (t==0) {
      error.number<-10000

      cat("\n","\n",crayon::bold$underline$blue("Step 3: SPEED CHECK, when an animal is too fast getting somewhere the detection is removed (and stored elsewhere)"))
      cat("\n","(Note: Sit back and relax, this an iterative process and it does take a while, once detections have been removed, speeds must be re-calculated)")
      cat("\n","(Calculations will get faster and faster: We only remake calculations on pairs of receivers that have had speed errors on the previous iteration)")
      cat("\n")
      cat("\n")


    }
    cat("", " \n")



      for (i in 1:length(these.animals)){

        cat(" ","Calculating time between detections for each animal:", crayon::cyan(round(i/length(these.animals)*100)), "%", " \r")

        if (ATfiltR_data.3[ID == these.animals[i], .N]<2) {
          next
        } else {


          ATfiltR_data.3[ID == these.animals[i] &
                           !Station.name %in% ignore.stations, Time.after := data.table::shift(ATfiltR_data.3[ID == these.animals[i] &
                                                                                                                !Station.name %in% ignore.stations, Date.and.Time], 1, type="lead")]
          ATfiltR_data.3[ID == these.animals[i] &
                           !Station.name %in% ignore.stations, Time.before := data.table::shift(ATfiltR_data.3[ID == these.animals[i] &
                                                                                                                 !Station.name %in% ignore.stations, Date.and.Time], 1)]

        }
      }
      cat("\n")

      ATfiltR_data.3[, Lag.before := as.numeric(difftime(ATfiltR_data.3$Date.and.Time, ATfiltR_data.3$Time.before, units="hours"))]
      ATfiltR_data.3[, Lag.after := as.numeric(difftime(ATfiltR_data.3$Time.after, ATfiltR_data.3$Date.and.Time, units="hours"))] ##calculate the delays




    for (i in 1:length(these.animals)){

      cat(" ","Attributing the previous station and associated range category:", crayon::cyan(round(i/length(these.animals)*100)), "%", " \r")

      if (ATfiltR_data.3[ID == these.animals[i], .N]<2) {
        next
      } else {

        ATfiltR_data.3[ID == these.animals[i] &
                         !Station.name %in% ignore.stations, Previous.station := data.table::shift(ATfiltR_data.3[ID == these.animals[i] &
                                                                                                                    !Station.name %in% ignore.stations, Station.name], 1)]
        ATfiltR_data.3[ID == these.animals[i] &
                         !Station.name %in% ignore.stations, Previous.range.category := data.table::shift(ATfiltR_data.3[ID == these.animals[i] &
                                                                                                                           !Station.name %in% ignore.stations, Range.category], 1)]
      }
    }
    cat("\n")

    #now we add the distance between receivers (the previous and the current station)
    for (i in 1:nrow(reduced.dist)){
      cat(" ","Getting distances between stations:", crayon::cyan(round(i/nrow(reduced.dist)*100)), "%"," \r")

      ATfiltR_data.3[.(reduced.dist[i,1],reduced.dist[i,2]), Distance.raw := reduced.dist[i,3], on=.(Station.name,Previous.station)]

      ATfiltR_data.3[.(reduced.dist[i,2],reduced.dist[i,1]), Distance.raw := reduced.dist[i,3], on=.(Station.name,Previous.station)]

    }
    cat(" \n")




    for (i in 1:nrow(reduced.range)){
      cat(" ","Adding the appropriate ranges", crayon::cyan(round(i/nrow(reduced.range)*100)), "%", " \r")

      ATfiltR_data.3[.(reduced.range[i,"Category"], reduced.range[i,"Time.step"]), Range.here := reduced.range[i,"Range.m"], on=.(Range.category,Time.step)]

      ATfiltR_data.3[.(reduced.range[i,"Category"], reduced.range[i,"Time.step"]), Range.previous := reduced.range[i,"Range.m"], on=.(Previous.range.category,Time.step)]

    }
    cat(" \n")

    ATfiltR_data.3[,Distance.full:= ATfiltR_data.3$Distance.raw-ATfiltR_data.3$Range.here-ATfiltR_data.3$Range.previous]

    ATfiltR_data.3[Distance.full<0, Distance.full := NA] ##if ranges overlap (negative distance), we don't need the speed, so I remove it here

    ## Then I calculate the travel speed from one receiver to the next

    ATfiltR_data.3[, Travel.speed := ATfiltR_data.3$Distance.full/ATfiltR_data.3$Lag.before]

    ATfiltR_data.3[Travel.speed > Swim.speed, Speed.error := as.numeric(NA)]
    ATfiltR_data.3[Travel.speed > Swim.speed, Speed.error := ATfiltR_data.3[Travel.speed > Swim.speed,Travel.speed]-ATfiltR_data.3[Travel.speed > Swim.speed,Swim.speed]]

    ##now we need to find if the errors are repeated

    for (i in 1:length(these.animals)){

      cat(" ","Attributing previous error status to only treat the first of a series of errors:", crayon::cyan(round(i/length(these.animals)*100)), "%", " \r")

      if (ATfiltR_data.3[ID == these.animals[i], .N]<2) {
        next
      } else {
        ATfiltR_data.3[ID == these.animals[i],Previous.error := data.table::shift(ATfiltR_data.3[ID == these.animals[i],Speed.error],1)]
      }

    }
    cat(" \n")

    reduced.dist<-dist.long[which((dist.long[,1] %in% ATfiltR_data.3[!is.na(Speed.error),Station.name] & dist.long[,2] %in% ATfiltR_data.3[!is.na(Speed.error),Previous.station])|
                                    (dist.long[,2] %in% ATfiltR_data.3[!is.na(Speed.error),Station.name] & dist.long[,1] %in% ATfiltR_data.3[!is.na(Speed.error),Previous.station])),]

    these.animals<-unique(ATfiltR_data.3[!is.na(Speed.error),ID])

    these.ranges<-range[which(range$Category %in% unique(ATfiltR_data.3[!is.na(Speed.error),c(Range.category, Previous.range.category)])),]

    t=t+1

    error.number<- ATfiltR_data.3[!is.na(Speed.error) & is.na(Previous.error),.N]

    cat(" \n")
    cat(crayon::bold("Repetition", t), " :", crayon::cyan(error.number), "speed errors detected and removed.", " \n")

    if (error.number == 0) { # & as.numeric(Sys.time()-start, units="hours")>1
      cat(" ","End of repetition.", " \n")

      if (!is.na(as.numeric(max.distance))){
        cat(crayon::cyan(ATfiltR_data.3[Distance.full>=eval(max.distance),.N]),"detections were spatially too far appart (max.distance set by user to", crayon::cyan(max.distance),").", " \n")
        if (ATfiltR_data.3[Distance.full>=eval(max.distance),.N]>0){

          if (all(is.na(ATfiltR_speedy.data))){
            ATfiltR_speedy.data<-ATfiltR_data.3[Distance.full>=eval(max.distance),]
          } else {
            ATfiltR_speedy.data<-rbind(ATfiltR_speedy.data, ATfiltR_data.3[Distance.full>=eval(max.distance),])
          }

          ATfiltR_data.3<-ATfiltR_data.3[Distance.full<eval(max.distance) | is.na(Distance.full),]
          cat("Removal of these detections.", " \n")}
      }

      cat("Time R spent calculating while you drank margharitas: ", as.numeric(Sys.time()-start, units="hours"), "hours",  " \n")
      break}

    else if (error.number > 0){

      if (all(is.na(ATfiltR_speedy.data))){
        ATfiltR_speedy.data<-ATfiltR_data.3[which(!is.na(Speed.error) & is.na(Previous.error)),]
      } else {
        ATfiltR_speedy.data<-rbind(ATfiltR_speedy.data, ATfiltR_data.3[which(!is.na(Speed.error) & is.na(Previous.error)),])
      }

      ATfiltR_data.3<-ATfiltR_data.3[-which(!is.na(Speed.error) & is.na(Previous.error)),]


      cat("Repeating the speed attributions to account for the deletion of some data points", " \n")

      # cat(nrow(data.compiled[which(data.compiled$Distance.full>=40000),]),"detections were spatially too far appart (>40000m).", " \n")
      #
      # if (nrow(data.compiled[which(data.compiled$Distance.full>=40000),])>0){
      #   data.compiled<- data.compiled[-which(data.compiled$Distance.full>=40000),]
      #   cat("Removal of these detections.", " \n")}
    }

  } ##end of repeat speed check


  ATfiltR_data.4<<-ATfiltR_data.3
  ATfiltR_speedy.data<<-ATfiltR_speedy.data

  if (save.speedy==TRUE){
    cat("Saving the data that happenned too fast ...", " \n")
    if (project==T){
    save(ATfiltR_speedy.data, file=here::here(detection.folder, paste0("ATfiltR_speedy.data_", Sys.Date(),".RData")))
    cat("File saved in your Detections folder under", paste0("ATfiltR_speedy.data_", Sys.Date(),".RData"), " \n")
} else{
  save(ATfiltR_speedy.data, file=paste0(getwd(), "/ATfiltR_speedy.data_", Sys.Date(),".RData"))
  cat("File saved in ", paste0(getwd(), "/ATfiltR_speedy.data_", Sys.Date(),".RData"), " \n")}
}

  if (save==TRUE){
    cat("Saving the compiled file after speed.check filtering...", " \n")

    if (project == T){
    save(ATfiltR_data.4, file=here::here(detection.folder, paste0("ATfiltR_data.4_", Sys.Date(),".RData")))
    cat("File saved in your Detections folder under", paste0("ATfiltR_data.4_", Sys.Date(),".RData"), " \n")
  } else {
    save(ATfiltR_data.4, file= paste0(getwd(),"/ATfiltR_data.4_", Sys.Date(),".RData"))
    cat("File saved in ", paste0(getwd(),"/ATfiltR_data.4_", Sys.Date(),".RData"), " \n")
  }
}

} ##end of the function
