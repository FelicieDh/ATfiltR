#' @title compileData
#'
#' @description Compiles the acoustic telemetry data in a folder into a file that is useable by ATfiltR.
#'
#'
#' @param detection.folder The name of the folder containing the detection data
#' @param file.ext The extension of your detection files (e.g. ".csv")
#' @param sep.type The column separator in your detection files (e.g. ",")
#' @param save Should the compiled data be saved in the detections folder (TRUE or FALSE)
#' if FALSE the compiled detections will be in your R environment only
#' @param remove.duplicates Should the duplicated data (i.e. detection of the
#' same tag, on the same receiver at the same time) be removed (TRUE or FALSE)
#' @param save.duplicates Should the duplicated detections be saved in your detections folder (TRUE or FALSE)
#' if FALSE the  duplicated detections will be in your R environment only
#'
#' @return A data frame object that contains your compiled detections, and another that contains the duplicates
#'
#' @examples
#' \dontrun{
#' # Before running the function, you must be in an R project that contains your detections folder
#' function(detection.folder="Detections", file.ext=".csv",
#' sep.type=",", save=TRUE, remove.duplicates=T, save.duplicates=F)
#' }
#' @export
#'
#' @importFrom here here
#' @import crayon
#' @importFrom lubridate parse_date_time
#' @importFrom data.table rbindlist
#'
#' @importFrom utils View read.table write.table
#'
#'

#gsub scan Sys.time cat dir c length as.character as.numeric repeat break for ncol exists if

##package: here::here


compileData<-function(detection.folder="Detections", file.ext=".csv",
                      sep.type=",", save=TRUE, remove.duplicates=T, save.duplicates=F, split=T){


  #####################################################################
  ########## Starting the timer and setting up the files ##############
  #####################################################################

  start<-Sys.time()

  path=here::here(detection.folder)

  files <- dir(path, pattern = file.ext)


  ##############################################
  ########## Defining the objects ##############
  ##############################################

  row.num.head<-NA ## the row in which the column names are stored
  erase.col<-NA ## the column.names that need to be removed
  erase.row<-NA ## the rows that need to be erased
  n.col<-NA ## the number of column.names
  header.fun<-NA ## shoudl we use header = T or header = F
  column.names<-NA ## custom column names
  datetime<-NA ## the column that contains date and time data
  date<-NA ## the column that contains date data
  time<-NA ## the column that contains time data
  dup.datetime<-NA ## should the column be duplicated
  trans<-NA ## the column that contains transmitter data
  dup.trans<-NA ## should the column be duplicated
  receiver<-NA ## the column that contains receiver data
  dup.receiver<-NA ## should the column be duplicated
  start.t<-NA ##characters to keep in the transmitter string
  stop.t<-NA ##characters to keep in the transmitter string
  start.r<-NA ##characters to keep in the receiver string
  stop.r<-NA ##characters to keep in the receiver string
  okdate<-FALSE ##checking wether the time object is good


  ##############################################
  ########## Visualizing the data ##############
  ##############################################

  cat("\n","\n",crayon::bold$yellow("ATfiltR compileData(): compiling your telemetry data into one useable file for ATfiltR."))
  cat("\n")
  cat("\n","\n",crayon::bold$underline$blue("Step 1: Before compiling, we need to check the format you data is in..."))
  cat("\n")
  cat("\n","This is an interactive process, we are going to need you to answer a few questions.")
  cat("\n")
  cat("\n","We suggest answers between brackets. So [y] means that you need to press on the y key of your keyboard.")
  cat("\n","To validate answers, just press [enter]")
  cat("\n")
  cat("\n")

  visual<-read.table(here::here(detection.folder,files[1]),header=F,fill=T,
                     sep=sep.type, dec=".")[1:20,] ##loading the first 20 rows of data, with no header

  myView(visual[1:15,])


  ###########################################
  ########## Column names ###################
  ###########################################

  ## Are the column names in the table?
  repeat{ #r1
    cat("\n","\n",crayon::bold("Here is one of your datasets: Are the column names indicated in the table ? [y]es or [n]o"))
    check<-scan("",what="character",nmax=1,fill=T, quiet=T)

    if (length(check)==1 & (check=="y" | check=="n")){ break }} ##end of r1 if a right answer is given

  # Yes, where?
  if (check=="y"){

    repeat{ ##r2
      cat("\n","\n",crayon::bold("Please indicate in which row (row number) the column names are stored:"))
      cat("\n","(Note: This has to be the same for every detection file in the folder)")

      row.num.head<-scan("",what="numeric",nmax=1,fill=T, quiet=T)
      row.num.head<-as.numeric(row.num.head)

      if (length(row.num.head)!=0 & row.num.head %in% 1:20){
        colnames(visual)<-gsub(" ", "_", visual[row.num.head,])
        myView(visual[1:15,])

        repeat{ ##r3
          cat("\n","\n",crayon::bold("Are the column names correct [y]es or [n]o"))
          cat("\n","(Note: We have replaced spaces with underscores)")

          check2<-scan("",what="character",nmax=1,fill=T, quiet=T)

          if (length(check2)==1 & (check2=="y" | check2=="n")) {break}} ##end of r3, if the user is happy with the names

        if (check2=="y"){

          if (row.num.head==1){
            header.fun=T; break ##end of r2, in a case when the row number is 1
          } else {
            header.fun=F; break } ##end of r2, in a case when the row number is not 1
        } ##end of if check2==y
      } ##end of first if statement
    } ##end of r2 bracket

    # No, then you can give us your own columns names
  } else if (check=="n"){

    repeat{ ##r4

      cat("\n","\n","Please type your desired column names.")
      cat("\n","(Note: We suggest that you stay away from special characters...")
      header.fun=F
      column.names<-rep(NA,ncol(visual))

      for (i in 1:ncol(visual)){
        cat("\n","Column", i ,":")
        column.names[i]<-scan("",what="character",nmax=1,fill=T, sep=",", quiet=T)
      } #acquiring the column names

      colnames(visual)<-gsub(" ", "_", as.character(column.names))

      print(gsub(" ", "_", as.character(column.names)))
      myView(visual[1:15,])

      repeat{ ##r5
        cat("\n","\n",crayon::bold("Are the column names correct [y]es or [n]o"))
        cat("\n","(Note: We have replaced spaces with underscores)")

        check<-scan("",what="character",nmax=1,fill=T, quiet=T)

        if (length(check)==1 & check %in% c("y","n")){ break }} ## end of r5 if a proper answer is given

      if(check=="y") {column.names<-column.names; break}} ##end r4 if the user is happy with the column names
  }

  n.col<-as.numeric(ncol(visual)) ## defining the number of column to speed up the data loading later



  #########################################################################
  ########## Reload the visual with the correct column names ##############
  #########################################################################

  visual<-read.table(here::here(detection.folder,files[1]), header=header.fun, fill=T,
                     sep=sep.type, dec=".", colClasses = rep("character", rep=n.col))[1:20,]

  if (header.fun==F & length(row.num.head)!=0){ colnames(visual)<-gsub(" ", "_", visual[row.num.head,]) }
  else if (header.fun==F & length(row.num.head)==0){ colnames(visual)<-gsub(" ", "_", as.character(column.names))}

  myView(visual[1:15,])


  ###########################################
  ########## Row removal ####################
  ###########################################

  repeat{ ##r6
    cat("\n","\n",crayon::bold("Should some of the first rows be omitted during the compilation: [y]es or [n]o"))
    cat("\n","(Note: This has to be the same for every detection file in the folder)")
    check<-scan("",what="character",nmax=1,fill=T, quiet=T)

    if (length(check)==1 & check %in% c("y","n")){

      if(check=="n") {break ## end of r6 if the user does not want to remove any rows
      } else if (check=="y"){ ##if the user does want to remove rows
        repeat{ ##r7
          cat("\n","\n","How many rows should be erased? (e.g. input [5] will erase the first 5 rows)")

          erase.row<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

          if (length(erase.row)!= 0 & suppressWarnings(as.numeric(erase.row)) %in% 1:20) {erase.row<-as.numeric(erase.row); break}} ##break r7 if the user gives a correct number
        break} ##then break r6

    } ##end of checking for the validity of the answer

  } ##end of r6 bracket


  ###########################################
  ########## Column removal #################
  ###########################################

  repeat{ ##r7

    cat("\n","\n",crayon::bold("Should some of the column be removed: [y]es or [n]o"))
    #cat("\n","(Note: This has to be the same for every detection file in the folder)")
    check<-scan("",what="character",nmax=1,fill=T, quiet=T)

    if (length(check)==1 & check %in% c("y","n")){
      if(check=="n") {break ## end of r7 if the user does not want to remove any columns
      } else if (check=="y"){ ## if the user does want to remove columns

        repeat{ ##r8
          cat("\n","\n",crayon::bold("Which ones should be erased?"))
          cat("\n","(Enter the column numbers one by one, and press [enter][enter] when you are done)")
          cat("\n","(Note: In RStudio, you can 'mouseover' the columns to see their numbers)")

          erase.col<-scan("",what="numeric",nmax=n.col,fill=T, quiet=T)

          if (length(erase.col)!= 0 & any(is.na(suppressWarnings(as.numeric(erase.col))))==F) {erase.col<-as.numeric(erase.col)

          if (length(erase.col[which(erase.col > n.col)])>0){erase.col<-erase.col[-which(erase.col > n.col)]} ##removing the columns numbers that are impossible
          break}} ##end of r8 if the answer is valid

        break} ##end of r7 if the user does want to remve columns

    } ##end of checking for a valid answer

  }##end of r7 bracket



  ############################################################
  ########## Checking for the necessary columns ##############
  ############################################################

  cat("\n")
  cat("\n","\n",crayon::bold$underline$blue("Step 2: Now, we need to know the content of a few of your column names..."))
  cat("\n")

  visual<-read.table(here::here(detection.folder,files[1]),header=header.fun,fill=T,
                     sep=sep.type, dec=".", colClasses = rep("character", rep=n.col))[1:(erase.row+20),]

  if (header.fun==F & length(row.num.head)!=0){ colnames(visual)<-gsub(" ", "_", visual[row.num.head,])
  } else if (header.fun==F & length(row.num.head)==0){colnames(visual)<-gsub(" ", "_", as.character(column.names))}

  if (!is.na("erase.row")){  visual<-visual[-c(1:as.numeric(erase.row)),]} ##removing the rows that should be removed
  if (!is.na("erase.col")){ visual<-visual[,-c(as.numeric(erase.col))]} ##removing the columns that should be removed

  myView(visual[1:15,])

  cat("\n")
  cat("\n","\n","Here is your dataframe, in the format you had indicated above.
                  If it doesn't match your expectations, please press [escape] and start again.")


  ###########################################
  ########## Defining the time ##############
  ###########################################

  cat("\n","\n",crayon::bold$underline("The first thing we need is to set up your date and time column."))

  ## Are date and time in separate column ?

  repeat{ ##r9

    cat("\n","\n",crayon::bold("Are the date and the time in separate columns? [y]es or [n]o"))
    check<-scan("",what="character",nmax=1,fill=T, quiet=T)

    if (length(check)==1 & (check=="y" | check=="n")){

      if (check == "n"){ ## if date and time are in the same column

        repeat{ ##r10

          cat("\n","Please enter the column number that contains the date and time info [column number]")
          datetime<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

          if (as.numeric(datetime) %in% 1:as.numeric(ncol(visual))){

            visu.date<-visual[1,as.numeric(datetime)]
            cat("\n",crayon::bold$yellow(visu.date))

            repeat{ ##r11

              cat("\n",crayon::bold("Here is the date and time object, does it look good? [y]es or [n]o"))
              cat("\n","(Note: decimals in the 'seconds' will be removed)")
              check2<-scan("",what="character",nmax=1,fill=T, quiet=T)

              if (length(check2)==1 & check2 %in% c("y","n")){

                if (check2=="y"){
                  okdate<-TRUE;break ##break r11 and validating date
                } else {break}##break r11 without validating date

              } ## end of checking the validit of check2
            }##end of r11 bracket

            if (okdate==T){break}} ##break r10 ##end of checking for a valid column number
        }##end of r10 bracket

        if (okdate==T){
          repeat{ ##r12 to ask for the potential duplication

            cat("\n",crayon::bold("We need the column to be called 'Date.and.Time', should we rename it ([y]es), or create a copy ([n]o?)"))
            check3<-scan("",what="character",nmax=1,fill=T, quiet=T)

            if (length(check3)==1 & (check3=="y" | check3=="n")){

              if(check3 =="y"){dup.datetime<-F; break ##break r12 with no duplication
              } else {dup.datetime<-T; break} ##break r12 with duplication

            } ##end of checking the validity of check3
          } ##end of r12 bracket
        }  ##end of the duplications's first if

      } else {## if date and time are NOT in the same column

        repeat{ ##r13
          cat("\n","Please enter the column number that contains the date info [column number]")
          date<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

          cat("\n","Please enter the column number that contains the time info [column number]")
          time<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

          if ((as.numeric(date) %in% 1:as.numeric(ncol(visual))) & (as.numeric(time) %in% 1:as.numeric(ncol(visual)))){

            visu.date<-paste0(visual[1,as.numeric(date)]," ",visual[1,as.numeric(time)])
            cat("\n",crayon::bold$yellow(visu.date))

            repeat{ ##r14

              cat("\n",crayon::bold("Here is the date and time object, does it look good? [y]es or [n]o"))
              cat("\n","(Note: decimals in the 'seconds' will be removed)")
              check2<-scan("",what="character",nmax=1,fill=T, quiet=T)

              if (length(check2)==1 & check2 %in% c("y","n")){
                if (check == "y"){ okdate==TRUE; break ##break r14 if the user is happy with the answer
                }else{break}} ##break r14 without validating the date

            } ## end of r14 bracket

            if (okdate==T){break}} ##break r13 ##end of checking the validity of the answer if the date and time are in the same column
        }  ## end of r13 bracket
      }  ## end of the if else statement
      if (okdate==T){break}} ##break r9 ##end of checking for a valid answer for separate or together columns
  } ##end of repeat r9 bracket





  ##################################################
  ########## Defining the Transmitter ##############
  ##################################################

  cat("\n","\n",crayon::bold$underline("Then we need to know where the transmitter info is."))

  repeat{ ##r15
    if(is.na(trans)){
      cat("\n","\n",crayon::bold("Please enter the column number that contains the transmitter info [column number]"))
      trans<-scan("",what="numeric",nmax=1,fill=T, quiet=T)}

    if (as.numeric(trans %in% 1:as.numeric(ncol(visual)))){
      visu.trans<-visual[1,as.numeric(trans)]


      repeat{ ##r16

        cat("\n",crayon::bold$yellow(visu.trans))

        cat("\n",crayon::bold("Here is the transmitter info, does it look good? [y]es or [n]o"))
        cat("\n","(Note: To 'look good' the transmitter info must be in the exact same format as the one in your 'animal' dataframe)",
            "\n","(You'll be given a chance to trim characters away if you type [n])")
        check<-scan("",what="character",nmax=1,fill=T, quiet=T)

        if (length(check)==1 & check %in% c("y","n")){
          if (check == "y"){
            if (is.na(start.t)){ ##making sure the character removal has not hapenned
              repeat{ ##r17 to ask for the potential duplication

                cat("\n",crayon::bold("We need the column to be called 'Transmitter', should we rename it ([y]es), or create a copy ([n]o?)"))
                check2<-scan("",what="character",nmax=1,fill=T, quiet=T)

                if (length(check2)==1 & (check2=="y" | check2=="n")){

                  if(check2 =="y"){dup.trans<-F; break ##break r17 with no duplication
                  } else {dup.trans<-T; break} ##break r17 with duplication

                } ##end of checking the validity of check2
              } ##end of r17 bracket
            } ##end of making sure the character removal has not hapenned
            break;break;break## if the transmitter info is good we can break both repeat statements after checking if we need duplicates

          } else {

            repeat{ #r18 to identify why the transmitter info doesn't look good
              cat("\n",crayon::bold("Do you wish to [a] select a different column or [b] remove some characters?"))
              check<-scan("",what="character",nmax=1,fill=T, quiet=T)

              if (length(check)==1 & check %in% c("a","b")){

                if (check=="a"){trans<-NA;break;break

                }else{

                  repeat{ ##r19 to trim characters
                    cat("\n",crayon::bold("Please enter the [character number] of the FIRST character you want to keep."))
                    cat("\n","(e.g. if the current transmitter info is '1234portkey5678', and you want to keep 'portkey', enter [5] as the fifth character is the first that you wish to keep)")
                    start.t<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

                    cat("\n",crayon::bold("Please enter the [character number] of the LAST character you want to keep."))
                    cat("\n","(e.g. if the current transmitter info is '1234portkey5678', and you want to keep 'portkey', enter [11] as the eleventh character is the last that you wish to keep)")
                    stop.t<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

                    if ((as.numeric(start.t) %in% 1:nchar(visu.trans)) & (as.numeric(stop.t) %in% 1:nchar(visu.trans)) & (start.t < stop.t)){
                      visu.trans<-substr(visu.trans, start.t, stop.t)
                      break

                    }

                  }  ##end of r19 bracket

                } ##end of ifelse statement for check a b

                if (!is.na(visu.trans)){break}}##end of checking the validity of a and b

            } #end of r18 bracket

          } ##end of the ifelse statement

          if (is.na(trans)){break}
        } ##end of checking the validity of the answer

      } ## end of r16 bracket

      if (!is.na(dup.trans) | !is.na(start.t)){break}}## end of checking the validity of the columns number

  } ##end of r15 bracket





  ###############################################
  ########## Defining the Receiver ##############
  ###############################################

  cat("\n","\n",crayon::bold$underline("Finally we need to know where to find the receiver info."))

  repeat{ ##r20
    if(is.na(receiver)){
      cat("\n","\n",crayon::bold("Please enter the column number that contains the receiver info [column number]"))
      receiver<-scan("",what="numeric",nmax=1,fill=T, quiet=T)}

    if (as.numeric(receiver %in% 1:as.numeric(ncol(visual)))){
      visu.receiver<-visual[1,as.numeric(receiver)]


      repeat{ ##r21

        cat("\n",crayon::bold$yellow(visu.receiver))

        cat("\n",crayon::bold("Here is the receiver info, does it look good? [y]es or [n]o"))
        cat("\n","(Note: To 'look good' the receiver info must be in the exact same format as the one in your 'animal' dataframe)",
            "\n","(You'll be given a chance to trim characters away if you type [n])")
        check<-scan("",what="character",nmax=1,fill=T, quiet=T)

        if (length(check)==1 & check %in% c("y","n")){
          if (check == "y"){
            if (is.na(start.r)){ ##making sure the character removal has not hapenned
              repeat{ ##r22 to ask for the potential duplication

                cat("\n",crayon::bold("We need the column to be called 'Receiver', should we rename it ([y]es), or create a copy ([n]o?)"))
                check2<-scan("",what="character",nmax=1,fill=T, quiet=T)

                if (length(check2)==1 & (check2=="y" | check2=="n")){

                  if(check2 =="y"){dup.receiver<-F; break ##break r22 with no duplication
                  } else {dup.receiver<-T; break} ##break r22 with duplication

                } ##end of checking the validity of check2
              } ##end of r22 bracket
            } ##end of making sure the character removal has not hapenned
            break;break;break## if the receivermitter info is good we can break both repeat statements after checking if we need duplicates

          } else {

            repeat{ #r23 to identify why the receivermitter info doesn't look good
              cat("\n",crayon::bold("Do you wish to [a] select a different column or [b] remove some characters?"))
              check<-scan("",what="character",nmax=1,fill=T, quiet=T)

              if (length(check)==1 & check %in% c("a","b")){

                if (check=="a"){receiver<-NA;break;break

                }else{

                  repeat{ ##r24 to trim characters
                    cat("\n",crayon::bold("Please enter the [character number] of the FIRST character you want to keep."))
                    cat("\n","(e.g. if the current receiver info is '1234nargles5678', and you want to keep 'nargles', enter [5] as the fifth character is the first that you wish to keep)")
                    start.r<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

                    cat("\n",crayon::bold("Please enter the [character number] of the LAST character you want to keep."))
                    cat("\n","(e.g. if the current receiver info is '1234nargles5678', and you want to keep 'nargles', enter [11] as the eleventh character is the last that you wish to keep)")
                    stop.r<-scan("",what="numeric",nmax=1,fill=T, quiet=T)

                    if ((as.numeric(start.r) %in% 1:nchar(visu.receiver)) & (as.numeric(stop.r) %in% 1:nchar(visu.receiver)) & (start.r < stop.r)){
                      visu.receiver<-substr(visu.receiver, start.r, stop.r)
                      break

                    }

                  }  ##end of r24 bracket

                } ##end of ifelse statement for check a b

                if (!is.na(visu.receiver)){break}}##end of checking the validity of a and b

            } #end of r23 bracket

          } ##end of the ifelse statement

          if (is.na(receiver)){break}
        } ##end of checking the validity of the answer

      } ## end of r21 bracket

      if (!is.na(dup.receiver) | !is.na(start.r)){break}}## end of checking the validity of the columns number

  } ##end of r20 bracket


  #############################################
  ########## Loading and sorting ##############
  #############################################

  cat("\n")
  cat("\n","\n",crayon::bold$underline$blue("Step 3: Ok, we are ready to compile your data, sit back and relax, this might take a little while!"))
  cat("\n")
  cat("\n")



  #################################################
  ########## Iterating through files ##############
  #################################################
   temp<-here::here(detection.folder,files)

  if (split==T){
    inc<-c(seq.int(from=0, to=length(temp), by=10), length(temp))
    if(length(inc[duplicated(inc)])>0){
      inc<-inc[-duplicated(inc)]
    }
  } else {
    inc<-0:length(temp)
  }

  for(i in 1:(length(inc)-1)){

if (split==T){
  cat("\r","Loading the detections, little by little...", i, "/" , (length(inc)-1))
}else{
  cat("\n","Loading the detections...")
  }

  data.list <- list()
  data.list = lapply(temp[c((inc[i]+1) : inc[i+1])], read.table,header=header.fun,fill=TRUE,sep=sep.type,dec=".", colClasses = rep("character", rep=n.col))

  if (header.fun==F & length(row.num.head)!=0){ data.list<-lapply(data.list, setNames, gsub(" ", "_", data.list[[1]][row.num.head,]))
  }else if (header.fun==F & length(row.num.head)==0){ data.list<-lapply(data.list, setNames, gsub(" ", "_", as.character(column.names))) }

  if (!is.na("erase.row")){ data.list<-lapply(data.list, function(x) {x<-x[-c(1:as.numeric(erase.row)),] ;x}) } ##removing the rows that should be removed
  if (!is.na("erase.col")){ data.list<-lapply(data.list, function(x) {x<-x[,-c(as.numeric(erase.col))] ;x})} ##removing the columns that should be removed


  columns<-colnames(data.list[[1]])


  if (!is.na(datetime)){ ##date and time are in the same column
    if(dup.datetime==F){ ##if we don't have to duplicate the column
      columns[as.numeric(datetime)]<-"Date.and.Time"
    } else if(dup.datetime==T){
      data.list<-lapply(data.list, function(x) {x<-cbind(x,Date.and.Time=x[,as.numeric(datetime)]); x})
      columns[length(columns)]<-"Date.and.Time"
    }
  }

  if (!is.na(date) & !is.na(time)){ ##date and time are in the same column
    data.list<-lapply(data.list, function(x) {x<-cbind(x,Date.and.Time=paste0(x[,as.numeric(date)]," ",x[,as.numeric(time)])); x})
    columns[length(columns)]<-"Date.and.Time"
  }

  if (!is.na(start.t)){
    data.list<-lapply(data.list, function(x) {x[,as.numeric(trans)]<-substr(x[,as.numeric(trans)], start.t, stop.t); x})
  } else{
    if(dup.trans==F){
      columns[as.numeric(trans)]<-"Transmitter"
    } else if (dup.trans==T){
      data.list<-lapply(data.list, function(x) {x<-cbind(x,Transmitter=x[,as.numeric(trans)]); x})
      columns[length(columns)]<-"Transmitter"
    }
  }

  if (!is.na(start.r)){
    data.list<-lapply(data.list, function(x) {x[,as.numeric(receiver)]<-substr(x[,as.numeric(receiver)], start.r, stop.r); x})
  } else{
    if(dup.receiver==F){
      columns[as.numeric(receiver)]<-"Receiver"
    } else if (dup.trans==T){
      data.list<-lapply(data.list, function(x) {x<-cbind(x,Receiver=x[,as.numeric(receiver)]); x})
      columns[length(columns)]<-"Receiver"
    }
  }


  for(j in 1:length(data.list)){
    data.list[[j]]$File<-files[j]
  }
  columns[length(columns)+1]<-"File"

  data.list<-lapply(data.list, setNames, columns)


  data.list<-data.table::rbindlist(data.list)

  saveRDS(data.list, file=here::here(detection.folder, paste0("temporary",i,".RDS")))

  rm(data.list)
  gc()
  } ##end of inc

  cat("\n", "\n")
  tempfiles <- dir(path, pattern = "temporary")

  for (i in 1:length(unique(tempfiles))){
    sub<-readRDS(here::here(detection.folder, tempfiles[i]))
  cat("\n","Merging the datasets", i, "/", length(unique(tempfiles)))
    if (i == 1){
      detects<-sub
    } else {
      detects<-rbind(detects,sub)
      rm(sub)
    }
  }
  gc()




  if (nchar(paste(detects$Date.and.Time[1]))>19) {
    detects$Date.and.Time<-substr(detects$Date.and.Time, 0,19)
  }
  detects$Date.and.Time<-lubridate::parse_date_time(detects$Date.and.Time, c("Ymd HMS", "ymd HMS","dmy HMS", "dmY HMS"), truncated = 3)
  detects$Date.and.Time<-as.POSIXct(detects$Date.and.Time, format="%Y-%m-%d %H:%M:%S")



  if (remove.duplicates==T){
    duplicates<-detects[duplicated(detects[,c("Transmitter","Receiver","Date.and.Time")]),]
    cat("We found", nrow(duplicates), "duplicates found in your data. (Out of ", nrow(detects),", which is approx.",round(nrow(duplicates)/nrow(detects)*100), "%)", " \n")
    if (save.duplicates==T){
      write.table(duplicates, here::here(detection.folder, paste0("ATfiltR_duplicates_", Sys.Date(),".RData")), sep=sep.type, row.names=F)
      cat(crayon::bold("Duplicates saved in your Detections folder under"), crayon::cyan$bold(paste0("ATfiltR_duplicates_", Sys.Date(),".Rdata"), " \n"))
    }
    cat("Removing duplicates from the compiled data...", " \n")
    if (nrow(duplicates)>0){
      detects<-detects[!duplicated(detects[,c("Transmitter","Receiver","Date.and.Time")]),]
      duplicates<<-duplicates
      }
  }


  ATfiltR_data.1<<-detects

  if (save==TRUE){
    cat("Saving the compiled file...", " \n")
    write.table(ATfiltR_data.1, here::here(detection.folder, paste0("ATfiltR_data.1_", Sys.Date(),".RData")), sep=",", row.names=F)
    cat(crayon::bold("File saved in your Detections folder under"), crayon::cyan$bold(paste0("ATfiltR_data.1_", Sys.Date(),".RData"), " \n"))}
  cat("\n")
  cat(crayon::bold$yellow("End of process for the data compilation. This took approximately", paste(round(difftime(Sys.time(), start, units="min"))), "minutes!"," \n"))

  file.remove(here::here(detection.folder, tempfiles))

} ##END OF THE FUNCTION


