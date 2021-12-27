#' @title compileData
#'
#' @description
#'
#'
#' @param file.type The type of detection file that you are importing (e.g. SONOTRONICS,
#' FATHOM, VUE)
#' @param save Should the conpiled data be saved in the Detections folder (TRUE or FALSE)
#' if FALSE the compiled detection will be in your R environment only
#' @param remove.duplicates Should the duplicated detections (i.e. detection of the
#' same tag, on the same receiver at the same time) be removed (TRUE or FALSE)
#' @param save.duplicates Should the duplicated detections be saved in the Detections folder (TRUE or FALSE)
#' if FALSE the  duplicated detection will be in your R environment only
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @importFrom here "here"
#'
#'

compileData<-function(file.type="FATHOM", save=TRUE, remove.duplicates=T, save.duplicates=F){
start<-Sys.time()
# if (!require("here")) install.packages("here")
# library(here) ##This forces you to get packages

path=here::here("Detections")

memory.limit(size=500000)

if (file.type=="FATHOM"){
  files <- dir(path, pattern =".csv")
  data<-read.table(here::here("Detections",files[1]) ,header=F,fill=TRUE,sep=",",dec=".",   colClasses = rep("character", rep=26))[1,1:11]
  for(j in 1:length(files)){
    tryCatch(data2<-read.table(here::here("Detections",files[j]),header=F,fill=TRUE,sep=",",dec=".", colClasses = rep("character", rep=26))[,1:11] ,error = function(e)
      print(files[j]))
    cat("We found",length(files), "files in you directory (From FATHOM). Compiling:", j, " \r")
    data<-rbind(data, data2)
    rm(data2)

  }

  colnames(data)<-gsub(" ", "_", data[which(data[,1]=="DET_SENS_DESC")[1],])
  data<-data[which(data[,1]=="DET"),]
  data<-data[,c(3,7,10)]
  colnames(data)[2]<-"Receiver"
  data$Receiver<-paste0("VR2Tx-", data$Receiver)
  data[,c("Transmitter.Name","Transmitter.Serial","Sensor.Value","Sensor.Unit","Station.Name","Latitude","Longitude")]<-NA
  colnames(data)[1]<-"Date.and.Time"
  colnames(data)[3]<-"Transmitter"


} else if (file.type=="VUE"){

  files <- dir(path, pattern =".csv")
  data<-read.table(here::here("Detections",files[1]) ,header=TRUE,fill=TRUE,sep=",",dec=".",   colClasses = rep("character", rep=10))[1,] ## Here I load just one detection to create the dataset
  for(j in 1:length(files)){
    tryCatch(data2<-read.table(here::here("Detections",files[j]),header=TRUE,fill=TRUE,sep=",",dec=".",   colClasses = rep("character", rep=10)),error = function(e)
      print(files[j]))
    cat("We found",length(files), "files in you directory (From VUE). Compiling:", j, " \r")
    data<-rbind(data, data2)}
  rm(data2) ##remove the data2 subset
  colnames(data)[1]<-"Date.and.Time"
}
else if (file.type=="SONOTRONICS"){

  files <- dir(path, pattern =".SUR", full.names=T)
  sur<-dir(files[1], pattern =".txt", full.names=T)

  for (i in 2:length(files)){
    sur2<-dir(files[i], pattern =".txt", full.names=T)
    sur<-c(sur,sur2)
  }
  files<-sur[grep("raw", sur)]

  # data<-read.table(files[1] ,header=F,fill=TRUE,sep=",",dec=".",   colClasses = rep("character", rep=4))[2:3,] ## Here I load just one detection to create the dataset
  # colnames(data) = c("time","date","Frequency","Interval") ##creates column names
  # data[c("Receiver")]<-paste0("S-",(data[1,1]))
  # data<-data[-1,]

  list.of.files<-rep(NA,length(files))

  for(j in 1:length(files)){
    tryCatch(data2<-read.table(files[j],header=F,fill=TRUE,sep=",",dec=".",   colClasses = rep("character", rep=4)),error = function(e)
      print(files[j]))
    cat("We found",length(files), "files in you directory (From SONOTRONICS). Compiling:", j, " \r")
    #if(data2[1,1]=="none") next
    if (dim(data2)[1]==1 & dim(data2)[2]==1){next}
    list.of.files[j]<-paste0("dataset",j)
    colnames(data2) = c("time","date","Frequency","Interval") ##creates column names
    data2<-data2[-1,]
    data2[c("Receiver")]<-paste0("S-",data2[1,1])
    data2<-data2[-1,]

    assign(list.of.files[j], data2)
    # data<-rbind(data, data2)
    # rm(data2) ##remove the data2 subset
  }

  Data.list <- lapply(na.omit(list.of.files), get)
  cat("Merging all datasets"," \n")

  data<-do.call(rbind, Data.list)

  rm(list=grep("dataset",ls(),value=TRUE))

  data$time<-substr(data$time, 4, 100) ## removes the first characters in time column

  head(data)

  unique(data$time)
  if(nrow(data[grep("[a-z]",data$time),])>0){ data<-data[-grep("[a-z]",data$time),] }
  if(nrow(data[grep("[a-z]",data$date),])>0){ data<-data[-grep("[a-z]",data$date),] }
  if(nrow(data[grep("[a-z]",data$Frequency),])>0){ data<-data[-grep("[a-z]",data$Frequency),] }
  if(nrow(data[grep("[a-z]",data$Interval),])>0){ data<-data[-grep("[a-z]",data$Interval),] }

  data$date.and.time<-paste(data$date, data$time, sep=" ")
  data<-data[-which(data$Frequency=="" & data$Interval==""),]
  data$Date.and.Time <- as.POSIXct(data$date.and.time, format="%m/%d/%y %H:%M:%S")


  data.check<<-data

  data<-data[, c(7,5,3,4)]


}

cat("\n")

if (nchar(paste(data$Date.and.Time[1]))==19) {
  cat("Dates and times look good!"," \n")
} else {
  cat("Dates and times are being sorted out..."," \n")
  data$Date.and.Time<-substr(data$Date.and.Time, 0,19)
}

if (remove.duplicates==T){
  duplicates<<-data[duplicated(data[,c(1,2,3,4)]),]
  cat(nrow(duplicates), "duplicates found in your data.", " \n")
  if (save.duplicates==T){
    write.table(duplicates, here::here("Detections", paste0("duplicates_", Sys.Date(),".txt")), sep=",", row.names=F)
    cat("Duplicates saved in your Detections folder under", paste0("duplicates_", Sys.Date(),".txt"), " \n")
  }
  cat("Removing duplicates from the compiled data. Relax, this might take a while...", " \n")
  if (nrow(duplicates)>0){
    data<-data[!duplicated(data[,c(1,2,3,4)]),]}
}




##creating receiver.name and transmitter.name
if ("Transmitter" %in% colnames(data)){
  if (nchar(data$Transmitter[1])==5) {
    data$Transmitter.Name<-data$Transmitter
  } else {
    data$Transmitter.Name<-substr(data$Transmitter, 10,14)
  }}

if (nchar(data$Receiver[1])==6) {
  data$Receiver.Name<-data$Receiver
} else {
  data$Receiver.Name<-substr(data$Receiver, 7,12)
}

data.compiled<<-data

if (save==TRUE){
  cat("Saving the compiled file...", " \n")
  write.table(data.compiled, here::here("Detections", paste0("data.compiled_", Sys.Date(),".txt")), sep=",", row.names=F)
  cat("File saved in your Detections folder under", paste0("data.compiled_", Sys.Date(),".txt"), " \n")}

cat("End of process for the data compilation. This took approximately", paste(round(difftime(Sys.time(), start, units="min"))), "minutes!"," \n")

}
