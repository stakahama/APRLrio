
library(APRLrio)
library(RJSONIO)
library(chron)

filename <- file.path("data","timeseries.json")

## do all

JSON2ICT(filename) 

## piece-wise (alternate)

BuildHeader <- get("BuildHeader", environment(JSON2ICT))
ReadTable <- get("ReadTable", environment(JSON2ICT))
WriteICT <- get("WriteICT", environment(JSON2ICT))

directory <- dirname(filename)
contents <- fromJSON(filename)
header <- BuildHeader(contents)
table <- ReadTable(file.path(directory, contents[["csvfile"]]),
                    contents[["time"]])
WriteICT(file.path(directory,contents[["ictfile"]]), header, table)

## read generated ICT

ict <- ReadICT1001(file.path(directory,contents[["ictfile"]]), TRUE)

## plot

indepvar <- "Start_time_UTC"
depvar <- "Nconc"

start <- with(list(x=strsplit(attributes(ict)$header[6],", ")[[1]][1:3]),
              unclass(chron(paste(x[c(2,3,1)],collapse="/"))))

plot(ict[,indepvar], ict[,depvar], type="l")
lines(as.chron(start+table[,indepvar]/86400), table[,depvar], type="l", col=2, lty=2)
