
#' ReadICT1001
#'
#' ICARTT data file reader in R
#' Accepts only format 1001 (comma delimited, all variables are real)
#'
#' @param filename name of file in ICARTT 1001 format
#' @param format.time [TRUE/FALSE]
#' @param ... additional arguments to \code{FormatTime}
#'
#' @return a data frame with additional attribute, "header", attached. Access header information with \code{header <- attr(dataframe,"header")} or \code{cat(paste(attr(dataframe,"header"),collapse="\n"),"\n")}. Note that modifications to this data frame [e.g., with \code{transform()}] may cause loss of this attribute.
#'
#' @examples
#'
#' ict <- ReadICT1001("example.ict", format.time = TRUE, vars = c("Start_UTC", "Stop_UTC"))
#'
#' for(x in c("Start_UTC", "Stop_UTC"))
#'   ict[,x] <- format(ict[,x], "%Y-%m-%d %T")
#'
#' write.csv(ict, "example_formattedtime.csv", row.names=FALSE)
#'
#' @export

###_* ReadICT1001
ReadICT1001 <- function(filename, format.time=FALSE, ...) {
  ## FFI = 1001: one real, unbounded independent variable; primary variables are real; no auxiliary variables; independent and primary variables are recorded in the same record
  ## inspired by ICARTTread.m (http://cires.colorado.edu/jimenez-group/wiki/index.php/Analysis_Software#ICARTT_Matlab_Software)
###_* header indices

  ix <- c(indep=9,numvar=10,scale=11,miss=12)-1

###_* open file
  f <- file(filename,open="r")

###_* read header
  nhdr <- scan(f,what=0,sep=",",nlines=1, quiet=TRUE)
  header <- StripWhite(scan(f, what="", sep="\n", nlines=nhdr-2, quiet=TRUE)) #nhdr-1 or -2?

###_* read contents
  data <- read.table(f, sep=",", header=TRUE, check.names=FALSE, row.names=NULL, colClasses="numeric", as.is=TRUE)

###_* close file
  close(f)

###_* parse header
  ###_ . dependent variables
  depvars <- setdiff(colnames(data), header[ix["indep"]])

###_ . scaling factor and missing values (changed to read.table to allow fill=TRUE)
  fields <- c("scale", "miss")
  ## parm <- structure(do.call(rbind,strsplit(header[ix[fields]],",[ ]+")),
  ##                   dimnames=list(fields,depvars))
  parm <- as.matrix(read.table(text=header[ix[fields]], sep=",", fill = TRUE,
                               row.names = fields, col.names = depvars,
                               check.names = FALSE))
  mode(parm) <- "numeric"

###_ . Limits of detection
  fields <- c("LLOD","ULOD")
  lod <- setNames(sapply(
    strsplit(sapply(sprintf("%s_FLAG", fields), grep, header, value=TRUE), "[:][ ]*"),
    `[`, 2), fields)
  mode(lod) <- "numeric"

###_* replace missing/llod/ulod values and scale variables in main data frame
###_ . missing values only
  ## comparing floats is dangerous...
  for(x in depvars) {
    data[,x] <- replace(data[,x],
                        data[,x]==parm["miss",x],
                        ## data[,x]==parm["miss",x] |
                        ## data[,x]==lod["LLOD"] |
                        ## data[,x]==lod["ULOD"],
                        NA)
###_ . scale factor
    if(parm["scale",x]!=1)
      data[,x] <- data[,x]*parm["scale",x]
  }

###_* attach attributes and return value
  attr(data,"header") <- header

  if(format.time)
    data <- FormatTime(data, ...)

  data
}


#' FormatTime
#'
#' Format time columns in ICT table. Nominally the time column is the sole independent variable, but additional columns can be specified with \code{vars}.
#'
#' @param x table
#' @param vars variable names
#'
#' @return a data frame with \code{vars} columns to chron objects

FormatTime <- function(x, vars=attributes(x)$header[8]) {
  range <- StripWhite(strsplit(attributes(x)$header[6],",")[[1]])
  start <- chron::chron(paste(range[c(2,3,1)], collapse="/"))
  for(.var in vars)
    x[,.var] <- start + x[,.var]/86400
  x
}
