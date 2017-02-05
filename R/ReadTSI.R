
#' ReadTSI
#'
#' Read output of TSI AIM software
#'
#' @param filename file name
#' @param sep delimiter
#'
#' @return TSI object (list).
#' @export

## ReadTSI <- function(filename, sep=",") {

##   ## read to table
##   lines <- RemoveEmptyLines(StripNonASCII(readLines(filename)))
##   iStart <- grep("^Sample \\#",lines)
##   headerline <- strsplit(lines[iStart], sep)[[1]]
##   meta <- with(list(x=do.call(rbind, strsplit(head(lines, iStart-1), sep))),
##                setNames(x[,2], x[,1]))
##   table <- read.table(text=tail(lines,-iStart), sep=sep, comment.char="",
##                       col.names=headerline, check.names=FALSE, as.is=TRUE)

##   ## metadata
##   header <- suppressWarnings(as.numeric(headerline))
##   iDiam <- which(!is.na(header))
##   iTime <- match(c("Date","Start Time"),names(table))
##   datetime <- chron::as.chron(do.call(paste, table[,iTime]), "%m/%d/%y %T")
##   diamlab <- sprintf("V%d",seq_along(iDiam))

##   ## return value
##   c(as.list(meta),
##     list(datetime=datetime,
##          diam=setNames(header[iDiam], diamlab),
##          nconc=data.matrix(setNames(table[,iDiam], diamlab)),
##          metatable=table[,-c(iTime, iDiam)]))
## }


ReadTSI <- function(filename, sep) {
  ##
  lines <- RemoveEmptyLines(StripNonASCII(readLines(filename)))
  iStart <- grep("^Sample \\#", lines)
  parameters <- with(list(x = do.call(rbind, strsplit(head(lines, iStart - 1), sep))),
                     setNames(as.list(x[, 2]), x[, 1]))
  paramtable <- as.data.frame(parameters, row.names=parameters[["Sample File"]], check.names=FALSE)
  ##
  if(strsplit(lines[iStart+1], sep)[[1]][1]=="Date") {
    ## *** long format ***
    ##
    iStop <- grep("^Scan Up Time", lines)
    ##
    headermat <- do.call(rbind, strsplit(lines[iStart + 0:2], sep))
    headertable <- setNames(as.data.frame(t(headermat[,-1])), headermat[,1])
    sample <- headertable[,"Sample #"]
    datetime <- chron::as.chron(do.call(paste, headertable[c("Date", "Start Time")]), "%m/%d/%y %T")
    names(datetime) <- sample
    ##
    datamat <- as.matrix(read.table(text = tail(head(lines, iStop - 1), -(iStart + 3)),
                            sep = sep, header=FALSE, check.names = FALSE,
                            comment.char = "", as.is = TRUE,
                            colClasses="numeric"))
    diamlab <- sprintf("V%d", seq(1, nrow(datamat)))
    diam <- setNames(datamat[,1], diamlab)
    datatable <- structure(t(datamat[,-1]), dimnames=list(sample, diamlab))
    ##
    metamat <- as.matrix(read.table(text = tail(lines, -(iStop - 1)), sep=sep, header=FALSE, comment.char=""))
    metatable <- setNames(as.data.frame(t(metamat[,-1]), row.names=sample), metamat[,1])
    metatable[] <- lapply(metatable, type.convert, as.is=TRUE)
    ##
    ## return value
    out <- list(
      parameters = paramtable,
      datetime = datetime,
      diam     = diam,
      nconc = datatable,
      metatable = metatable
    )
  } else {
    ## *** wide format ***
    ##
    headerline <- strsplit(lines[iStart], sep)[[1]]
    table <- read.table(text = tail(lines, -iStart), sep = sep,
                        comment.char = "", col.names = headerline, check.names = FALSE,
                        as.is = TRUE)
    ##
    header <- suppressWarnings(as.numeric(headerline))
    iDiam <- which(!is.na(header))
    iTime <- match(c("Date", "Start Time"), names(table))
    datetime <- chron::as.chron(do.call(paste, table[, iTime]), "%m/%d/%y %T")
    diamlab <- sprintf("V%d", seq_along(iDiam))
    ##
    ## return value
    out <- list(
      parameters = paramtable,
      datetime = setNames(datetime, row.names(table)),
      diam = setNames(header[iDiam], diamlab),
      nconc = data.matrix(setNames(table[, iDiam], diamlab)),
      metatable = table[, -c(iTime, iDiam)]
    )
  }
  class(out) <- c("TSI", class(out))
  out
}

#' merge.TSI
#'
#' Merge TSI objects.
#'
#' @param x first TSI object
#' @param y second TSI object
#'
#' @return TSI object (list).
#' @export

merge.TSI <- function(x, y, basename=NULL) {
  library(Rfunctools)
  ##
  out <- list()
  ##
  elem <- "parameters"
  out[[elem]] <- rbind(x[[elem]], y[[elem]])
  ##
  elem <- "diam"
  if(!isTRUE(all.equal(x[[elem]], y[[elem]]))) # for now, later align
    stop("--- diameters don't match ---")
  out[[elem]] <- x[[elem]]
  ##
  xnum <- suppressWarnings(as.integer(rownames(x[["nconc"]])))
  ynum <- as.integer(rownames(y[["nconc"]]))
  if(!is.logical(basename)) {
    xi <- xnum
    yi <- max(xi) + ynum
  } else {
    fn <- if(basename) function(x) Last(strsplit(x, "\\\\")) else identity
    if(!all(is.na(xnum))) { # already MultiIndex
      xi <- MultiIndex(list(Last(fn(row.names(x[["parameters"]]))), xnum))
    } else {
      xi <- rownames(x[["nconc"]])
    }
    yi <- MultiIndex(list(Last(fn(row.names(y[["parameters"]]))), ynum))
  }
  ##
  for(elem in c("nconc", "metatable")) {
    rownames(x[[elem]]) <- xi
    rownames(y[[elem]]) <- yi
    out[[elem]] <- rbind(x[[elem]], y[[elem]])
  }
  ##
  elem <- "datetime"
  out[[elem]] <- c(x[[elem]], y[[elem]])
  names(out[[elem]]) <- c(xi, yi)
  ##
  ## return value
  class(out) <- c("TSI", class(out))
  out
}
