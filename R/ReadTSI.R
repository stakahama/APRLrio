
#' ReadTSI
#'
#' Read output of TSI AIM software
#'
#' @param filename
#' @param delimiter 
#'
#' @return list
#' @export

ReadTSI <- function(filename, delimiter=",") {
  
  ## read to table
  lines <- RemoveEmptyLines(StripNonASCII(readLines(filename)))
  iStart <- grep("^Sample \\#",lines)
  headerline <- strsplit(lines[iStart], delimiter)[[1]]
  meta <- with(list(x=do.call(rbind, strsplit(head(lines, iStart-1), delimiter))),
               setNames(x[,2], x[,1]))
  table <- read.table(text=tail(lines,-iStart), sep=",", comment.char="",
                      col.names=headerline, check.names=FALSE, as.is=TRUE)

  ## metadata
  header <- suppressWarnings(as.numeric(headerline))
  iDiam <- which(!is.na(header))
  iTime <- match(c("Date","Start Time"),names(table))
  datetime <- chron::as.chron(do.call(paste, table[,iTime]), "%m/%d/%y %T")
  diamlab <- sprintf("V%d",seq_along(iDiam))

  ## return value
  c(as.list(meta),
    list(datetime=datetime,
         diam=setNames(header[iDiam], diamlab),
         nconc=data.matrix(setNames(table[,iDiam], diamlab)),
         metatable=table[,-c(iTime, iDiam)]))
}


