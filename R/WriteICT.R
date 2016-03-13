
#' JSON2ICT
#'
#' Generate ICT-formatted files
#'
#' @param jsonfile
#' @param csvfile [optional]
#' @param ictfile [optional]
#'
#' @return produces side effect (writing ICT files)
#' @export

## "variables and units"="var1 (units), var2 (units)"

JSON2ICT <- local({

  ## ---------------------------------------------------------------------------
  CountLines <- function(x)
    length(strsplit(x,"\n")[[1]])+1

  Sprintf <- function(format, arglist) {
    ## format specifies %(name)s, etc.
    ## accepts arglist of named arguments (as list)
    patt <- "%\\(([^)]*)\\)"
    fmt <- gsub(patt, "%", format)
    args <- arglist[gsubfn::strapplyc(format, patt)[[1]]]
    do.call("sprintf", c(fmt, args))
  }

  Join <- function(...) {
    args <- lapply(list(...),Strip)
    if(length(args)==1) {
      do.call(paste,c(args,collapse="\n"))
    } else {
      do.call(paste,c(args,sep="\n"))
    }
  }

  ## ---------------------------------------------------------------------------  
  Template <- function(x) {
    switch(x,
           "header" = "%(userlines)s
%(independent variable)s 
%(number of variables)d
%(scale factors)s
%(missing data indicators)s
%(variables and units)s
",
           "special comments" = "%(nlines)d
%(userlines)s
",
           "normal comments" = "%(nlines)d
%(userlines)s
")
  }

  HeaderVars <- function(variables) {
    ## list of {independent, dependent}:
    ##   independent variable: "var1"
    ##   dependent variable: c("var2 (units)","var3 (units)")
    ## scale factor is defined as 1
    ## missing data encoded as -9999
    independent <- variables[["independent"]]
    dependent <- variables[["dependent"]]
    nvars <- length(dependent)
    list("independent variable"=independent,
         "number of variables"=nvars,
         "scale factors"=paste(rep(1,nvars),collapse=", "),
         "missing data indicators"=paste(rep(-9999,nvars),collapse=", "),
         "variables and units"=paste(dependent,collapse=", "))
  }

  Args <- function(userlines,variables=NULL) {
    if(is.null(variables)) {
      out <- list(nlines=CountLines(userlines),
                  userlines=Strip(userlines))
    } else {
      out <- c(userlines=Strip(userlines),
               HeaderVars(variables))
    }
    out
  }

  Build <- function(...) {
    ## add (header lines, format code)
    header <- Join(...)
    nlines <- CountLines(header)
    Join(sprintf("%d, 1001",nlines), header)
  }

  ## -----------------------------------------------------------------------------
  BuildHeader <- function(contents) {
    Build(
      Sprintf(Template("header"),
              Args(contents[["header"]],
                   contents[["variables"]])),
      Sprintf(Template("special comments"),
              Args(contents[["special comments"]])),
      Sprintf(Template("normal comments"),
              Args(contents[["normal comments"]]))
      )
  }

  ReadTable <- function(tablefile, time) {
    table <- read.csv(tablefile, row.names=NULL, as.is=TRUE)
    days2secs <- function(x) x*86400
    timecols <- table[,time[["variables"]]]
    for(x in names(timecols)) {
      timecols[,x] <- days2secs(c(unclass(
        chron::as.chron(timecols[,x],time[["format"]])-
          chron::as.chron(time[["reference"]],time[["format"]])
        )))
    }
    table[,time[["variables"]]] <- timecols
    table
  }

  WriteICT <- function(file, header, table, na="-9999") {
    sink(file)
    cat(header)
    cat("\n")
    write.table(table, sep= ",", col.names= TRUE, row.names=FALSE,
                quote=FALSE,na=na)
    sink()   
  }

  function(jsonfile, csvfile, ictfile) {
    contents <- RJSONIO::fromJSON(jsonfile)
    directory <- dirname(jsonfile)
    if(missing(csvfile))
      csvfile <- file.path(directory, contents[["csvfile"]])
    if(missing(ictfile))
      ictfile <- file.path(directory,contents[["ictfile"]])
    WriteICT(ictfile,
             BuildHeader(contents),
             ReadTable(csvfile, contents[["time"]]))
  }

})
