
MW <- c(H=1.01,OH=17.00,SO4=96.06,NO3=62.00,NH4=18.04,
        Na=22.99,Cl=35.45,Br=79.90,HSO4=97.07,H2O=18.02,
        HNO3=63.01,NH3=17.03)

v <- c(H=1,OH=-1,SO4=-2,NO3=-1,NH4=1,Na=1,Cl=-1,Br=-1)


model.salts <- list(
  "2"="1	ice
3	H2SO4 · H2O
4	H2SO4 · 2H2O
5	H2SO4 · 3H2O
6	H2SO4 · 4H2O
7	H2SO4 · 6.5H2O
8	HNO3 · H2O
9	HNO3 · 3H2O
10	(NH4)2SO4
11	(NH4)3H(SO4)2
12	NH4HSO4
13	NH4NO3
14	2NH4NO3 · (NH4)2SO4
15	3NH4NO3 · (NH4)2SO4
16	NH4NO3 · NH4HSO4
28	HCl · 3H2O
29	HNO3 · 2H2O",
  "3"="10	(NH4)2SO4
11	(NH4)3H(SO4)2
12	NH4HSO4
13	NH4NO3
14	2NH4NO3 · (NH4)2SO4
15	2NH4NO3 · (NH4)2SO4
16	NH4NO3 · NH4HSO4
17	NH4Cl
18	Na2SO4
19	Na2SO4 · 10H2O
20	Na3H(SO4)2
21	NaHSO4 · H2O
22	NaHSO4
23	NaH3(SO4)2 · H2O
24	Na2SO4 · (NH4)2SO4 · 4H2O
25	NaNO3
26	NaNO3 · Na2SO4 · H2O
27	NaCl"
    )


cmpd.units <- list("ice" = c(H2O=1),
              "H2O" = c(H2O=1),
              "H2SO4" = c(H=2,SO4=1),
              "HNO3" = c(H=1,NO3=1),
              "NH4NO3" = c(NH4=1,NO3=1),
              "NH4HSO4" = c(NH4=1,H=1,SO4=1),
              "(NH4)2SO4" = c(NH4=2,SO4=1),
              "(NH4)3H(SO4)2" = c(NH4=3,H=1,SO4=2),
              "NaNO3" = c(Na=1,NO3=1),
              "Na2SO4" = c(Na=2,SO4=1),
              "NaHSO4" = c(Na=1,H=1,SO4=1),
              "NaH3(SO4)2" = c(Na=1,H=3,SO4=2),
              "Na3H(SO4)2" = c(Na=3,H=1,SO4=2),
              "NH4Cl" = c(NH4=1,Cl=1),
              "HCl" = c(H=1,Cl=1),
              "NaCl" = c(Na=1,Cl=1))



#' ReadEAIM
#'
#' Read E-AIM HTML
#'
#' @param filename file name
#' @param divider line divider between blocks of data
#'
#' @return List of 3 tables.
#' @export

ReadEAIM <- local({

  ReadHTML <- function(url) {
    ## http://stackoverflow.com/questions/1844829/how-can-i-read-and-parse-the-contents-of-a-webpage-in-r
    webpage <- readLines(url)
    pagetree <- XML::htmlTreeParse(webpage, error=function(...) NULL, useInternalNodes=TRUE)
    x <- XML::xpathSApply(pagetree, "//*/pre", XML::xmlValue)  # parse the tree by tables
    lines <- unlist(strsplit(x, "\n")) # do some clean up with regular expressions
  }

  ReadSingle <- function(start, end, lines, fixedwidths=FALSE) {
    if(fixedwidths) {
      ## find widths
      numeol <- max(nchar(lines[start:end]))
      whites <- gregexpr("[ ]+",lines[start:end])
      maxcol <- sapply(whites[-1],length)==max(sapply(whites[-1],length))
      widthtable <- do.call(rbind,lapply(whites[-1][maxcol],diff))
      lastfield <- max(sapply(whites,tail,1))
      widths <- c(apply(widthtable,2,max,na.rm=TRUE),numeol-lastfield+1) #min?
      ## read the data
      header <- scan(text=lines[start],what="",quiet=TRUE)
      tc <- textConnection(paste(collapse="\n",lines[seq(start+1,end)]))
      table <- read.fwf(tc, widths=widths, header=FALSE, as.is=TRUE)
      close(tc)
    } else {
      header <- scan(text=lines[start],what="",quiet=TRUE)
      tc <- textConnection(paste(collapse="\n",lines[seq(start+1,end)]))
      table <- read.table(tc, col.names=header[!grepl("Err",header)], fill=TRUE, as.is=TRUE)
      close(tc)
    }
    ##
    iFail <- grep("iFail",header)
    if(length(iFail)>0 && ncol(table) < length(header)) {
      table <- data.frame(table[,seq(1,iFail)],Err=NA,table[,-seq(1,iFail)])
    }
    names(table) <- header
    table
  }

  function(filename, divider="[-]{5,}") {
    lines <- ReadHTML(filename)
    lines <- lines[!lines %in% c("", " ")]
    ih <- grep(divider, lines)+1
    tables <- Map(ReadSingle,ih,c(head(ih,-1)+(diff(ih)-3),length(lines)),
                  MoreArgs=list(lines))
    tables
  }

})


#' ExpandSolids
#'
#' Tabularize solid columns in E-AIM table
#'
#' @param table E-AIM table
#'
#' @return table with expanded solids columns
#' @export

ExpandSolids <- local({

  Extract1 <- function(table, var) {
    patt <- sprintf("%s_(s[0-9]+)", var)
    select <- grep(patt,names(table), value=TRUE)
    long <- reshape2::melt(subset(table,,c("I",select)), id.vars="I")
    within(long,{
      solid <- sub(patt,"\\1", variable)
      variable <- NULL
    })
  }

  function(table) {
    select <- grep("(moles|vol|id|name)_s[0-9]+",names(table),value=TRUE)
    if(length(select)==0)
      return(table)
    wide <- merge(Extract1(table,"name"), Extract1(table,"moles"),
                  by=c("I","solid"), suffix=c(".name",".moles"), all=TRUE)
    wide <- subset(wide,!is.na(value.moles))
    wide$value.name[] <- ifelse(!is.na(wide$value.name),
                                sprintf("n_%s(s)",StripWhite(wide$value.name)),
                                NA)
    solids <- reshape2::dcast(wide,I~value.name,value.var="value.moles")
    merge(subset(table,,names(table)[!names(table) %in% select]),
          solids,by="I",all=TRUE)
  }
})


#' CalcMoles
#'
#' Calculate moles
#'
#' @param table E-AIM table
#' @param vars ions
#'
#' @return table of moles
#' @export

CalcMoles <- local({

  Txt2table <- function(label, text)
    cbind(data.frame(model=label),
          read.table(text=text, sep="\t", col.names=c("id","name"),
                     encoding="UTF-8", as.is=TRUE))

  PrefixNum <- function(x) {
    num <- as.numeric(sub("^([0-9.]+)([A-Z].+)","\\1",x))
    cmpd <- sub("^([0-9.]+)([A-Z].+)","\\2",x)
    setNames(replace(num,is.na(num),1),cmpd)
  }

  Fill <- function(x,u) {
    y <- x[u]
    setNames(replace(y,is.na(y),0),u)
  }

  ExtractNames <- function(x)
    unique(names(unlist(unname(x))))


  solids <- do.call(rbind, Map(Txt2table, names(model.salts), model.salts))
  solids$name[] <- gsub("[ ]·[ ]","*",solids$name)
  ##
  decomp <- suppressWarnings(lapply(strsplit(setNames(,solids$name),"\\*"), PrefixNum))
  solidelems <- do.call(rbind, lapply(decomp, Fill, ExtractNames(decomp)))
  solidunits <- do.call(rbind, lapply(cmpd.units, Fill, ExtractNames(cmpd.units)))
  solidmat <- solidelems %*% solidunits[colnames(solidelems),]
  ## solidmw <- rowSums(sweep(solidmat,2,MW[colnames(solidmat)],"*"))

  function(table, vars=c("SO4","NO3","NH4","Cl","Na")) {
    ## totalmoles
    patt <- "n_(.+)\\(aq|s\\)"
    aq <- grep(patt,names(table),value=TRUE)
    sn <- sub(patt,"\\1",aq)
    starred <- setNames(sub("\\.","*",names(table)),names(table))
    moles <- mapply(function(x) {
      ilp <- aq[grep(x,sn)]
      isp <- starred[starred %in% names(solidmat[,x])]
      (if(length(ilp)>0) rowSums(table[,ilp,drop=FALSE],na.rm=TRUE)
       else 0) + (if(length(isp)>0) rowSums(sweep(table[,names(isp),drop=FALSE],2,solidmat[,x][isp],"*"), na.rm=TRUE)
                  else 0)
    }, vars)
    data.frame(moles)
  }
})

#' CalcMasses
#'
#' Table of masses
#'
#' @param table E-AIM table
#'
#' @return table of masses
#' @export

CalcMasses <- function(table) {
  masses <- sweep(table,2,MW[colnames(table)],"*")
  masses$PM <- rowSums(masses)
  masses
}
