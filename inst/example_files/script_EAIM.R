
library(APRLrio)

htmlfiles <- file.path("data", c("E-AIM Output.html","E-AIM Output_2.html"))

MW <- get("MW", environment(CalcMasses))

StackedPolygon <- function(x,ymat,...) {
  col <- list(...)$col
  if(is.null(col)) {
    col <- 1:ncol(ymat)
  }
  y <- rep(0,nrow(ymat))
  for( i in 1:ncol(ymat) ) {
    polygon(c(x,rev(x)),c(ymat[,i]+y,rev(y)),col=col[i],border=NA)
    y <- ymat[,i]+y
  }
}

matplot <- function(...) graphics::matplot(...,ann=FALSE)

title <- function(...) graphics::title(...,xpd=NA)

color <- rainbow(5)

par(mfcol=c(4,2))
par(xaxs="i",yaxs="i")
par(mar=c(2,2,.5,.5),oma=c(2,2,1,1),mgp=c(2,.5,0))
i <- 1
for( filename in htmlfiles ) {
  out <- ReadEAIM(filename)

  conc <- out[[1]]
  sconc <- ExpandSolids(conc)
  moles <- CalcMoles(sconc)
  masses <- CalcMasses(moles)

  xvalues <- masses$NO3+sconc$`n_HNO3(g)`/MW["HNO3"]
  solids <- grep("(s)",names(sconc),value=TRUE)
  gases <- Filter(function(.x) !grepl("H2O",.x),grep("(g)",names(sconc),value=TRUE))
  aq <- Filter(function(.x) !grepl("n_H\\(aq\\)|Volume|H2O|OH",.x),
               grep("\\(aq\\)",names(sconc),value=TRUE))

  matplot(xvalues,sconc[,solids],type="l")
  legend("topright",lty=1:length(solids),col=1:length(solids),solids)
  if(i==1) title(ylab=expression("Moles"))
  
  matplot(xvalues,sconc[,gases],type="l")
  legend("topright",lty=1:length(gases),col=1:length(gases),gases)
  if(i==1) title(ylab=expression("Moles"))
  
  matplot(xvalues,sconc[,aq],type="l",lty=1:length(aq),col=1:length(aq))
  legend("topleft",lty=1:length(aq),col=1:length(aq),aq)
  if(i==1) title(ylab=expression("Moles"))
  
  plot.new()
  plot.window(c(0,100),c(0,120),xaxs="i",yaxs="i")
  axis(1); axis(2); box()
  StackedPolygon(masses$NO3+sconc$`n_HNO3(g)`/MW["HNO3"],
                  masses[,c("Na","Cl","NO3","NH4","SO4")],
                  col=color)
  legend("topleft",c("Na","Cl","NO3","NH4","SO4"),fill=color)
  if(i==1) title(ylab=expression("Mass"~(mu*g/m^3)))

  mtext(expression("HNO3"~(mu*g/m^3)),1,outer=TRUE)
  i <- i+1
}
