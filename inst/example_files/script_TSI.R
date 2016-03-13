library(APRLrio)
library(chron)

sizes <- ReadTSI(file.path("data","ENV400_20140307.txt"))

## with(sizes, image(datetime, diam, nconc, log="y"))

ix <- sizes$datetime < chron("03/07/14","09:00:00")
with(sizes, image(datetime[ix], diam, nconc[ix,], log="y"))

with(sizes, matplot(diam, t(nconc[ix,]), log="x", type="l"))
