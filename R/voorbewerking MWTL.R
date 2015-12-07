#######################################################################
####                                                               ####
####          Script by willem.stolte@deltares.nl                  ####
####    Reads MWTL csv and saves file for presentation purpose     ####
####                                                               ####
####                copyright Deltares                             ####
####                                                               ####
#######################################################################

require(scales)
require(plyr)
require(reshape2)
require(ggplot2)

## script eats waterbase data (live.waterbase.nl)
## and produces .Rdata file for use in Shiny app

submap <- read.csv2("d:/Tools_Scripts/Mapping tables/RWS2DELWAQ2names.csv", header = T, stringsAsFactors=FALSE)
locmap <- read.csv2("d:/Tools_Scripts/Mapping tables/RWS2DELWAQ2locations.csv", header = T, stringsAsFactors=FALSE)
# locmod = c("Huibertgat_oost", "IMARES_st_2", "IMARES_st_3b", "Bocht_van_Watum", "IMARES_st_4b", "IMARES_st_5", "Groote_Gat_noord")

# rws_dat           <- read.csv2("d:\\GIS-DATA\\Nederland\\EemsDoll\\naarPostGis\\RWS\\MWTL_all.csv",dec = ".")
rws_dat2           <- read.csv2("R/DATA/collected-data.csv", header = T)
# rws_dat$waarde       <- as.numeric(rws_dat$waarde)
rws_dat2$datetime   <- as.POSIXct(
  paste(rws_dat2$datum, rws_dat2$tijd),
  format = "%Y-%m-%d %H:%M")

## make wide table for building composite variables
require(reshape2)
rws_dat.wide <- dcast(rws_dat2, locatie + datetime + eenheid + x.lat + y.long ~ waarnemingssoort, value.var = "waarde", mean)
rws_dat.wide$'Opgelost anorganisch stikstof' <- rws_dat.wide$`Nitraat in mg/l uitgedrukt in stikstof / na filtratie in oppervlaktewater` +
  rws_dat.wide$`Nitriet in mg/l uitgedrukt in stikstof / na filtratie in oppervlaktewater` +
  rws_dat.wide$`Ammonium in mg/l uitgedrukt in stikstof / na filtratie in oppervlaktewater`

rws_dat <- melt(rws_dat.wide, id.vars = c("locatie", "datetime", "eenheid", "x.lat", "y.long"), 
                variable.name = "waarnemingssoort", 
                value.name = "waarde", na.rm = T)

rws_dat$variable   <- mapvalues(as.character(rws_dat$waarnemingssoort), from = submap$RWS_wns, to = submap$NL_name, warn_missing = F)
# rws_dat$location   <- mapvalues(as.character(rws_dat$locoms), from = locmap$locoms, to = locmap$Delwaq_ED, warn_missing = F)
# rws_dat$variable   <- mapvalues(as.character(rws_dat$variable), from = submap$Delwaq, to = submap$Delwaq_long_name, warn_missing = F)
rws_dat$month      <- format(rws_dat$datetime, format = "%m")
rws_dat$year <- as.numeric(format(rws_dat$datetime, "%Y"))
rws_dat$season <- ifelse(rws_dat$month %in% c("10", "11", "12", "01", "02"), "winter", "summer")
# rws_dat <- subset(rws_dat, rws_dat$kwccod < 50 )
# save(rws_dat, file = "d:/Tools_Scripts/R/ShinyMeetVeersemeer/data/MWTL_Veersemeer_bewerkt.Rdata")
#filter for high PO4 and NH4 measurements
rws_dat <- subset(rws_dat, rws_dat$waarde < 2 | rws_dat$variable != "opgelost fosfaat")
rws_dat <- subset(rws_dat, rws_dat$waarde < 4 | rws_dat$variable != "totaal opgelost fosfaat")
rws_dat <- subset(rws_dat, rws_dat$waarde < 3 | rws_dat$variable != "totaal fosfaat")
rws_dat <- subset(rws_dat, rws_dat$waarde < 2 | rws_dat$variable != "ammonium")
# rws_dat <- subset(rws_dat, rws_dat$waarde < 75 | rws_dat$variable != "chlorophyll-a")
# rws_dat <- subset(rws_dat, rws_dat$waarde < 3 | rws_dat$variable != "N pg")
# rws_dat <- subset(rws_dat, rws_dat$waarde < 30 | rws_dat$variable != "dissolved org C")
# rws_dat <- subset(rws_dat, rws_dat$waarde < 500 | rws_dat$variable != "suspended solids")
# rws_dat <- subset(rws_dat, rws_dat$waarde < 1000 )
# rws_dat <- subset(rws_dat, rws_dat$waarde >= 0 )
# rws_dat <- subset(rws_dat, rws_dat$kwccod == 0 )
# 
# save(rws_dat, file = "d:/Tools_Scripts/R/ShinyMeetVeersemeer/data/MWTL_Veersemeer_filtered.Rdata")
save(rws_dat, file = "R/data/MWTL_Veersemeer_bewerkt.Rdata")

