## Script door Willem Stolte
## Leest meetpaal data Veerse meer - nu nog van N schijf
## later liefst van RWS laten lezen

## ontwikkeling - lees data van RWS en download naar P: schijf of liever nog een repository

## ontwikkeling - Check individuele datafiles, en bind de files tot 1

## Operationeel - lees "verrijkte" files (door Arno nu al met de hand gemaakt) en maak grafieken

require(readr)
require(ggplot2)
require(plyr)
require(reshape2)
require(scales)

wdir <- "n:/Projects/1220000/1220248/B. Measurements and calculations/Meetpalen VM3 VM4 VM5"
VM5_all <- list.files(pattern = "VM5")

## Voor snel grafieken maken, ga naar regel 48 (load data)

VM5<-read.csv2(file.path(wdir, "Meetpaal_VM5_2000-2015.csv"), na = 999.999, dec = ".")
VM5 <- VM5[c("Datum", "Peil", "Waterstand", "Temperatuur_boven", "Temperatuur_beneden", "Saliniteit_boven", "Saliniteit_beneden")]
VM5$Datumtijd <- as.POSIXlt(VM5$Datum, format = "%d-%m-%Y %H:%M", tz = "UTC")
VM5$Datumtijd <- as.POSIXct(VM5$Datumtijd)
VM5$Meetpaal <- "Meetpaal VM5 Schotsman/Ruiterplaat"

VM4 <- read.csv2(file.path(wdir, "Meetpaal_VM4_2000-2015.csv"), na = 999.999, dec = ".")
VM4 <- VM4[c("Datum", "Peil", "Waterstand..m.NAP.", "Temperatuur_boven", "Temperatuur_beneden", "Saliniteit_boven", "Saliniteit_beneden")]
colnames(VM4) <- c("Datum", "Peil", "Waterstand", "Temperatuur_boven", "Temperatuur_beneden", "Saliniteit_boven", "Saliniteit_beneden")
VM4$Datumtijd <- as.POSIXlt(VM4$Datum, format = "%d-%m-%Y %H:%M", tz = "UTC")
VM4$Datumtijd <- as.POSIXct(VM4$Datumtijd)
VM4$Meetpaal <- "Meetpaal VM4 Oranjeplaat"

VM <- rbind(VM4, VM5)

##Bereken daggemiddelde
VM$Datum <- as.Date(VM$Datumtijd)
VMdag <- ddply(VM, .(Datum, Meetpaal), summarize,
               'Waterstand' = mean(Waterstand),
               'Temperatuur boven' = mean(Temperatuur_boven),
               'Temperatuur beneden' = mean(Temperatuur_beneden), 
               'Saliniteit boven' = mean(Saliniteit_boven),
               'Saliniteit beneden' = mean(Saliniteit_beneden))
VMdag_tidy <- melt(VMdag, id.vars = c("Datum", "Meetpaal"))
VMdag_tidy$Meting <- paste(VMdag_tidy$Meetpaal, VMdag_tidy$variable)

save(VM, VMdag, VMdag_tidy, file = "VMdata.rdata")

## To reload processed data (much quicker than processing above code)
load(file.path(wdir, "VMdata.rdata"))

## daggemiddelde Saliniteit

plot.meetpaal.data <- function (file, xx = "Datum", yy = "value", pointcolor = "Meting", ylabel, xlabel) {
  plot <- ggplot(file ,aes_string(xx, yy))
  plot + 
    geom_line(aes_string(color = pointcolor)) +
    geom_vline(xintercept = as.numeric(as.Date("2004-06-01")), size = 1, linetype = 2, color = "black", alpha = 0.5) +
    ylab(ylabel) + xlab(xlabel) +theme(legend.position = "top", legend.direction = "horizontal") +
    scale_x_date(minor_breaks = date_breaks("year"))
}

## select salinity data and plot
newsubset <- subset(VMdag_tidy, VMdag_tidy$variable %in% c("Saliniteit boven", "Saliniteit beneden"))
plot.meetpaal.data(newsubset, ylabel = "Daggemiddelde saliniteit (PSU)", xlabel = "Datum")

## select temperature data and plot
newsubset <- subset(VMdag_tidy, VMdag_tidy$variable %in% c("Temperatuur boven", "Temperatuur beneden"))
plot.meetpaal.data(newsubset, ylabel = "Daggemiddelde temperatuur (C)", xlabel = "Datum")

