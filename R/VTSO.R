
require(readr)
vtso <- read_delim("R/DATA/vm_all_01.csv", 
                  col_types = "cnnnnnnnnnnnnn__nn_Diinnii_____",
                  na = "9999", col_names = T, delim = ";")
summary(vtso)

colmapping <- read_csv2("R/DATA/names.csv") 

require(plyr)
colnames(vtso) <- mapvalues(x = colnames(vtso), colmapping$`colnames(vtso)`, colmapping$NL_name)

plot(vtso$zuurstofverzadiging)
# filter out unreasonable high values
vtso <- vtso[vtso$zuurstofverzadiging < 300,]
vtso_plot <- vtso[vtso$locatie %in% 8,]   #c(1,2,3,8,10,16,17)
vtso_plot <- vtso_plot[vtso_plot$maand %in% c(5,6),]   #c(1,2,3,8,10,16,17)


## er is iets niet goed met de laagjes, kijk maar
qplot(data = vtso_plot, x = laag, y = dieptemeting)

require(ggplot2)
limits <- aes(ymax = zuurstofverzadiging + se, ymin=zuurstofverzadiging - se)
pl <- ggplot(vtso_plot, aes(-dieptemeting, zuurstofverzadiging))
pl + geom_point(aes(color = as.factor(laag))) +
  facet_grid(locatie ~ maand) +
  coord_flip()

pl <- ggplot(vtso_plot, aes(-dieptemeting, zuurstofverzadiging))
pl + geom_boxplot(aes(group = as.factor(laag), fill = as.factor(laag))) +
  # geom_jitter(aes(color = as.factor(laag)), size = 2) +
  facet_grid(maand ~ locatie) +
  coord_flip()
