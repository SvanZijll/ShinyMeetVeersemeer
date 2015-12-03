library(ggplot2)
library(plyr)
library(scales)


## Load data, specify path when needed
load(file = "R/DATA/MWTL_Veersemeer_bewerkt.Rdata")
alllocs <- as.character(unique(rws_dat$locatie))
variablelist <- unique(rws_dat$variable)
variablelist

sel = variablelist[2]
subdf = subset(rws_dat, rws_dat$variable == sel &
                 rws_dat$locatie == "Soelekerkepolder oost" &
                 rws_dat$year >= 2000 &
                 rws_dat$year <= 2015)


#=========== Trend line plot ===========================

trend.plot <- function (subdf, variable, xx, yy, unit = "eenheid") {
 x = as.numeric(subdf[,xx]); y = subdf[,yy]    
  regression <- summary(glm(y ~ x + I(cos(2 * pi * x / 31556995)) + I(sin(2 * pi * x / 31556995))))
  slope      <- regression$coefficients[2,1]  #slope of regression including season effect
  pvalue     <- format(regression$coefficients[2,4], digits = 2)
  intercept  <- regression$coefficients[[1]]
  minscale   <- min(subdf[,xx])
  maxscale   <- max(subdf[,xx])
  yposition  <- quantile(subdf[,yy], 0.99)
  substance  <- unique(subdf[,variable])
  unit = unique(subdf[,unit])
  pl <- ggplot(subdf, aes_string(xx, yy))
  pl <- pl + geom_point(aes(), size = 3, color = "seagreen4", alpha = 0.5)
  pl <- pl + geom_smooth(aes(), method='lm', formula = y ~ x+I(cos(2*pi*x/31622400))+I(sin(2*pi*x/31622400)), size = 0.7, alpha = 0.3, n = 1000)
  pl <- pl + geom_abline(intercept = intercept, slope = slope , color = "darkblue", size = 1)
  pl <- pl + annotate("text", label = paste("slope =", format(slope*31622400, digits = 2), unit, "per year | ", "p=", pvalue), x = maxscale - 0.5 * (maxscale - minscale), y = yposition)
  pl <- pl + theme(text = element_text(size = 16), legend.position = "bottom")
  pl <- pl + scale_x_datetime(minor_breaks = date_breaks("1 year"))
  pl <- pl + xlab("year") + ylab(paste(substance, unit))
  print(pl)
}

trend.plot(subdf, "variable", xx = "datetime", yy = "waarde", unit = "eenheid")

##===== Loess curve fitting (pattern recognition) ==========================================

lspan = 0.5  ## can be varied
pl <- ggplot(subdf, aes(datetime, waarde))
# pl <- pl + facet_grid(variable ~ locatie, scales = "free")
#            pl <- pl + geom_smooth(aes(color = season) , method="lm", size = 2)
pl <- pl + geom_point(aes(), color = "seagreen4", alpha = 0.5)
pl <- pl + geom_smooth(aes(), method='loess', span = lspan, size = 1, n = 1000)
pl <- pl + theme(text = element_text(size = 16), legend.position = "bottom")
pl <- pl + scale_x_datetime(minor_breaks = date_breaks("1 year"))
pl <- pl + xlab("date") + ylab(unit)
print(pl)

##====== 90 percentile plotting

subdf_year <- ddply(subdf, ~ year + season, summarize, perc = quantile(waarde, probs = 0.9), na.rm = T)
subdf_year$year <- as.POSIXct(paste(as.character(subdf_year$year), "-07-01 00:00:00", sep = ""))

pl <- ggplot(subdf, aes(datetime, waarde))
pl <- pl + facet_grid(variable ~ locatie, scales = "free")
pl <- pl + geom_point(aes(color = season), size = 3, alpha = 0.6)
pl <- pl + geom_crossbar(data = subdf_year, aes(year, perc, color = season), size = 0.75, ymin =F, ymax = F)
pl <- pl + theme(text = element_text(size = 16), legend.position = "bottom")
pl <- pl + scale_x_datetime(minor_breaks = date_breaks("1 year"))
pl <- pl + xlab("date") + ylab(unit)
print(pl)



##===== make boxplot ====================

minscale <- min(subdf$datetime)
maxscale <- max(subdf$datetime)

summer90 <- quantile(subset(subdf, subdf$season == "summer")$waarde,  probs = 0.9)
winter90 <- quantile(subset(subdf, subdf$season == "winter")$waarde,  probs = 0.9)
subdf_season <- ddply(subdf, ~ season, summarize, perc = quantile(waarde, probs = 0.9))

## Create plot
q <- ggplot(subdf,aes(month,waarde))
  q <- q + geom_jitter(position = position_jitter(width = .5), colour = "seagreen4", alpha = 0.3)
  q <- q + geom_boxplot(outlier.colour = "orange", alpha = 0.5, color = "orange", notch = F, notchwidth = 0.5, outlier.size = F)
  # q <- q + geom_smooth(aes(group = 1), method='lm', formula = y ~ I(cos(2*pi*x/12))+I(sin(2*pi*x/12)), size = 0.7, alpha = 0.2)
  q <- q + facet_grid(variable ~ locatie, scales = "free") +
    theme(text = element_text(size = 16), legend.position = "bottom") +
    xlab("month") + ylab(unit)
  print(q)
  
  
