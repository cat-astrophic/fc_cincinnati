# This script runs regressions for the Cincinnati FC project

# Loading libraries

library(AER)
library(stargazer)
library(sandwich)
library(jtools)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(modelsummary)
library(sf)
library(leaflet)

# Project directory info

direc <- 'D:/cincinnati_fc/'

# Reading in the data

data <- read.csv(paste(direc, 'data/real_house_prices.csv', sep = ''))

# Event dates

usl.announed <- '8/12/2015'
mls.announed <- '5/29/2018'
mls.match <- '3/2/2019'
tql.announed <- '12/18/2018' # groundbreaking date
tql.opened <- '5/16/2021'

# Adding indicators for period in which transactions occurred

# Two years before event

usl.a0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('8/12/2015', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('8/12/2013', '%m/%d/%Y')) # Two years before
mls.a0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/29/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/29/2016', '%m/%d/%Y')) # Two years before
mls.m0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('3/2/2019', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('3/2/2017', '%m/%d/%Y')) # Two years before
tql.a0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('12/18/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('12/18/2016', '%m/%d/%Y')) # Two years before
tql.o0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/16/2021', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/16/2019', '%m/%d/%Y')) # Two years before

# Two years after event

usl.a1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('8/12/2015', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('8/12/2017', '%m/%d/%Y')) # Two years after
mls.a1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/29/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/29/2020', '%m/%d/%Y')) # Two years after
mls.m1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('3/2/2019', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('3/2/2021', '%m/%d/%Y')) # Two years before
tql.a1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('12/18/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('12/18/2020', '%m/%d/%Y')) # Two years before
tql.o1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/16/2021', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/16/2023', '%m/%d/%Y')) # Two years before

# Add to data.frame

data$Pre.USL.Announced <- usl.a0
data$Post.USL.Announced <- usl.a1
data$Pre.MLS.Announced <- mls.a0
data$Post.MLS.Announced <- mls.a1
data$Pre.MLS.Match <- mls.m0
data$Post.MLS.Match <- mls.m1
data$Pre.TQL.Announced <- tql.a0
data$Post.TQL.Announced <- tql.a1
data$Pre.TQL.Opened <- tql.o0
data$Post.TQL.Opened <- tql.o1

# Create event markers

data$usl.a <- usl.a0 + usl.a1
data$mls.a <- mls.a0 + mls.a1
data$mls.m <- mls.m0 + mls.m1
data$tql.a <- tql.a0 + tql.a1
data$tql.o <- tql.o0 + tql.o1

# Creating a month-year variable

data$MY <- format(as.Date(data$Transfer.Date, format = '%m/%d/%Y'), format = '%m/%Y')

# Dropping foreclosures from the data set

data <- data[which(data$Foreclosure == 'No'),]

# Since no names are provided for sellers or buyers, this bit rules out outliers

# Initial histogram of real house prices as a reference

hist(data$Real.Price, breaks = 100)

# removing 

data <- data[which(data$Real.Price < quantile(data$Real.Price, .99)),]
data <- data[which(data$Real.Price > quantile(data$Real.Price, .01)),]

# Running a simple hedonic to look at outliers via residuals bc no arms length transactions indicator exists

outlier.finder <- lm(log(Real.Price) ~ log(FinSqFt) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2)
                     + Rooms*log(FinSqFt) + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt)
                     + Half.Baths*log(FinSqFt) + Acreage*factor(School.District) + factor(MY)
                     + factor(Deed.Type) + factor(Owner.Residence), data = data)

# Clustering at the school district level so that I can compare results here to main results later on

outlier.finder.x <- coeftest(outlier.finder, vcov = vcovCL, cluster = ~School.District)

# Viewing the results

stargazer(outlier.finder, outlier.finder.x, type = 'text')

# Get residuals

residuals <- outlier.finder$residuals

# View residuals

hist(residuals, breaks = 100)
abline(v =- sd(residuals)*2)
abline(v = sd(residuals)*2)

# Based on the histogram, let's keep anything within 3 standard deviations of the mean

keep <- which(abs(residuals) < sd(residuals)*2)

# Subset data based on residuals - 98.69% of the data set remains => 1.31% was dropped (dropped 5,890 obs)

data <- data[keep,]

# Updating the residuals and real prices histograms

resid <- residuals[keep]
hist(resid, breaks = 100)
hist(data$Real.Price, breaks = 100)
hist(log(data$Real.Price), breaks = 100)

# Creating treatment variables

data$Treatment.Nippert.hm <- as.numeric(data$Nippert <= .8)
data$Treatment.Nippert.1m <- as.numeric(data$Nippert <= 1.6) - data$Treatment.Nippert.hm
data$Treatment.TQL.hm <- as.numeric(data$TQL <= .8)
data$Treatment.TQL.1m <- as.numeric(data$TQL <= 1.6) - data$Treatment.TQL.hm

# Creating control variables

data$Control.Nippert <- as.numeric(data$Nippert > 1.6) * as.numeric(data$Nippert <= 8)
data$Control.TQL <- as.numeric(data$TQL > 1.6) * as.numeric(data$TQL <= 8)

# Create event-specific data.frames

data$usl.a.x <- data$usl.a * (data$Treatment.Nippert.hm + data$Treatment.Nippert.1m + data$Control.Nippert)
data$mls.a.x <- data$mls.a * (data$Treatment.Nippert.hm + data$Treatment.Nippert.1m + data$Control.Nippert)
data$mls.m.x <- data$mls.m * (data$Treatment.Nippert.hm + data$Treatment.Nippert.1m + data$Control.Nippert)
data$tql.a.x <- data$tql.a * (data$Treatment.TQL.hm + data$Treatment.TQL.1m + data$Control.TQL)
data$tql.o.x <- data$tql.o * (data$Treatment.TQL.hm + data$Treatment.TQL.1m + data$Control.TQL)

data.usl.a <- data[which(data$usl.a.x == 1),]
data.mls.a <- data[which(data$mls.a.x == 1),]
data.mls.m <- data[which(data$mls.m.x == 1),]
data.tql.a <- data[which(data$tql.a.x == 1),]
data.tql.o <- data[which(data$tql.o.x == 1),]

# Run models for each scenario

# USL Announcement

usl.mod <- lm(log(Real.Price) ~ Treatment.Nippert.hm*Post.USL.Announced + Treatment.Nippert.1m*Post.USL.Announced
              + log(FinSqFt) + I(log(FinSqFt)^2) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
              + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
              + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
              + factor(Owner.Residence), data = data.usl.a)

usl.clustered <- coeftest(usl.mod, vcov = vcovCL, cluster = ~Parcel.Number)

stargazer(usl.mod, usl.clustered, type = 'text', omit = c('Deed.Type', 'School.District', 'MY'))

# MLS Announcement

mls.mod <- lm(log(Real.Price) ~ Treatment.Nippert.hm*Post.MLS.Announced + Treatment.Nippert.1m*Post.MLS.Announced
              + log(FinSqFt) + I(log(FinSqFt)^2) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
              + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
              + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
              + factor(Owner.Residence), data = data.mls.a)

mls.clustered <- coeftest(mls.mod, vcov = vcovCL, cluster = ~Parcel.Number)

stargazer(mls.mod, mls.clustered, type = 'text', omit = c('Deed.Type', 'School.District', 'MY'))

# TQL Announcement

tqla.mod <- lm(log(Real.Price) ~ Treatment.TQL.hm*Post.TQL.Announced + Treatment.TQL.1m*Post.TQL.Announced
               + log(FinSqFt) + I(log(FinSqFt)^2) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
               + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
               + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
               + factor(Owner.Residence), data = data.tql.a)

tqla.clustered <- coeftest(tqla.mod, vcov = vcovCL, cluster = ~Parcel.Number)

stargazer(tqla.mod, tqla.clustered, type = 'text', omit = c('Deed.Type', 'School.District', 'MY'))

# TQL Opening

tqlo.mod <- lm(log(Real.Price) ~ Treatment.TQL.hm*Post.TQL.Opened + Treatment.TQL.1m*Post.TQL.Opened
               + log(FinSqFt) + I(log(FinSqFt)^2) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
               + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
               + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
               + factor(Owner.Residence), data = data.tql.o)

tqlo.clustered <- coeftest(tqlo.mod, vcov = vcovCL, cluster = ~School.District)

stargazer(tqlo.mod, tqlo.clustered, type = 'text', omit = c('Deed.Type', 'School.District', 'MY'))

# All results

stargazer(usl.mod, usl.clustered, mls.mod, mls.clustered, tqla.mod, tqla.clustered, tqlo.mod, tqlo.clustered, type = 'text', omit = c('Deed.Type', 'School.District', 'MY'))

write.csv(stargazer(usl.clustered, mls.clustered, tqla.clustered, tqlo.clustered, omit = c('Deed.Type', 'School.District', 'MY')), paste0(direc, 'results/results_robust.txt'), row.names = FALSE)

write.csv(stargazer(usl.mod, mls.mod, tqla.mod, tqlo.mod, type = 'text', omit = c('Deed.Type', 'School.District', 'MY')), paste0(direc, 'results/results_raw.txt'), row.names = FALSE)

# Creating figures incrementing a single treatment radius over distance from the stadiums

usl.a.increments.co <- c()
mls.a.increments.co <- c()
tql.a.increments.co <- c()
tql.o.increments.co <- c()

usl.a.increments.se <- c()
mls.a.increments.se <- c()
tql.a.increments.se <- c()
tql.o.increments.se <- c()

for (i in 5:16) {

  print(i)

  data.usl.a$Treated <- as.numeric(data.usl.a$Nippert <= i/10)

  usl.in <- lm(log(Real.Price) ~ Treated*Post.USL.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
               + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
               + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
               + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
               + factor(Owner.Residence), data = data.usl.a)

  usl.in.cl <- coeftest(usl.in, vcov = vcovCL, cluster = ~Parcel.Number)

  xxx <- broom::tidy(usl.in.cl)
  id <- which(xxx$term == 'Treated:Post.USL.Announced')

  usl.a.increments.co <- c(usl.a.increments.co, xxx$estimate[id])
  usl.a.increments.se <- c(usl.a.increments.se, xxx$std.error[id])

}

for (i in 5:16) {

  print(i)

  data.mls.a$Treated <- as.numeric(data.mls.a$Nippert <= i/10)

  mls.in <- lm(log(Real.Price) ~ Treated*Post.MLS.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
               + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
               + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
               + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
               + factor(Owner.Residence), data = data.mls.a)

  mls.in.cl <- coeftest(mls.in, vcov = vcovCL, cluster = ~Parcel.Number)

  xxx <- broom::tidy(mls.in.cl)
  id <- which(xxx$term == 'Treated:Post.MLS.Announced')

  mls.a.increments.co <- c(mls.a.increments.co, xxx$estimate[id])
  mls.a.increments.se <- c(mls.a.increments.se, xxx$std.error[id])

}

for (i in 5:16) {

  print(i)

  data.tql.a$Treated <- as.numeric(data.tql.a$TQL <= i/10)

  tql.in <- lm(log(Real.Price) ~ Treated*Post.TQL.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
               + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
               + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
               + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
               + factor(Owner.Residence), data = data.tql.a)

  tql.in.cl <- coeftest(tql.in, vcov = vcovCL, cluster = ~Parcel.Number)

  xxx <- broom::tidy(tql.in.cl)
  id <- which(xxx$term == 'Treated:Post.TQL.Announced')

  tql.a.increments.co <- c(tql.a.increments.co, xxx$estimate[id])
  tql.a.increments.se <- c(tql.a.increments.se, xxx$std.error[id])

}

for (i in 5:16) {

  print(i)

  data.tql.o$Treated <- as.numeric(data.tql.o$TQL <= i/10)

  tql.in <- lm(log(Real.Price) ~ Treated*Post.TQL.Opened + log(FinSqFt) + I(log(FinSqFt)^2)
               + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2) + Rooms*log(FinSqFt)
               + Bedrooms*log(FinSqFt) + Full.Baths*log(FinSqFt) + Half.Baths*log(FinSqFt)
               + Acreage*factor(School.District) + factor(MY) + factor(Deed.Type)
               + factor(Owner.Residence), data = data.tql.o)

  tql.in.cl <- coeftest(tql.in, vcov = vcovCL, cluster = ~Parcel.Number)

  xxx <- broom::tidy(tql.in.cl)
  id <- which(xxx$term == 'Treated:Post.TQL.Opened')

  tql.o.increments.co <- c(tql.o.increments.co, xxx$estimate[id])
  tql.o.increments.se <- c(tql.o.increments.se, xxx$std.error[id])

}

# Plotting the results with both sets of standard errors

Distance <- c(5:16) * 1000 / 10
Breaks <- c(1:10)/ 2 * 1000
Coefficient <- usl.a.increments.co
SE.low <- Coefficient - 1.96*usl.a.increments.se
SE.high <- Coefficient + 1.96*usl.a.increments.se
res.df <- as.data.frame(cbind(Distance, Coefficient, SE.low, SE.high))

png(paste(direc, 'figures/usl_plot.png', sep = ''))

ggplot(data = res.df, aes(x = Distance, y = Coefficient)) +
  geom_line(size = 1, color = 'red4') +
  geom_ribbon(aes(ymin = SE.low, ymax = SE.high), size = 1, alpha = 0.5, color = 'orange') +
  xlab('Distance in Meters') +
  ylab('Hedonic Coefficient') +
  labs(title = 'Estimated Effect of USL Announcement w.r.t. Distance') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = Breaks) +
  geom_hline(yintercept = 0, size = 2)

dev.off()

Coefficient <- mls.a.increments.co
SE.low <- Coefficient - 1.96*mls.a.increments.se
SE.high <- Coefficient + 1.96*mls.a.increments.se
res.df <- as.data.frame(cbind(Distance, Coefficient, SE.low, SE.high))

png(paste(direc, 'figures/mls_plot.png', sep = ''))

ggplot(data = res.df, aes(x = Distance, y = Coefficient)) +
  geom_line(size = 1, color = 'red4') +
  geom_ribbon(aes(ymin = SE.low, ymax = SE.high), size = 1, alpha = 0.5, color = 'orange') +
  xlab('Distance in Meters') +
  ylab('Hedonic Coefficient') +
  labs(title = 'Estimated Effect of MLS Announcement w.r.t. Distance') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = Breaks) +
  geom_hline(yintercept = 0, size = 2)

dev.off()

Coefficient <- tql.a.increments.co
SE.low <- Coefficient - 1.96*tql.a.increments.se
SE.high <- Coefficient + 1.96*tql.a.increments.se
res.df <- as.data.frame(cbind(Distance, Coefficient, SE.low, SE.high))

png(paste(direc, 'figures/tqla_plot.png', sep = ''))

ggplot(data = res.df, aes(x = Distance, y = Coefficient)) +
  geom_line(size = 1, color = 'red4') +
  geom_ribbon(aes(ymin = SE.low, ymax = SE.high), size = 1, alpha = 0.5, color = 'orange') +
  xlab('Distance in Meters') +
  ylab('Hedonic Coefficient') +
  labs(title = 'Estimated Effect of TQL Announcement w.r.t. Distance') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = Breaks) +
  geom_hline(yintercept = 0, size = 2)

dev.off()

Coefficient <- tql.o.increments.co
SE.low <- Coefficient - 1.96*tql.o.increments.se
SE.high <- Coefficient + 1.96*tql.o.increments.se
res.df <- as.data.frame(cbind(Distance, Coefficient, SE.low, SE.high))

png(paste(direc, 'figures/tqlo_plot.png', sep = ''))

ggplot(data = res.df, aes(x = Distance, y = Coefficient)) +
  geom_line(size = 1, color = 'red4') +
  geom_ribbon(aes(ymin = SE.low, ymax = SE.high), size = 1, alpha = 0.5, color = 'orange') +
  xlab('Distance in Meters') +
  ylab('Hedonic Coefficient') +
  labs(title = 'Estimated Effect of TQL Opening w.r.t. Distance') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = Breaks) +
  geom_hline(yintercept = 0, size = 2)

dev.off()

# Leaflets

# Convert to spatial data.frame

lats <- c()
lons <- c()

for (i in 1:nrow(data)) {
  
  print(i)
  raw.text <- strsplit(data$Coordinates[i], ',')
  lat <- gsub(' ', '', raw.text[[1]][1], fixed = TRUE)
  lat <- gsub('(', '', lat, fixed = TRUE)
  lon <- gsub(' ', '', raw.text[[1]][2], fixed = TRUE)
  lon <- gsub(')', '', lon, fixed = TRUE)
  lats <- c(lats, as.numeric(lat))
  lons <- c(lons, as.numeric(lon))
  
}

data$latitude <- lats
data$longitude <- lons

datax <- st_as_sf(data, coords = c('longitude', 'latitude'), crs = 4269)

# Subset

data.usl.ax <- datax[which(datax$usl.a.x == 1),]
data.mls.ax <- datax[which(datax$mls.a.x == 1),]
data.tql.ax <- datax[which(datax$tql.a.x == 1),]
data.tql.ox <- datax[which(datax$tql.o.x == 1),]

data.usl.ax$colorx <- data.usl.ax$Treatment.Nippert.hm + 2*data.usl.ax$Treatment.Nippert.1m
data.mls.ax$colorx <- data.mls.ax$Treatment.Nippert.hm + 2*data.mls.ax$Treatment.Nippert.1m
data.tql.ax$colorx <- data.tql.ax$Treatment.TQL.hm + 2*data.tql.ax$Treatment.TQL.1m
data.tql.ox$colorx <- data.tql.ox$Treatment.TQL.hm + 2*data.tql.ox$Treatment.TQL.1m

# Add legend info

labs <- c('Control', 'Half Mile Radius', 'One Mile Radius')

data.usl.ax$leg <- labs[data.usl.ax$colorx+1]
data.mls.ax$leg <- labs[data.mls.ax$colorx+1]
data.tql.ax$leg <- labs[data.tql.ax$colorx+1]
data.tql.ox$leg <- labs[data.tql.ox$colorx+1]

# Stadium locations

sdf <- as.data.frame(cbind(c('Nippert', 'TQL'), c(39.131121300000004, 39.111), c(-84.51622983928571, -84.52053)))
colnames(sdf) <- c('Stadium', 'lat', 'lon')
sdf <- st_as_sf(sdf, coords = c('lon', 'lat'), crs = 4269)

# Plot

pal <- colorFactor(c('black', 'red4', 'orange'), domain = labs)

leaflet(data.usl.ax) %>% setView(lng = -84.51622983928571, lat = 39.131121300000004, zoom = 12) %>% addTiles() %>%
  addCircles(data = sdf[1,], radius = 200, color = 'blue', weight = 3, fill = TRUE, fillOpacity = 1) %>%
  addCircleMarkers(radius = .05, opacity = 1, color = pal(data.usl.ax$leg)) %>%
  addCircles(data = sdf[1,], radius = 800, color = 'black', weight = 3, fill = FALSE) %>%
  addCircles(data = sdf[1,], radius = 1600, color = 'black', weight = 3, fill = FALSE) %>%
  addCircles(data = sdf[1,], radius = 8000, color = 'black', weight = 3, fill = FALSE) %>%
  addLegend(position = 'bottomright', pal = pal, values = data.usl.ax$leg, title = 'Legend')

leaflet(data.mls.ax) %>% setView(lng = -84.51622983928571, lat = 39.131121300000004, zoom = 12) %>% addTiles() %>%
  addCircles(data = sdf[1,], radius = 200, color = 'blue', weight = 3, fill = TRUE, fillOpacity = 1) %>%
  addCircleMarkers(radius = .05, opacity = 1, color = pal(data.mls.ax$leg)) %>%
  addCircles(data = sdf[1,], radius = 800, color = 'black', weight = 3, fill = FALSE) %>%
  addCircles(data = sdf[1,], radius = 1600, color = 'black', weight = 3, fill = FALSE) %>%
  addCircles(data = sdf[1,], radius = 8000, color = 'black', weight = 3, fill = FALSE) %>%
  addLegend(position = 'bottomright', pal = pal, values = data.mls.ax$leg, title = 'Legend')

leaflet(data.tql.ax) %>% setView(lng = -84.52053, lat = 39.111, zoom = 12) %>% addTiles() %>%
  addCircles(data = sdf[2,], radius = 200, color = 'blue', weight = 3, fill = TRUE, fillOpacity = 1) %>%
  addCircleMarkers(radius = .05, opacity = 1, color = pal(data.tql.ax$leg)) %>%
  addCircles(data = sdf[2,], radius = 800, color = 'black', weight = 3, fill = FALSE) %>%
  addCircles(data = sdf[2,], radius = 1600, color = 'black', weight = 3, fill = FALSE) %>%
  addCircles(data = sdf[2,], radius = 8000, color = 'black', weight = 3, fill = FALSE) %>%
  addLegend(position = 'bottomright', pal = pal, values = data.tql.ax$leg, title = 'Legend')

leaflet(data.tql.ox) %>% setView(lng = -84.52053, lat = 39.111, zoom = 12) %>% addTiles() %>%
  addCircles(data = sdf[2,], radius = 200, color = 'blue', weight = 3, fill = TRUE, fillOpacity = 1) %>%
  addCircleMarkers(radius = .05, opacity = 1, color = pal(data.tql.ox$leg)) %>%
  addCircles(data = sdf[2,], radius = 800, color = 'black', weight = 3, fill = FALSE) %>%
  addCircles(data = sdf[2,], radius = 1600, color = 'black', weight = 3, fill = FALSE) %>%
  addCircles(data = sdf[2,], radius = 8000, color = 'black', weight = 3, fill = FALSE) %>%
  addLegend(position = 'bottomright', pal = pal, values = data.tql.ox$leg, title = 'Legend')

