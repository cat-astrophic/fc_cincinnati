# This script runs regressions for the Cincinnati FC project

# Loading libraries

library(sf)
library(AER)
library(stargazer)
library(sandwich)
library(jtools)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(modelsummary)
library(leaflet)
library(lfe)

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

usl.a0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('8/12/2015', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('8/12/2012', '%m/%d/%Y')) # Two years before
mls.a0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/29/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/29/2015', '%m/%d/%Y')) # Two years before
tql.a0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('12/18/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('12/18/2015', '%m/%d/%Y')) # Two years before
tql.o0 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/16/2021', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/16/2018', '%m/%d/%Y')) # Two years before

# Two years after event

usl.a1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('8/12/2015', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('8/12/2018', '%m/%d/%Y')) # Two years after
mls.a1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/29/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/29/2021', '%m/%d/%Y')) # Two years after
tql.a1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('12/18/2018', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('12/18/2021', '%m/%d/%Y')) # Two years before
tql.o1 <- as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') >= as.Date('5/16/2021', '%m/%d/%Y'))*as.numeric(as.Date(data$Transfer.Date, '%m/%d/%Y') < as.Date('5/16/2024', '%m/%d/%Y')) # Two years before

# Add to data.frame

data$Pre.USL.Announced <- usl.a0
data$Post.USL.Announced <- usl.a1
data$Pre.MLS.Announced <- mls.a0
data$Post.MLS.Announced <- mls.a1
data$Pre.TQL.Announced <- tql.a0
data$Post.TQL.Announced <- tql.a1
data$Pre.TQL.Opened <- tql.o0
data$Post.TQL.Opened <- tql.o1

# Create event markers

data$usl.a <- usl.a0 + usl.a1
data$mls.a <- mls.a0 + mls.a1
data$tql.a <- tql.a0 + tql.a1
data$tql.o <- tql.o0 + tql.o1

# Creating a month-year variable

data$MY <- format(as.Date(data$Transfer.Date, format = '%m/%d/%Y'), format = '%m/%Y')

# Dropping foreclosures from the data set

data <- data[which(data$Foreclosure == 'No'),]

# Since no names are provided for sellers or buyers, this bit rules out outliers

# Initial histogram of real house prices as a reference

hist(data$Real.Price, breaks = 100)

# Running a simple hedonic to look at outliers via residuals

outlier.finder <- lm(log(Real.Price) ~ log(FinSqFt) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2)
                     + Rooms + Bedrooms + Full.Baths + Half.Baths + Acreage + factor(School.District)
                     + factor(MY) + factor(Deed.Type) + factor(Owner.Residence), data = data)

# Clustering at the school district level so that I can compare results here to main results later on

outlier.finder.x <- coeftest(outlier.finder, vcov = vcovCL(outlier.finder), type = 'HC1')

# Viewing the results

# stargazer(outlier.finder, outlier.finder.x, type = 'text')

# Get residuals

residuals <- outlier.finder$residuals

# View residuals

hist(residuals, breaks = 100)
abline(v =- sd(residuals)*2)
abline(v = sd(residuals)*2)

# Based on the histogram, let's keep anything within 2 standard deviations of the mean

keep <- which(abs(residuals) < sd(residuals)*2)

# Subset data based on residuals - 98.69% of the data set remains =>  1.31% was dropped

data <- data[keep,]

# Updating the residuals and real prices histograms

resid <- residuals[keep]
hist(resid, breaks = 100)
hist(data$Real.Price, breaks = 100)
hist(log(data$Real.Price), breaks = 100)

# Non-parametric price change plots

p10 <- data[which(data$Nippert < 8 & data$usl.a == 1 & data$Pre.USL.Announced == 1),]
p11 <- data[which(data$Nippert < 8 & data$usl.a == 1 & data$Post.USL.Announced == 1),]
p11$Real.Price <- p11$Real.Price - (mean(p11[which(p11$Nippert > 3.22),]$Real.Price) - mean(p10[which(p10$Nippert > 3.22),]$Real.Price))
pdf1 <- rbind(p10, p11)

p20 <- data[which(data$Nippert < 8 & data$mls.a == 1 & data$Pre.MLS.Announced == 1),]
p21 <- data[which(data$Nippert < 8 & data$mls.a == 1 & data$Post.MLS.Announced == 1),]
p21$Real.Price <- p21$Real.Price - (mean(p21[which(p21$Nippert > 3.22),]$Real.Price) - mean(p20[which(p20$Nippert > 3.22),]$Real.Price))
pdf2 <- rbind(p20, p21)

p30 <- data[which(data$TQL < 8 & data$tql.a == 1 & data$Pre.TQL.Announced == 1),]
p31 <- data[which(data$TQL < 8 & data$tql.a == 1 & data$Post.TQL.Announced == 1),]
p31$Real.Price <- p31$Real.Price - (mean(p31[which(p31$Nippert > 3.22),]$Real.Price) - mean(p30[which(p30$Nippert > 3.22),]$Real.Price))
pdf3 <- rbind(p30, p31)

p40 <- data[which(data$TQL < 8 & data$tql.o == 1 & data$Pre.TQL.Opened == 1),]
p41 <- data[which(data$TQL < 8 & data$tql.o == 1 & data$Post.TQL.Opened == 1),]
p41$Real.Price <- p41$Real.Price - (mean(p41[which(p41$Nippert > 3.22),]$Real.Price) - mean(p40[which(p40$Nippert > 3.22),]$Real.Price))
pdf4 <- rbind(p40, p41)

ggplot(pdf1, aes(x = Nippert, y = Real.Price, color = factor(Post.USL.Announced))) +
  geom_smooth(data = pdf1[which(pdf1$Pre.USL.Announced == 1),], method = 'loess', se = TRUE, span = 0.5, level = 0.95) +
  geom_smooth(data = pdf1[which(pdf1$Post.USL.Announced == 1),], method = 'loess', se = TRUE, span = 0.5, level = 0.95) +
  labs(
    x = 'Distance (km)',
    y = 'Real Transaction Price ($)',
    title = 'LOESS Nonparametric Smoother of Prices over Distance\n - USL Announcement - '
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(c(0,8)) +
  #geom_vline(xintercept = 2) +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 1)) +
  scale_color_manual(name = '', breaks = c(1,0),
                     labels = c('Post USL Announcement', 'Pre USL Announcement'),
                     values = c('red', 'blue'))

ggplot(pdf2, aes(x = Nippert, y = Real.Price, color = factor(Post.MLS.Announced))) +
  geom_smooth(data = pdf2[which(pdf2$Pre.MLS.Announced == 1),], method = 'loess', se = TRUE, span = 0.5, level = 0.95) +
  geom_smooth(data = pdf2[which(pdf2$Post.MLS.Announced == 1),], method = 'loess', se = TRUE, span = 0.5, level = 0.95) +
  labs(
    x = 'Distance (km)',
    y = 'Real Transaction Price ($)',
    title = 'LOESS Nonparametric Smoother of Prices over Distance\n - MLS Announcement - '
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(c(0,8)) +
  #geom_vline(xintercept = 2) +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 1)) +
  scale_color_manual(name = '', breaks = c(1,0),
                     labels = c('Post MLS Announcement', 'Pre MLS Announcement'),
                     values = c('red', 'blue'))

ggplot(pdf3, aes(x = TQL, y = Real.Price, color = factor(Post.TQL.Announced))) +
  geom_smooth(data = pdf3[which(pdf3$Pre.TQL.Announced == 1),], method = 'loess', se = TRUE, span = 0.5, level = 0.99) +
  geom_smooth(data = pdf3[which(pdf3$Post.TQL.Announced == 1),], method = 'loess', se = TRUE, span = 0.5, level = 0.99) +
  labs(
    x = 'Distance (km)',
    y = 'Real Transaction Price ($)',
    title = 'LOESS Nonparametric Smoother of Prices over Distance\n - TQL Announcement - '
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(c(0,8)) +
  #geom_vline(xintercept = 2) +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 1)) +
  scale_color_manual(name = '', breaks = c(1,0),
                     labels = c('Post TQL Announcement', 'Pre TQL Announcement'),
                     values = c('red', 'blue'))

ggplot(pdf4, aes(x = TQL, y = Real.Price, color = factor(Post.TQL.Opened))) +
  geom_smooth(data = pdf4[which(pdf4$Pre.TQL.Opened == 1),], method = 'loess', se = TRUE, span = 0.5, level = 0.99) +
  geom_smooth(data = pdf4[which(pdf4$Post.TQL.Opened == 1),], method = 'loess', se = TRUE, span = 0.5, level = 0.99) +
  labs(
    x = 'Distance (km)',
    y = 'Real Transaction Price ($)',
    title = 'LOESS Nonparametric Smoother of Prices over Distance\n - TQL Opened - '
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(c(0,8)) +
  #geom_vline(xintercept = 2) +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 1)) +
  scale_color_manual(name = '', breaks = c(1,0),
                     labels = c('Post TQL Opened', 'Pre TQL Opened'),
                     values = c('red', 'blue'))

# Creating treatment variables

data$Treatment.Nippert <- as.numeric(data$Nippert <= 2)
data$Treatment.TQL <- as.numeric(data$TQL <= 2)

# Creating control variables

data$Control.Nippert <- as.numeric(data$Nippert > 2) * as.numeric(data$Nippert < 8)
data$Control.TQL <- as.numeric(data$TQL > 2) * as.numeric(data$TQL < 8)

# Run models for each scenario

# USL Announcement

usl.2k <- felm(log(Real.Price) ~ Treatment.Nippert*Post.USL.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
             + log(FinSqFt) + I(log(FinSqFt)^2) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2)
             + Rooms + log(FinSqFt) + Bedrooms + Full.Baths + Half.Baths
             + Acreage | School.District + MY + Deed.Type
             + Owner.Residence | 0 | Parcel.Number, data = data[which(data$Nippert < 8 & data$usl.a == 1),])

# MLS Announcement

mls.2k <- felm(log(Real.Price) ~ Treatment.Nippert*Post.MLS.Announced + log(FinSqFt) + I(log(FinSqFt)^2)
               + log(FinSqFt) + I(log(FinSqFt)^2) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2)
               + Rooms + log(FinSqFt) + Bedrooms + Full.Baths + Half.Baths
               + Acreage | School.District + MY + Deed.Type
               + Owner.Residence | 0 | Parcel.Number, data = data[which(data$Nippert < 8 & data$mls.a == 1),])

# TQL Announcement

tqla.2k <- felm(log(Real.Price) ~ Treatment.TQL*Post.TQL.Announced
                + log(FinSqFt) + I(log(FinSqFt)^2) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2)
                + Rooms + log(FinSqFt) + Bedrooms + Full.Baths + Half.Baths
                + Acreage | School.District + MY + Deed.Type
                + Owner.Residence | 0 | Parcel.Number, data = data[which(data$TQL < 8 & data$tql.a == 1),])

# TQL Announcement - decaying

tqla.4k <- felm(log(Real.Price) ~ Treatment.TQL*Post.TQL.Announced*TQL
                + log(FinSqFt) + I(log(FinSqFt)^2) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2)
                + Rooms + log(FinSqFt) + Bedrooms + Full.Baths + Half.Baths
                + Acreage | School.District + MY + Deed.Type
                + Owner.Residence | 0 | Parcel.Number, data = data[which(data$TQL < 8 & data$tql.a == 1),])

# TQL Opening

tqlo.2k <- felm(log(Real.Price) ~ Treatment.TQL*Post.TQL.Opened + log(FinSqFt) + I(log(FinSqFt)^2)
                + log(FinSqFt) + I(log(FinSqFt)^2) + log(Zero.Coded.Age + 1) + I(log(Zero.Coded.Age + 1)^2)
                + Rooms + log(FinSqFt) + Bedrooms + Full.Baths + Half.Baths
                + Acreage | School.District + MY + Deed.Type
                + Owner.Residence | 0 | Parcel.Number, data = data[which(data$TQL < 8 & data$tql.o == 1),])

# Viewing the results

stargazer(usl.2k, mls.2k, tqla.2k, tqlo.2k, tqla.4k, type = 'text', omit = c('Deed.Type', 'School.District', 'MY'))

# Saving the results

write.csv(stargazer(usl.2k, mls.2k, tqla.2k, tqlo.2k, tqla.4k, type = 'text',
                    omit = c('Deed.Type', 'School.District', 'MY')),
          paste0(direc, 'results/results.txt'), row.names = FALSE)

write.csv(stargazer(usl.2k, mls.2k, tqla.2k, tqlo.2k, tqla.4k,
                    omit = c('Deed.Type', 'School.District', 'MY')),
          paste0(direc, 'results/results_tex.txt'), row.names = FALSE)

# Create a spatial dataframe

lats <- c()
lons <- c()

for (i in 1:nrow(data)) {
  
  print(i)
  lats <- c(lats, as.numeric(substr(strsplit(data$Coordinates[i], ', ')[[1]][1], 2, nchar(strsplit(data$Coordinates[i], ', ')[[1]][1]))))
  lons <- c(lons, as.numeric(substr(strsplit(data$Coordinates[i], ', ')[[1]][2], 1, nchar(strsplit(data$Coordinates[i], ', ')[[1]][2])-1)))
  
}

data$lon <- lats
data$lat <- lons

xxx <- st_as_sf(data, coords = c('lat', 'lon'))
xxx <- st_set_crs(xxx, 4326)

# Leaflets

xxx$PALN <- xxx$Treatment.Nippert + 2*xxx$Control.Nippert
xxx$PALT <- xxx$Treatment.TQL + 2*xxx$Control.TQL

paln <- colorNumeric(palette = c('white', 'black', 'red3'), domain = xxx$PALN)
palt <- colorNumeric(palette = c('white', 'black', 'red3'), domain = xxx$PALT)

leaflet(xxx[which(xxx$Nippert < 8 & xxx$usl.a == 1),]$geometry) %>% addTiles() %>% setView(lat = 39.1311213, lng = -84.5162298, zoom = 12) %>%
  addCircleMarkers(lat = 39.1311213, lng = -84.5162298, radius = 6.66, color = 'blue', fillOpacity = 1) %>%
  addCircleMarkers(radius = 1, fillOpacity = 1, col = paln(xxx[which(xxx$Nippert < 8 & xxx$usl.a == 1),]$PALN)) %>%
  addCircles(lat = 39.1311213, lng = -84.5162298, radius = 8000, col = 'black', fillOpacity = 0, weight = 10) %>%
  addCircles(lat = 39.1311213, lng = -84.5162298, radius = 2000, col = 'black', fillOpacity = 0, weight = 10)

leaflet(xxx[which(xxx$Nippert < 8 & xxx$mls.a == 1),]$geometry) %>% addTiles() %>% setView(lat = 39.1311213, lng = -84.5162298, zoom = 12) %>%
  addCircleMarkers(lat = 39.1311213, lng = -84.5162298, radius = 6.66, color = 'blue', fillOpacity = 1) %>%
  addCircleMarkers(radius = 1, fillOpacity = 1, col = paln(xxx[which(xxx$Nippert < 8 & xxx$mls.a == 1),]$PALN)) %>%
  addCircles(lat = 39.1311213, lng = -84.5162298, radius = 8000, col = 'black', fillOpacity = 0, weight = 10) %>%
  addCircles(lat = 39.1311213, lng = -84.5162298, radius = 2000, col = 'black', fillOpacity = 0, weight = 10)

leaflet(xxx[which(xxx$TQL < 8 & xxx$tql.a == 1),]$geometry) %>% addTiles() %>% setView(lat = 39.1111789, lng = -84.5222288, zoom = 12) %>%
  addCircleMarkers(lat = 39.1111789, lng = -84.5222288, radius = 6.66, color = 'blue', fillOpacity = 1) %>%
  addCircleMarkers(radius = 1, fillOpacity = 1, col = palt(xxx[which(xxx$TQL < 8 & xxx$tql.a == 1),]$PALT)) %>%
  addCircles(lat = 39.1111789, lng = -84.5222288, radius = 8000, col = 'black', fillOpacity = 0, weight = 10) %>%
  addCircles(lat = 39.1111789, lng = -84.5222288, radius = 2000, col = 'black', fillOpacity = 0, weight = 10)

leaflet(xxx[which(xxx$TQL < 8 & xxx$tql.o == 1),]$geometry) %>% addTiles() %>% setView(lat = 39.1111789, lng = -84.5222288, zoom = 12) %>%
  addCircleMarkers(lat = 39.1111789, lng = -84.5222288, radius = 6.66, color = 'blue', fillOpacity = 1) %>%
  addCircleMarkers(radius = 1, fillOpacity = 1, col = palt(xxx[which(xxx$TQL < 8 & xxx$tql.o == 1),]$PALT)) %>%
  addCircles(lat = 39.1111789, lng = -84.5222288, radius = 8000, col = 'black', fillOpacity = 0, weight = 10) %>%
  addCircles(lat = 39.1111789, lng = -84.5222288, radius = 2000, col = 'black', fillOpacity = 0, weight = 10)

