# The packages we will need
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("tidync")
install.packages("doParallel")
install.packages("rerddap")
install.packages("plyr") # Note that this library should never be loaded, only installed
install.packages("foreach")
install.packages("RNetCDF")
library(RNetCDF)
# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing
library(foreach, rerddap)
# The information for the NOAA OISST data
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")
# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = c(36.6, 36.7),
                       longitude = c(28.6, 28.7),
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}
# Date download range by start and end dates per year
dl_years <- data.frame(date_index = 1:5,
                       start = as.Date(c("2012-04-01", "2013-04-01", "2014-04-01", 
                                         "2015-04-01", "2016-04-01")),
                       end = as.Date(c("2012-10-01", "2013-10-01", "2014-10-01", 
                                       "2015-10-01", "2016-10-01"))
)
# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
system.time(
  SST2012to16 <- dl_years %>% 
    group_by(date_index) %>% 
    group_modify(~OISST_sub_dl(.x)) %>% 
    ungroup() %>% 
    select(lon, lat, t, temp)
) # 38 seconds, ~8 seconds per batch
write.csv(OISST_data, "SST Data 2012 - 2016.csv", row.names = TRUE)
#Second batch
dl_years <- data.frame(date_index = 1:5,
                       start = as.Date(c("2017/04/01", "2018/04/01", "2019/04/01", 
                                         "2020/04/01", "2021/04/01")),
                       end = as.Date(c("2017/10/01", "2018/10/01", "2019/10/01", 
                                       "2020/10/01", "2021/10/01"))
)
# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
system.time(
  SST2017to21 <- dl_years %>% 
    group_by(date_index) %>% 
    group_modify(~OISST_sub_dl(.x)) %>% 
    ungroup() %>% 
    select(lon, lat, t, temp)
) # 38 seconds, ~8 seconds per batch

library(lubridate)
SST2012 = read.csv("2012 SST Data.csv")
SST2013 = read.csv("2013 SST Data.csv")
SST2014 = read.csv("2014 SST Data.csv")
SST2015 = read.csv("2015 SST Data.csv")
SST2016 = read.csv("2016 SST Data.csv")
SST2017 = read.csv("2017 SST Data.csv")
SST2018 = read.csv("2018 SST Data.csv")
SST2019 = read.csv("2019 SST Data.csv")
SST2020 = read.csv("2020 SST Data.csv")
SST2021 = read.csv("2021 SST Data.csv")
SST = d
SST$`Nesting Date` = mdy(SST$`Nesting Date`)
SST$`Hatch Date` = mdy(SST$`Hatch Date`)

for (i in 1:497){
  SST[i,14] = month(SST[i,7])
  SST[i,15] = day(SST[i,7])
}

for (i in 1:497){
  if (SST[i,14] == 4){SST[i,16] = ceiling(SST[i,6]/3) + SST[i,15]}
  if (SST[i,14] == 5){SST[i,16] = ceiling(SST[i,6]/3) +SST[i,15] + 30}
  if (SST[i,14] == 6){SST[i,16] = ceiling(SST[i,6]/3) +SST[i,15] + 61}
  if (SST[i,14] == 7){SST[i,16] = ceiling(SST[i,6]/3) +SST[i,15] + 91}
  if (SST[i,14] == 8){SST[i,16] = ceiling(SST[i,6]/3) +SST[i,15] + 122}
  if (SST[i,14] == 9){SST[i,16] = ceiling(SST[i,6]/3) +SST[i,15] + 153}
  if (SST[i,14] == 10){SST[i,16] = ceiling(SST[i,6]/3) +SST[i,15] + 184}
}

#Now hatch date
for (i in 1:497){
  SST[i,17] = month(SST[i,12])
  SST[i,18] = day(SST[i,12])
}

for (i in 1:497){
  if (SST[i,17] == 4){SST[i,19] = ceiling(SST[i,6]/3) + SST[i,16]}
  if (SST[i,17] == 5){SST[i,19] = ceiling(SST[i,6]/3) + SST[i,16]}
  if (SST[i,17] == 6){SST[i,19] = ceiling(SST[i,6]/3) + SST[i,16]}
  if (SST[i,17] == 7){SST[i,19] = ceiling(SST[i,6]/3) + SST[i,16]}
  if (SST[i,17] == 8){SST[i,19] = ceiling(SST[i,6]/3) + SST[i,16]}
  if (SST[i,17] == 9){SST[i,19] = ceiling(SST[i,6]/3) + SST[i,16]}
  if (SST[i,17] == 10){SST[i,19] = ceiling(SST[i,6]/3) + SST[i,16]}
}
for (i in 1:497){
  if(SST[i,3] == "2012"){
    z = SST[i,16]
    x = SST[i,19]
    SST[i, 20] = mean(SST2012[z:x,5])
    SST[i, 21] = sd(SST2012[z:x,5])
  }
  if(SST[i,3] == "2013"){
    z = SST[i,16]
    x = SST[i,19]
    SST[i, 20] = mean(SST2013[z:x,5])
    SST[i, 21] = sd(SST2013[z:x,5])
  }
  if(SST[i,3] == "2014"){
    z = SST[i,16]
    x = SST[i,19]
    SST[i, 20] = mean(SST2014[z:x,5])
    SST[i, 21] = sd(SST2014[z:x,5])
  }
  if(SST[i,3] == "2015"){
    z = SST[i,16]
    x = SST[i,19]
    SST[i, 20] = mean(SST2015[z:x,5])
    SST[i, 21] = sd(SST2015[z:x,5])
  }
  if(SST[i,3] == "2016"){
    z = SST[i,16]
    x = SST[i,19]
    SST[i, 20] = mean(SST2016[z:x,5])
    SST[i, 21] = sd(SST2016[z:x,5])
  }
  if(SST[i,3] == "2017"){
    z = SST[i,16]
    x = SST[i,19]
    SST[i, 20] = mean(SST2017[z:x,5])
    SST[i, 21] = sd(SST2017[z:x,5])
  }
  if(SST[i,3] == "2018"){
    z = SST[i,16]
    x = SST[i,19]
    SST[i, 20] = mean(SST2018[z:x,5])
    SST[i, 21] = sd(SST2018[z:x,5])
  }
  if(SST[i,3] == "2019"){
    z = SST[i,16]
    x = SST[i,19]
    SST[i, 20] = mean(SST2019[z:x,5])
    SST[i, 21] = sd(SST2019[z:x,5])
  }
  if(SST[i,3] == "2020"){
    z = SST[i,16]
    x = SST[i,19]
    SST[i, 20] = mean(SST2020[z:x,5])
    SST[i, 21] = sd(SST2020[z:x,5])
  }
  if(SST[i,3] == "2021"){
    z = SST[i,16]
    x = SST[i,19]
    SST[i, 20] = mean(SST2021[z:x,5])
    SST[i, 21] = sd(SST2021[z:x,5])
  }
}
colnames(SST) = c("Mean_Temperature", "Standard Deviation", 
                          "Year", "Code", "Hatch Percentage", "Incubation Period", 
                          "Nesting Date", "Nesting Month", "Nesting Bin", "TSP Mid Point"
                          , "TSP Bin", "Hatch Date", "Males", "Nest Month", "Nest Date", 
                          "TSP Start Numeral", "Laid Month", "EndDate", "TSP End Numeral", 
                          "Mean_SST", "SD SST")
ggplot(SST, mapping = aes(x = `Mean Temperature`,
                                  col = as.factor(`Year`))) +
  theme(axis.title.y = element_blank())+
  geom_point(cex = 0.3, aes(y = `Males`)) +
  geom_point(cex = 0.3, aes(y = `Mean SST` - 20))

SST_Temp_Plot = ggplot(SST, mapping = aes(`Mean Temperature`, `Mean SST`)) +
  geom_point(cex = 0.3) +
  geom_smooth(method='glm',formula= y ~ x, cex = 0.3)

SSTnesttempmodel = glm(`Mean_Temperature` ~ `Mean_SST`,
                               data = SST) 
summary(SSTnesttempmodel)
#calculate McFadden's R-squared for model
with(summary(SSTnesttempmodel), 1 - deviance/null.deviance)
#0.4922026
#`Mean Air Temperature`  estimate =0.54693, std er=0.02507, t =21.82, P =<2e-16 

ggplot(SST, na.rm= TRUE, aes(x=`Year`, y=`Mean Air Temperature`, 
                                     col = as.factor(`Year`))) +
  geom_boxplot(notch = FALSE, aes(group = as.factor(`Year`))) +
  geom_jitter(shape=16, cex = 0.5, position=position_jitter(0.2)) +
  theme_minimal()

SSTTSP_Overlap = subset(SST, `TSP Bin` == "B"|`TSP Bin` == "C"|
                                  `TSP Bin` == "D")
ggplot(SSTTSP_Overlap, na.rm= TRUE, aes(x=`Year`, y=`Mean Air Temperature`, 
                                                                        col = as.factor(`Year`))) +
  geom_boxplot(notch = FALSE, aes(group = as.factor(`Year`))) +
  geom_jitter(shape=16, cex = 0.5, position=position_jitter(0.2)) +
  theme_minimal()

plot.window(xlim = 2, ylim = 1, SSTTSPAirTemp, TSPMales)

SSTmodel = glm(SST$`Mean Temperature` ~ SST$`Mean Air Temperature`, family = gaussian, 
                       data = SST)
plot(SSTmodel)

install.packages("gridExtra")
library(gridExtra)
grid.arrange(SSTTSPAirTemp, TSPTemps, TSPIncubation, TSPMales, nrow = 2, ncol=2)



