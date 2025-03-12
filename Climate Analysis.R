###CLIMATE DATA SECTION#####
#This section is to isolate whether SST or Air temperature have a greater
#influence on internal nest temperature
install.packages("lubridate")
library(lubridate)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
#I had to download the data using r studio cloud, the script is below
#but I just read in csv files I had already downloaded
Weather2012 = read.csv("2012 Temperatures.csv")
Weather2012$datetime = ymd(Weather2012$datetime)
Weather2013 = read.csv("2013 Temperatures.csv")
Weather2013$datetime = ymd(Weather2013$datetime)
Weather2014 = read.csv("2014 Temperatures.csv")
Weather2014$datetime = ymd(Weather2014$datetime)
Weather2015 = read.csv("2015 Temperatures.csv")
Weather2015$datetime = ymd(Weather2015$datetime)
Weather2016 = read.csv("2016 Temperatures.csv")
Weather2016$datetime = ymd(Weather2016$datetime)
Weather2017 = read.csv("2017 Temperatures.csv")
Weather2017$datetime = ymd(Weather2017$datetime)
Weather2018 = read.csv("2018 Temperatures.csv")
Weather2018$datetime = ymd(Weather2018$datetime)
Weather2019 = read.csv("2019 Temperatures.csv")
Weather2019$datetime = ymd(Weather2019$datetime)
Weather2020 = read.csv("2020 Temperatures.csv")
Weather2020$datetime = ymd(Weather2020$datetime)
Weather2021 = read.csv("2021 Temperatures.csv")
Weather2021$datetime = ymd(Weather2021$datetime)
#Create a new dataframe for analysis
weatherdata = d
#Convert dates into readable formate using the lubridate package
weatherdata$Laid = dmy(weatherdata$Laid)
weatherdata$Hatch = dmy(weatherdata$Hatch)
#Convert date into integers for time into 
for (i in 1:524){
  weatherdata[i,12] = month(weatherdata[i,7])
  weatherdata[i,13] = day(weatherdata[i,7])
}
#Start and end of TSP in integers
for (i in 1:524){
  if (weatherdata[i,12] == 4){weatherdata[i,14] = ceiling(weatherdata[i,6]/3) + weatherdata[i,13]}
  if (weatherdata[i,12] == 5){weatherdata[i,14] = ceiling(weatherdata[i,6]/3) +weatherdata[i,13] + 30}
  if (weatherdata[i,12] == 6){weatherdata[i,14] = ceiling(weatherdata[i,6]/3) +weatherdata[i,13] + 61}
  if (weatherdata[i,12] == 7){weatherdata[i,14] = ceiling(weatherdata[i,6]/3) +weatherdata[i,13] + 91}
  if (weatherdata[i,12] == 8){weatherdata[i,14] = ceiling(weatherdata[i,6]/3) +weatherdata[i,13] + 122}
  if (weatherdata[i,12] == 9){weatherdata[i,14] = ceiling(weatherdata[i,6]/3) +weatherdata[i,13] + 153}
  if (weatherdata[i,12] == 10){weatherdata[i,14] = ceiling(weatherdata[i,6]/3) +weatherdata[i,13] + 184}
}
for (i in 1:524){
  weatherdata[i,15] = ceiling(weatherdata[i,6]/3) + weatherdata[i,14]}
#Now I use these integers to determine the mean temperature during TSP
for (i in 2:524){
  if(weatherdata[i,4] == "2012"){
    z = weatherdata[i,14]
    x = weatherdata[i,15]
    x = weatherdata[i,16] = mean(Weather2012[z:x,2])
    x = weatherdata[i,17] = sd(Weather2012[z:x,2])
  }
  if(weatherdata[i,4] == "2013"){
    z = weatherdata[i,14]
    x = weatherdata[i,15]
    x = weatherdata[i,16] = mean(Weather2013[z:x,2])
    x = weatherdata[i,17] = sd(Weather2013[z:x,2])
  }
  if(weatherdata[i,4] == "2014"){
    z = weatherdata[i,14]
    x = weatherdata[i,15]
    x = weatherdata[i,16] = mean(Weather2014[z:x,2])
    x = weatherdata[i,17] = sd(Weather2014[z:x,2])
  }
  if(weatherdata[i,4] == "2015"){
    z = weatherdata[i,14]
    x = weatherdata[i,15]
    x = weatherdata[i,16] = mean(Weather2015[z:x,2])
    x = weatherdata[i,17] = sd(Weather2015[z:x,2])
  }
  if(weatherdata[i,4] == "2016"){
    z = weatherdata[i,14]
    x = weatherdata[i,15]
    x = weatherdata[i,16] = mean(Weather2016[z:x,2])
    x = weatherdata[i,17] = sd(Weather2016[z:x,2])
  }
  if(weatherdata[i,4] == "2017"){
    z = weatherdata[i,14]
    x = weatherdata[i,15]
    x = weatherdata[i,16] = mean(Weather2017[z:x,2])
    x = weatherdata[i,17] = sd(Weather2017[z:x,2])
  }
  if(weatherdata[i,4] == "2018"){
    z = weatherdata[i,14]
    x = weatherdata[i,15]
    x = weatherdata[i,16] = mean(Weather2018[z:x,2])
    x = weatherdata[i,17] = sd(Weather2018[z:x,2])
  }
  if(weatherdata[i,4] == "2019"){
    z = weatherdata[i,14]
    x = weatherdata[i,15]
    x = weatherdata[i,16] = mean(Weather2019[z:x,2])
    x = weatherdata[i,17] = sd(Weather2019[z:x,2])
  }
  if(weatherdata[i,4] == "2020"){
    z = weatherdata[i,14]
    x = weatherdata[i,15]
    x = weatherdata[i,16] = mean(Weather2020[z:x,2])
    x = weatherdata[i,17] = sd(Weather2020[z:x,2])
  }
  if(weatherdata[i,4] == "2021"){
    z = weatherdata[i,14]
    x = weatherdata[i,15]
    x = weatherdata[i,16] = mean(Weather2021[z:x,2])
    x = weatherdata[i,17] = sd(Weather2021[z:x,2])
  }
}
#Label the columns
colnames(weatherdata) = c("serial", "Mean_Temperature", "Standard_Deviation", 
                          "Year", "Hatch_Percentage", "Incubation_Period", 
                          "Nesting_Date", "TSP_Mid_Point"
                          , "TSP_Bin", "Hatch_Date", "Males", "TSP_Start_Month", "TSP_Start_Date", 
                          "TSP_Start_Numeral", "TSP_End_Numeral",
                          "Mean_Air_Temperature", "SD_Air_Temperature")
#GLM first
Air_GLM = glm(`Mean_Temperature` ~ `Mean_Air_Temperature`, data = weatherdata) 
summary(Air_GLM)
with(summary(Air_GLM), 1 - deviance/null.deviance)
#p < 2e-16, AIC 1357, r-sq 0.5336968
#GAM, gaussian
Air_GAM = gam(`Mean_Temperature` ~ s(`Mean_Air_Temperature`), data = weatherdata)
summary(Air_GAM)
AIC(Air_GAM)
#p < 2e-16, AIC 1252.536, r-sq 0.625, %dev = 63.1
#Visualise data
AirPlots = ggplot(weatherdata, mapping = aes(`Mean_Air_Temperature`, `Mean_Temperature`)) +
  geom_point(cex = 0.2) +
  ylim(c(26,34)) +
  xlab("Mean Air Temperature (\u00b0C)") +
  ylab("Mean Incubation Temperature (\u00b0C)") +
  geom_smooth(method='gam',formula= y ~ s(x), cex = 0.3, col = "blue") + 
  geom_smooth(method = 'glm', formula = y ~ x, cex = 0.3, col = "red") +
  theme_minimal()
AirPlots
#######SST#########

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
                       start = as.Date(c("2032-04-01", "2033-04-01", "2034-04-01", 
                                         "2035-04-01", "2036-04-01")),
                       end = as.Date(c("2032-10-01", "2033-10-01", "2034-10-01", 
                                       "2035-10-01", "2036-10-01"))
)
# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
system.time(
  SST2032to36 <- dl_years %>% 
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
)
# I manually seperated data by year and read them back in from csv files
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

#Create a dataframe for analysis
SST = weatherdata

#Following the same process as with air temperature we can estimate 
#mean SST during TSP
for (i in 2:524){
  if(SST[i,4] == "2012"){
    z = SST[i,14]
    x = SST[i,15]
    x = SST[i,16] = mean(SST2012[z:x,5])
    x = SST[i,17] = sd(SST2012[z:x,5])
  }
  if(SST[i,4] == "2013"){
    z = SST[i,14]
    x = SST[i,15]
    x = SST[i,16] = mean(SST2013[z:x,5])
    x = SST[i,17] = sd(SST2013[z:x,5])
  }
  if(SST[i,4] == "2014"){
    z = SST[i,14]
    x = SST[i,15]
    x = SST[i,16] = mean(SST2014[z:x,5])
    x = SST[i,17] = sd(SST2014[z:x,5])
  }
  if(SST[i,4] == "2015"){
    z = SST[i,14]
    x = SST[i,15]
    x = SST[i,16] = mean(SST2015[z:x,5])
    x = SST[i,17] = sd(SST2015[z:x,5])
  }
  if(SST[i,4] == "2016"){
    z = SST[i,14]
    x = SST[i,15]
    x = SST[i,16] = mean(SST2016[z:x,5])
    x = SST[i,17] = sd(SST2016[z:x,5])
  }
  if(SST[i,4] == "2017"){
    z = SST[i,14]
    x = SST[i,15]
    x = SST[i,16] = mean(SST2017[z:x,5])
    x = SST[i,17] = sd(SST2017[z:x,5])
  }
  if(SST[i,4] == "2018"){
    z = SST[i,14]
    x = SST[i,15]
    x = SST[i,16] = mean(SST2018[z:x,5])
    x = SST[i,17] = sd(SST2018[z:x,5])
  }
  if(SST[i,4] == "2019"){
    z = SST[i,14]
    x = SST[i,15]
    x = SST[i,16] = mean(SST2019[z:x,5])
    x = SST[i,17] = sd(SST2019[z:x,5])
  }
  if(SST[i,4] == "2020"){
    z = SST[i,14]
    x = SST[i,15]
    x = SST[i,16] = mean(SST2020[z:x,5])
    x = SST[i,17] = sd(SST2020[z:x,5])
  }
  if(SST[i,4] == "2021"){
    z = SST[i,14]
    x = SST[i,15]
    x = SST[i,16] = mean(SST2021[z:x,5])
    x = SST[i,17] = sd(SST2021[z:x,5])
  }
}
#Label the columns
colnames(SST) = c("serial", "Mean_Temperature", "Standard_Deviation", 
                  "Year", "Hatch_Percentage", "Incubation_Period", 
                  "Nesting_Date", "TSP_Mid_Point"
                  , "TSP_Bin", "Hatch_Date", "Males", "TSP_Start_Month", "TSP_Start_Date", 
                  "TSP_Start_Numeral", "TSP_End_Numeral",
                  "Mean_SST_Temperature", "SD_SST_Temperature")
#GLM first
SST_GLM = glm(`Mean_Temperature` ~ `Mean_SST_Temperature`, data = SST) 
summary(SST_GLM)
with(summary(SST_GLM), 1 - deviance/null.deviance)
#p < 2e-16, AIC 1367, r-sq 0.5242863
#GAM, gaussian
SST_GAM = gam(`Mean_Temperature` ~ s(`Mean_SST_Temperature`), data = SST)
summary(SST_GAM)
AIC(SST_GAM)
#p < 2e-16, AIC 1295.614, r-sq 0.589, % dev = 59.3
#Visualise data
SSTPlots = ggplot(SST, mapping = aes(`Mean_SST_Temperature`, `Mean_Temperature`)) +
  geom_point(cex = 0.2) +
  ylim(c(26,34)) +
  xlab("Mean SST Temperature (\u00b0C)") +
  ylab(element_blank()) +
  geom_smooth(method='gam',formula= y ~ s(x), cex = 0.3,  aes(colour="GAM")) +
  geom_smooth(method = 'glm', formula = y ~ x, cex = 0.3, aes(colour="GLM"))+
  scale_colour_manual(name="Model Type", values=c("blue", "red")) +
  theme_minimal()
SSTPlots
#Save both plots side by side
jpeg("Nest - Climate Temperatures.jpeg", units="in", width=15, height=5, res=300)
grid.arrange(AirPlots, SSTPlots, nrow = 1, ncol=2)
dev.off()

sd(d$V11, na.rm=T)
