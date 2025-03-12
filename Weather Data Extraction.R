library(lubridate)
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
weatherdata = d
weatherdata$`Nesting Date` = mdy(weatherdata$`Nesting Date`)
weatherdata$`Hatch Date` = mdy(weatherdata$`Hatch Date`)

for (i in 1:497){
  weatherdata[i,14] = month(weatherdata[i,7])
  weatherdata[i,15] = day(weatherdata[i,7])
}

for (i in 1:497){
  if (weatherdata[i,14] == 4){weatherdata[i,16] = ceiling(weatherdata[i,6]/3) + weatherdata[i,15]}
  if (weatherdata[i,14] == 5){weatherdata[i,16] = ceiling(weatherdata[i,6]/3) +weatherdata[i,15] + 30}
  if (weatherdata[i,14] == 6){weatherdata[i,16] = ceiling(weatherdata[i,6]/3) +weatherdata[i,15] + 61}
  if (weatherdata[i,14] == 7){weatherdata[i,16] = ceiling(weatherdata[i,6]/3) +weatherdata[i,15] + 91}
  if (weatherdata[i,14] == 8){weatherdata[i,16] = ceiling(weatherdata[i,6]/3) +weatherdata[i,15] + 122}
  if (weatherdata[i,14] == 9){weatherdata[i,16] = ceiling(weatherdata[i,6]/3) +weatherdata[i,15] + 153}
  if (weatherdata[i,14] == 10){weatherdata[i,16] = ceiling(weatherdata[i,6]/3) +weatherdata[i,15] + 184}
}

#Now hatch date
for (i in 1:497){
  weatherdata[i,17] = month(weatherdata[i,12])
  weatherdata[i,18] = day(weatherdata[i,12])
}

for (i in 1:497){
  if (weatherdata[i,17] == 4){weatherdata[i,19] = ceiling(weatherdata[i,6]/3) + weatherdata[i,16]}
  if (weatherdata[i,17] == 5){weatherdata[i,19] = ceiling(weatherdata[i,6]/3) + weatherdata[i,16]}
  if (weatherdata[i,17] == 6){weatherdata[i,19] = ceiling(weatherdata[i,6]/3) + weatherdata[i,16]}
  if (weatherdata[i,17] == 7){weatherdata[i,19] = ceiling(weatherdata[i,6]/3) + weatherdata[i,16]}
  if (weatherdata[i,17] == 8){weatherdata[i,19] = ceiling(weatherdata[i,6]/3) + weatherdata[i,16]}
  if (weatherdata[i,17] == 9){weatherdata[i,19] = ceiling(weatherdata[i,6]/3) + weatherdata[i,16]}
  if (weatherdata[i,17] == 10){weatherdata[i,19] = ceiling(weatherdata[i,6]/3) + weatherdata[i,16]}
}
for (i in 1:497){
  if(weatherdata[i,3] == "2012"){
    z = weatherdata[i,16]
    x = weatherdata[i,19]
    weatherdata[i, 20] = mean(Weather2012[z:x,2])
    weatherdata[i, 21] = sd(Weather2012[z:x,2])
  }
  if(weatherdata[i,3] == "2013"){
    z = weatherdata[i,16]
    x = weatherdata[i,19]
    weatherdata[i, 20] = mean(Weather2013[z:x,2])
    weatherdata[i, 21] = sd(Weather2013[z:x,2])
  }
  if(weatherdata[i,3] == "2014"){
    z = weatherdata[i,16]
    x = weatherdata[i,19]
    weatherdata[i, 20] = mean(Weather2014[z:x,2])
    weatherdata[i, 21] = sd(Weather2014[z:x,2])
  }
  if(weatherdata[i,3] == "2015"){
    z = weatherdata[i,16]
    x = weatherdata[i,19]
    weatherdata[i, 20] = mean(Weather2015[z:x,2])
    weatherdata[i, 21] = sd(Weather2015[z:x,2])
  }
  if(weatherdata[i,3] == "2016"){
    z = weatherdata[i,16]
    x = weatherdata[i,19]
    weatherdata[i, 20] = mean(Weather2016[z:x,2])
    weatherdata[i, 21] = sd(Weather2016[z:x,2])
  }
  if(weatherdata[i,3] == "2017"){
    z = weatherdata[i,16]
    x = weatherdata[i,19]
    weatherdata[i, 20] = mean(Weather2017[z:x,2])
    weatherdata[i, 21] = sd(Weather2017[z:x,2])
  }
  if(weatherdata[i,3] == "2018"){
    z = weatherdata[i,16]
    x = weatherdata[i,19]
    weatherdata[i, 20] = mean(Weather2018[z:x,2])
    weatherdata[i, 21] = sd(Weather2018[z:x,2])
  }
  if(weatherdata[i,3] == "2019"){
    z = weatherdata[i,16]
    x = weatherdata[i,19]
    weatherdata[i, 20] = mean(Weather2019[z:x,2])
    weatherdata[i, 21] = sd(Weather2019[z:x,2])
  }
  if(weatherdata[i,3] == "2020"){
    z = weatherdata[i,16]
    x = weatherdata[i,19]
    weatherdata[i, 20] = mean(Weather2020[z:x,2])
    weatherdata[i, 21] = sd(Weather2020[z:x,2])
  }
  if(weatherdata[i,3] == "2021"){
    z = weatherdata[i,16]
    x = weatherdata[i,19]
    weatherdata[i, 20] = mean(Weather2021[z:x,2])
    weatherdata[i, 21] = sd(Weather2021[z:x,2])
  }
}
colnames(weatherdata) = c("Mean Temperature", "Standard Deviation", 
                "Year", "Code", "Hatch Percentage", "Incubation Period", 
                "Nesting Date", "Nesting Month", "Nesting Bin", "TSP Mid Point"
                , "TSP Bin", "Hatch Date", "Males", "Nest Month", "Nest Date", 
                "Nest Numeral", "Laid Month", "Laid Date", "Laid Numeral", 
                "Mean Air Temperature", "SD Air Temperature")
ggplot(weatherdata, mapping = aes(x = `Mean Temperature`,
                        col = as.factor(`Year`))) +
  theme(axis.title.y = element_blank())+
  geom_point(cex = 0.3, aes(y = `Males`)) +
  geom_point(cex = 0.3, aes(y = `Mean Air Temperature` - 20))

Air_Temp_TSP_GAM = ggplot(weatherdata, mapping = aes(`Mean Temperature`, `Mean Air Temperature`)) +
  geom_point(cex = 0.3) +
  geom_smooth(method='gam',formula= y ~ s(x), cex = 0.3)

Air_Temp_TSP = ggplot(weatherdata, mapping = aes(`Mean Temperature`, `Mean Air Temperature`)) +
  geom_point(cex = 0.3) +
  geom_smooth(method='glm',formula= y ~ x, cex = 0.3)

Air_GLM = glm(`Mean Temperature` ~ `Mean Air Temperature`,
                         data = weatherdata) 
summary(Air_GLM)
#calculate McFadden's R-squared for model
with(summary(Air_GLM), 1 - deviance/null.deviance)
#0.4902639
#`Mean Air Temperature`  estimate =0.56136, std er=0.02573, t =21.82, P =<2e-16 

ggplot(weatherdata, na.rm= TRUE, aes(x=`Year`, y=`Mean Air Temperature`, 
                           col = as.factor(`Year`))) +
  geom_boxplot(notch = FALSE, aes(group = as.factor(`Year`))) +
  geom_jitter(shape=16, cex = 0.5, position=position_jitter(0.2)) +
  theme_minimal()

weatherdataTSP_Overlap = subset(weatherdata, `TSP Bin` == "B"|`TSP Bin` == "C"|
                       `TSP Bin` == "D")
weatherdataTSPAirTemp = ggplot(weatherdataTSP_Overlap, na.rm= TRUE, aes(x=`Year`, y=`Mean Air Temperature`, 
                               col = as.factor(`Year`))) +
  geom_boxplot(notch = FALSE, aes(group = as.factor(`Year`))) +
  geom_jitter(shape=16, cex = 0.5, position=position_jitter(0.2)) +
  theme_minimal()

plot.window(xlim = 2, ylim = 1, weatherdataTSPAirTemp, TSPMales)

weatherdatamodel = glm(weatherdata$`Mean Temperature` ~ weatherdata$`Mean Air Temperature`, family = gaussian, 
    data = weatherdata)
plot(weatherdatamodel)

install.packages("gridExtra")
library(gridExtra)
grid.arrange(dummyTSPAirTemp, TSPTemps, SST_Temp_Plot, TSPMales, nrow = 2, ncol=2)
#Climate - Nest Temperature Plotting
jpeg("Climate_Temperature.jpeg", units="in", width=15, height=5, res=300)
grid.arrange(SST_Temp_Plot, Air_Temp_TSP, nrow = 1, ncol = 2)
dev.off()
dummy = matrix(nrow = 497, ncol = 5)
dummy[,1] = weatherdata$`Mean Air Temperature`
dummy[,2] = SST$`Mean SST`
#check the tables are in the same order before I continue
dummy[,3] = weatherdata$Code
dummy[,4] = SST$Code
#overwrite codes, they are not needed for analysis
dummy[,3] = weatherdata$Year
dummy[,4] = weatherdata$Males
dummy[,5] = weatherdata$`Mean Temperature`
colnames(dummy) = c("Air", "SST", "Year", 
                    "Males", "Nest")  
dummy = as.data.frame(dummy)
ggplot(dummy, mapping = aes(x = `Nest`)) +
  theme(axis.title.y = element_blank())+
  geom_point(cex = 0.3, aes(y = `SST`), col = "red") +
  geom_point(cex = 0.3, aes(y = `Air`)) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

library(mgcv)
#GAM for SST
j = matrix(nrow = 497, ncol = 2)
j[,1] = SST$`Mean Temperature`
j[,2] = SST$`Mean SST`
j = as.data.frame(j) 
gam(V1 ~ s(V2), data=j, method = "REML")
SST_GAM = gam(V1 ~ s(V2), data=j)
#GAM for Air Temp
k = matrix(nrow = 497, ncol = 2)
k[,1] = weatherdata$`Mean Temperature`
k[,2] = weatherdata$`Mean Air Temperature`
k = as.data.frame(k) 
gam(V1 ~ s(V2), data=k, method = "REML")
Air_GAM = gam(V1 ~ s(V2), data=k)
library("AICcmodavg")


#define list of models
models <- list(Air_GAM, Air_GLM)

#specify model names
mod.names <- c('air.gam', 'air.glm')

#calculate AIC of each model
aictab(cand.set = models, modnames = mod.names)
AICc(Air_GAM)
AICc(Air_GLM)
plot(Air_GAM)

Air_GAM = Air_Temp_TSP + geom_smooth(method = "gam", formula = y ~ s(x),
                           cex = 0.3, color = "red")
SST_GAM = SST_Temp_Plot + geom_smooth(method = "gam", formula = y ~ s(x),
                            cex = 0.3, color = "red")
jpeg("Climate_GAMS.jpeg", units="in", width=15, height=5, res=300)
grid.arrange(SST_GAM, Air_GAM, nrow = 1, ncol = 2)
dev.off()

