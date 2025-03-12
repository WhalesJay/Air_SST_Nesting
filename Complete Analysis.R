#Read in the data from previous script, which has now been added
#to in excel,
d = read.csv("d.csv",na.strings=c("","NA"))
#install and load all the packages we will need for analysis
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("mgcv")
install.packages("lubridate")
install.packages("knitr")
install.packages("reshape")
library(reshape)
library(knitr)
library(mgcv)
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(lubridate)
# I am going to apply the hill equation per Laloe et al., 2015 
#to predict sex ratio using covariates derived from Kaska &
#Girondot 2015
p = 29
s = -0.0336281
K = 0.1
for (i in 1:520){
  t = d[i,2]
  q = exp((1/s)*(log(p+K)-log(t+K)))
  sr = 1/(1+q)
  d[i,11] = sr
}

#Lets check the overall relationship it should follow
#a standard s-shaped curve
ggplot(d, mapping = aes(`Mean.Temp`, `V11`, 
                        col = as.factor(`Year`))) +
  geom_point(cex = 0.3) +
  theme_minimal()

#Now lets add error, accounting for the standard deviation
#of temperature recorded during the TSP
errorbars = d[,2:4]
errorbars[,4] = (errorbars[,1]-errorbars[,2])
errorbars[,5] = (errorbars[,1]+errorbars[,2])
#Now we can apply the Hill equation to the upper and 
#lower average thermal estimations
p = 29.306
s = -0.025
K = 0.1

#Lower limit first
for (i in 1:520){
  t = errorbars[i,4]
  q = exp((1/s)*(log(p+K)-log(t+K)))
  sr = 1/(1+q)
  errorbars[i,6] = sr
}
#Then upper limit 
for (i in 1:520){
  t = errorbars[i,5]
  q = exp((1/s)*(log(p+K)-log(t+K)))
  sr = 1/(1+q)
  errorbars[i,7] = sr
}
#Now Mean
for (i in 1:520){
  t = errorbars[i,1]
  q = exp((1/s)*(log(p+K)-log(t+K)))
  sr = 1/(1+q)
  errorbars[i,8] = sr
}
#add column names
colnames(errorbars) = c("Mean_Temperature", "Standard Deviation", "Year"
                        , "Lower Limit", "Upper Limit", "Lower Male Estimate"
                        , "Upper Male Estimate", "Males")
#Remember that the upper and lower estimates are flipped
#in reference to male proportion because an increase 
#in temperature results in less males so the "lower limit"
#needs to be set as the upper limit for the error bar
WithError = ggplot(data = errorbars, mapping = aes(x = `Mean_Temperature`, y = Males)) +
  geom_point(cex = 0.3) +
  geom_errorbar(ymin = errorbars$`Upper Male Estimate`,
                ymax = errorbars$`Lower Male Estimate`, 
                width=.2) +
  xlab("Mean TSP Temperature (\u00b0C)")+
  ylab("Hatchling Male Proportion")
#save graph as an image
jpeg("WithError.jpeg", units="in", width=10, height=5, res=300)
WithError
dev.off()

#I want to check for a correlation between standard deviation 
#of temperatures and Mean_Temperatures, as male proportion
#seems significantly more variable at lower temperatures
cor.test(d$Mean.Temp, d$Std.dev, method = "pearson")
#t = -10.729, df = 474, p-value < 2.2e-16, cor = -0.4420254

#Before we start subsetting lets look at the overall 
#sex ratio trends between years
jpeg("Males All Available Data.jpeg", units="in", width=10, height=5, res=300)
ggplot(d, na.rm= TRUE, aes(x=`Year`, y=`V11`, 
                           col = as.factor(`Year`))) + 
  ylim(0, 1) +
  geom_boxplot(notch = FALSE, aes(group = as.factor(`Year`))) +
  geom_jitter(shape=16, cex = 0.5, position=position_jitter(0.2)) +
  theme_minimal() +
  ylab("Male Percentage")
dev.off()

#Because temperature changes with time of year, and sampling 
#is not equal between years, it does not make sense to compare
#all of the data, so instead I am selecting nests which overlap in 
#TSP timing, those  in TSP Bin B, C, D, aka centered between July 15
#and august 31st
TSP_Overlap = subset(d, `TSP.Bin` == "C"|`TSP.Bin` == "D"|
                       `TSP.Bin` == "E")
#Plot the temperatures of each year's overlapping period
TSPMales = ggplot(TSP_Overlap, na.rm= TRUE, aes(x=`Year`, y=`V11`, 
                                                col = as.factor(`Year`))) + 
  ylim(0, 1) +
  geom_boxplot(notch = FALSE, aes(group = as.factor(`Year`))) +
  geom_jitter(shape=16, cex = 0.5, position=position_jitter(0.2)) +
  theme_minimal()+
  ylab("Hatchling Male Ratio")
jpeg("Males TSP Overlap.jpeg", units="in", width=10, height=5, res=300)
TSPMales
dev.off()
aggregate(TSP_Overlap$V11, list(TSP_Overlap$Year), FUN=mean) 
#Now we will do the same but for the mean temperature inside nests 
#by year
TSPTemps = ggplot(TSP_Overlap, na.rm= TRUE, aes(x=`Year`, y=`Mean.Temp`, col = as.factor(`Year`))) +
  geom_boxplot(notch = FALSE, aes(group = as.factor(`Year`))) +
  geom_jitter(shape=16, cex = 0.5, position=position_jitter(0.2)) +
  theme_minimal() +
  ylab("Male Sex Ratio (%)")
#Save these two in a joint plot

jpeg("TSP Overlap Figures.jpeg", units="in", width=10, height=5, res=300)
TSPMales
dev.off()

#Diverging bars plot to visualise the changes in proportion
#of males through years.
referencemean = mean(TSP_Overlap$V11, na.rm = T)
Divergingbars = aggregate(TSP_Overlap$V11, list(TSP_Overlap$Year), FUN=mean, na.rm = T)
Divergingbars[,3] = Divergingbars[,2] - referencemean
Divergingbars[,4] = ifelse(Divergingbars[,3] < 0, "below", "above")  # above / below avg flag
colnames(Divergingbars) = c("Year", "Males", "Normalised Male Proportion", "Above Below")
Divergingbars$Year<- as.factor(Divergingbars$Year)# convert to factor to retain sorted order in plot.
DivergingPlot = ggplot(Divergingbars, aes(x=`Year`, y=`Normalised Male Proportion`, label=`Males`)) + 
  geom_bar(stat='identity', aes(fill=`Above Below`), width=0.7)  +
  scale_fill_manual(name="Male Proportion", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised Male Ratio", 
       title= "Diverging Bars") + 
  coord_flip()
jpeg("TSP Overlap Diverging Bars.jpeg", units="in", width=12, height=7, res=300)
DivergingPlot
dev.off()
#anova to test between years
yearsaov = aov(V11 ~ as.factor(Year), data = TSP_Overlap)
summary(yearsaov)
plot(yearsaov)

tukey.two.way<-TukeyHSD(yearsaov)
write.csv(tukey.two.way$`as.factor(Year)`, "YbY TSP Male Tukey.csv")
plot(tukey.two.way)
#Run t tests to compare between years
pairwiseTtest = matrix(nrow = 10, ncol = 10)
pairwiseTtest = pairwise.t.test(TSP_Overlap$V11, TSP_Overlap$Year,
                p.adjust.method = "BH", pool.sd = FALSE)
write.csv(pairwiseTtest$p.value, "Year by Year TSP Overlap Male T Test Results.csv")

#Hatch_Percentage across all years including all data
d$Survival<-gsub("%$","",d$Survival)
HatchPercent = subset(d, Incubation>40)
Hatchpercentplot = ggplot(HatchPercent, aes(x=Mean.Temp, y=as.numeric(Incubation))) + 
  ylim(30, 80) +
  ylab("Incubation Period (Days)")+
  xlab("Mean Incubation Temperature (°C)")+
  geom_point(cex = 0.3) +
  theme_minimal() +
  geom_smooth(method = "gam", formula = y ~ s(x))
s = gam(as.numeric(d$Survival) ~ s(as.numeric(d$Mean.Temp)) + s(as.numeric(d$Incubation)),
        data = d)
summary(s)

jpeg("HatchPercent.jpeg", units="in", width=10, height=5, res=300)
Hatchpercentplot
dev.off()
class(d$Incubation)
d$TSP = dmy(d$TSP)
ggplot(d, na.rm= TRUE, aes(x=Year, y=yday(TSP), col = as.factor(`Year`))) + 
  geom_jitter(shape=16, cex = 0.5, position=position_jitter(0.2)) +
  geom_boxplot(notch = F, aes(group = as.factor(`Year`))) +
  theme_minimal() 

mean(d$Incubation, na.rm = T) #53.94677
sd(d$Incubation, na.rm = T) #7.490503
max(d$Incubation, na.rm = T) #79
min(d$Incubation, na.rm = T) #20
OverallTempIncubation = ggplot(d, mapping = aes(d$Mean.Temp, Incubation))+
  geom_point(cex = 0.3) +
  geom_smooth(method = "gam",formula= y ~ s(x), cex = 0.3) +
  xlab("Mean TSP Temperature (\u00b0C)")+
  ylab("Incubation Period (days)")
jpeg("OverallTempIncubation.jpeg", units="in", width=5, height=5, res=300)
OverallTempIncubation
dev.off()
tempincmodel = gam(as.numeric(d$Incubation) ~ s(as.numeric(d$Mean.Temp)),
                   data = d)
summary(tempincmodel)
AIC(tempincmodel)
#r sq 0.327, dev explained = 33.2%, AIC = 3253.655
tempincglmodel = glm(as.numeric(d$Incubation) ~ as.numeric(d$Mean.Temp) + yday(TSP),
                   data = d)
summary(tempincglmodel)
with(summary(tempincglmodel), 1 - deviance/null.deviance)
#r sq 0.3142, AIC = 3261.7

########################
#Building our predictive model######
###################
pred = d
pred$SST = SST$Mean_SST_Temperature
pred$Air = weatherdata$Mean_Air_Temperature
pred$TSP = dmy(pred$TSP)
pred$TSP = yday(pred$TSP)
pred$Laid =dmy(pred$Laid)
m1 = gam(Mean.Temp ~ s(SST) + s(Air), data = pred)
summary(m1)
#66.7%
m2 = gam(Mean.Temp ~ s(yday(TSP)) + s(SST), data = pred)
summary(m2)
#64.5%
m3 = gam(Mean.Temp ~ s(yday(TSP)) + s(SST) + s(Air), data = pred)
summary(m3)
#69%, I'm not selecting this model because it has a very high error later in 
#the year, and will misinterpret the increased temperatures earlier in the year 

m5 = gamm(Mean.Temp ~ s(yday(TSP)) + te(SST, yday(TSP)) + te(yday(TSP), Air) +
            s(Incubation), data = pred)
summary(m5$gam)
#r sq 0.728, this model again fails to comprehend the higher temperatures earlier in 
#the season
with(summary(m5), 1 - deviance/null.deviance)
vgam = na.omit(pred)
m7 = vgam(`Mean.Temp` ~ s(`Air`) + s(`SST`), family = poissonff, data = vgam)
summary(m7)
#Not selected as very data intensive and no more accurate than simpler models 

#######Applying our predictions

#First create a matrix for predictions, this will have 1,000
#"nests" and will be used to form the prediction of a single
#hypothetical years gender ratio
#Read in our climatic predictions
Predicted_SST = read.csv("~/Dalyan TSP Analysis/Predicted_SST.csv", header = TRUE)
Predicted_Air_Temp = read.csv("~/Dalyan TSP Analysis/Predicted_Air.csv", header = TRUE)
#Make blank input table for GAM
gam_input = as.data.frame(matrix(nrow = 1000, ncol = 2))
colnames(gam_input) = c( "SST", "Air")
gam_shifted_10days_input = as.data.frame(matrix(nrow = 1000, ncol = 2))
colnames(gam_shifted_10days_input) = c( "SST", "Air")
gam_shifted_20days_input = as.data.frame(matrix(nrow = 1000, ncol = 2))
colnames(gam_shifted_20days_input) = c( "SST", "Air")
#Make blank table for simulated results
simulated_results = as.data.frame(matrix(nrow = 1000, ncol = 9))
colnames(simulated_results) = c("Near", "Near Shifted 10 days", "Near Shifted 20 days",
                                "Mid", "Mid Shifted 10 days", "Near Shifted 20 days",
                                "Long", "Long Shifted days", "Near Shifted 20 days")
#Make blank table for randomised nests
future_predictions = as.data.frame(matrix(nrow = 1000, ncol = 16))
colnames(future_predictions) = c("Mid_Point", "Incubation_Period"
                                 , "TSP_Start", "TSP_End",
                                 "Mean_Near_SST_Temp", "Mean_Near_Air_Temp",
                                 "Mean_Near_Incubation_Temp", "Near_Gender_Ratio",
                                 "Mean_mid_SST_Temp", "Mean_mid_Air_Temp",
                                 "Mean_mid_Incubation_Temp", "mid_Gender_Ratio",
                                 "Mean_long_SST_Temp", "Mean_long_Air_Temp",
                                 "Mean_long_Incubation_Temp", "long_Gender_Ratio")
future_predictions_shifted_10days = as.data.frame(matrix(nrow = 1000, ncol = 16))
colnames(future_predictions_shifted_10days) = c("Mid_Point", "Incubation_Period"
                                                , "TSP_Start", "TSP_End",
                                                "Mean_Near_SST_Temp", "Mean_Near_Air_Temp",
                                                "Mean_Near_Incubation_Temp", "Near_Gender_Ratio",
                                                "Mean_mid_SST_Temp", "Mean_mid_Air_Temp",
                                                "Mean_mid_Incubation_Temp", "mid_Gender_Ratio",
                                                "Mean_long_SST_Temp", "Mean_long_Air_Temp",
                                                "Mean_long_Incubation_Temp", "long_Gender_Ratio")
future_predictions_shifted_20days = as.data.frame(matrix(nrow = 1000, ncol = 16))
colnames(future_predictions_shifted_20days) = c("Mid_Point", "Incubation_Period"
                                                , "TSP_Start", "TSP_End",
                                                "Mean_Near_SST_Temp", "Mean_Near_Air_Temp",
                                                "Mean_Near_Incubation_Temp", "Near_Gender_Ratio",
                                                "Mean_mid_SST_Temp", "Mean_mid_Air_Temp",
                                                "Mean_mid_Incubation_Temp", "mid_Gender_Ratio",
                                                "Mean_long_SST_Temp", "Mean_long_Air_Temp",
                                                "Mean_long_Incubation_Temp", "long_Gender_Ratio")


#Make a blank table for recording number of nests over 33 degrees
Lethal_Temp = as.data.frame(matrix(nrow = 1000, ncol = 3))
colnames(Lethal_Temp) = c("Near", "Mid", "Far")
Lethal_Temp_Shifted_10days = as.data.frame(matrix(nrow = 1000, ncol = 3))
colnames(Lethal_Temp_Shifted_10days) = c("Near", "Mid", "Far")
Lethal_Temp_Shifted_20days = as.data.frame(matrix(nrow = 1000, ncol = 3))
colnames(Lethal_Temp_Shifted_20days) = c("Near", "Mid", "Far")
nearnormal = as.data.frame(matrix(nrow = 1000, ncol = 1000))
midnormal = as.data.frame(matrix(nrow = 1000, ncol = 1000))
farnormal = as.data.frame(matrix(nrow = 1000, ncol = 1000))
near10d = as.data.frame(matrix(nrow = 1000, ncol = 1000))
mid10d = as.data.frame(matrix(nrow = 1000, ncol = 1000))
far10d = as.data.frame(matrix(nrow = 1000, ncol = 1000))
near20d = as.data.frame(matrix(nrow = 1000, ncol = 1000))
mid20d = as.data.frame(matrix(nrow = 1000, ncol = 1000))
far20d = as.data.frame(matrix(nrow = 1000, ncol = 1000))
#####CHANGE THIS PART FOR MANUAL OVERRIDE

for (a in 817:1000){
  
  #Mid point is normally distributed around July 16th, with a standard
  #deviation of 20 days, so the mid point of TSP for simulated 
  #data will be chose randomly from this range
  
  for (i in 1:1000){
    future_predictions[i,1] = ceiling(rnorm(1, mean = 195.2844, sd = 20.50018))
    future_predictions_shifted_10days[i,1] = ceiling(rnorm(1, mean = 185.2844, sd = 20.50018))
    future_predictions_shifted_20days[i,1] = ceiling(rnorm(1, mean = 175.2844, sd = 20.50018))
    
    #Incubation period also follows a normal distribution around
    #a mean of 54.95229 +/- 7.496565 so the same process is used
    
    future_predictions[i,2] = ceiling((rnorm(1, mean = 53.95229, sd = 7.496565))/3)
    future_predictions_shifted_10days[i,2] = ceiling((rnorm(1, mean = 53.95229, sd = 7.496565))/3)
    future_predictions_shifted_20days[i,2] = ceiling((rnorm(1, mean = 53.95229, sd = 7.496565))/3)
    #Now can then calculate start and end of TSP from these dates 
    future_predictions[i,3] = floor(future_predictions[i,1] - 
                                      (future_predictions[i,2]/2))
    future_predictions_shifted_10days[i,3] = floor(future_predictions_shifted_10days[i,1] - 
                                                     (future_predictions_shifted_10days[i,2]/2))
    future_predictions_shifted_20days[i,3] = floor(future_predictions_shifted_20days[i,1] - 
                                                     (future_predictions_shifted_20days[i,2]/2))
    future_predictions[i,4] = ceiling(future_predictions[i,1] + 
                                        (future_predictions[i,2]/2))
    future_predictions_shifted_10days[i,4] = ceiling(future_predictions_shifted_10days[i,1] + 
                                                       (future_predictions_shifted_10days[i,2]/2))
    future_predictions_shifted_20days[i,4] = ceiling(future_predictions_shifted_20days[i,1] + 
                                                       (future_predictions_shifted_20days[i,2]/2))
    
    
    #Our limitations on how early nests can begin
    future_predictions = subset(future_predictions, TSP_Start >= 92)
    future_predictions_shifted_10days = subset(future_predictions_shifted_10days, TSP_Start >=92)
    future_predictions_shifted_20days = subset(future_predictions_shifted_20days, TSP_Start >=92)
    
    #Logging how long these are recorded
    fp = as.numeric(nrow(future_predictions))
    fps10 = as.numeric(nrow(future_predictions_shifted_10days))
    fps20 = as.numeric(nrow(future_predictions_shifted_20days))
    
    #Calculate mean SST during simulated TSP (remember our climatic
    #data starts April 1st the 91st day of the year)
    z = (future_predictions[i,3]) - 91
    x = (future_predictions[i,4]) - 91
    #Copy in data with 10 day nesting phenology shift
    b = (future_predictions_shifted_10days[i,3]) - 91
    c = (future_predictions_shifted_10days[i,4]) - 91
    #Copy in data with 10 day nesting phenology shift
    j = (future_predictions_shifted_20days[i,3]) - 91
    k = (future_predictions_shifted_20days[i,4]) - 91
    #Fetch Near SST
    future_predictions[i,5] = mean(Predicted_SST[z:x,17])
    future_predictions_shifted_10days[i,5] = mean(Predicted_SST[b:c,17])
    future_predictions_shifted_20days[i,5] = mean(Predicted_SST[j:k,17])
    
    #Same again for predicted Near Air temp
    future_predictions[i,6] = mean(Predicted_Air_Temp[z:x,14])
    future_predictions_shifted_10days[i,6] = mean(Predicted_Air_Temp[b:c,14])
    future_predictions_shifted_20days[i,6] = mean(Predicted_Air_Temp[j:k,14])
    
    #Fetch Mid SST
    future_predictions[i,9] = mean(Predicted_SST[z:x,18])
    future_predictions_shifted_10days[i,9] = mean(Predicted_SST[b:c,18])
    future_predictions_shifted_20days[i,9] = mean(Predicted_SST[j:k,18])
    
    #Same again for predicted Mid Air temp
    future_predictions[i,10] = mean(Predicted_Air_Temp[z:x,15])
    future_predictions_shifted_10days[i,10] = mean(Predicted_Air_Temp[b:c,15])
    future_predictions_shifted_20days[i,10] = mean(Predicted_Air_Temp[j:k,15])
    
    #Fetch Far SST
    future_predictions[i,13] = mean(Predicted_SST[z:x,19])
    future_predictions_shifted_10days[i,13] = mean(Predicted_SST[b:c,19])
    future_predictions_shifted_20days[i,13] = mean(Predicted_SST[j:k,19])
    
    #Same again for predicted Far Air temp
    future_predictions[i,14] = mean(Predicted_Air_Temp[z:x,16])
    future_predictions_shifted_10days[i,14] = mean(Predicted_Air_Temp[b:c,16])
    future_predictions_shifted_20days[i,14] = mean(Predicted_Air_Temp[j:k,16])
    
    
    
  }
  #Then apply GAM
  #Choose which model you're using and create a dataframe with 
  #matching col names of data input 
  #NEAR 
  gam_input = as.data.frame(matrix(nrow = fp, ncol = 2))
  colnames(gam_input) = c( "SST", "Air")
  gam_input$SST = future_predictions[,5]
  gam_input$Air = future_predictions[,6]
  for (n in 1:fp){
    future_predictions[n,7] = predict.gam(m1,gam_input[n,], type="link",
                                          se.fit=F)
  }
  
  gam_shifted_10days_input = as.data.frame(matrix(nrow = fps10, ncol = 2))
  colnames(gam_shifted_10days_input) = c( "SST", "Air")
  
  gam_shifted_10days_input$SST = future_predictions_shifted_10days[,5]
  gam_shifted_10days_input$Air = future_predictions_shifted_10days[,6]
  
  for (n in 1:fps10){
    future_predictions_shifted_10days[n,7] = predict.gam(m1,gam_shifted_10days_input[n,], type="link",
                                                         se.fit=F)
  }
  
  
  gam_shifted_20days_input = as.data.frame(matrix(nrow = fps20, ncol = 2))
  colnames(gam_shifted_20days_input) = c( "SST", "Air")
  gam_shifted_20days_input$SST = future_predictions_shifted_20days[,5]
  gam_shifted_20days_input$Air = future_predictions_shifted_20days[,6]
  
  for (n in 1:fps20){
    future_predictions_shifted_20days[n,7] = predict.gam(m1,gam_shifted_20days_input[n,], type="link",
                                                         se.fit=F)
  }
  
  
  #Then apply Hill equation to predicted nest temperatures
  for (i in 1:fp){
    t = future_predictions[i,7]
    q = exp((1/s)*(log(p+K)-log(t+K)))
    sr = 1/(1+q)
    future_predictions[i,8] = sr
  }
  for (i in 1:fps10){
    t = future_predictions_shifted_10days[i,7]
    q = exp((1/s)*(log(p+K)-log(t+K)))
    sr = 1/(1+q)
    future_predictions_shifted_10days[i,8] = sr
  }
  
  for (i in 1:fps20){
    t = future_predictions_shifted_20days[i,7]
    q = exp((1/s)*(log(p+K)-log(t+K)))
    sr = 1/(1+q)
    future_predictions_shifted_20days[i,8] = sr
  } 
  #MID
  gam_input$SST = future_predictions[,9]
  gam_input$Air = future_predictions[,10]
  
  for (n in 1:fp){
    future_predictions[n,11] = predict.gam(m1,gam_input[n,], type="link",
                                           se.fit=F)
  }
  
  gam_shifted_10days_input$SST = future_predictions_shifted_10days[,9]
  gam_shifted_10days_input$Air = future_predictions_shifted_10days[,10]
  
  for(n in 1:fps10){
    future_predictions_shifted_10days[n,11] = predict.gam(m1,gam_shifted_10days_input[n,], type="link",
                                                          se.fit=F)
  } 
  
  gam_shifted_20days_input = as.data.frame(matrix(nrow = fps20, ncol = 2))
  colnames(gam_shifted_20days_input) = c( "SST", "Air")
  gam_shifted_20days_input$SST = future_predictions_shifted_20days[,9]
  gam_shifted_20days_input$Air = future_predictions_shifted_20days[,10]
  for(n in 1:fps20){
    future_predictions_shifted_20days[n,11] = predict.gam(m1,gam_shifted_20days_input[n,], type="link",
                                                          se.fit=F)
  }
  #Then apply Hill equation to predicted nest temperatures
  for (i in 1:fp){
    t = future_predictions[i,11]
    q = exp((1/s)*(log(p+K)-log(t+K)))
    sr = 1/(1+q)
    future_predictions[i,12] = sr
  }
  for (i in 1:fps10){
    t = future_predictions_shifted_10days[i,11]
    q = exp((1/s)*(log(p+K)-log(t+K)))
    sr = 1/(1+q)
    future_predictions_shifted_10days[i,12] = sr
  }
  for (i in 1:fps20){
    t = future_predictions_shifted_20days[i,11]
    q = exp((1/s)*(log(p+K)-log(t+K)))
    sr = 1/(1+q)
    future_predictions_shifted_20days[i,12] = sr
  }
  #FAR
  
  gam_input$SST = future_predictions[,13]
  gam_input$Air = future_predictions[,14]
  
  for (n in 1:fp){
    future_predictions[n,15] = predict.gam(m1,gam_input[n,], type="link",
                                           se.fit=F)
  }
  
  
  gam_shifted_10days_input$SST = future_predictions_shifted_10days[,13]
  gam_shifted_10days_input$Air = future_predictions_shifted_10days[,14]
  
  for (n in 1:fps10){
    future_predictions_shifted_10days[n,15] = predict.gam(m1,gam_shifted_10days_input[n,], type="link",
                                                          se.fit=F)
  }
  
  gam_shifted_20days_input$SST = future_predictions_shifted_20days[,13]
  gam_shifted_20days_input$Air = future_predictions_shifted_20days[,14]
  
  for (n in 1:fps20){
    future_predictions_shifted_20days[n,15] = predict.gam(m1,gam_shifted_20days_input[n,], type="link",
                                                          se.fit=F)
  }
  #Then apply Hill equation to predicted nest temperatures
  for (i in 1:fp){
    t = future_predictions[i,15]
    q = exp((1/s)*(log(p+K)-log(t+K)))
    sr = 1/(1+q)
    future_predictions[i,16] = sr
  }
  for (i in 1:fps10){
    t = future_predictions_shifted_10days[i,15]
    q = exp((1/s)*(log(p+K)-log(t+K)))
    sr = 1/(1+q)
    future_predictions_shifted_10days[i,16] = sr
  }
  
  for (i in 1:fps20){
    t = future_predictions_shifted_20days[i,15]
    q = exp((1/s)*(log(p+K)-log(t+K)))
    sr = 1/(1+q)
    future_predictions_shifted_20days[i,16] = sr
  } 
  
  
  Lethal_Temp[a, 1] = sum(as.numeric(future_predictions$Mean_Near_Incubation_Temp)>32, na.rm = T)
  Lethal_Temp[a, 2] = sum(as.numeric(future_predictions$Mean_mid_Incubation_Temp)>32, na.rm = T)
  Lethal_Temp[a, 3] = sum(as.numeric(future_predictions$Mean_far_Incubation_Temp)>32, na.rm = T)
  
  Lethal_Temp_Shifted_10days[a, 1] = sum(future_predictions_shifted_10days$Mean_Near_Incubation_Temp>32, na.rm = T)
  Lethal_Temp_Shifted_10days[a, 2] = sum(future_predictions_shifted_10days$Mean_mid_Incubation_Temp>32, na.rm = T)
  Lethal_Temp_Shifted_10days[a, 3] = sum(future_predictions_shifted_10days$Mean_far_Incubation_Temp>32, na.rm = T)
  
  Lethal_Temp_Shifted_20days[a, 1] = sum(future_predictions_shifted_20days$Mean_Near_Incubation_Temp>32, na.rm = T)
  Lethal_Temp_Shifted_20days[a, 2] = sum(future_predictions_shifted_20days$Mean_mid_Incubation_Temp>32, na.rm = T)
  Lethal_Temp_Shifted_20days[a, 3] = sum(future_predictions_shifted_20days$Mean_far_Incubation_Temp>32, na.rm = T)
  
  #Then record mean into overall simulation dataframe
  simulated_results[a,1] = mean(na.omit(future_predictions[,8]))
  simulated_results[a,2] = mean(na.omit(future_predictions_shifted_10days[,8]))
  simulated_results[a,3] = mean(na.omit(future_predictions_shifted_20days[,8]))
  
  simulated_results[a,4] = mean(na.omit(future_predictions[,12]))
  simulated_results[a,5] = mean(na.omit(future_predictions_shifted_10days[,12]))
  simulated_results[a,6] = mean(na.omit(future_predictions_shifted_20days[,12]))
  
  simulated_results[a,7] = mean(na.omit(future_predictions[,16]))
  simulated_results[a,8] = mean(na.omit(future_predictions_shifted_10days[,16]))
  simulated_results[a,9] = mean(na.omit(future_predictions_shifted_20days[,16]))
  
  print(a)
}
View(simulated_results)
summarySimulatedresults = as.data.frame(matrix(nrow = 2, ncol = 9))
colnames(summarySimulatedresults) = colnames(simulated_results)
row.names(summarySimulatedresults) = c("Mean", "Standard Deviation")
for (i in 1:9) {
  summarySimulatedresults[1,i] = mean(simulated_results[,i])
  summarySimulatedresults[2,i] = sd(simulated_results[,i])
}
write.csv(summarySimulatedresults, "Summary.csv")
###
####
##
#####Creating a figure 
######

predictions_figure_data = simulated_results
colnames(predictions_figure_data) = c("Near Term (2021-2040)", "Near Term Nesting Shifted 10 Days (2021-2040)","Near Term Nesting Shifted 20 Days (2021-2040)",
                                      "Mid Term (2041-2060)", "Mid Term Nesting Shifted 10 Days (2041-2060)","Mid Term Nesting Shifted 20 Days (2041-2060)",
                                      "Far Term (2081-2100)", "Far Term Nesting Shifted 10 Days (2081-2100)","Far Term Nesting Shifted 20 Days (2081-2100)")
predictions_figure_data = melt(predictions_figure_data)
colnames(predictions_figure_data) = c("Nesting Conditions", "Male Ratio")
prediction_image =
  ggplot(predictions_figure_data, na.rm= TRUE, aes(x=`Nesting Conditions`, y=`Male Ratio`, col = as.factor(`Nesting Conditions`))) +
  geom_hline(yintercept=0.2572537, linetype="dashed", color = "red")+
  scale_color_manual(values = c('Near Term (2021-2040)' = "purple",
                                `Near Term Nesting Shifted 10 Days (2021-2040)` = "orange",
                                "Near Term Nesting Shifted 20 Days (2021-2040)" = "green",
                                "Mid Term (2041-2060)" = "purple",
                                "Mid Term Nesting Shifted 10 Days (2041-2060)" = "orange", 
                                "Mid Term Nesting Shifted 20 Days (2041-2060)" = "green",
                                "Far Term (2081-2100)" = "purple",
                                "Far Term Nesting Shifted 10 Days (2081-2100)" = "orange",
                                "Far Term Nesting Shifted 20 Days (2081-2100)" = "green"), name = "Phenological Pattern") +
  geom_hline(yintercept = lowerCI, color = "cyan", cex = 0.5)+
  geom_hline(yintercept = upperCI, color = "cyan", cex = 0.5) +
  geom_boxplot(notch = FALSE, aes(group = as.factor(`Nesting Conditions`))) +
  geom_jitter(shape=16, cex = 0.1, position=position_jitter(0.2)) +
  theme_minimal()+
  theme(axis.text.x=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


jpeg("Predicitons.jpeg", units="in", width=10, height=5, res=300)
prediction_image
dev.off()

#95% CI Margins 
samplesize = nrow(d)
standarddeviation = sd(d$V11, na.rm = T)
margin = qt(0.975,df = samplesize-1)*standarddeviation/sqrt(samplesize)
meanMales = mean(d$V11, na.rm = T)
upperCI = meanMales + margin
lowerCI = meanMales - margin
