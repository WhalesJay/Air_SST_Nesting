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
