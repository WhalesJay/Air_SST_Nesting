nesttiming = d

nesttiming$Nesting_Date = dmy(nesttiming$Nesting_Date)
for (i in 1:527){
  nesttiming[i,12] = month(nesttiming[i,7])
  nesttiming[i,13] = day(nesttiming[i,7])
}

for (i in 1:527){
  if (nesttiming[i,12] == 4){nesttiming[i,14] = nesttiming[i,13]}
  if (nesttiming[i,12] == 5){nesttiming[i,14] = nesttiming[i,13] + 30}
  if (nesttiming[i,12] == 6){nesttiming[i,14] = nesttiming[i,13] + 61}
  if (nesttiming[i,12] == 7){nesttiming[i,14] = nesttiming[i,13] + 91}
  if (nesttiming[i,12] == 8){nesttiming[i,14] = nesttiming[i,13] + 122}
  if (nesttiming[i,12] == 9){nesttiming[i,14] = nesttiming[i,13] + 153}
  if (nesttiming[i,12] == 10){nesttiming[i,14] = nesttiming[i,13] + 184}
}

colnames(nesttiming) = c("serial", "Mean_Temperature", "Standard_Deviation", 
                         "Year", "Hatch_Percentage", "Incubation_Period", 
                         "Nesting_Date", "TSP_Mid_Point"
                         , "TSP_Bin", "Hatch_Date", "Males", "Laid_Month", "Laid_Date", 
                         "Laid_Numeral")
#GLM first
trial = gam(Laid_Numeral ~ s(Year), data = nesttiming)
summary(trial)


jpeg("Nest Timing.jpeg", units="in", width=10, height=5, res=300)
ggplot(nesttiming, na.rm= TRUE, aes(x=Year, y=Laid_Numeral, col = as.factor(`Year`))) + 
  geom_jitter(shape=16, cex = 0.5, position=position_jitter(0.2)) +
  geom_boxplot(notch = F, aes(group = as.factor(`Year`))) +
  theme_minimal() 
dev.off()

f = matrix(nrow = 527, ncol =2)
f[,1] = yday(nesttiming$Nesting_Date)
f[,2] = nesttiming$Laid_Numeral
