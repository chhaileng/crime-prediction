#load data set
originalData <- read.csv("/Users/Chhaileng/Documents/data_mining/crime_rate_prediction_dataset.csv")


# correlation
#install.packages("GGally")
library(GGally)
ggpairs(data = originalData, columns = 2:5, title = "Crime data")

#convert to district to dummy variable
originalData$Districts<-factor(originalData$Districts)

#fit data linear
# lm is linear regression function
fit_linear <- lm(N.Crime ~ Camera + Population_thousands * GRDP_billion_won+Districts, data=originalData)
summary(fit_linear)

predict(fit_linear,data.frame(Camera=1722	,Population_thousands=583,GRDP_billion_won=64643,Districts="Gangnam-gu"))



#run predict model
predicted_model <- predict(fit_linear,originalData,se.fit = TRUE)


crime_predicted <-unname(predicted_model$fit)  
crime_actual <- originalData$N.Crime

crimerate_final <- data.frame(crime_predicted, crime_actual)

print(crimerate_final)

#plot scatter plot
ggplot(data = crimerate_final, aes(x = crime_actual, y = crime_predicted))+
  geom_point()+
  stat_smooth(method = "lm", col = "dodgerblue3")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Model fit between actual and predicted")




#find error rate
#install.packages("MLmetrics")
library(MLmetrics)
#RMSE(crimerate_predicted,crimerate_actual)
#MAPE(crimerate_predicted,crimerate_actual)

RMSE(crime_predicted,crime_actual)
MAPE(crime_predicted,crime_actual)







