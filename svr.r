
library(e1071)
#load data set
originalData <- read.csv("/Users/Chhaileng/Documents/data_mining/crime_rate_prediction_dataset.csv")

# convert district to dummy variables
originalData$Districts<-factor(originalData$Districts)
library(GGally)
#predictors correlations
ggpairs(data = originalData, columns = 2:5, title = "Crime data")

#fit data SMVcan
fit_svm = svm(N.Crime~Camera+Population_thousands*GRDP_billion_won+Districts, originalData,kernel="linear")


predYsvm = predict(fit_svm,originalData)

predDataFrame <- data.frame(predYsvm)


points(originalData$N.Crime, predYsvm, col = "red", pch=16)

predFinal <- data.frame(predYsvm,originalData$N.Crime)

print(predFinal)

#plot scatter plot
ggplot(data = predFinal, aes(x = originalData.N.Crime, y = predYsvm))+
  geom_point()+
  stat_smooth(method = "lm", col = "dodgerblue3")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Model fit between actual and predicted")

#find error rate
library(MLmetrics)
RMSE(predYsvm,originalData$N.Crime)
MAPE(predYsvm,originalData$N.Crime)
