library(readr)
library(openxlsx)
airlines <- read.xlsx(file.choose())
#there are 12 months,so we create 12 dummy variables
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
View(X)
colnames(X)<-month.abb
View(X)
airdata<-cbind(airlines,X) #attaching dummy variables to the existing datatset airlines
colnames(airdata)[2] <- "passengers"
colnames(airdata)
airdata["t"] <- c(1:96)
View(airdata)
airdata["log_passengers"] <- log(airdata["passengers"])
airdata["t_square"] <- airdata["t"]*airdata["t"]
attach(airdata)

#data partition
train <- airdata[1:89,]
test <- airdata[90:96,]

#creating linear model
linear_model <- lm(passengers~.,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear

#creating exponential model
exp_model <- lm(log_passengers~t,data=train)
summary(exp_model)
expo_pred <- data.frame(predict(exp_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

#creating quadratic model
quad_model <- lm(passengers~t+t_square,data=train)
summary(quad_model)
quad_pred<-data.frame(predict(quad_model,interval='predict',newdata=test))
rmse_quad<-sqrt(mean((test$passengers-quad_pred$fit)^2,na.rm=T))
rmse_quad

#additive seasonality
sea_add_model<-lm(passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

#additive seasonality with quadratic
Add_sea_Quad_model<-lm(passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

#multiplicative seasonality
multi_sea_model<-lm(log_passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

#multiplicative additive seasonality
multi_add_sea_model<-lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

#preparing a model on model and its RMSE values
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#Multiplicative additive seasonality has the least RMSE value