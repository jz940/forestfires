library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(GGally)
library(gridExtra)
library(reshape)
forestfiredata <- read.csv("forestfires.csv",header=TRUE,as.is = TRUE)
forestfiredata$ISI = ifelse(forestfiredata$ISI>23,10,forestfiredata$ISI) 
 
X = as.numeric(forestfiredata$X)
Y = as.numeric(forestfiredata$Y)
forestfiredata$area = as.numeric(forestfiredata$area)
forestfiredata$area = log(forestfiredata$area) + 1
forestfiredata$area = ifelse(forestfiredata$area>0,forestfiredata$area,0)
 
 
 
data_melt <- melt(forestfiredata)

d_day <-  forestfiredata %>% 
   group_by(X,Y) %>%
   summarize(n=n())
 
d_day %>% ggplot(aes(X, Y)) +
   geom_tile(aes(fill=n)) +
   scale_fill_gradient(low = "yellow", high = "red")  
 
 
d_fire <- forestfiredata %>%
   group_by(new_day) %>% 
   summarize(n=n()) %>%
   left_join(forestfiredata,by="new_day") %>%
   select(FFMC,DMC,DC,ISI,X,Y,month,area,n) %>% 
   unique() %>%
   mutate(FS=mean(FFMC),DMS=mean(DMC),DS=mean(DC),IS=mean(ISI),sum=(FFMC/FS+DMC/DMS+DC/DS+ISI/IS))
 
d_fires <- group_by(d_fire,X,Y)
fires_sum <- summarize(d_fires,my_mean=mean(sum))
 
fires_sum <- fires_sum %>% left_join(d_day,by=c("X","Y")) 
 
fires_sum <- fires_sum %>% mutate(avg=n/52,weight=avg*my_mean) %>% select(X,Y,weight)
 
fires_sum %>% ggplot(aes(X, Y)) +
   geom_tile(aes(fill=weight)) +
   scale_fill_gradient(low = "yellow", high = "red") 

d_fires <- d_fire %>% left_join(fires_sum,by=c("X","Y"))
 
d1 <- forestfiredata %>%
   group_by(new_day) %>% 
   summarize(n=n()) %>%
   left_join(forestfiredata,by="new_day") %>%
   select(FFMC,DMC,DC,ISI,month,n) %>% 
   unique() %>%
   mutate(FS=mean(FFMC),DMS=mean(DMC),DS=mean(DC),IS=mean(ISI),sum=(FFMC/FS+DMC/DMS+DC/DS+ISI/IS))
 
d_mine <- group_by(d1,month)
new_sum <- summarize(d_mine,my_mean=mean(sum))

d_fires <- d_fires %>% left_join(new_sum,by="month")

d_fires <- d_fires %>% mutate(my_weight=weight*my_mean)

d_fires <- d_fires %>% filter(area>0)

FFMC = d_fires$FFMC
DMC = d_fires$DMC
DC = d_fires$DC
ISI = d_fires$ISI
X = d_fires$X
Y = d_fires$Y
weight = d_fires$weight
mean = d_fires$my_mean
area = d_fires$area


d1 <- forestfiredata %>%
  group_by(new_day) %>%
  summarize(n=n()) %>%
  left_join(forestfiredata,by="new_day") %>%
  select(FFMC,DMC,DC,ISI,month,n) %>%
  unique() %>%
  mutate(FS=mean(FFMC),DMS=mean(DMC),DS=mean(DC),IS=mean(ISI),sum=(FFMC/FS+DMC/DMS+DC/DS+ISI/IS))

d_mine <- group_by(d1,month)
new_sum <- summarize(d_mine,my_mean=mean(sum))

d1 <- d1 %>%
  left_join(new_sum,by="month")

d1 <- d1 %>%
  mutate(error=(my_mean-sum)^2)

d1 %>% ggplot(aes(x=my_mean,y=sum,size=error,color=month,alpha=0.75)) + geom_point() + geom_smooth(se=FALSE)

sum(d1$error)/nrow(d1)


d1 %>% ggplot(aes(x=sum)) + geom_histogram()
  #group_by(month) %>%summarize(my_n = n(),full=nrow(data)) %>%mutate(my_month=my_n/full) %>% ungroup()

FFMC = d1$FFMC
DMC = d1$DMC
DC = d1$DC
ISI = d1$ISI
month = d1$month
n = d1$n
sum = d1$sum
mean = d1$my_mean
error = d1$error


tempdataset <- data.frame(FFMC,DMC,DC,ISI,X,Y,weight,mean,area)

Dataset<-tempdataset[complete.cases(tempdataset), ]

Dataset = Dataset[sample(nrow(Dataset)),]

DataSetTraining = Dataset[1:120,]
DataSetValidation = Dataset[121:240,]
DataSetTesting = Dataset[241:263,]
detach(DataSetTraining)


attach(DataSetTraining)
# Plot scatterplot and correlation matrix
data <- data.frame(area,mean,weight,FFMC,DMC,DC,ISI)
ggpairs(data, lower = list(continuous = wrap("points", alpha = 0.3,    size=0.1)))

m.mls <- lm(area ~ mean+weight)
m.mls
summary(m.mls)


StanResMLS <- rstandard(m.mls)
dataMLS <- data.frame(area,StanResMLS)

ggplot() +
  geom_point(data=dataMLS, aes(x=area, y=StanResMLS, color = "MLS"), size = 1) +
  geom_hline(yintercept=2,color='blue') + geom_hline(yintercept=-2, color='blue') +
  scale_color_manual(name = element_blank(), labels = c("MLS"), values = c("blue")) +
  labs(y = "Standarized Residual") + ggtitle("Standarized Residuals MLS Plot")

Fitted = fitted(m.mls)
dataMLSFitted <- data.frame(Fitted,StanResMLS)

# MLS Stan. Res. vs Fitted plot
ggplot() +
  geom_point(data=dataMLSFitted, aes(x=Fitted, y=StanResMLS, color = "MLS"), size = 1) +
  geom_hline(yintercept=2,color='blue') + geom_hline(yintercept=-2, color='blue') +
  scale_color_manual(name = element_blank(), labels = c("MLS"), values = c("blue")) +
  labs(y = "Standarized Residual") + labs(x = "Fitted value") +
  ggtitle("Standarized Residuals MLS Plot (Fitted) ")


plot(m.mls)
# Residuals for training data
ResMLS <- resid(m.mls)

# Residuals for validation data
output<-predict(m.mls, se.fit = TRUE, newdata=data.frame(mean=DataSetValidation$mean,weight=DataSetValidation$weight))
ResMLSValidation <- DataSetValidation$area - output$fit

# Mean Square Error for training data
mean((ResMLS)^2)

mean((ResMLSValidation)^2)

mean((ResMLSValidation)^2) / mean((DataSetValidation$area)^2)

test = data.frame(DataSetValidation$area,output$fit, 1:length(output$fit));
colnames(test)[1] = "area"
colnames(test)[2] = "Prediction"
colnames(test)[3] = "Index"

# Plot GroundCO vs Prediction for Validation Data Set
ggplot(data = test, aes(x = area, y = Prediction)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Validation sum vs Prediction")

ggplot(data = test, aes(x = Index)) +
  geom_line(aes(y = area, color = "area")) +
  geom_line(aes(y = Prediction, color="Prediction"), linetype="twodash") +
  scale_color_manual(name = element_blank(), labels = c("area","Prediction"),
                     values = c("darkred", "steelblue")) + labs(y = "") +
  ggtitle("Validation")

d <- forestfiredata %>%
  group_by(X,Y,month,day) %>%
  summarize(n=n())

d %>%
  ggplot(aes(x=X,y=Y,size=n,color=month)) + geom_jitter(width=0.25,height=0.25,alpha=0.7)


d2 <- forestfiredata %>%
  group_by(X,Y,month,day,new_day) %>%
  summarize(n=n())

d2 %>%
  ggplot(aes(x=X,y=Y,color=month)) +
  geom_jitter(width=0.25,height=0.25,alpha=0.7)

d3 <- forestfiredata %>%
  group_by(X,Y,month) %>%
  summarize(n=n())

d3 %>%
  ggplot(aes(x=X,y=Y,size=n,color=month)) +
  geom_jitter(width=0.25,height=0.25,alpha=0.7)

d4 <-  forestfiredata %>%
  group_by(new_day) %>%
  summarize(n=n()) %>%
  left_join(forestfiredata,by="new_day") %>%
  group_by(X,Y,month,day,n) %>%
  select(X,Y,month,day,n) %>%
  unique()

d4 %>%
  ggplot(aes(x=X,y=Y,size=n,color=month)) +
  geom_jitter(width=0.25,height=0.25,alpha=0.7)


#d4 <- d %>%
#  unite(col=new,month,day,new_day) %>%
#  select(-n) %>%
#  group_by(new) %>%
#  summarize(n=n()) %>%
#  separate(new, into=c("letter","month","day","new_day")) %>%
#  unite(col=month,letter,month)


#



#d4 %>% ggplot(aes(x=month,y=day,size=n)) + geom_jitter(width=0.25,height=0.25)




d_day %>%
  ggplot(aes(x=X,y=Y,size=n)) +
  geom_point()
  geom_jitter(width=0.25,height=0.25,alpha=0.7)


FFMC = (forestfiredata$FFMC)
DMC = (forestfiredata$DMC)
DC = (forestfiredata$DC)
ISI = (forestfiredata$ISI)
temp = (forestfiredata$temp)
RH = as.numeric(forestfiredata$RH)
wind = (forestfiredata$wind)
rain = ifelse(forestfiredata$rain<=0.5,0,forestfiredata$rain-0.5)
RS = ifelse(forestfiredata$month=="nov" | forestfiredata$month=="dec" | forestfiredata$month=="jan" | forestfiredata$month=="feb",1,0)


fire_data <- data.frame(X,Y, FFMC, DMC, DC, ISI, temp, RH, wind, rain, RS, area)
fire_data <- fire_data %>%
  left_join(d)

fire_num = as.numeric(fire_data$n)
month = (fire_data$month)
day = (fire_data$day)

#ggpairs(fire_data, lower = list(continuous = wrap("points", alpha = 0.3,    size=0.1)))

Dataset<-fire_data[complete.cases(fire_data), ]
attach(Dataset)


y=Dataset$ISI
x=Dataset$n
m.ols <- lm(x ~ y)

m.quadls <- lm(x~ y+ I(y^2))

m.quartls <- lm(x~y + I(y^2) + I(y^3) + I(y^4))

ISINew<-seq(0,10,len=length(y))
predLinear = predict(m.ols,newdata=data.frame(y=ISINew))
predQuad   = predict(m.quadls,newdata=data.frame(y=ISINew))
#predQuart  = predict(m.quartls,newdata=data.frame(y=ISINew))


ggplot(Dataset, aes(x, y)) +  #geom_point(size=0.1) +
  geom_smooth(method='loess',formula=y~x,aes(x=ISINew,y=predLinear,color="blue"),na.rm = TRUE) +
  geom_smooth(method='loess',formula=y~x,aes(x=ISINew,y=predQuad,color="red"),na.rm = TRUE) +
  #geom_line(mapping = aes(x = ISINew, y = predLinear, color="blue")) +
  #geom_line(mapping = aes(x = ISINew, y = predQuad, color = "red")) +
  #geom_line(mapping = aes(x = ISINew, y = predQuart, color = "green")) +
  scale_color_discrete(name = "Prediction", labels = c("Linear", "Quadratic")) +
  ggtitle("LS Predictions") +
  ylab("prediction")+
  xlab("isi")


FFMC = fire_data$FFMC
DMC = (fire_data$DMC)
DC = (fire_data$DC)
ISI = (fire_data$ISI)
temp = (fire_data$temp)
RH = (fire_data$RH)
wind = (fire_data$wind)
rain = fire_data$rain
RS =fire_data$RS
n = fire_data$n
area = fire_data$n

weather_mls <- glm(n ~ temp + RH + wind + rain)
summary(weather_mls)

weather_2 <- glm(n~temp+RH)
summary(weather_2)

code_mls <- glm(n ~ FFMC + DMC + DC + ISI)
summary(code_mls)

code_2 <- lm(n~DMC)
summary(code_2)


StanResLS <- rstandard(m.ols)
dataLS <- data.frame(x,StanResLS)
ggplot(dataLS, aes(x, StanResLS)) + geom_point(size = 0.1) +
geom_hline(yintercept=2,color='blue') + geom_hline(yintercept=-2, color='blue') +
  ggtitle("Standarized Residuals for OLS")


StanResQLS <- rstandard(m.quadls)
dataQLS <- data.frame(x,StanResQLS)
ggplot(dataQLS, aes(x, StanResQLS)) + geom_point(size = 0.1) +
  geom_hline(yintercept=2,color='blue') + geom_hline(yintercept=-2, color='blue') +
  ggtitle("Standarized Residuals")


ggplot() + geom_point(data=dataLS, aes(x, StanResLS, color = "Linear"), size = 0.1) +
geom_point(data=dataQLS, aes(x, StanResQLS, color = "Quadratic"), size = 0.1) +
geom_hline(yintercept=2,color='blue') + geom_hline(yintercept=-2, color='blue') +
  scale_color_manual(name = element_blank(), labels = c("Linear","Quadratic"), values = c("blue","red")) +
labs(y = "Standarized Residual") + ggtitle("Standarized Residuals Plot")



detach(Dataset)


