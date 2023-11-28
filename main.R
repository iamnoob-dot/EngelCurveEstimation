library(ggplot2)

data_total= read.csv("Maharasthra.csv")

HHID = data_total$HHID
items = data_total$Srl__No_
totvalue = data_total$Value

data = cbind(HHID,items,totvalue)

sum(as.numeric(is.na(data)))

hhd = data[,1]
hhd =  unique(hhd)
length(hhd)
expen = 0*hhd
itemexp = 0*expen


# 6 = Cereal products, 20 = oil and other food, 31 = entertainment and toilet items, 
#39 = education,Medical,Clothing and durable goods

##################################################################################
# for Cereal Products

for(i in 1:length(hhd)){
  house = subset(data, data[,1] == hhd[i] )
  first = subset(house, house[,2] == 43)
  second = subset(house, house[,2] == 6)
  third = subset(house, house[,2] == 41)
  expen[i] = first[3]
  itemexp[i] = second[3]/third[3]
}

expen = expen/100

sum(as.numeric(is.na(expen)))
sum(as.numeric(is.na(itemexp)))

dabs = cbind(expen,itemexp)
dabs = data.frame(na.omit(dabs))
dabs = as.matrix(dabs)

plot(dabs[,2] ~ dabs[,1])

drel = dabs
drel[,2] = drel[,2]/drel[,1]
plot(drel[,2]~drel[,1])

plot(log(dabs[,2])~log(dabs[,1]))
reg1 = lm(log(dabs[,2]) ~ log(dabs[,1])) # double log
summary(reg1)
plot(reg1)

reg2 = lm(log(dabs[,2]) ~ 1/dabs[,1]) # sigmoid
summary(reg1)
plot(reg1)

reg3 = lm(log(dabs[,2]) ~ log(dabs[,1]))
summary(reg1)
plot(reg1)


coeff = reg1$coefficients

gglog <- ggplot(data = as.data.frame(dabs), aes(x = log(dabs[,1]), y = log(dabs[,2])))+
  geom_point()+labs(title = "Scatterplot of log of Expenditure on Cereal Products vs \n log of Total Expenditure",
                    x = "log of total Expenditure",y = "log of total Expenditure on Cereal Products")

gglog + geom_abline(intercept = coeff[1] , slope = coeff[2] , color="blue",  
                     linetype="solid", size=1.5)

engelcurve <- function(x){
  return(exp(coeff[1])*(x^(coeff[2])))
}

ggabs <- ggplot(data = as.data.frame(dabs), aes(x = (dabs[,1]), y = (dabs[,2]))) +
  geom_point() +
  stat_function(fun = engelcurve, col = "red" )+
  labs(title = "Engel Curve for Cereal Products: Total Expenditure on Cereal Products vs \n Total Expenditure",
       x = "Total Expenditure",
       y = "Total Expenditure on Cereal Products")

ggrel <- ggplot(data = as.data.frame(drel), aes(x = (drel[,1]), y = (drel[,2]))) +
  geom_point() +
  labs(title = "Scatterplot of Relative Expenditure on Cereal Products vs \n Total Expenditure",
       x = "Total Expenditure",
       y = "Relative Expenditure on Cereal Products")

gglog
gglog + geom_abline(intercept = coeff[1] , slope = coeff[2] , color="blue",  
                    linetype="solid", size=1.5)
ggsave('cer1.png')
ggabs
ggsave('cer2.png')
ggrel
ggsave('cer3.png')



####################################################################################

for(i in 1:length(hhd)){
  house = subset(data, data[,1] == hhd[i] )
  first = subset(house, house[,2] == 43)
  second = subset(house, house[,2] == 20)
  third = subset(house, house[,2] == 41)
  expen[i] = first[3]
  itemexp[i] = second[3]/third[3]
}

expen = expen/100

sum(as.numeric(is.na(expen)))
sum(as.numeric(is.na(itemexp)))

dabs = cbind(expen,itemexp)
dabs = data.frame(na.omit(dabs))
dabs = as.matrix(dabs)

plot(dabs[,2] ~ 1/dabs[,1])

drel = dabs
drel[,2] = drel[,2]/drel[,1]
plot(drel[,2]~drel[,1])

plot(log(dabs[,2])~log(dabs[,1]))
reg1 = lm(log(dabs[,2]) ~ log(dabs[,1]))
summary(reg1)
plot(reg1)


reg2 = lm(log(dabs[,2]) ~ 1/dabs[,1]) # sigmoid
summary(reg1)
plot(reg1)

coeff = reg1$coefficients

gglog <- ggplot(data = as.data.frame(dabs), aes(x = log(dabs[,1]), y = log(dabs[,2])))+
  geom_point()+labs(title = "Scatterplot of log of Expenditure on Edible Oils \n and Food vs log of Total Expenditure", 
                    x = "log of total Expenditure",y = "log of total Expenditure on Edible Oils and Food")

gglog + geom_abline(intercept = coeff[1] , slope = coeff[2] , color="red",  
                    linetype="solid", size=1.5)

engelcurve <- function(x){
  return(exp(coeff[1])*(x^(coeff[2])))
}

ggabs <- ggplot(data = as.data.frame(dabs), aes(x = (dabs[,1]), y = (dabs[,2]))) +
  geom_point() +
  stat_function(fun = engelcurve, col = "red" )+
  labs(title = "Engel Curve for Edible Oils and Food: Total Expenditure on \n Edible Oils and Food vs Total Expenditure",
       x = "Total Expenditure",
       y = "Total Expenditure on Edible Oils and Food")

ggrel <- ggplot(data = as.data.frame(drel), aes(x = (drel[,1]), y = (drel[,2]))) +
  geom_point() +
  labs(title = "Scatterplot of Relative Expenditure on Edible Oils and Food \n vs Total Expenditure",
       x = "Total Expenditure",
       y = "Relative Expenditure on Edible Oils and Food")

gglog
gglog + geom_abline(intercept = coeff[1] , slope = coeff[2] , color="blue",  
                    linetype="solid", size=1.5)
ggsave('food1.png')
ggabs
ggsave('food2.png')
ggrel
ggsave('food3.png')

####################################################################################

for(i in 1:length(hhd)){
  house = subset(data, data[,1] == hhd[i] )
  first = subset(house, house[,2] == 43)
  second = subset(house, house[,2] == 31)
  third = subset(house, house[,2] == 41)
  expen[i] = first[3]
  itemexp[i] = second[3]/third[3]
}

expen = expen/100

sum(as.numeric(is.na(expen)))
sum(as.numeric(is.na(itemexp)))

dabs = cbind(expen,itemexp)
dabs = data.frame(na.omit(dabs))
dabs = as.matrix(dabs)

plot(dabs[,2] ~ dabs[,1])

drel = dabs
drel[,2] = drel[,2]/drel[,1]
plot(drel[,2]~drel[,1])

plot(log(dabs[,2])~log(dabs[,1]))
reg1 = lm(log(dabs[,2]) ~ log(dabs[,1]))
summary(reg1)
plot(reg1)

coeff = reg1$coefficients

gglog <- ggplot(data = as.data.frame(dabs), aes(x = log(dabs[,1]), y = log(dabs[,2])))+
  geom_point()+labs(title = "Scatterplot of log of Expenditure on Fuel, Entertainment etc \n vs log of Total Expenditure", 
                    x = "log of total Expenditure",y = "log of total Expenditure on Fuel, Entertainment etc")

gglog + geom_abline(intercept = coeff[1] , slope = coeff[2] , color="red",  
                    linetype="solid", size=1.5)

engelcurve <- function(x){
  return(exp(coeff[1])*(x^(coeff[2])))
}

ggabs <- ggplot(data = as.data.frame(dabs), aes(x = (dabs[,1]), y = (dabs[,2]))) +
  geom_point() +
  stat_function(fun = engelcurve, col = "red" )+
  labs(title = "Engel Curve for Fuel, Entertainment etc: Total Expenditure on Fuel,\n Entertainment etc vs Total Expenditure",
       x = "Total Expenditure",
       y = "Total Expenditure on Fuel, Entertainment etc")

ggrel <- ggplot(data = as.data.frame(drel), aes(x = (drel[,1]), y = (drel[,2]))) +
  geom_point() +
  labs(title = "Scatterplot of Relative Expenditure on Fuel, Entertainment etc \n vs Total Expenditure",
       x = "Total Expenditure",
       y = "Relative Expenditure on Fuel, Entertainment etc")

gglog
gglog + geom_abline(intercept = coeff[1] , slope = coeff[2] , color="blue",  
                    linetype="solid", size=1.5)
ggsave('fuel1.png')
ggabs
ggsave('fuel2.png')
ggrel
ggsave('fuel3.png')


####################################################################################

for(i in 1:length(hhd)){
  house = subset(data, data[,1] == hhd[i] )
  first = subset(house, house[,2] == 43)
  second = subset(house, house[,2] == 39)
  third = subset(house, house[,2] == 41)
  expen[i] = first[3]
  itemexp[i] = second[3]/third[3]
}

expen = expen/100

sum(as.numeric(is.na(expen)))
sum(as.numeric(is.na(itemexp)))

dabs = cbind(expen,itemexp)
dabs = data.frame(na.omit(dabs))
dabs = as.matrix(dabs)

plot(dabs[,2] ~ dabs[,1])

drel = dabs
drel[,2] = drel[,2]/drel[,1]
plot(drel[,2]~drel[,1])

plot(log(dabs[,2])~log(dabs[,1]))
reg1 = lm(log(dabs[,2]) ~ log(dabs[,1]))
summary(reg1)
plot(reg1)

coeff = reg1$coefficients

gglog <- ggplot(data = as.data.frame(dabs), aes(x = log(dabs[,1]), y = log(dabs[,2])))+
  geom_point()+labs(title = "Scatterplot of log of Expenditure on  Medical, \n Education and other Durable Goods vs log of Total Expenditure", 
                    x = "log of total Expenditure",y = "log of total Expenditure on Medical, Education and other Durable Goods")

gglog + geom_abline(intercept = coeff[1] , slope = coeff[2] , color="red",  
                    linetype="solid", size=1.5)

engelcurve <- function(x){
  return(exp(coeff[1])*(x^(coeff[2])))
}

ggabs <- ggplot(data = as.data.frame(dabs), aes(x = (dabs[,1]), y = (dabs[,2]))) +
  geom_point() +
  stat_function(fun = engelcurve, col = "red" )+
  labs(title = "Engel Curve for Medical, Education and other Durable Goods \n  Total Expenditure on Medical, Education and other Durable Goods vs Total Expenditure",
       x = "Total Expenditure",
       y = "Total Expenditure on Medical, Education and other Durable Goods")

ggrel <- ggplot(data = as.data.frame(drel), aes(x = (drel[,1]), y = (drel[,2]))) +
  geom_point() +
  labs(title = "Scatterplot of Relative Expenditure on Medical, Education \n and other Durable Goods vs Total Expenditure",
       x = "Total Expenditure",
       y = "Relative Expenditure on Medical, Education and other Durable Goods")

gglog
gglog + geom_abline(intercept = coeff[1] , slope = coeff[2] , color="blue",  
                    linetype="solid", size=1.5)
ggsave('med1.png')
ggabs
ggsave('med2.png')
ggrel
ggsave('med3.png')

