setwd("C:\\Personal\\study\\DataScience\\Hackathons\\PredictUsedCarPrice\\Participants_Data_Used_Cars")
library(readxl)
Data_Train = read_excel("Data_Train.xlsx")
Data_Test_Clean = read_excel("Data_Test.xlsx")
#str(Data_Train)
Data_Train_Clean = Data_Train
View(Data_Train_Clean)
#--------------------------------------------------------------------------------------
#Variable 1 - Name
#This is not significant for our model

#--------------------------------------------------------------------------------------
#Variable 2 - Location
Data_Train_Clean$Location = as.factor(Data_Train_Clean$Location)
Data_Test_Clean$Location = as.factor(Data_Test_Clean$Location)

#--------------------------------------------------------------------------------------
#Variable 3 - Year
sum(is.na(Data_Train_Clean$Year))#there are no NA's
Data_Train_Clean$Year = 2019 - Data_Train_Clean$Year
Data_Test_Clean$Year = 2019 - Data_Test_Clean$Year
class(Data_Train_Clean$Year)
table(Data_Train_Clean$Year)

#--------------------------------------------------------------------------------------
#Variable 4 - Kilometers_Driven
sum(is.na(Data_Train_Clean$Kilometers_Driven)) #there are no NA's
sum(is.na(Data_Test_Clean$Kilometers_Driven)) #there are no NA's

#--------------------------------------------------------------------------------------
#Variable 5 - Fuel_Type
Data_Train_Clean$Fuel_Type = as.factor(Data_Train_Clean$Fuel_Type)
Data_Test_Clean$Fuel_Type = as.factor(Data_Test_Clean$Fuel_Type)
levels(Data_Train_Clean$Fuel_Type)
table(Data_Train_Clean$Fuel_Type)

table(Data_Test_Clean$Fuel_Type)

#Data_Train_Clean = Data_Train_Clean[Data_Train_Clean$Fuel_Type!="Electric",]

#--------------------------------------------------------------------------------------
#Variable 6 - Transmission
Data_Train_Clean$Transmission = as.factor(Data_Train_Clean$Transmission)
Data_Test_Clean$Transmission = as.factor(Data_Test_Clean$Transmission)
table(Data_Test_Clean$Transmission)
class(Data_Train_Clean$Transmission)

#--------------------------------------------------------------------------------------
#Variable 7 - Owner_Type
Data_Train_Clean$Owner_Type = as.factor(Data_Train_Clean$Owner_Type)
Data_Test_Clean$Owner_Type = as.factor(Data_Test_Clean$Owner_Type)
table(Data_Train_Clean$Owner_Type)
class(Data_Train_Clean$Owner_Type)

#--------------------------------------------------------------------------------------
#Variable 8 - Mileage
#library(sqldf)
#sqldf("select CASE WHEN Mileage LIKE '%kmpl%' THEN Mileage ELSE 'abc' END AS hide from Data_Train_Clean")
str(Data_Train_Clean$Mileage)
#--------------------------------------------------------------------------------------
#Variable 9 - Engine
Data_Train_Clean$Engine[is.na(Data_Train_Clean$Engine)] = 1197 #replace NA with mode
Data_Test_Clean$Engine[is.na(Data_Test_Clean$Engine)] = 1197 #replace NA with mode
sum(is.na(Data_Train_Clean$Engine))
sum(is.na(Data_Test_Clean$Engine))
#max(table(Data_Test_Clean$Engine))
#max(table(Data_Train_Clean$Engine))
#median(Data_Train_Clean$Engine)
#--------------------------------------------------------------------------------------
#Variable 10 - Power
str(Data_Train_Clean$Power)
sum(is.na(Data_Train_Clean$Power)) #find out number of NA's
Data_Train_Clean$Power = as.double(Data_Train_Clean$Power)
Data_Train_Clean$Power[Data_Train_Clean$Power==0] #check if there are any values with 0 value
Data_Train_Clean$Power[is.na(Data_Train_Clean$Power)] = 0 #replace NS's by 0
mean(Data_Train_Clean$Power) #110.56
Data_Train_Clean$Power[Data_Train_Clean$Power==0] = 110.56 #replace Ns's with mean of power

sum(is.na(Data_Test_Clean$Power)) #find out number of NA's
Data_Test_Clean$Power = as.double(Data_Test_Clean$Power)
Data_Test_Clean$Power[Data_Test_Clean$Power==0] #check if there are any values with 0 value
Data_Test_Clean$Power[is.na(Data_Test_Clean$Power)] = 0
mean(Data_Test_Clean$Power) #107.518
Data_Test_Clean$Power[Data_Test_Clean$Power==0] = 107.51 #replace Ns's with mean of power

#--------------------------------------------------------------------------------------
#Variable 11 - Seats
sum(is.na(Data_Train_Clean$Seats)) #there are 42 NA's
table(Data_Train_Clean$Seats) #there are huge number of cars with 5 seats
hist(Data_Train_Clean$Seats)
#let's replace NA's with 5 as it will not impact too much
Data_Train_Clean$Seats[is.na(Data_Train_Clean$Seats)] = 5

sum(is.na(Data_Test_Clean$Seats))
table(Data_Test_Clean$Seats)
Data_Test_Clean$Seats[is.na(Data_Test_Clean$Seats)] = 5

#--------------------------------------------------------------------------------------
#Variable 12 - new price
summary(Data_Train_Clean$New_Price)
sum(is.na(Data_Train_Clean$New_Price)) #5195 entries with blank
#this variable is of no use in model

summary(Data_Test_Clean$New_Price)
sum(is.na(Data_Test_Clean$New_Price))

#--------------------------------------------------------------------------------------
#Variable 12 - price
sum(is.na(Data_Train_Clean$Price))
str(Data_Train_Clean$Price)

#--------------------------------------------------------------------------------------
#Remove outliers
plot(Data_Train_Clean$Kilometers_Driven[Data_Train_Clean$Kilometers_Driven<800000],Data_Train_Clean$Price[Data_Train_Clean$Kilometers_Driven<800000])
#there is only one entry having kms driven as more than 10,00,000, let's remove that
Data_Train_Clean = Data_Train_Clean[which(Data_Train_Clean$Kilometers_Driven<800000),]
plot(Data_Train_Clean$Mileage,Data_Train_Clean$Price)
nrow( Data_Train_Clean[which(Data_Train_Clean$Mileage==0),]) #68 entries with mileage as 0
#0 mileage seems to be unrealistic, there must be some sort of data entry issue
nrow( Data_Train_Clean[which(Data_Train_Clean$Mileage>0),])
Data_Train_Clean = Data_Train_Clean[which(Data_Train_Clean$Mileage>0),]#remove rows having mileage =0


#-------------------- Modelling NN -----------------------------------\
#For NN, we require all variables to be numeric. 
#Let's convert all categorical variables into dummy

Location_matrix = model.matrix(~Data_Train_Clean$Location-1,data = Data_Train_Clean)
Data_Train_Clean = data.frame(Data_Train_Clean,Location_matrix)
Location_matrix1 = model.matrix(~Data_Test_Clean$Location-1,data = Data_Test_Clean)
Data_Test_Clean = data.frame(Data_Test_Clean,Location_matrix1)
#View(Data_Test_Clean)

Fuel_Type_matrix = model.matrix(~Data_Train_Clean$Fuel_Type-1,data = Data_Train_Clean)
Data_Train_Clean = data.frame(Data_Train_Clean,Fuel_Type_matrix)
Fuel_Type_matrix1 = model.matrix(~Data_Test_Clean$Fuel_Type-1,data = Data_Test_Clean)
Data_Test_Clean = data.frame(Data_Test_Clean,Fuel_Type_matrix1)

Transmission_matrix = model.matrix(~Data_Train_Clean$Transmission-1,data = Data_Train_Clean)
Data_Train_Clean = data.frame(Data_Train_Clean,Transmission_matrix)
Transmission_matrix1 = model.matrix(~Data_Test_Clean$Transmission-1,data = Data_Test_Clean)
Data_Test_Clean = data.frame(Data_Test_Clean,Transmission_matrix1)

OwnerType_matrix = model.matrix(~Data_Train_Clean$Owner_Type-1,data = Data_Train_Clean)
Data_Train_Clean = data.frame(Data_Train_Clean,OwnerType_matrix)
OwnerType_matrix1 = model.matrix(~Data_Test_Clean$Owner_Type-1,data = Data_Test_Clean)
Data_Test_Clean = data.frame(Data_Test_Clean,OwnerType_matrix1)

View(Data_Train_Clean)
View(Data_Test_Clean)
#----------------------------------------------------------------------------------------------------
#FOR Train DATA

x = subset(Data_Train_Clean, 
           select = c("Year", "Kilometers_Driven", "Mileage", "Engine", "Power", "Seats"))

x.scaled = scale(x)
View(x.scaled)

x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.LocationBangalore"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.LocationChennai"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.LocationCoimbatore"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.LocationDelhi"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.LocationHyderabad"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.LocationJaipur"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.LocationKochi"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.LocationKolkata"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.LocationMumbai"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.LocationPune"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.Fuel_TypeCNG"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.Fuel_TypeDiesel"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.Fuel_TypeElectric"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.Fuel_TypeLPG"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.Fuel_TypePetrol"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.TransmissionAutomatic"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.TransmissionManual"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.Owner_TypeFirst"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.Owner_TypeFourth...Above"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.Owner_TypeSecond"])
x.scaled = cbind(x.scaled,Data_Train_Clean["Data_Train_Clean.Owner_TypeThird"])

x.scaled = cbind(x.scaled,Data_Train_Clean["Price"])

View(x.scaled)

library("neuralnet")
neural_net <- neuralnet(formula = Price ~ ., 
                        data = x.scaled, 
                        hidden = 3, #specify number of neurons
                        err.fct = "sse", #setting "sum of squared errors" as cost function 
                        linear.output = TRUE, #we want continuous prediction, set FALSE for binary
                        lifesign = "full", #gives output of the actions
                        lifesign.step = 10, #at every 10th step, show output
                        threshold = 2, #first order differential of cost function
                        stepmax = 50000 #setting maximum number of iterations to compute optimal value
                        ##startweights = startweightsObj #using synaptic weights of previous NN iteration
)

neural_net$net.result[[1]]
library(rmse)
library(hydroGOF)
rmse(x.scaled$Price,as.double(neural_net$net.result[[1]]))

x.scaled$Predicted = as.double(neural_net$net.result[[1]])
View(x.scaled)

#----------------------------------------------------------------------------------------------------
#FOR TEST DATA

x.test = subset(Data_Test_Clean, 
           select = c("Year", "Kilometers_Driven", "Mileage", "Engine", "Power", "Seats"))

x.test.scaled = scale(x.test)
#View(x.test.scaled)
#View(Data_Test_Clean)

x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.LocationBangalore"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.LocationChennai"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.LocationCoimbatore"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.LocationDelhi"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.LocationHyderabad"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.LocationJaipur"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.LocationKochi"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.LocationKolkata"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.LocationMumbai"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.LocationPune"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.Fuel_TypeCNG"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.Fuel_TypeDiesel"])
#x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.Fuel_TypeElectric"])
x.test.scaled$Data_Test_Clean.Fuel_TypeElectric = 0
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.Fuel_TypeLPG"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.Fuel_TypePetrol"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.TransmissionAutomatic"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.TransmissionManual"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.Owner_TypeFirst"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.Owner_TypeFourth...Above"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.Owner_TypeSecond"])
x.test.scaled = cbind(x.test.scaled,Data_Test_Clean["Data_Test_Clean.Owner_TypeThird"])



View(x.test.scaled )
str(x.test.scaled)

compute.output = compute(neural_net, x.test.scaled)
#compute.output$net.result
#View(compute.output$net.result)

write.csv(compute.output$net.result,"my_output.csv")
sum(is.na(compute.output$net.result))
sum(compute.output$net.result==0)

View(x.test.scaled[24,])
