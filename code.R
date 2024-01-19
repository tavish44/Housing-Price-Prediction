library(dplyr)
library(Metrics)
library(car)
library(ggplot2)
library(visdat)
library(corrplot)
library(Amelia)


train = read.csv(r"(C:\Users\tavis\Documents\Housing Price Prediction\housing_train.csv)", stringsAsFactors = F)
test = read.csv(r"(C:\Users\tavis\Documents\Housing Price Prediction\housing_test.csv)", stringsAsFactors = F)



#Imputing NA values and Cleaning the data:

#Train:
apply(train,2,function(x)sum(is.na(x)))

train$Bedroom2[is.na(train$Bedroom2)]=median(train$Bedroom2,na.rm=T)

train$Bathroom[is.na(train$Bathroom)]=round(mean(train$Bathroom,na.rm=T),0)

train$Car[is.na(train$Car)]=round(mean(train$Car,na.rm=T),0)

train$Landsize[is.na(train$Landsize)]=round(mean(train$Landsize,na.rm=T),0)

train$BuildingArea[is.na(train$BuildingArea)]=round(mean(train$BuildingArea,na.rm=T),0)

train$YearBuilt[is.na(train$YearBuilt)]=round(mean(train$YearBuilt,na.rm=T),0)

apply(train,2,function(x)sum(is.na(x)))


#Test:
apply(test,2,function(x)sum(is.na(x)))

test$Bedroom2[is.na(test$Bedroom2)]=median(test$Bedroom2,na.rm=T)

test$Bathroom[is.na(test$Bathroom)]=round(mean(test$Bathroom,na.rm=T),0)

test$Car[is.na(test$Car)]=round(mean(test$Car,na.rm=T),0)

test$Landsize[is.na(test$Landsize)]=round(mean(test$Landsize,na.rm=T),0)

test$BuildingArea[is.na(test$BuildingArea)]=round(mean(test$BuildingArea,na.rm=T),0)

test$YearBuilt[is.na(test$YearBuilt)]=round(median(test$YearBuilt,na.rm=T),0)


missmap(train,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE) ##% of data missing
vis_dat(train) ##type of data missing



numeric_columns <- sapply(train, is.numeric)
numeric_train <- train[, numeric_columns]

corrplot(cor(numeric_train)) ##Check co-relation b/w numeric columns



#Data Preparation:
#Combining both train n test datasets prior to data preparation.
test$Price=NA
train$data='train'
test$data='test'
all_data=rbind(train,test)



glimpse(all_data)


#Dummy variables for Suburb

t=table(all_data$Suburb)
View(t)
t1=round(tapply(all_data$Price,all_data$Suburb,mean,na.rm=T),0)
View(t1)
t1=sort(t1)

all_data=all_data %>% 
  mutate(
    sub_1=as.numeric(Suburb%in%c("Campbellfield","Jacana")),
    sub_2=as.numeric(Suburb%in%c("Kealba","Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner")),
    sub_3=as.numeric(Suburb%in%c("Glenroy","Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
    sub_4=as.numeric(Suburb%in%c("Airport West","Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
    sub_5=as.numeric(Suburb%in%c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
    sub_6=as.numeric(Suburb%in%c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
    sub_7=as.numeric(Suburb%in%c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
    sub_8=as.numeric(Suburb%in%c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
    sub_9=as.numeric(Suburb%in%c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
    sub_10=as.numeric(Suburb%in%c("Williamstown","East Melbourne","Seaholme")),
    sub_11=as.numeric(Suburb%in%c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
    sub_12=as.numeric(Suburb%in%c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
    sub_13=as.numeric(Suburb%in%c("Brighton East","Eaglemont","Hampton")),
    sub_14=as.numeric(Suburb%in%c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
    sub_15=as.numeric(Suburb%in%c("Brighton","Middle Park")),
    sub_16=as.numeric(Suburb%in%c("Albert Park","Balwyn","Malvern"))
  ) %>% 
  select(-Suburb)

glimpse(all_data)


#Deleting variable address as it is unique.

all_data=all_data %>% 
  select(-Address)

#Making dummies for variable type.

glimpse(all_data)
table(all_data$Type)
all_data=all_data %>%
  mutate(Type_t=as.numeric(Type=="t"),
         type_u=as.numeric(Type=="u"))
all_data=all_data %>% 
  select(-Type)


#Making dummies for Method.

glimpse(all_data)  
table(all_data$Method)
all_data=all_data %>%
  mutate(Method_PI=as.numeric(Method=="PI"),
         Method_SA=as.numeric(Method=="SA"),
         Method_SP=as.numeric(Method=="SP"),
         Method_VB=as.numeric(Method=="VB")) %>% 
  select(-Method)


##Making dummies for varible SellerG

glimpse(all_data)
t=table(all_data$SellerG)
View(sort(t))
View(t)
all_data=all_data %>%
  mutate(Gnelson=as.numeric(SellerG=="Nelson"),
         GJellis=as.numeric(SellerG=="Jellis"),
         Ghstuart=as.numeric(SellerG=="hockingstuart"),
         Gbarry=as.numeric(SellerG=="Barry"),
         GMarshall=as.numeric(SellerG=="Marshall"),
         GWoodards=as.numeric(SellerG=="Woodards"),
         GBrad=as.numeric(SellerG=="Brad"),
         GBiggin=as.numeric(SellerG=="Biggin"),
         GRay=as.numeric(SellerG=="Ray"),
         GFletchers=as.numeric(SellerG=="Fletchers"),
         GRT=as.numeric(SellerG=="RT"),
         GSweeney=as.numeric(SellerG=="Sweeney"),
         GGreg=as.numeric(SellerG=="Greg"),
         GNoel=as.numeric(SellerG=="Noel"),
         GGary=as.numeric(SellerG=="Gary"),
         GJas=as.numeric(SellerG=="Jas"),
         GMiles=as.numeric(SellerG=="Miles"),
         GMcGrath=as.numeric(SellerG=="McGrath"),
         GHodges=as.numeric(SellerG=="Hodges"),
         GKay=as.numeric(SellerG=="Kay"),
         GStockdale=as.numeric(SellerG=="Stockdale"),
         GLove=as.numeric(SellerG=="Love"),
         GDouglas=as.numeric(SellerG=="Douglas"),
         GWilliams=as.numeric(SellerG=="Williams"),
         GVillage=as.numeric(SellerG=="Village"),
         GRaine=as.numeric(SellerG=="Raine"),
         GRendina=as.numeric(SellerG=="Rendina"),
         GChisholm=as.numeric(SellerG=="Chisholm"),
         GCollins=as.numeric(SellerG=="Collins"),
         GLITTLE=as.numeric(SellerG=="LITTLE"),
         GNick=as.numeric(SellerG=="Nick"),
         GHarcourts=as.numeric(SellerG=="Harcourts"),
         GCayzer=as.numeric(SellerG=="Cayzer"),
         GMoonee=as.numeric(SellerG=="Moonee"),
         GYPA=as.numeric(SellerG=="YPA")
  ) %>% 
  select(-SellerG)

#Making dummies for variable CouncilArea.
glimpse(all_data)

table(all_data$CouncilArea)

all_data=all_data %>%
  mutate(CA_Banyule=as.numeric(CouncilArea=="Banyule"),
         CA_Bayside=as.numeric(CouncilArea=="Bayside"),
         CA_Boroondara=as.numeric(CouncilArea=="Boroondara"),
         CA_Brimbank=as.numeric(CouncilArea=="Brimbank"),
         CA_Darebin=as.numeric(CouncilArea=="Darebin"),
         CA_Glen_Eira=as.numeric(CouncilArea=="Glen Eira"),
         CA_Monash=as.numeric(CouncilArea=="Monash"),
         CA_Melbourne=as.numeric(CouncilArea=="Melbourne"),
         CA_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
         CA_Manningham=as.numeric(CouncilArea=="Manningham"),
         CA_Kingston=as.numeric(CouncilArea=="Kingston"),
         CA_Hume=as.numeric(CouncilArea=="Hume"),
         CA_HobsonsB=as.numeric(CouncilArea=="Hobsons Bay"),
         CA_MoonValley=as.numeric(CouncilArea=="Moonee Valley"),
         CA_Moreland=as.numeric(CouncilArea=="Moreland"),
         CA_PortP=as.numeric(CouncilArea=="Port Phillip"),
         CA_Stonnington=as.numeric(CouncilArea=="Stonnington"),
         CA_Whitehorse=as.numeric(CouncilArea=="Whitehorse"),
         CA_Yarra=as.numeric(CouncilArea=="Yarra")) %>% 
  select(-CouncilArea)

#Data preparation Complete

View(all_data)

numeric_columns <- sapply(all_data, is.numeric)
numeric_df <- all_data[, numeric_columns]

corrplot(cor(numeric_df))


#Separating test and train:
train=all_data %>% 
  filter(data=='train') %>% 
  select(-data)


test=all_data %>% 
  filter(data=='test') %>% 
  select(-data,-Price)



#Divide the train dataset 80:20.
set.seed(123)
s=sample(1:nrow(train),0.80*nrow(train))
train_80=train[s,] 
test_20=train[-s,]  


#Model Building
fit=lm(Price ~ .,data=train_80)
summary(fit)

#CALCULATING VIF
vif_value = vif(fit)
sort(vif_value,decreasing = T)[1:3]


fit=lm(Price ~ .-Postcode-sub_3,data=train_80)
summary(fit)
vif_value=vif(fit)
sort(vif_value,decreasing = T)[1:3]
summary(fit)

best_linear_model <- step(fit,direction = "both",trace = T)


#Now removing all variables whose p value is >0.05 one by one.

fit=lm(Price ~ .-Landsize-GRaine-GMoonee-CA_Bayside-GLITTLE-Gnelson-GSweeney-Ghstuart-CA_Kingston-Gbarry-GRay-GStockdale-GNoel-GJas-GBiggin-GYPA-CA_PortP-CA_Whitehorse-GRendina-GFletchers-GBrad-GHodges-GVillage-GLove-sub_4-GGary-CA_Hume-CA_Boroondara-Method_SA-GWilliams-GHarcourts-GNick-GGreg-CA_Monash-GWoodards-CA_Stonnington-GCayzer-Postcode-sub_3,data=train_80)
summary(fit)

#LR Model Complete

#Test on splitted dataset and calculating RMSE

final_test_20=predict(fit,newdata =test_20)
final_test_20=round(final_test_20,1)
class(final_test_20)


final_train = predict(fit, newdata = train_80)

#Calculating the RMSE and Plotting the graph.

#Real price vs Predicted price (test_20):
plot(test_20$Price,final_test_20)


res=test_20$Price-final_test_20 #(real value-predicted value)

res2 = train_80$Price - final_train
RMSE_train=sqrt(mean(res2^2))

#root mean square error is as follows
RMSE_test_20=sqrt(mean(res^2))
RMSE_test_20


div = (RMSE_train - RMSE_test_20)/RMSE_train * 100

#the passing criteria mentioned,was to have value >0.5 which we have successfuly crossed.Hence our model is good.
212467/RMSE_test_20

#Diagonostic plots for linearity:
d=data.frame(real=test_20$Price,predicted=final_test_20)
ggplot(d,aes(x=real,y=predicted))+geom_point()

?plot
plot(fit,which = 1) #gives residual vs fitted plot (For Linearity)

plot(fit,which = 2) #gives q-q-plot (To Check Normailty)

plot(fit,which = 3) #gives scale-location plot

plot(fit,which = 4) #gives cooks distance (For Outliers)

#Predicting Real Estate Prices for the final Test Dataset.

final_test=predict(fit,newdata =test)
final_test=round(final_test,1)
summary(final_test)
write.csv(final_test, "TavishPagore_P1_part2.csv", row.names = F)




#Question 1: What is the difference in average price between house type h and t?
avg_price_h <- mean(train[train$Type == 'h', 'Price'], na.rm = TRUE)
avg_price_t <- mean(train[train$Type == 't', 'Price'], na.rm = TRUE)

difference <- avg_price_h - avg_price_t


#Question 2: How many unique values variable postcode takes?
length(unique(train$Postcode))


#Question 3: Which seller has maximum value transactions?
seller_sum <- aggregate(Price ~ SellerG, data = train, sum, na.rm = TRUE)
max_seller <- seller_sum[which.max(seller_sum$Price), ]

class(train$Postcode)


#Question 4: which CouncilArea has maximum variance in the price?
variance_by_council <- aggregate(Price ~ CouncilArea, data = train, var, na.rm = TRUE)
max_variance_council <- variance_by_council[which.max(variance_by_council$Price), ]


#Question 5: Which CouncilArea has maximum average price?
average_price_by_council <- aggregate(Price ~ CouncilArea, data = train, mean, na.rm = TRUE)
max_average_council <- average_price_by_council[which.max(average_price_by_council$Price), ]


#Quesion 6: Find out how many observations have missing values for variable 'YearBuilt'?
length(train$YearBuilt) - sum(is.na(train$YearBuilt))


#Question 7: Find the variance of the target variable 'Price'.
var(train$Price)
