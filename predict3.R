library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(data.table)
library(Metrics)
library(randomForest)
library(caTools)
library(DescTools)
#library()
na_list<-c("","\\?")
Test <- read_csv("/cloud/project/Test.csv", na = na_list)
Train <-read_csv("/cloud/project/Train.csv", na = na_list)

#unique(Train$Item_Type)
#unique(Train$Item_Fat_Content)
#unique(Train$Outlet_Size)
#unique(Train$Outlet_Location_Type)
#unique(Train$Outlet_Type)
#unique(Test$Item_Type)
#unique(Test$Item_Fat_Content)
###unique(Test$Outlet_Size)
#unique(Test$Outlet_Location_Type)
#unique(Test$Outlet_Type)


# Let's change all these
Train <- Train %>%
  mutate(Item_Fat_Content = case_when(
    Item_Fat_Content == "Low Fat" ~ "Low Fat",
    Item_Fat_Content == "low fat" ~ "Low Fat",
    Item_Fat_Content == "LF" ~ "Low Fat",
    Item_Fat_Content == "Regular" ~ "Regular",
    Item_Fat_Content == "reg" ~ "Regular"
  ))

Test <- Test %>%
  mutate(Item_Fat_Content = case_when(
    Item_Fat_Content == "Low Fat" ~ "Low Fat",
    Item_Fat_Content == "low fat" ~ "Low Fat",
    Item_Fat_Content == "LF" ~ "Low Fat",
    Item_Fat_Content == "Regular" ~ "Regular",
    Item_Fat_Content == "reg" ~ "Regular"
  ))

Train$Item_Type <- as.factor(Train$Item_Type)
Train$Item_Fat_Content <- as.factor(Train$Item_Fat_Content)
Train$Outlet_Size <- as.factor(Train$Outlet_Size)
Train$Outlet_Location_Type <- as.factor(Train$Outlet_Location_Type)
Train$Outlet_Type <- as.factor(Train$Outlet_Type)
Train$Outlet_Identifier <- as.factor(Train$Outlet_Identifier)

Test$Item_Type <- as.factor(Test$Item_Type)
Test$Item_Fat_Content <- as.factor(Test$Item_Fat_Content)
Test$Outlet_Size <- as.factor(Test$Outlet_Size)
Test$Outlet_Location_Type <- as.factor(Test$Outlet_Location_Type)
Test$Outlet_Type <- as.factor(Test$Outlet_Type)
Test$Outlet_Identifier <- as.factor(Test$Outlet_Identifier)
glimpse(Test)

Train <- as.data.frame(Train)
sample_3 <- sample.split(Y = Train, SplitRatio = 0.70)
Training <- subset(Train, sample_3 == T)
Testing <- subset(Train, sample_3 == F)

Training <- Training %>%
  dplyr::select(-c(Item_Weight, Outlet_Size))

Testing <- Testing %>%
  dplyr::select(-c(Item_Weight, Outlet_Size ))

# Imputing variables

#Impute the outlet type with the mode
Train%>%
  group_by(Outlet_Size, Outlet_Type, Outlet_Location_Type)

Train<-Train%>%
  mutate(Outlet_Size = case_when(
    Outlet_Type == "Grocery Store" ~ "Small",
    Outlet_Type == "Supermarket Type1"& Outlet_Location_Type == "Tier 2"  ~ "Small",
    Outlet_Type == "Supermarket Type1"& Outlet_Location_Type == "Tier 1" ~ "Small",
    TRUE ~ as.character(Outlet_Size)
  ))
Test<-Test%>%
  mutate(Outlet_Size = case_when(
    Outlet_Type == "Grocery Store" ~ "Small",
    Outlet_Type == "Supermarket Type1"& Outlet_Location_Type == "Tier 2"  ~ "Small",
    Outlet_Type == "Supermarket Type1"& Outlet_Location_Type == "Tier 1" ~ "Small",
    TRUE ~ as.character(Outlet_Size)
  ))
Test %>%
  group_by(Outlet_Size, Outlet_Type)%>%
  summarize(n())
Train%>%
  group_by(Outlet_Size,Outlet_Type)%>%
  summarize(n())

### Imputing visibility with visibility for avg vis for "Item Type"
ggplot(Train, aes(x = Item_Visibility))+
  geom_histogram()

Train <-Train%>%
  mutate(Item_Visibility = case_when(
    Item_Visibility == 0 ~ mean(Item_Visibility),
    TRUE ~ as.numeric(Item_Visibility)
  )) 
Test <- Test %>%
  mutate(Item_Visibility = case_when(
    Item_Visibility == 0 ~ mean(Item_Visibility),
    TRUE ~ as.numeric(Item_Visibility)))


ggplot(Train_2, aes(x = Item_Visibility))+
  geom_histogram()

####################################################
Forest1_Training <- randomForest(Item_Outlet_Sales ~
                                   Item_Fat_Content + 
                                   Item_Visibility + Item_Type + 
                                   Item_MRP + Outlet_Establishment_Year +
                                   Outlet_Location_Type + Outlet_Type,
                                 data = Train,
                                 ntree = 300, importance = T)


Predict_1Testing <- predict(Forest1_Training, newdata = Test)



Predict_1Testing <- data.frame(Predict_1Testing)

Predict_1Testing<-cbind(Test$Item_Identifier, Test$Outlet_Identifier, Predict_1Testing)

Predict_1Testing <- Predict_1Testing %>%
  mutate(Item_Identifier = `Test$Item_Identifier`,
         Outlet_Identifier = `Test$Outlet_Identifier`,
         Item_Outlet_Sales = Predict_1Testing)

Predict_1Testing  <- Predict_1Testing %>% 
  dplyr::select(Item_Identifier, Outlet_Identifier, Item_Outlet_Sales)


write_csv(Predict_1Testing, path = '/cloud/project/Predict_3Train.csv')

write_csv(Train, path = '/cloud/project/Train_Clean.csv')





