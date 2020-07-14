                      # RELIANCE MART SALES PREDICTION
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Importing the neccesary libraries

library(dplyr)

library(ggplot2)

library(caret)

library(tidyverse)

# Importing of data and understanding about it
df1 <- read.csv(file.choose())#train data

#Item_Outlet_sales is a response variable.
data1 = df1

dim(data1)#to see dimesions of the data.
#So there are about 8523 observations and 12 variables
head(data1)

class(data1)# to see the type of data

str(data1)# to see the data type of a variable in a dataframe
names(data1)
#Categorical variables are :
#1)Item_identifier 2) Item_Fat_content(with 2 levels)
#3)Item_type(with 16 levels) 4) Outlet_identifier(10 levels)
#4)Outlet_size(3 levels) 5)Outlet_location_type(3levels) 6)Outlet_Type(4 levels)

#Numerical variables:Item_outlet_sales(response variable), Item_mrp,
#item_wieght, item_visibility and outlet_establishment_year

summary(data1)# to see the summary of the data
names(data1)
#visualising the fat_content variable.
ggplot(data = data1, aes(data1$Item_Fat_Content, data1$Item_Outlet_Sales))+
  geom_bar(stat = 'identity')+geom_text(aes(label=Item_Fat_Content),vjust= -0.3,size=5)
#Data-preprocessing:
# here low fat , lf and LF are same; reg and regular are same
data1$Item_Fat_Content = ifelse(data1$Item_Fat_Content=='LF' | data1$Item_Fat_Content=='low fat','Low Fat',
                                ifelse(data1$Item_Fat_Content=='reg' | data1$Item_Fat_Content=='Regular','Regular','Low Fat'))
names(data1)
levels(as.factor(data1$Item_Fat_Content))

#Outlet_size variable
levels(as.factor(data1$Outlet_Size))
summary(as.factor(data1$Outlet_Size))
data1$Outlet_Size = ifelse(data1$Outlet_Size=='Small','Small',ifelse(data1$Outlet_Size=='High','High', 'Medium'))
summary(as.factor(data1$Outlet_Size))

#checking all the categorical variables level
levels(as.factor(data1$Item_Fat_Content))
levels(as.factor(data1$Outlet_Size))
summary(as.factor(data1$Item_Type))
summary(as.factor(data1$Outlet_Identifier))
levels(as.factor(data1$Outlet_Location_Type))
summary(as.factor(data1$Outlet_Type))

# We can remove the Item_identifier variable as it is no use for visualisation.
#But can be used for feature Engineering place 
names(data1)
data1 <- data1[, -c(1)]
library(lattice)
#Histogram of all continous variables.
trellis.device()
par(mfrow=c(2,2))
hist(data1$Item_Visibility,main="Item Visibility",col="Yellow",xlab = "Visiblity")
hist(data1$Item_Weight,main="Item Weight",col="Yellow",xlab = "Weight")
hist(data1$Item_MRP,main="Item MRP",col="Yellow",xlab = "MRP")
hist(data1$Item_Outlet_Sales,main="Item Output Sales",col="Yellow",xlab = "Outlet Sales")
names(data1)
#Correlation plot
corp <- data1[,c(1,3,5,11)]
cor(corp)
library(corrplot)
corrplot(corr=cor(corp),method="ellipse")
library(car)
scatterplotMatrix(~data1$Item_Visibility+data1$Item_Weight+data1$Item_MRP+data1$Item_Outlet_Sales,col="Red")

#Missing value check
sum(is.na(data1)) # so there are totally 1463 missing values
miss <- function(x){sum(is.na(x))}
apply(data1,2,miss)/nrow(data1)
#Thus item_weight contains 17 percent of missing values
summary(data1$Item_Weight)

boxplot(data1$Item_Weight)$out #Thus there is no outlier so we can use mean imputation
newd <- data1 %>% mutate(Item_Weight=replace(Item_Weight, is.na(Item_Weight), round(mean(Item_Weight, na.rm = T),2)))
sum(is.na(newd))
names(newd)


#VISUALISATION AND EXPLORATORY DATA ANALYSIS
library(ggplot2)
library(lattice)
trellis.device()
#creating a scatter plot to understand the behaviour of the continous variables
plot(newd$Item_Visibility,newd$Item_Outlet_Sales,col='blue', main = 'scatter plot',xlab = 'visibility',ylab = 'sales')
#We can say that an item with zero Item_Visibility  has more sales which is not possible at all
# So we have to impute zero values of visibility with its mean
hist(newd$Item_Outlet_Sales)
#Univariate analysis
ggplot(newd,aes(Item_Visibility))+
  geom_histogram(bins = 100,binwidth=0.01,color='Black',fill='Sky Blue') +
  ylab('Count') +
  ggtitle("Item Visibility Count")
#The Item visibility is right skewed, so it has to be normally distributed

ggplot(newd,aes(Item_Weight)) +
  geom_histogram(bins = 100,color='Black',fill='Sky Blue') +
  ylab('Count') + 
  ggtitle("Item Weight Count") 
#The Item_weight is a uniformly distributed

ggplot(newd,aes(Item_MRP)) + 
  geom_histogram(bins = 100,color='Black',fill='Sky Blue') + 
  ylab('Count') + 
  ggtitle("Item MRP Count")
#The Item_MRP is also normally distributed

#SCATTER PLOT BETWEEN ITEM-WEIGH AND ITEM-SALES
ggplot(newd,aes(Item_Weight, Item_Outlet_Sales)) + 
  geom_point(color='yellow') +
  ggtitle("Item Weight Vs Sales") 
#The correlation between both the variables is good

#SCATTER PLOT BETWEEN MRP AND SALES
ggplot(newd,aes(Item_MRP, Item_Outlet_Sales)) + 
  geom_point(color='green') +
  ggtitle("Item MRP Vs Sales") 
#there is a slight increase in sales when mrp of the product increases

                              #BI-VARIATE ANALYSIS

#CATEGORICAL VARIABLES ANALYSIS
#Finding the means of sales for each categorical type 
#so that understanding of visualisation will be easy

#Fat_content by average sales
fat_content_sales <-  newd %>% group_by(Item_Fat_Content) %>% 
  summarise_at(vars(Item_Outlet_Sales), funs(Count = n(),Sales_Mean = mean))
fat_content_sales

#Item type by average sales
item_type_sales <-  newd %>% group_by(Item_Type) %>% 
  summarise_at(vars(Item_Outlet_Sales), funs(Count = n(),Sales_Mean = mean))
item_type_sales

#OUTLET-SIZE BY AVERAGE SALES
outletsize_bysales = newd %>% group_by(Outlet_Size) %>% 
  summarise_at(vars(Item_Outlet_Sales), funs(Count = n(),Sales_Mean = mean))
outletsize_bysales

# Outlet-Location-Type by average sales
outletlocation_bysales = newd %>% group_by(Outlet_Location_Type) %>% 
  summarise_at(vars(Item_Outlet_Sales), funs(Count = n(),Sales_Mean = mean))
outletlocation_bysales

# Outlet-Type by average sales
outlettype_bysales = newd %>% group_by(Outlet_Type) %>% 
  summarise_at(vars(Item_Outlet_Sales), funs(Count = n(),Sales_Mean = mean))
outlettype_bysales

# Outlet-Identifier by average sales
outletid_bysales = newd %>% group_by(Outlet_Identifier) %>% 
  summarise_at(vars(Item_Outlet_Sales), funs(Count = n(),Sales_Mean = mean))
outletid_bysales

# Outlet-Establishment-Year by average sales
outletyear_bysales = newd %>% group_by(Outlet_Establishment_Year) %>% 
  summarise_at(vars(Item_Outlet_Sales), funs(Count = n(),Sales_Mean = mean))
outletyear_bysales

#Visualisation of Categorical variables

#fat-content
ggplot(fat_content_sales, aes(Item_Fat_Content, Sales_Mean)) + 
  geom_bar(stat='summary', fun.y='mean',fill='slateblue1',color='black') + 
  geom_point() + 
  geom_text(aes(label = round(Sales_Mean)), vjust=-0.3, size=3.5) + 
  ggtitle("Item Fat Content Vs Sales")
#Mean-Sales for regular fat content is slightly more

#Item-type
ggplot(item_type_sales, aes(Item_Type, Sales_Mean)) + 
  geom_bar(stat='summary', fun.y='mean',fill='lightpink',color='black') + 
  geom_point() + 
  geom_text(aes(label = round(Sales_Mean)), vjust=-0.3, size=3.5) + 
  ggtitle("Item Type Vs Sales") + 
  theme(axis.text.x = element_text(angle=45,vjust=0.5))
#Mean-sales for sea fodd is more compared to other products

#Outlet-size
ggplot(outletsize_bysales, aes(Outlet_Size, Sales_Mean)) + 
  geom_bar(stat='summary', fun.y='mean',fill='slateblue1',color='black') + 
  geom_point() + 
  geom_text(aes(label = round(Sales_Mean)), vjust=-0.3, size=6.5) + 
  ggtitle("Outlet Size Vs Sales")
#Mean-sales of small-size storesin terms of ground area are low compared to others

#Outlet-Loaction
ggplot(outletlocation_bysales, aes(Outlet_Location_Type, Sales_Mean)) + 
  geom_bar(stat='summary', fun.y='mean',fill='blue',color='black') + 
  geom_text(aes(label = round(Sales_Mean)), vjust=-0.3, size=6) + 
  ggtitle("Outlet Location Vs Sales")
#Mean-sales for Tier-1 Location is less
trellis.device()
#Outlet-type
ggplot(outlettype_bysales, aes(Outlet_Type, Sales_Mean)) + 
  geom_bar(stat='summary', fun.y='mean',fill='lightgreen',color='black') + 
  geom_point() + 
  geom_text(aes(label = round(Sales_Mean)), vjust=-0.3, size=6) + 
  ggtitle("Outlet Location Type Vs Sales")
#Grocery stores have less mean-sales and super market three has more average sales

#Outlet_id by sales
ggplot(outletid_bysales, aes(Outlet_Identifier, Sales_Mean)) + 
  geom_bar(stat='summary', fun.y='mean',fill='slateblue1',color='black') + 
  geom_point() + 
  geom_text(aes(label = round(Sales_Mean)), vjust=-0.2, size=3.5) + 
  ggtitle("Outlet Identifier Vs Sales")
#OUtlet 027 has more sales while outlet 19 has less sales

#Outlet Establishment year
ggplot(outletyear_bysales, aes(Outlet_Establishment_Year, Sales_Mean)) + 
  geom_bar(stat='summary', fun.y='mean',fill='slateblue1',color='black') + 
  geom_point() + 
  geom_text(aes(label = round(Sales_Mean)), vjust=-0.2, size=3.5) + 
  ggtitle("Outlet Location Type Vs Sales")
#During the Year 2012-2013 the mean sales very low.

#Item_outlet_sales vs Fat_content
ggplot(newd,aes(x=Item_Fat_Content, y=Item_Outlet_Sales)) + 
  geom_violin(fill='darkblue') +
  ggtitle('Fat Content Vs Sales')
#The graph shows the distribution of fat-content with respect to sales

#Outlet_identifier vs Sales
ggplot(newd,aes(x=Outlet_Identifier, y=Item_Outlet_Sales)) + 
  geom_violin(fill='midnightblue') + 
  theme(axis.text.x = element_text(angle=45,vjust=0.5)) +
  ggtitle('Outlet Identifier Vs Sales') +
  theme(plot.title = element_text(hjust = 0.5))
#The mean sales of Out_010 and out19 is low

#Outlet_type vs Sales
ggplot(newd,aes(x=Outlet_Type, y=Item_Outlet_Sales)) + 
  geom_violin(fill='midnightblue') + 
  theme(axis.text.x = element_text(angle=45,vjust=0.5)) +
  ggtitle('Outlet Identifier Vs Sales') +
  theme(plot.title = element_text(hjust = 0.5))
#The distribution of products sold in grocery shops in less

#Outlet_location vs Sales
ggplot(newd,aes(x=Outlet_Location_Type, y=Item_Outlet_Sales)) + 
  geom_violin(fill='midnightblue') + 
  theme(axis.text.x = element_text(angle=45,vjust=0.5)) +
  ggtitle('Outlet Identifier Vs Sales') +
  theme(plot.title = element_text(hjust = 0.5))
#The Tier-1 and Tier-3 has slight similiar distribution

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                              #MULTI-VARIATE ANALYSIS
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Outlet_Location_Type vs Outlet_Type vs Item_Outlet_Sales
m1 <- newd %>% group_by(Outlet_Location_Type, Outlet_Type) %>% 
  summarise_at(vars(Item_Outlet_Sales), funs(Sales_mean = mean))

m1#Table form visualisation
ggplot(m1,aes(Outlet_Location_Type,Sales_mean,fill=Outlet_Type)) + geom_bar(stat='identity')

#The sales in the grocery shop in tier3 is more compared to tier1 and 
#we can also say that tier2 location has only type1 supermarkets while type2
#Super markets are present in tier3 location

#Years vs item_type vs Sales
m2 <- newd %>% group_by(Outlet_Establishment_Year,Item_Type) %>% 
  summarise_at(vars(Item_Outlet_Sales),funs(Sales_mean = mean))
class(m2)
m2 <- as.data.frame(m2)
m2
newd$Outlet_Establishment_Year <- as.factor(newd$Outlet_Establishment_Year)
#In the year wise the sales of top products are as follows:
2008- canned, breakfast and breads 
2010 and 2011- snack foods, softdrinks and fruits and vegetables
2013 - Seafoods average sale is less

#Years vs Oultetlocation vs Sales
m3 <- newd %>% group_by(Outlet_Establishment_Year,Outlet_Location_Type) %>% 
  summarise_at(vars(Item_Outlet_Sales),funs(Sales_count = n()))
trellis.device()
ggplot(m3,aes(Outlet_Establishment_Year,Sales_count, fill=Outlet_Location_Type))+geom_bar(stat = 'identity')
#the products has been distributed in the tier location in different year wise manner
#In 2008,2011 and 2014 the product sales has been recorded in tier1 while in 
#tier2 has 2015-2017. In the year 2013 the sales count is less in Tier3, and
# the sales count is high in the tier1 location during the year 2008.
trellis.device()
#Years vs Oultet-type vs Sales
m4 <- newd %>% group_by(Outlet_Establishment_Year,Outlet_Type) %>% 
  summarise_at(vars(Item_Outlet_Sales),funs(Sales_count = n()))

ggplot(m4,aes(Outlet_Establishment_Year,Sales_count, fill=Outlet_Type))+geom_bar(stat = 'identity')
trellis.device()
#Groceery shop has higher sales count during the year2008 and less in 2013.

#Years vs fat_content vs sales
m5 <- newd %>% group_by(Outlet_Establishment_Year,Item_Fat_Content) %>% 
  summarise_at(vars(Item_Outlet_Sales),funs(Sales_count = n()))
ggplot(m5,aes(Outlet_Establishment_Year,Sales_count, fill=Item_Fat_Content))+geom_bar(stat = 'identity')
#low fat content products has higher sales during all the years

#Years vs fat_content_MRP rate
m6 <- newd %>% group_by(Outlet_Establishment_Year,Item_Fat_Content) %>% 
  summarise_at(vars(Item_MRP),funs(MRP_count = n()))
ggplot(m6,aes(Outlet_Establishment_Year,MRP_count, fill=Item_Fat_Content))+geom_bar(stat = 'identity')
#The MRP count is also high for the Low fat content products 

#fatcontent vs outlet size vs mrp
m7 <- newd %>% group_by(Item_Fat_Content,Outlet_Size) %>% 
  summarise_at(vars(Item_MRP),funs(MRP_count = n()))
ggplot(m7,aes(Item_Fat_Content,MRP_count, fill=Outlet_Size))+geom_bar(stat = 'identity')
#The Mrp products of the small size products is less

#fatcontent vs outlet type vs Sales
m8 <- newd %>% group_by(Item_Fat_Content,Outlet_Type) %>% 
  summarise_at(vars(Item_Outlet_Sales),funs(Sale_count = n()))
ggplot(m8,aes(Item_Fat_Content,Sale_count, fill=Outlet_Type))+geom_bar(stat = 'identity')
#The grocery stores has higher sales count in both low and regular fat content product

#fatcontent vs outlet location vs Sales
m9 <- newd %>% group_by(Item_Fat_Content,Outlet_Location_Type) %>% 
  summarise_at(vars(Item_Outlet_Sales),funs(Sale_count = n()))
ggplot(m9,aes(Item_Fat_Content,Sale_count, fill=Outlet_Location_Type))+geom_bar(stat = 'identity')
#tier1 location has higher sales count for both low and regular fat-content

#OUtlet size vs outlet type vs Sales
m10 <- newd %>% group_by(Outlet_Size,Outlet_Type) %>% 
  summarise_at(vars(Item_Outlet_Sales),funs(Sale_count = n()))
ggplot(m10,aes(Outlet_Size,Sale_count, fill=Outlet_Type))+geom_bar(stat = 'identity')
#SM type1 stores only solds high size products while grocery store more number of small and medium size products

#OUtlet size vs outlet location type vs Sales
m11 <- newd %>% group_by(Outlet_Size,Outlet_Location_Type) %>% 
  summarise_at(vars(Item_Outlet_Sales),funs(Sale_count = n()))
ggplot(m11,aes(Outlet_Size,Sale_count, fill=Outlet_Location_Type))+geom_bar(stat = 'identity')
#The tier-3 location doesnt have the stores with low area ground covered.
# The tier-1 location with medium area of store has more sales.
# The high area of ground staorange in tier 3 has less sales.


#******************&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&**************%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                            #MODEL BUILDING PURPOSE
#######&&&&&&&&&&&&&&&&&&&&&&&&&&**********************************************%%%%%%%%%%%%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$4
#Calling the necessary library
library(dplyr)# To make use of pipelining process,cleaning of data and descriptive analysis of data

library(ggplot2) # For visualisation purpose

library(caret)# To tune the model, and for feature selection

library(tidyverse)# To clean the data
d1 <- read.csv(file.choose())# Importing the data

dk <- d1
#Seeing the datatype of all variables
str(dk)
#Summary of the data
summary(dk)
dim(dk)

library(corrgram)
da<-dk[,c(2,4,6,12)]
names(da)
corrgram(da, order=TRUE, 
         lower.panel=panel.shade,
         upper.panel=panel.pie, 
         text.panel=panel.txt,
         main="Corrgram")
cor(da)
                # Feature Engineering/Data Cleaning

# To know the levels of categorical varaibles in data frame
sapply(dk[,2:ncol(dk)], levels)

#Finding missing values and blank space in a variable
sapply(dk, function(x) sum(is.na(x)))#Item_weight has missing value
sapply(dk, function(x) sum(x == ''))#Outlet_size has blank space

#Imputing the item_weight missing values with item_types mean weight
#Seeing the no of missing values in Item_weight present in the item_type
item_type_na1 <- dk %>%
  group_by(Item_Type) %>%
  filter(is.na(Item_Weight)) %>%
  count()

item_type_na1

#Plot Visualisation
library(lattice)
trellis.device()
ggplot(item_type_na1, aes(Item_Type, n)) +
  geom_bar(stat = 'identity',fill = 'red') +
  theme(axis.text.x = element_text(angle=75, vjust=0.7))#Frozen foods and Snack foods have more missing values

#Creating a user-defined function for imputing the missing values 
impute_mean1 <- function(x) replace(x, is.na(x), mean(x, na.rm = T))

#Mean-Imputation
dk1 <- dk %>%
  group_by(Item_Type) %>%
  mutate(Item_Weight = impute_mean1(Item_Weight))

#Making Fat_content  two levels Low fat and Regular
dk2 <- dk1 %>%
  mutate(Item_Fat_Content = replace(Item_Fat_Content, Item_Fat_Content == 'LF', 'Low Fat')) %>%
  mutate(Item_Fat_Content = replace(Item_Fat_Content, Item_Fat_Content == 'low fat', 'Low Fat')) %>%
  mutate(Item_Fat_Content = replace(Item_Fat_Content, Item_Fat_Content == 'reg', 'Regular')) %>%
  droplevels()

#Scatter plot to determine the relation between the item_sales and visibility
ggplot(dk2, aes(Item_Visibility, Item_Outlet_Sales)) + 
  geom_point(size = 0.75)

#Thus the item with zero visibility is not possible to sold the product,
#Now converting the zero values of visibilty to its mean

zero_vis1 <- dk2 %>%
  filter(Item_Visibility == 0 & Item_Outlet_Sales > 0)
#Seeing the rows which have zero visibility and sales greater than zero
zero_vis1

avg <- dk2 %>%
  summarize(mean(Item_Visibility))
#Seeing the average of all products visisbility
avg

dk2$
#Imputation
dk3 <- dk2 %>%
  mutate(Item_Visibility = replace(Item_Visibility, Item_Visibility == 0, mean(Item_Visibility))) %>%
  mutate(Item_Visibility = as.double(Item_Visibility))


#OUtlet_size variable imputing blank space
levels(as.factor(dk3$Outlet_Size))
dk3$Outlet_Size = ifelse(dk3$Outlet_Size=='Small','Small',ifelse(dk3$Outlet_Size=='High','High', 'Medium'))

#checking of missing values and balnk space in a variable
sapply(dk3, function(x) sum(x == ''))

# to see the variable names
names(dk3)
#************************************************************************************
                              #FEATURE- ENGINEERING
#***********************************************************************************
#Changing the Item identifier into new type of categorical variable
dk3$Item_Identifier
#From analysing this variable we can say that:
#1) FD- FOOD  2)DR-Drinks 3)NC- Non-Consumable

#Using the string function we are going to take first two letters in the variable 
dk3$Item_Identifier_new <- substr(dk3$Item_Identifier, 1 , 2)
dk3$Item_Identifier_new 

#Replacing with better values using the function gsub function 
dk3$Item_Identifier_new <-gsub("FD", "Food" , dk3$Item_Identifier_new)
dk3$Item_Identifier_new <-gsub("NC", "Non consumable" , dk3$Item_Identifier_new)
dk3$Item_Identifier_new <-gsub("DR", "Drinks" , dk3$Item_Identifier_new)
dk3$Item_Identifier_new 

#Change of Outlet_establishment_year into numerical variable for easy imputations

dk3$outlet_years <- 2019 - dk3$Outlet_Establishment_Year
summary(dk3$outlet_years)

# Reducing the Item type variables of 16 levels into 
#perishable(which will decay quickly), non-perishable and not-sure
levels(as.factor(dk3$Item_Type))
perishable <- c('Dairy', 'Fruits and Vegetables', 'Breads', 'Breakfast', 'Meat', 'Seafood')
non_perishable <- c("Frozen Foods", "Canned", "Baking Goods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")
dk3[, 'Item_Type_new'] = ifelse(dk3$Item_Type %in% perishable, 'perishable', ifelse(dk3$Item_Type %in% non_perishable, 'non_perishable', 'not_sure'))
dk3$Item_Type_new


#Feature-Scaling of Response variable and Visibility
skew(newd$Item_Outlet_Sales)
skew(newd$Item_Visibility)
#there is little problem of skewness in repsonse variable
hist(dk3$Item_Outlet_Sales,col = rainbow(7))
#Standarisation - since the response varibles have outliers

newd$Item_Visibility <- scale(newd$Item_Visibility,center = TRUE, scale=TRUE)
skew(newd$Item_Visibility)
str(newd)
newd$Item_Outlet_Sales <- scale(newd$Item_Outlet_Sales,scale = TRUE, center = TRUE)
skew(newd$Item_Outlet_Sales)

#Feature engineering
#BY visualisation and EDA we can say that MRP is important variables for sales prediction

#Creating the MRP by weight
dk3$mrpbyweight <- dk3$Item_MRP/dk3$Item_Weight
cor(dk3$mrpbyweight, dk3$Item_Outlet_Sales)
#dk3$itembyvolume <- dk3$Item_Outlet_Sales/dk3$Item_MRP
#cor(dk3$itembyvolume, dk3$Item_Outlet_Sales)
levels(as.factor(dk3$Item_Identifier_new))
str(dk3)

#Based on the Item_identifier variable we are going to include the new level in fat_content variable
dk3$Item_Fat_Content <- as.character(dk3$Item_Fat_Content)
dk3$Item_Fat_Content[dk3$Item_Identifier_new=="Non consumable"]<- "Non edible"
sum(is.na(dk3$Item_Fat_Content))

class(dk3$Item_Fat_Content)


#Converting the character variables into factor
dk3$Item_Type_new <- as.factor(dk3$Item_Type_new)
dk3$Item_Identifier_new <- as.factor(dk3$Item_Identifier_new)
dk3$Outlet_Size <- as.factor(dk3$Outlet_Size)
dk3$Item_Fat_Content <- as.factor(dk3$Item_Fat_Content)
levels(as.factor(dk3$Item_Fat_Content))
names(dk3)

#removing the unnecessary variables
dk51 <-dk3[,-c(1,5,8)]
names(dk51)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@NEW visualisation:@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Item_Type_new
ggplot(dk51,aes(x=Item_Type_new, y=Item_Outlet_Sales)) + 
  geom_violin(fill='darkblue') +
  ggtitle('Item_type_new Vs Sales')
plot(dk51$Item_Type_new, xlab = 'xaxis', ylab = 'yaxis', 
        main = 'bar plot', horiz = FALSE)
#the count of the non-perishable item is more

#new Item_type by sales
item_type_sales <-  dk51 %>% group_by(Item_Type_new) %>% 
  summarise_at(vars(Item_Outlet_Sales), funs(Count = n(),Sales_Mean = mean))
item_type_sales
ggplot(item_type_sales, aes(Item_Type_new, Sales_Mean)) + 
  geom_bar(stat='summary', fun.y='mean',fill='slateblue1',color='black') + 
  geom_point() + 
  geom_text(aes(label = round(Sales_Mean)), vjust=-0.3, size=3.5) + 
  ggtitle("Item Type Vs Sales")

#Item_Identifier
ggplot(dk51,aes(x=Item_Identifier_new, y=Item_Outlet_Sales)) + 
  geom_boxplot(fill='darkblue') +
  ggtitle('Item_Identifier_new Vs Sales')
plot(dk51$Item_Identifier_new, xlab = 'xaxis', ylab = 'yaxis', 
     main = 'bar plot', horiz = FALSE)
#Analysing average sales by item_id
item_id_sales <-  dk51 %>% group_by(Item_Identifier_new) %>% 
  summarise_at(vars(Item_Outlet_Sales), funs(Count = n(),Sales_Mean = mean))
item_id_sales
ggplot(item_id_sales, aes(Item_Identifier_new, Sales_Mean)) + 
  geom_bar(stat='summary', fun.y='mean',fill='slateblue1',color='black') + 
  geom_point() + 
  geom_text(aes(label = round(Sales_Mean)), vjust=-0.3, size=3.5) + 
  ggtitle("Item ID Vs Sales")
#food products has more sales

#Multivariate analysis of ITem id and Item type
mn <- dk51 %>% group_by(Item_Identifier_new, Item_Type_new) %>% 
  summarise_at(vars(Item_Outlet_Sales), funs(Sales_mean = mean))

mn#Table form visualisation
ggplot(mn,aes(Item_Identifier_new,Sales_mean,fill=Item_Type_new)) + geom_bar(stat='identity')


#ENCODING CONCEPT OF CATEGORICAL VARIABLES:

#label encoding for fat_content variables
dk51[, sapply(dk51, is.factor)]
levels(as.factor(dk51$Outlet_Size))
dk51$Item_Fat_Content <- ifelse(dk51$Item_Fat_Content=="Low Fat",2,ifelse(dk51$Item_Fat_Content=="Regular",1,0))
#dk51$Outlet_Size <- ifelse(dk51$Outlet_Size=="Small",0,
                           ifelse(dk51$Outlet_Size=="Medium",1,2))
#dk51$Outlet_Location_Type <- ifelse(dk51$Outlet_Location_Type=="Tier 3",0,
                                    ifelse(dk51$Outlet_Location_Type=="Tier 2",1,2))
str(dk51)
dk51$Item_Fat_Content
library(caret)
#ONe-hot or dummyvariable encoding
dmy <- dummyVars(" ~ .", data = dk51, fullRank = T)
data_transformed <- data.frame(predict(dmy, newdata = dk51))

str(data_transformed)
names(data_transformed)
#NOTE- We should repeat the same procedure for the upcoming TEST data
#$@$@$@$@$@$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$@@@@@@@@@@$$$$$$$$$$$$$@@@@@@@@@$$$$$$****************************88
                              #     TEST DATA
#@@@@@@@@@@$$$$$$$$$$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++++++++++++++++++++++++++++
#********(((((((((((((())))))))))))))))))))))))))))!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
d2 <- read.csv(file.choose())#test data

dk_test <- d2


str(dk_test)

#A quick glance at the data to get a general sense of it, what the data looks like and what I need to clean.

summary(dk_test)

names(dk_test)
#2. Feature Engineering/Cleaning
#I want to see if we have redundant levels for categorical variables. In this case we do, in Item_Fat_Content we have 3 types of low fat and 2 types of regular. I'll standardize it to 'Low Fat' and 'Regular'

sapply(dk_test[,2:ncol(dk_test)], levels)

#Finding missing values. Item weight has 2439 NA. Outlet_Size has 2410 blank instances. Every other variable does not have any missing values. 
#In the grand scheme of things I don't think these variables will have much predictive power so I won't bother to fill them in.

sapply(dk_test, function(x) sum(is.na(x)))
sapply(dk_test, function(x) sum(x == ''))

#We have to address the NA in Item_Weight. 
#Looking at the distribution of the NA among Item_Types there seems to 
#be some Item_Types that have a larger amount of NA. 
#Note: these values are not normalized so very little 
#inferences can be drawn from this histogram. 
#Instead I'll take the mean of each Item_Type and replace the NA for that type with the mean.
item_type_na1 <- dk_test %>%
  group_by(Item_Type) %>%
  filter(is.na(Item_Weight)) %>%
  count()

item_type_na1

ggplot(item_type_na1, aes(Item_Type, n)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle=75, vjust=0.7))
#Oh my, so after hours of trying to figure out how to replace 
#all the NAs with the means for that group I came across the answer. 
#All the NAs for Item_Weight are gone, and are replaced by 
#the mean weight for that item's type.
impute_mean1 <- function(x) replace(x, is.na(x), mean(x, na.rm = T))

dk_test1 <- dk_test %>%
  group_by(Item_Type) %>%
  mutate(Item_Weight = impute_mean1(Item_Weight))
#Fixing some inconsistencies with the factors for Item_Fat_Content.

dk_test2 <- dk_test1 %>%
  mutate(Item_Fat_Content = replace(Item_Fat_Content, Item_Fat_Content == 'LF', 'Low Fat')) %>%
  mutate(Item_Fat_Content = replace(Item_Fat_Content, Item_Fat_Content == 'low fat', 'Low Fat')) %>%
  mutate(Item_Fat_Content = replace(Item_Fat_Content, Item_Fat_Content == 'reg', 'Regular')) %>%
  droplevels()
ggplot(dk_test2, aes(Item_Visibility, Item_Weight)) + 
  geom_point(size = 0.75)

#Having an item with zero Item_Visibility that has sales makes little sense. 
#Additionally, because Item_Visibility is a percetage of the store 
#display area, having a value of over 1 makes no sense either. 
#I'm going to make the assumption that people aren't buying items that 
#are stored in the back of the store. Running with Item_Outlet_Sales == 0 
#yields 0 results, so all the cases of Item_Visibility are beening bought. 
#We'll set the Item_Visibilty for equal to the mean of Item_Visibility.

zero_viz1 <- dk_test2 %>%
  filter(Item_Visibility == 0)

zero_viz1

avg1 <- dk_test2 %>%
  summarize(mean(Item_Visibility))

dk_test3 <- dk_test2 %>%
  mutate(Item_Visibility = replace(Item_Visibility, Item_Visibility == 0, mean(Item_Visibility))) %>%
  mutate(Item_Visibility = as.double(Item_Visibility))

summary(dk_test3)

ggplot(dk_test3, aes(Outlet_Size, Outlet_Location_Type, color=Outlet_Type)) +
  geom_point()

#OUtlet_size
levels(as.factor(dk_test3$Outlet_Size))
dk_test3$Outlet_Size = ifelse(dk_test3$Outlet_Size=='Small','Small',ifelse(dk_test3$Outlet_Size=='High','High', 'Medium'))

sapply(dk_test3, function(x) sum(x == ''))

names(dk_test3)
#Item_Identifier: Changing the levels of item identifier that is making them into perfect categorical variable
dk_test3$Item_Identifier_new <- substr(dk_test3$Item_Identifier, 1 , 2)
dk_test3$Item_Identifier_new 

#Replacing with better identifiable values using the function gsub 
dk_test3$Item_Identifier_new <-gsub("FD", "Food" , dk_test3$Item_Identifier_new)
dk_test3$Item_Identifier_new <-gsub("NC", "Non consumable" , dk_test3$Item_Identifier_new)
dk_test3$Item_Identifier_new <-gsub("DR", "Drinks" , dk_test3$Item_Identifier_new)
dk_test3$Item_Identifier_new 

#Oultet_establishment_year
#calculate outlet_establishment years( to check how old it is with reference to its outlet sales)
#PS. Since in the given problem..the year upto 2013 is to be considered. hence.

dk_test3$outlet_years <- 2019 - dk_test3$Outlet_Establishment_Year
summary(dk_test3$outlet_years)

# Classifying Item_Type into perishable and non-perishable
perishable <- c('Breads', 'Breakfast', 'Dairy', 'Fruits and Vegetables', 'Meat', 'Seafood')
non_perishable <- c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")
dk_test3[, 'Item_Type_new'] = ifelse(dk_test3$Item_Type %in% perishable, 'perishable', ifelse(dk_test3$Item_Type %in% non_perishable, 'non_perishable', 'not_sure'))
dk_test3$Item_Type_new

#Feature engineering
dk_test3$mrpbyweight <- dk_test3$Item_MRP/dk_test3$Item_Weight

#dk_test3$itembyvolume <- dk_test3$Item_Outlet_Sales/dk_test3$Item_MRP
#cor(dk_test3$itembyvolume, dk_test3$Item_Outlet_Sales)
levels(as.factor(dk_test3$Item_Identifier_new))
str(dk_test3)
dk_test3$Item_Type_new <- as.factor(dk_test3$Item_Type_new)
dk_test3$Item_Identifier_new <- as.factor(dk_test3$Item_Identifier_new)
dk_test3$Outlet_Size <- as.factor(dk_test3$Outlet_Size)

#Based on the Item_identifier variable we are going to include the new level in fat_content variable
dk_test3$Item_Fat_Content <- as.character(dk_test3$Item_Fat_Content)
dk_test3$Item_Fat_Content[dk_test3$Item_Identifier_new=="Non consumable"]<- "Non edible"
sum(is.na(dk_test3$Item_Fat_Content))
dk_test3$Item_Fat_Content <- as.factor(dk_test3$Item_Fat_Content)
names(dk_test3)
dk_test51 <-dk_test3[,-c(1,5,8)]
names(dk_test51)


#ENCODING CONCEPT:
dk_test51[, sapply(dk_test51, is.factor)]
levels(as.factor(dk_test51$Item_Fat_Content))
dk_test51$Item_Fat_Content <- ifelse(dk_test51$Item_Fat_Content=="Low Fat",2,ifelse(dk_test51$Item_Fat_Content=="Regular",1,0))
str(dk_test51)

library(caret)

dmy <- dummyVars(" ~ .", data = dk_test51, fullRank = T)
data_transformed_test <- data.frame(predict(dmy, newdata = dk_test51))


str(data_transformed_test)
#data_transformed1 <- data_transformed[,-c(28)]
####################################################################################
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                            #Train and test split
index <- sample(1:nrow(data_transformed),0.75*nrow(data_transformed))
tdr <- data_transformed[index,]
names(tsdr)
tsdr <- data_transformed[-index,]
dim(tdr);dim(tsdr)
linearmod <- lm(Item_Outlet_Sales~., data = tdr) 
summary(linearmod)
pd <- predict(linearmod, newdata = tsdr)

library(caret)
RMSE(pd, tsdr$Item_Outlet_Sales)
plot(linearmod)

#Assumptions check of Linear model
#1)Linearity check
library(psych)#package used to check linearity
library(lattice)
trellis.device()
pairs.panels(dk51, method = "pearson", pch = 30)

#NORMALITY CHECK OF RESPONSE VARIABLE
library(nortest)
ad.test(data_transformed$Item_Outlet_Sales)
shapiro.test(data_transformed$Item_Outlet_Sales)
#the shapiro test is only applicable for observations between 3 and 5000
lillie.test(data_transformed$Item_Outlet_Sales)
#there is a problem of normality
library(lmtest)#used to do the linear model test
dwtest(linearmod)#No problem of auto-correlation
library(car)
ncvTest(linearmod)
gqtest(linearmod)
bptest(linearmod)

#Error normality check
eror <- tsdr$Item_Outlet_Sales - pd
library(nortest)
library(psych)
skew(eror)
ad.test(eror)
shapiro.test(eror)
#Still the normality fails
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
                      #Ridge regression for normal skewed data
#$%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ridge<-train(Item_Outlet_Sales~.,data=tdr,method="glmnet",tuneGrid=expand.grid(alpha=0,lambda=seq(0.001,0.1,length=10)))
summary(ridge)
p1 <- predict(ridge, newdata = tsdr)
RMSE(p1,tsdr$Item_Outlet_Sales)
#RMSE is 1132
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                      #Cross-validation Models
#$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
custom <- trainControl(method="repeatedcv",
                       number=10,
                       repeats=5,
                       verboseIter = T)
lmod<-train(Item_Outlet_Sales~.,data=tdr,
            method="lm",trControl=custom)

lmod
summary(lmod)
#RMSE value is 1130.484 and R square is 56

ridge1<-train(Item_Outlet_Sales~.,data=tdr,
              method="glmnet",trControl=custom,
              tuneGrid=expand.grid(alpha=0.7,lambda=0.001))
ridge1             
#RMSE is 1112.362 and Rsquare value is 57

                  #LASSO and ELASTIC NET MODELS With cross validation

Lasso<-train(Item_Outlet_Sales~.,data=tdr,method="glmnet",trControl=custom,tuneGrid=expand.grid(alpha=1,lambda=seq(0.001,0.1,length=10)))
elastic<-train(Item_Outlet_Sales~.,data=tdr,method="glmnet",trControl=custom,tuneGrid=expand.grid(alpha=seq(0,1,0.1),lambda=seq(0.001,0.1,length=10)))
trellis.device()
plot(elastic)

pred_lm<-predict(lmod,tsdr)
pred_ridge<-predict(ridge1,tsdr)
pred_lasso<-predict(Lasso,tsdr)
pred_elastic<-predict(elastic,tsdr)
rmse_lm<-RMSE(pred_lm,tsdr$Item_Outlet_Sales)
rmse_ridge<-RMSE(pred_ridge,tsdr$Item_Outlet_Sales)
rmse_lasso<-RMSE(pred_lasso,tsdr$Item_Outlet_Sales)
rmse_elastic<-RMSE(pred_elastic,tsdr$Item_Outlet_Sales)
values<-c(rmse_lm,rmse_ridge,rmse_lasso,rmse_elastic)
values
#Lasso RMSE is 1123.671
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#**********************************Decision Tree Regressor**************************
#$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@$$$$$$$$$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(rpart)
library(rpart.plot)
dtree1 <- rpart(Item_Outlet_Sales~., data = tdr, 
                method = "anova", parms = list(split= "information"))
# parms means parameter
trellis.device()
plotcp(dtree1)
predr <- predict(dtree1, newdata = tsdr)
library(caret)
RMSE(pred = predr, obs = tsdr$Item_Outlet_Sales)
#RMSE value is 1125.581
rpart.plot(dtree1)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$&&&&&&&&&&&&&&&&&&&&
                      #Random-Forest Model Building  
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#cross-validation
my_Control <- trainControl(method = "cv", number = 5)
tGrid <- expand.grid(
  .mtry = 5,
  .splitrule = "variance",
  .min.node.size = 20
)
rf_mod <- train(Item_Outlet_Sales~.,data = tdr, 
                method = "ranger", 
                trControl = my_Control, 
                tuneGrid = tGrid,
                num.trees = 400,
                importance = "permutation"
)
rm_pred <- predict(rf_mod, newdata = tsdr)
RMSE(rm_pred, tsdr$Item_Outlet_Sales)
#RMSE is 1114.589
trellis.device()
plot(varImp(rf_mod))
names(data_transformed)

#Now building the RANDOM FOREST MODEL WITH IMPORTANT VARIABLES
data_transformed_rf <- data_transformed[,-c(23,25,6,26,10,11,2,22,12,13,3,5,16)]
index_rf <- sample(1:nrow(data_transformed_rf), 0.75*nrow(data_transformed_rf))
tdr_rf <- data_transformed_rf[index_rf,]
tsdr_rf <- data_transformed_rf[-index_rf,]
dim(tdr_rf);dim(tsdr_rf)


rf_mod_new <- train(Item_Outlet_Sales~.,data = tdr_rf, 
                method = "ranger", 
                trControl = my_Control, 
                tuneGrid = tGrid,
                num.trees = 400,
                importance = "permutation"
)
rm_pred_new <- predict(rf_mod_new, newdata = tsdr_rf)
RMSE(rm_pred_new, tsdr_rf$Item_Outlet_Sales)
#RMSE is 1096.843

#RANDOM-FOREST HYPER-PARAMETER TUNING
#Using the grid-search cv
my_Control1 <- trainControl(method = "repeatedcv", 
                            number = 10, repeats = 3, search = 'grid')
tGrid1 <- expand.grid(
  .mtry = c(1:25),
  .splitrule = "variance",
  .min.node.size = 30
)
rf_tune1 <- train(Item_Outlet_Sales~.,data = tdr, 
                  method = "ranger", 
                  trControl = my_Control1, 
                  tuneGrid = tGrid1,
                  num.trees = 1000,
                  importance = "permutation"
)
print(rf_tune1)#mtry can be 6 or 7
pred_tune <- predict(rf_tune1,newdata = tsdr)
RMSE(pred_tune, tsdr$Item_Outlet_Sales)

rf_tune1$bestTune
#therefore mtry=6, splitrule=variance and min no of node =30
rf_tune1$method
rf_tune1$modelInfo
rf_tune1$modelType
rf_tune1$results
rf_tune1$call#what parameter you have used in your models
rf_tune1$dots#tells you about the number of trees and importance you entered
rf_tune1$metric#tells you about the metric
rf_tune1$control#?
rf_tune1$finalModel#gives you the details about the final model that you want to choose
rf_tune1$resample#fold06 and repitition 3
plot(rf_tune1)

library(randomForest)
#now building the model hyper-tuned model
rf_grid <- randomForest(Item_Outlet_Sales~.,data = tdr,method= 'ranger',
                        .mtry=6,ntree=1000,.min.node.size=30,na.action = na.roughfix,
                        importance=TRUE)
pred_grid <- predict(rf_grid,newdata=tsdr)
RMSE(pred_grid,tsdr$Item_Outlet_Sales)
#RMSE is 1060.405

pd <- predict(rf_grid, newdata = data_transformed_test)
pd
result_rf <- data.frame('Item_Identifier'=dk_test$Item_Identifier,
                        'Outlet_Identifier'=dk_test$Outlet_Identifier,
                        'Item_Outlet_Sales'=pd)
write.csv(result_rf,"C:\\Users\\BABU\\Desktop\\result.csv",row.names = FALSE)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$OTHER-Ensemble Methods:@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(caretEnsemble)
library(gridExtra)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, savePredictions = TRUE, classProbs = TRUE)

algorithmList <- c('glm', 'glmnet', 'lm', 'ranger', 'treebag', 'gbm', 'bagEarth')

models <- caretList(Item_Outlet_Sales ~ ., tdr, trControl = control, methodList = algorithmList)

results <- resamples(models)
summary(results)

models


#BAGGING ENSEMBLE:
stack_bag <- caretStack(models, method = "bagEarth", 
                        trControl = trainControl(method = "repeatedcv", 
                                                 number = 10, 
                                                 repeats = 3, 
                                                 savePredictions = TRUE))
stack_bag
predictions_bag <- predict(stack_bag, tsdr)
error <- predictions_bag - tsdr$Item_Outlet_Sales
sqrt(mean(error^2))
#RMSE is 1076.629

