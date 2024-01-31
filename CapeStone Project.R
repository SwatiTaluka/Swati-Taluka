df <- read.csv('marketing_campaign.csv',sep='\t',header = TRUE)
head(marketing_campaign)
View(marketing_campaign)
head(df)
View(df)
nrow(df)
ncol(df)
dim(df)
str(df)
df$Dt_Customer <- as.Date(df$Dt_Customer,format='%d-%m-%Y')
str(df$Dt_Customer)
install.packages("lubridate")
library(lubridate)
search()
dmy(df$Dt_Customer)

###2)	Find the variables with missing values. 
######If the proportion of missing values is less than 5%, then delete the rows

summary(df)

sum(is.na(df))
colSums(is.na(df))

###% of missinf values

nrow(df)
24/nrow(df)

###now delete the rows, as the percentage is lesser than 5 %

df <- na.omit(df)

nrow(df)
colSums(is.na(df))

####3)	Calculate the newest and oldest customer's enrolment date in the records

df$Dt_Customer

recent <- max(df$Dt_Customer)
Oldest <- min(df$Dt_Customer)


####4)	Create a feature "Customer_For" of the number of days the customers started to
####shop in the store relative to the last recorded date

df$Customer_For <- recent - df$Dt_Customer

df$Customer_For

###convert it to numeric function

as.numeric(df$Customer_For)
str(df$Customer_For)

###5)	Find the "Age" of customers by the "Year_Birth" indicating the birth year
library(lubridate)
colnames(df)
## todays date
Sys.Date()
year(Sys.Date())
     
## age = present year - year of birth

df$age <- year(Sys.Date()) - df$Year_Birth
df$age

####6)	Create a feature "Spent" indicating the total amount spent by the customer 
####in various categories over two years

colnames(df)

df$Spent <- df$MntWines + df$MntFruits + df$MntMeatProducts + df$MntFishProducts + df$MntSweetProducts + df$MntGoldProds
df$Spent

###7)	Create another feature "Living_With" out of "Marital_Status" 
####to extract the living situation of couples. 
####Consider a value ‘Partner’ for the variable "Living_With" for the instances
####where "Marital_Status" is either “Married” or “Together”. Rest all can be taken as ‘Alone’

df$Marital_Status
table(df$Marital_Status)

df$Living_with <- ifelse(df$Marital_Status %in% c('Married', 'Together'), 'Partner', 'Alone')
df$Living_with 

### 8)	Create a feature "Children" to indicate the 
####total number of children in a household that is, kids and teenagers

df$Children <- df$Kidhome + df$Teenhome
df$Children

###9)	To get further clarity on a household, create a feature indicating "Family_Size"

df$Family_Size <- ifelse(df$Living_with=='Alone', 1, 2) + df$Children
df$Family_Size

###10)	Create a feature "Is_Parent" to indicate the parenthood status

View(df)

df$Is_Parent <- ifelse(df$Children > 0, 1, 0)
df$Is_Parent

###11)	Keep only two categories in the field – ‘Education’ – Undergraduate, Graduate
table(df$Education)

df$Education <- ifelse(df$Education %in% c("Graduation", "Master", "PhD "), " Graduate", " Undergraduate")
df$Education

####12)	For clarity, change the name of the variables 

###MntWines	Wines
###MntFruits	Fruits
###MntMeatProducts	MeatProducts
###MntFishProducts	FishProducts
###MntSweetProducts	SweetsProducts
###MntGoldProds	GoldProds


colnames(df)[colnames(df) == "MntWines"] <- "Wines"
colnames(df)[colnames(df) == "MntFruits"] <- "Fruits"
colnames(df)[colnames(df) == "MntMeatProducts"] <- "MeatProducts"
colnames(df)[colnames(df) == "MntFishProducts"] <- "FishProducts"
colnames(df)[colnames(df) == "MntSweetProducts"] <- "SweetsProducts"
colnames(df)[colnames(df) == "MntGoldProds"] <- "GoldProds"

View(df)

###13)	 Drop the redundant columns: "Marital_Status", "Dt_Customer", "Z_CostContact", "Z_Revenue", "Year_Birth", and "ID"

columns_to_drop <- c("Marital_Status", "Dt_Customer", "Z_CostContact", "Z_Revenue", "Year_Birth", "ID")

df<- df[, !names(df) %in% columns_to_drop]

###14)	Create box plots and histograms for age and income. Identify the outliers and delete rows with outliers

boxplot(df$age, main = "Boxplot for age")
boxplot(df$Income, main = "Boxplot for Income")

### delete the rows with outliers

## Lets find out Quantiles to identify outlier values in age

Q1 <- quantile(df$age, 0.25)
Q3 <- quantile(df$age, 0.75)
IOR_Value = Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IOR_Value
upper_bound <- Q3 + 1.5 * IOR_Value


###Identify outliers
outliers_in_age <- df$age[df$age < lower_bound | df$age > upper_bound]
outliers_in_age

length(df$age) 

df = df[!(df$age < lower_bound | df$age > upper_bound), ]
dim(df)


## Lets find our Quantiles to identify outlier values in income

q1 <- quantile(df$Income, 0.25)
q3 <- quantile(df$Income, 0.75)
iqr_Value = q3 - q1

# Define lower and upper bounds for outliers
lower_bo <- q1 - 1.5 * iqr_Value
upper_bo <- q3 + 1.5 * iqr_Value

###Identify outliers
outliers_in_income <- df$Income[df$Income < lower_bo | df$Income > upper_bo]
outliers_in_income

length(outliers_in_income)

# Remove rows with outlier values

df <- df[!(df$Income < lower_bo | df$Income > upper_bo), ]
dim(df)

hist(df$age)
hist(df$Income)


####15)	Find out the correlation between variables. Create a heatmap to visualize the correlation plot
str(df)

### in order to perform corelation, we need to make class of all variable as integer or number
drop_col <- sapply(df,class) == 'character'

df_num <- df[-drop_col]

str(df_num)

df_num$Living_with <- NULL
df_num$Customer_For <- NULL

str(df_num)

## lets find corelation 

cor_mat <- cor(df_num)

library(corrplot)

search()

corrplot(cor_mat)


###16)	Now you should 
######prepare the data for cluster analysis and Categorical variables must be incorporated in clustering. 
##############Perform necessary encoding techniques to transform the categorical variables



###for clustering we need to find similar observations and similarity is found on the basis of distance. 
##we can calculate the distance between numeric variables, 
####but for character field we need a technique. We need to do dummy encoding in this case then. 

names(df[sapply(df, class) == 'character'])

df$Education <- as.numeric(df$Education)
df$Living_with <- as.numeric(df$Living_with)
df$Customer_For <- as.numeric(df$Customer_For)

str(df)

####17)	Find the appropriate number of clusters using the elbow method

## for clustering, we have to scale the observation, scaling means
### bringing values to the same magnitude. What does it means? We observe that the income is in lakhs and kids count is in 0,1 figure.
### we need to normalize this. We will do normalization

### normalized value formula = (x-mean)/SD

df_new1 <- scale(df)

###now our data is scaled and i ready to perform k-means clustering. We will have to give the number how many clusters we want to have in this

clus_result <-  kmeans(df_new1, centers = 3, iter.max = 10)
clus_result

table(clus_result$cluster) ### this shows the observations in each cluster

clus_result$tot.withinss ### total within sum of squares(ss) inside the clusters

clus_result$withinss ### total winthin ss within each of the clusters

####18. Perform cluster analysis using k-means

withinss <- sapply(1:15, function(x){kmeans(df_new1, centers = x, iter.max = 10)$tot.withinss})


## plotting elbow club
plot(1:15,withinss)

### Therefore, our requisite number of cluster = 4, from the result of elbow curve

clus_final = kmeans(df_new1, centers = 4, iter.max = 10)
clus_final$cluster

table(clus_final$cluster)

###19)	After a cluster analysis is performed,
###it is important for the business to define the clusters. 
###Perform cluster profiling, describe each cluster in detail, and use appropriate visualizations to support your views
library(factoextra)

df_new1$cluster <- clus_final$cluster
View(df_new1)

fviz_cluster(clus_final, data = df_new1, geom = "point")
str(df_new1)

library(dplyr)

df_new1 %>% group_by(cluster) %>% summarise(avg_income = mean(df_new1$income))














