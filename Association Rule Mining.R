# Association Task: To decide which catalog to send to existing customers for prospective purchase

data <- read.table('MyShopData.csv',sep=',',header=TRUE) # Importing data: .csv format
nrow(data) # No. of rows in dataset
ncol(data) # No. of cols in dataset

head(data) # Extra column at the end (contains summary data)
tail(data) # Extra row at the end (contains summary data)

data <- data[-4999,-11] # Removing the last row & column from the data
tail(data)

# Converting data to single format
library(tidyr)
class(data)
names(data) # Col names of the dataframe
data_v1 <- as.data.frame(pivot_longer(data,-Customer.Number,names_to = "category",values_to = "purchase")) # Pivoting the data
head(data_v1) # inspecting the data frame
nrow(data_v1) 
ncol(data_v1)

# data_v1 (transformed data) will have data for each customer for all categories: 0 and 1
# Removing the rows with purchase value 0(purchase not made)
data_v2 <- subset(data_v1,purchase==1) # subsetting data with purchase value 1
head(data_v2) # inspecting the data frame
nrow(data_v2)
ncol(data_v2)

# Removing the purchase column; converting to single format 
data_v3 <- data_v2[,1:2]
head(data_v3)

# writing the data into a new file
write.csv(data_v3,"MyShopData_single.csv",row.names=F)

# Installing library arules for association rules
library(arules)

# Importing data in transaction format
data<-read.transactions("MyShopData_single.csv",format="single",sep=",",cols=c(1,2))
head(inspect(data))
head(data)
# apriori for generating the association rules
ass_rules<-apriori(data)

# Viewing the result: association rules
print(inspect(ass_rules))

# Sorting the association rule result
print(inspect(sort(ass_rules,by="lift")))
print(inspect(sort(ass_rules,by="support")))

# Cutoffs for support and confidence given (0.4 and 0.6 respectively)
ass_rules_v2 <- apriori(data,parameter=list(sup=0.4,conf=0.6,minlen=2))
print(inspect(ass_rules_v2))

library(dplyr)
data_v4 <- filter(data_v3,category!='Health.Products.Division')
head(data_v4)

# writing the data into a new file
write.csv(data_v4,"MyShopData_single_v2.csv",row.names=F)

# Importing data in transaction format
data_new<-read.transactions("MyShopData_single_v2.csv",format="single",sep=",",cols=c(1,2))
head(inspect(data_new))
# apriori for generating the association rules
ass_rules_new<-apriori(data_new)

# Viewing the result: association rules
print(inspect(ass_rules_new))

# Sorting the association rule result
print(inspect(sort(ass_rules_new,by="lift")))
print(inspect(sort(ass_rules_new,by="support")))

# Cutoffs for support and confidence given (0.4 and 0.6 respectively)
ass_rules_new_v2 <- apriori(data_new,parameter=list(sup=0.2,conf=0.5,minlen=2))
print(inspect(ass_rules_new_v2))

# Plot the rules
#install.packages('arulesViz')
#install.packages('arulesViz')
library(arulesViz)

plot(ass_rules_new_v2) # Scatterplot of the association rules
plot(ass_rules_new_v2,method="graph",control=list(type="items",arrowSize=0.6,cex=0.8))# Graphical plot

# Identifying the customers for sending the catalogs

raw_data <- read.table('MyShopData.csv',sep=',',header=TRUE) # Importing data: .csv format
raw_data <- raw_data[-4999,-11]

ass_rule_1 <- filter(raw_data,(Novelty.Gift.Division==1 |Housewares.Division==1 | Garden.Division==1),
                 Personal.Electronics.Division!=1)
ass_rule_2 <- filter(raw_data,(Personal.Electronics.Division == 1 | Jewelry.Division == 1),
                     Housewares.Division!=1)

ass_rule_1$Catalog.To.Send <- 'Personal.Electronics.Division'
ass_rule_2$Catalog.To.Send <- 'Housewares.Division.Division'

Final_List <- rbind(ass_rule_1,ass_rule_2)
Final_List <- Final_List[,c(1,11)]
head(Final_List)
tail(Final_List)

write.csv(Final_List,"CustomerList.csv",row.names=F)
