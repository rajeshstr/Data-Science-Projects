library(datasets)
data <- datasets::mtcars
head(data)
dim(data)

str(data$am) # Data type of am is Numeric. Converting to factor
data$am <- as.factor(data$am)
levels(data$am) <- c('automatic','manual') # Renaming the levels as 'automatic' & 'manual'

#Exploratory Data Analysis
### Histogram
hist(data$mpg, breaks = 12,
     xlab='Miles per Gallon (MPG)', main = 'Distribution of MPG', col = 'Red')

### Box Plot
library(ggplot2)
names(data)
box_plot <- ggplot(data = data, aes(x = am, y = mpg))
box_plot <- box_plot + geom_boxplot(aes(fill=am))
box_plot <- box_plot + labs(x = 'Transmission Type', y = 'Miles per Gallon',
                            title = 'MPG by Transmission type')
box_plot <- box_plot + theme(plot.title = element_text(color="grey25", face="bold",hjust=0.5))
box_plot <- box_plot +  scale_fill_manual(values=c("firebrick","deepskyblue")) 
box_plot

# T-test
data_auto <- data[data$am == 'automatic', ]$mpg
data_man <- data[data$am == 'manual', ]$mpg

t.test(data_auto,data_man)

paste('Median of Automatic Cars is: ',print(median(data_auto)))
paste('Median of Manual Cars is: ',print(median(data_man)))

# Linear Regression
model_v1 <- lm(mpg~am, data=data)
summary(model_v1)
model_v1$coefficients
print('The coefficients of the model: ')
model_v1$coefficients

# Multiple Linear Regression
model_v2 <- lm(mpg~., data=data)
summary(model_v2)

# Stepwise regression
install.packages('MASS')
library(MASS)
model_v3 <- stepAIC(model_v2, direction = 'both', trace = F)
summary(model_v3)

### ANOVA
anova(model_v1, model_v3)

# Residual Plots
plot(model_v3)
