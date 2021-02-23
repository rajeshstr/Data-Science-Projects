data <- read.csv('ReproRes/repdata_data_StormData.csv',header=T)
dim(data)

stormData <- data[,c(8,23:28)]
dim(stormData)
head(stormData)
str(stormData)

table(stormData$PROPDMGEXP)
table(stormData$CROPDMGEXP)

library(dplyr)
library(reshape2)

names(stormData)

stormData <- stormData %>% mutate(CROPDMG = CROPDMG * case_when(
  CROPDMGEXP == 'B' ~ 10^9,
  CROPDMGEXP == 'K' | CROPDMGEXP == 'k' ~ 10^3,
  CROPDMGEXP == 'M' | CROPDMGEXP == 'm' ~ 10^6,
  CROPDMGEXP == 2 ~ 2,
  TRUE ~ 0
),CROPDMGEXP = NULL)

stormData <- stormData %>% mutate(PROPDMG = PROPDMG * case_when(
  PROPDMGEXP == 'B' ~ 10^9,
  PROPDMGEXP == 'K' | PROPDMGEXP == 'k' ~ 10^3,
  PROPDMGEXP == 'M' | PROPDMGEXP == 'm' ~ 10^6,
  PROPDMGEXP == 'H' | PROPDMGEXP == 'h' ~ 10^2,
  PROPDMGEXP == 1 ~ 1,
  PROPDMGEXP == 2 ~ 2,
  PROPDMGEXP == 3 ~ 3,
  PROPDMGEXP == 4 ~ 4,
  PROPDMGEXP == 5 ~ 5,
  PROPDMGEXP == 6 ~ 6,
  PROPDMGEXP == 7 ~ 7,
  PROPDMGEXP == 8 ~ 8,
  TRUE ~ 0
),PROPDMGEXP = NULL)

head(stormData)

stormData_v1 <- stormData %>% group_by(EVTYPE) %>% summarize(damage = sum(FATALITIES,INJURIES))
stormData_v1 <- as.data.frame(stormData_v1)
stormData_v1 <- stormData_v1[order(-stormData_v1$damage),]

barplot(stormData_v1[1:10,'damage'],names.arg=stormData_v1[1:10,'EVTYPE'],las=2,col="Violet", 
        ylab="Damage", main="Top 10 EVTYPES affecting health",cex.names = 0.7)


stormData_v2 <- melt(stormData,id = 'EVTYPE', variable.name = 'CATEGORY', 
                   value.name = 'VALUE', na.rm = T)
stormData_v2 <- aggregate(VALUE~EVTYPE + CATEGORY,stormData_v2[stormData_v2[,'CATEGORY'] %in% c('CROPDMG','PROPDMG'),],sum)
stormData_v2 <- stormData_v2[order(-stormData_v2$VALUE),]
stormData_v2[1:20,]

library(ggplot2)
ggplot(stormData_v2[1:20,], aes(EVTYPE, VALUE/10^9,colour=factor(CATEGORY))) + 
  geom_bar(stat="identity") + 
  facet_grid(CATEGORY ~ .) +
  labs(title = "Top 20 EVTYPES with economic consequences", y = "Damage-$B") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
