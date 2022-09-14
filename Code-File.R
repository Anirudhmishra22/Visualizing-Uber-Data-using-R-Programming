#Dashboard: Piechart, heatmap, linegraph, barcharts, pollution level graphs

#1
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

#2
colors1 = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
colors2 = c("#3edbf0", "#04009a", "#77acf1", "#c0fefc", "#e84545", "#f05945", "#007580")
colors3 = c("#a2b29f", "#194350", "#822659", "#ff75a0", "#fce38a", "#95e1d3", "#ff8882")

#3
apr_data <- read.delim(file = "D:\\Career\\Codes\\R Programming\\Visualizing Uber Data using R Programming\\Datasets\\April Data.csv", header=TRUE,sep=",")
apr_data %>% View()
may_data <- read.delim(file= "D:\\Career\\Codes\\R Programming\\Visualizing Uber Data using R Programming\\Datasets\\May Data.csv",header=TRUE,sep=",")
may_data %>% View()
jun_data <- read.delim(file= "D:\\Career\\Codes\\R Programming\\Visualizing Uber Data using R Programming\\Datasets\\June Data.csv",header=TRUE,sep=",")
jun_data %>% View()
jul_data <- read.delim(file= "D:\\Career\\Codes\\R Programming\\Visualizing Uber Data using R Programming\\Datasets\\July Data.csv",header=TRUE,sep=",")
jul_data %>% View()
aug_data <- read.delim(file= "D:\\Career\\Codes\\R Programming\\Visualizing Uber Data using R Programming\\Datasets\\August Data.csv",header=TRUE,sep=",")
aug_data %>% View()
sep_data <- read.delim(file= "D:\\Career\\Codes\\R Programming\\Visualizing Uber Data using R Programming\\Datasets\\September Data.csv",header=TRUE,sep=",")
sep_data %>% View()

data_2014 <- rbind(apr_data, may_data, jun_data, jul_data, aug_data, sep_data)
data_2014 %>% View()
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Date.Time %>% View()
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Time %>% View()
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)
data_2014$Date.Time %>% View()
data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$day %>% View()
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$month %>% View()
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$year %>% View()
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))
data_2014$dayofweek %>% View()

#4
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$hour
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$minute
data_2014$second <- factor(second(hms(data_2014$Time)))
data_2014$second

#5
hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
hour_data %>% View()
datatable(hour_data)

#6
ggplot(hour_data, aes(hour, Total*537/24)) + 
  geom_bar( stat = "identity", fill = "blue", color = "green") +
  ggtitle("Pollution Levels Every Hour (24H format)") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total*537/24, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Pollution Levels Every Hour in each Month (24H format)") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors1)

#7
day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

#8
ggplot(day_group, aes(day, Total*537)) + 
  geom_bar( stat = "identity", fill = "#f8a1d1", color = "#822659") +
  ggtitle("Pollution Levels Every Day (24H format)") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#9
day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total*537, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Pollution Levels Every Day in each Month (24H format)") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors3) 
  
#10
month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)

#11
ggplot(month_group, aes(month, Total*537*31, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Pollution Levels Every Month (24H format)") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors2)

#12
month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors3)

#13
ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

#14
x <- (unique(apr_data$Base))
y <- (apr_data$Base)
for (i in 1:5)
{
  count <- 0
  for (j in 1:564516)
  {
    if (y[j]==x[i])
    {
      count <- count+1
    }
  }
  print(paste(x[i],count))
}

a <- c(35536, 183263, 108001, 227808, 9908)
labels <- c("John F.Kennedy Airport", "Times Square", "Central Park", "Empire State Building", "Statue of Liberty")
colors1 = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840")
piepercent<- round(100*a/sum(a), 1)
pie(a, labels = piepercent, main = "Number of Trips by Bases (April)", col = colors1)
legend("topright", c("John F.Kennedy Airport", "Times Square", "Central Park", "Empire State Building", "Statue of Liberty"), 
       cex = 0.6, fill = colors1)
 
ggplot(hour_data, aes(hour, Total, group=1)) + 
  geom_line(linetype="dashed", color="red") +
  ggtitle("Trips Every Hour") +
  geom_point() +
  scale_y_continuous(labels = comma)

#15
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

ggplot(month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips Every Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors2)

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors2)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "#f8a1d1", color = "#822659") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

ggplot(hour_data, aes(hour, Total, group=1)) + 
  geom_line(linetype="dashed", color="red") +
  ggtitle("Trips Every Hour") +
  geom_point() +
  scale_y_continuous(labels = comma)


