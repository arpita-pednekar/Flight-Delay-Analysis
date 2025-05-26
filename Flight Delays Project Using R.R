
#Install the required packages
install.packages("readxl")
library(readxl)

#Read the dataset
flight_delays <- read_excel("C:/Users/Alexa/Downloads/1657873325_flightdelays.xlsx")
View(flight_delays)

#Understand the data
str(flight_delays)

#Find out the null values
colSums(is.na(flight_delays))

#Understand the summary of descriptive statistics
summary(flight_delays)

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("plotly")
library(plotly)

#Plot the histograms to understand the relationships between scheduled time, carrier, 
#destination, origin, weather, and day of the week

for (var in c("schedtime", "carrier","dest","origin","weather","dayweek")){
  p <- plot_ly(flight_delays, x= ~get(var),type="histogram", nbinsx=30, 
               marker = list(color = 'green', line = list(color = 'black', width = 1))) %>%
    layout(title = list(text = paste("Histogram of", var), font = list(size = 18)),
           xaxis = list(title = var, titlefont = list(size = 14)),
           yaxis = list(title = "Count", titlefont = list(size = 14)),
           margin = list(l = 60, r = 30, b = 80, t = 80),
           plot_bgcolor = "#f9f9f9",
           paper_bgcolor = "#f9f9f9")
  print(p)
  readline(prompt = "Press [Enter] to continue...")
}

#Plot the scatter plot for flights on time and delayed
plot_ly(data = flight_delays, x = ~schedtime, y = ~deptime, type = 'scatter', mode = 'markers',
        color = ~delay, colors = c("red", "blue"), # red = delayed, blue = ontime
        marker = list(size = 6)) %>%
  layout(title = "Scatter plot of Scheduled vs Departure Time",
         xaxis = list(title = "Scheduled Time"),
         yaxis = list(title = "Departure Time"))


#Plot the box plot to understand how many days in a month flights are delayed by what time

plot_ly(flight_delays, x = ~as.factor(daymonth), y = ~deptime, color = ~delay, type = "box") %>%
  layout(title = list(text = "Box plot for flight delays by day of the Month"),
    xaxis = list(title = "Day of Month"),
    yaxis = list(title = "Departure Time"),
    boxmode = "group",
    plot_bgcolor = "#f9f9f9",
    paper_bgcolor = "#f9f9f9"
  )

#Define the hours of departure


# Step 1: Convert departure time to character
flight_delays$deptime_char <- as.character(flight_delays$deptime)

# Step 2: Pad with leading zeros to ensure 4-digit times (e.g., 755 → "0755")
flight_delays$deptime_padded <- stringr::str_pad(flight_delays$deptime_char, width = 4, pad = "0")

# Step 3: Extract the hour part (first two characters)
flight_delays$dephour <- substr(flight_delays$deptime_padded, 1, 2)

# Step 4: Convert hour to numeric for plotting or analysis
flight_delays$dephour <- as.numeric(flight_delays$dephour)



#Create a categorical representation of data using a table

table(flight_delays$delay)
table(flight_delays$dayweek, flight_delays$delay)
table(flight_delays$carrier, flight_delays$delay)
addmargins(table(flight_delays$carrier, flight_delays$delay))


#DH had the highest number of flights (551), and also the highest number of delays (137).

#RU also had a relatively high delay count (94).

#OH and UA had very few flights and delays.

#Out of 2201 total flights, 428 were delayed, which is about 19.4%.

428/2201*100


# Redefine the delay variables

flight_delays$delay <- ifelse(flight_delays$delay == "ontime", 0, 1)


# Understand the summary of major variables

major_variables <- c('schedtime', 'deptime', 'distance', 'weather', 'dayweek', 'daymonth', 'delay')
summary(flight_delays[, major_variables])


#Plot histograms of major variables
for (var in c('schedtime', 'deptime', 'distance', 'weather', 'dayweek', 'daymonth', 'delay')) {
  p <- plot_ly(flight_delays, x = ~get(var), type = "histogram", nbinsx = 30) %>%
    layout(title = paste("Histogram of", var))
  print(p)
  readline(prompt = "Press [Enter] to continue...")
}



# Recalculate the delay_counts to ensure correct order
delay_counts <- table(flight_delays$delay)

# Convert names (0, 1) to meaningful labels
labels <- c("On Time", "Delayed")

# Create pie chart using numeric values and proper labels
plot_ly(labels = labels, values = as.numeric(delay_counts), type = 'pie') %>%
  layout(title = "Distribution of Flight Delays")

