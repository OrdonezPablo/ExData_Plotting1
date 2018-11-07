library(readr)
household_power_consumption <- read_delim(
    "C:/Users/pordo/Documents/Coursera/Data_Science/Exploratory Data Analysis/Assig1/exdata%2Fdata%2Fhousehold_power_consumption/household_power_consumption.txt", 
    ";", 
    escape_double = FALSE,
    col_types = cols(Time = col_time(format = "%H:%M:%S")),
    trim_ws = TRUE)

household_power_consumption$Date <- as.Date(household_power_consumption$Date,"%d/%m/%Y")

class(household_power_consumption$Date)
class(household_power_consumption$Time)


library(tidyverse)
household_p_c <- 
    household_power_consumption %>%
    mutate(Year = year(Date)) %>%
    mutate(Month = month(Date)) %>%
    mutate(Day = day(Date)) %>%
    filter(Year == 2007 & Month == 2) %>%
    filter( Day %in% c("1", "2"))

rm(household_power_consumption)

with(household_p_c, 
     hist(Global_active_power, 
          col = "red", 
          main = "Global Active Power",
          xlab="Global Active Power (kilowatts)", 
          ylab="Frequency" )
     )

dev.copy(png, file = "plot1.png", width=480, height=480)

dev.off()
