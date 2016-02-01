# This Application allows the user to input a date, and uses 1996-2000
# data from NOAA's National Climatic Data Center to predict
# the mean, minimum, and maximum temperature, as well as displaying the
# historical minimum and maximum temperatures for those years.

# 1) Load Libraries
require(shiny)
require(ggplot2)
require(dplyr)
require(lubridate)
require(scales)

# 2) Process DC National Airport Climate Data 1996-2016
# from National Airport Station Global Historical Climatology Network (GHSN)
# http://ncdc.noaa.gov
# Fields:
# DATE - YYYYMMDD format
# TMAX - Maximum temperature in Celsius degrees to tenths
# TMIN - Minimum temperature in Celsius degrees to tenths
dc.raw <- read.csv("dc_air_temperature_1996-2016.csv")

# 3) Create columns for mean, min, and max, and day of year
dc <- dc.raw %>% 
  mutate(DATE=ymd(DATE)) %>%
  mutate(Year=year(DATE), Month=month(DATE), Day=day(DATE)) %>%
  mutate(Day.of.year=yday(DATE)) %>%
  mutate(Temp.C.max=TMAX/10, Temp.C.min=TMIN/10) %>%
  mutate(Temp.C.mean=(Temp.C.max+Temp.C.min)/2)

# 4) We want a single dataframe with 366 rows, one for each day of the
# year.  First, we calculate the minimum minimum and maximum maximum for
# each day of the year
superlative.days <- data.frame()
for (d in 1:366) {
  day.to.consider <- dc[dc$Day.of.year==d,]
  max.day <- day.to.consider[which.max(day.to.consider$TMAX),]
  min.day <- day.to.consider[which.min(day.to.consider$TMAX),]
  superlative.days <- rbind(superlative.days, 
                            data.frame(Day.of.year=d,
                                       Month=month(max.day$Month, label=TRUE), Day=max.day$Day,
                                       Historic.max.day=max.day$DATE,
                                       Historic.max=max.day$Temp.C.max,
                                       Historic.min.day=min.day$DATE,
                                       Historic.min=min.day$Temp.C.max
                            ))
}

# 5) Now, we calculate the mean minimum, maximum, and mean temperatures for
# each day of the year.
average.days <- dc %>%
  group_by(Day.of.year) %>%
  summarize(mean.max=mean(Temp.C.max), 
            mean.min=mean(Temp.C.min), 
            mean.mean=mean(Temp.C.mean)) %>%
  select(Day.of.year, mean.max, mean.min, mean.mean)

# 6) We now merge these superlative and average days into a single dataframe
merged.days <- merge(average.days, superlative.days)

# Here are some utility functions:
# Celsius to Fahrenheit converter
fahr <- function(C) {
  C * 9/5 + 32
}

# Breaks for the beginnings of each month
month.breaks <- yday(as.Date(paste("2016", sprintf("%02d",1:12), "01", sep="-")))
month.labels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# 7) Finally, here is the actual Shiny Server which created the predictions
# based on the date.  We use ggplot2 to output the prediction.
shinyServer(function(input, output) {
  output$tempPlot <- renderPlot({
    yd <- yday(as.Date(input$userDay))
    display <- merged.days %>% filter(Day.of.year==yd) %>%
      select(Month, Day, mean.mean, mean.max, mean.min, 
             Historic.max.day, Historic.max, 
             Historic.min.day, Historic.min)
    
    msg <- paste(
      input$userDay, "\n",
      "Predicted Temp:", round(display$mean.mean,1), "C /", round(fahr(display$mean.mean),1), "F\n",
      "Predicted Range:", round(display$mean.min,1), "to", round(display$mean.max,1), "C /",
      round(fahr(display$mean.min),1), "to", round(fahr(display$mean.max),1), "F\n",
      "Historic Minimum:", round(display$Historic.min,1), "C /",
      round(fahr(display$Historic.min),1),"F on", display$Historic.min.day, "\n",
      "Historic Maximum:", round(display$Historic.max,1), "C /",
      round(fahr(display$Historic.max),1),"F on", display$Historic.max.day, "\n",
      "(1996-2016 Data from http://ncdc.noaa.gov)"
    )
    
    ggplot(merged.days) +
      geom_point(aes(x=Day.of.year, y=Historic.max), pch=21, fill="white", color="red") + 
      geom_point(aes(x=Day.of.year, y=Historic.min), pch=21, fill="white", color="blue") + 
      geom_errorbar(aes(ymin=mean.min, ymax=mean.max, position=mean.mean, x=Day.of.year, color=mean.mean)) +
      geom_line(aes(x=Day.of.year, y=mean.mean), color="black", lwd=2) +
      geom_vline(xintercept=yd, color="green", lwd=2) +
      geom_text(aes(x=150, y=10, label=msg, vjust="outward", hjust="inward")) +
      scale_y_continuous(name="Temperature (C)") +
      scale_x_continuous(name="Day of the Year", breaks=month.breaks, labels=month.labels) +
      scale_color_gradient2(low=muted("blue"),mid=muted("blue"),high=muted("red")) +
      guides(color=FALSE) +
      theme_bw()
  })
})