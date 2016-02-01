# This Application allows the user to input a date, and uses 1996-2000
# data from NOAA's National Climatic Data Center to predict
# the mean, minimum, and maximum temperature, as well as displaying the
# historical minimum and maximum temperatures for those years.

require(shiny)
require(lubridate)

shinyUI(
  fluidPage(
    titlePanel(title = "Washington, DC: Predicted Temperature", windowTitle = "DC Temperature"),
    column(width=12, inputPanel(
      dateInput(inputId="userDay", label="Date input: yyyy-mm-dd", value=today()),
      submitButton(text = "Show Predictions"), 
      helpText("This application predicts the temperature in Washington, DC based on 1996-2016 NOAA data.",
               "Input a date to see the predicted mean, minimum, and maximum temperatures",
               "as well as historic minimum and maximum temperatures in Celsius and Fahrenheit.")
    )),
    column(width=12, wellPanel(plotOutput("tempPlot")))
  )
)
