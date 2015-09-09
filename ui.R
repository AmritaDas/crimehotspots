library(shiny)
#install.packages("shiny")

shinyUI(fluidPage(
  titlePanel(title="Crime Hotspot Detection in City of Raleigh"),
  #h2(code("NOTE: wait till the first graph loads......")),
  #headerPanel(title="Crime Hotspot Detection in City of Raleigh2"),
  
  sidebarLayout(position="right",    
                sidebarPanel(
                  ("TOP 10 Crime Type Trends"),
                  selectInput("plot","Select the trend type: CrimeCount(log)",
                              choices = c("VsYear","VsMonth","VsWeekday","VsHour"),
                              selected="VsYear",selectize = TRUE),
                  ("Crime probabilty Density plot"),
                  selectInput("den","Select the crime type",
                              choices = c("ASSAULT","WEAPON","MURDER","BURGLARY / THEFT",
                                          "STOLEN PROPERTY","JUVENILE","SEX OFFENSE","ALCOHOL ABUSE",
                                          "ARSON","HUMANE","DRUGS RELATED",
                                          "DAMAGE TO PROPERTY",
                                          "DISORDERLY CONDUCT","FRAUD",
                                          "FORGERY","GAMBLING",
                                          "LARCENY","CHILD ABUSE","FALSE REPORT",
                                          "All Others"),
                              selected="ASSAULT",selectize = TRUE)),
                mainPanel(
                  h4("Trend Plot:"),
                  h5(code("NOTE: wait till the plot loads......")),
                  plotOutput("plot1"),
                  h4("Crime Hotspot Density Plot"),
                  h5(code("NOTE: wait till the graph loads......")),
                  imageOutput("den1")
                  )
                )
  )
)