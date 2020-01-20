
library(rjson)
library(jsonlite)
library(dplyr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)

df <- fromJSON("WaterQuality.json") %>% flatten(recursive = TRUE)
df[is.na(df)] <- "0"
df$amount <- as.numeric(df$amount)
df <- df %>% filter(county != "0", amount!=0)
df$dt <- as.POSIXct(df$date_time, format("%Y-%m-%dT%H:%M:%S"), tz = "MST")
df$tm <- format(df$dt, "%H:%M:%S")
df$dt <- as.character(date(df$dt))

station_type <- c("All", unique(df$station_type))
county <- c("All", sort(unique(df$county)))
date_time <- c("All", unique(df$date_time))
dt <- c("All", unique(df$dt))
tm <- c("All", unique(df$tm))


shinyUI(fluidPage(
    
    titlePanel("Water Quality Report"),
    sidebarLayout(
        sidebarPanel(
            selectInput("Station_Type",
                        label = h6("Select Station Type"),
                        choices = station_type,
                        selected = station_type[1],
                        multiple = FALSE),
            
            selectInput("County",
                        label = h6("Select the County"),
                        choices = county,
                        selected = county[1],
                        multiple = FALSE),
            
            selectInput("Date",
                        label = h6("Select the Date"),
                        choices = dt,
                        selected = dt[1],
                        multiple = FALSE),
            
            selectInput("Time",
                        label = h6("Select the Time"),
                        choices = tm,
                        selected = tm[1],
                        multiple = FALSE),
            width = "3"
        ),
        
        mainPanel(plotlyOutput("plotData1"),
                  plotlyOutput("plotData2"),
                  plotlyOutput("plotData3")
        )
    )
    
))
