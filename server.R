#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(gridExtra)

library(rjson)
library(jsonlite)
library(dplyr)
library(data.table)
library(lubridate)

df <- fromJSON("WaterQuality.json") %>% flatten(recursive = TRUE)
df[is.na(df)] <- "0"

df$amount <- as.numeric(df$amount)
df <- df %>% filter(county != "0", amount != 0) 

df$dt <- as.POSIXct(df$date_time, format("%Y-%m-%dT%H:%M:%S"), tz = "MST")
df$tm <- format(df$dt, "%H:%M:%S")
df$dt <- as.character(date(df$dt))

filteredDF <- df %>% select(station_type, county, amount, date_time, dt, tm, `:@computed_region_nku6_53ud`)
plotDF <- filteredDF 

shinyServer(function(input, output, session) {
    
    pltData <- reactive({
        
        
        if (input$Station_Type != "All") {
            plotDF <- plotDF %>% filter(station_type == input$Station_Type)
        }
        if (input$County != "All") {
            plotDF <- plotDF %>% group_by(station_type, county, date_time, dt, tm) %>% summarize(amount = sum(amount)) %>% filter(county == input$County)
        }
        if (input$Date != "All") {
            plotDF <- plotDF %>% filter(dt == input$Date)
        }
        if (input$Time != "All") {
            if (input$Date != "All") {
                plotDF <- plotDF %>% filter(dt == input$Date, tm == input$Time) }
            else { 
                plotDF <- plotDF %>% group_by(tm) %>% slice(1)  }
        }
        plotDF
    })
    
    plot1 <- reactive({
        plotDF <- plotDF %>% group_by(station_type) %>% summarize(amount = sum(amount))
        ggplotly(ggplot(plotDF, aes(x=station_type, y=amount/1000, fill=station_type)) + 
                     geom_bar(stat="identity", color="black") +
                     ggtitle("Total Discharge by Station Type") + ylab("Amount") + xlab("Station Type"))
    })
    
    plot2 <- reactive({
        pd <- pltData() %>% group_by(station_type, county) %>% summarize(amount = sum(amount))
        if (input$County != "All") {
            pd <- pd %>% filter(county == input$County) }
        ggplotly(ggplot(pd, aes(x=county, y=amount, fill=station_type)) + geom_bar(stat = "identity") +
                     ggtitle("Discharge by County") + ylab("Amount") + xlab("County"))
    })
    
    plot3 <- reactive({
        if (input$Time!="All") {xlabel = (pltData()$date_time)} 
        else {xlabel = (pltData()$dt)}
        p <- ggplot(pltData(), aes(x=date_time, y=amount, fill=station_type)) + geom_bar(stat = "identity") +  
            theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
            ggtitle("Discharge by Date and Time") + xlab(xlabel) + ylab("Amount")
        ggplotly(p)
        
    })
    
    
    output$plotData1 <- renderPlotly({
        plot1()
    })
    
    output$plotData2 <- renderPlotly({
        plot2()
    })
    
    output$plotData3 <- renderPlotly({
        plot3()    
    })
    
    observeEvent(input$Station_Type, {
        if (input$County != "All") {
            updateSelectInput(session, 'Date', choices = c("All", unique(filteredDF$dt[filteredDF$county==input$County])))
            updateSelectInput(session, 'Time', choices = c("All", unique(filteredDF$tm[filteredDF$county==input$County]))) }
        else {
            updateSelectInput(session, 'Date', choices = c("All", unique(pltData()$dt))) 
            updateSelectInput(session, 'Time', choices = c("All", unique(pltData()$tm))) }
    }, ignoreInit = TRUE)
    
    observeEvent(input$County, {
        if (input$County != "All") {
            updateSelectInput(session, 'Station_Type', choices = c("All", unique(filteredDF$station_type[filteredDF$county==input$County])))
            updateSelectInput(session, 'Date', choices = c("All", unique(filteredDF$dt[filteredDF$county==input$County])))
            updateSelectInput(session, 'Time', choices = c("All", unique(filteredDF$tm[filteredDF$county==input$County]))) }
        else {
            updateSelectInput(session, 'Station_Type', choices = c("All", unique(pltData()$station_type))) 
            updateSelectInput(session, 'Date', choices = c("All", unique(pltData()$dt))) 
            updateSelectInput(session, 'Time', choices = c("All", unique(pltData()$tm))) }
    }, ignoreInit = TRUE)
    
    observeEvent(input$Date, {
        if (input$County != "All") {
            updateSelectInput(session, 'Time', choices = c("All", sort(unique(filteredDF$tm[filteredDF$county==input$County & filteredDF$dt == input$Date]))))}
        else {
            updateSelectInput(session, 'Time', choices = c("All", unique(pltData()$tm)))} 
    }, ignoreInit = TRUE)
    
})
