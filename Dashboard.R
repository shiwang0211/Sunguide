library(leaflet)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
source('global.R')
library(htmltools)
library(shinythemes)
library(shinydashboard)


# UI -------------------------------------------------- 

ui <- dashboardPage(skin="black",
                    
  dashboardHeader(title = "FDOT TSS Detector"),
  
  dashboardSidebar(width=300,
    sidebarMenu(id = "sidebarMenu",
      menuItem("List of Criterias", tabName="Criterias", icon=icon("dashboard")),  
      menuItem("ByLocation", tabName="ByLocation", icon=icon("dashboard")),
      menuItem("ByLocationReport", tabName="ByLocationReport", icon=icon("dashboard")), 
      menuItem("Configuration", tabName="Configuration", icon=icon("dashboard")), 
      menuItem("Threshold", tabName="Threshold", icon=icon("dashboard")), 
      menuItem("ByDetector", tabName="ByDetector", icon=icon("dashboard")),
      menuItem("ByCriteriaAndTime", tabName="ByCriteria", icon=icon("dashboard")),
      
      conditionalPanel(
        "input.sidebarMenu=='ByCriteria'",
        sliderInput("binsize",
                    label = "Binsize",
                    min = 100, max = 400, value = 250),
        selectInput("date",
                    label = "Choose a Date",
                    choices = Dates,
                    selected = Dates[1])
      ),
      
      conditionalPanel(
        "input.sidebarMenu=='ByDetector'",
        sidebarSearchForm(textId = "SearchDetectorText_BD", buttonId = "SearchDetector_BD", label = "Search Detector ID ..."),
        selectInput("percent",
                    label = "Show Number or Percent in Chart",
                    choices = c("Number", "Percent"),
                    selected = "Number"),
        selectInput("detector_ID", "Select Detector for Analysis",
                    UniqueValidDetectors)
      ),

      conditionalPanel(
        "input.sidebarMenu=='Threshold'",
        sliderInput("threshold",
                    label = "threshold for the number of flags/tickets in one hour",
                    min = 0, max = 400, value = 250),
        checkboxInput("AggToDetector",
                      label = "For detectors with multiple flagged lanes, only count the flags once (per 30sec)")
      ),
      
      conditionalPanel(
        "input.sidebarMenu=='ByLocation'",
        sidebarSearchForm(textId = "SearchDetectorText_BL", buttonId = "SearchDetector_BL", label = "Search Detector ID ..."),
        selectInput("CriteriaNo", "Select Criteria Number for Analysis",
                    c("flag",CriteriaCols),selected = "flag"),
        selectInput("DetectorNo", "Select Detector for Analysis (Or Select From Map)",
                    UniqueValidDetectors),
        checkboxInput("NumberOrPercent",
                      label = "Show Percent of Flagged Records instead of Numbers"),
        checkboxInput("ByLocationBar",
                      label = "Show Bar Chart for Each Criteria for Selected Detector"),
        checkboxInput("ByLocationTable",
                      label = "Show Data Table for flagged Records for Selected Detector")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Criterias", 
              imageOutput("CriteriaImage")
      ),
      tabItem(tabName = "ByCriteria", 
              box(title="ByCriteria",status="primary",solidHeader=TRUE,plotlyOutput("fig_BC")),
              box(title="ByTime",status="primary",solidHeader=TRUE,plotlyOutput("fig_BT"))
      ),
      
      tabItem(tabName = "ByDetector", 
              box(title="Number of flags Chart",status="primary",solidHeader=TRUE,width=400,plotlyOutput("fig_BD")), 
              box(title="Table for Selected Detector",status="primary",solidHeader=TRUE,width=400,dataTableOutput("table_BD"))
      ),
      
      tabItem(tabName = "Threshold",
              box(checkboxInput("scatterOffThreshold",label = "Turn Off Scatter Plot to Reduce Computation Time"),width=400, selected=TRUE),
              fluidRow(
                box(title="Number of flagged records within one hour for one detector",status="primary",collapsible = TRUE,solidHeader=TRUE,plotlyOutput("fig_Threshold_0")), 
                box(title="Number of flagged records within one hour for one detector",status="primary",collapsible = TRUE,solidHeader=TRUE,plotlyOutput("fig_Threshold_1"))
              ),
              box(title="Number of hours with # of flags exceeding threshold",status="primary",solidHeader=TRUE,width=400,plotlyOutput("fig_Threshold_2")), 
              #verbatimTextOutput("text_threshold"),
              box(title="",width=400,dataTableOutput("table_threshold"))
      ),
      tabItem(tabName = "ByLocation",
        sliderInput("datetime", "Select Hour/Day for Analysis",width=1000,
                    min = minDatetime,
                    max = maxDatetime,
                    value = minDatetime+3600,
                    animate = animationOptions(interval=200)),
        leafletOutput("map",width="100%",height="650px"),
        textOutput('Click_text'),
        conditionalPanel(
          condition = "input.ByLocationBar == true",
          plotlyOutput("barchart_BL")
        ),
        conditionalPanel(
          condition = "input.ByLocationTable == true",
          dataTableOutput("table_BL")
        )
      ),

      tabItem(tabName = "ByLocationReport", 
              downloadButton('table_BL_Report_DL', 'Download Report Table'),
              dataTableOutput("table_BL_Report"),
              verbatimTextOutput("text_BL_Report")
      ),
      tabItem(tabName = "Configuration", 
              dataTableOutput("table_Config")
      )
    )
  )
)


server <- function(input, output, session) {
  

# ByCriteria   #input$date --------------------------------------------------
  
  output$fig_BC<- renderPlotly({ 
  
    Data <- CombinedDataByCriteria$flag[which(CombinedDataByCriteria$date == input$date)]  
    p <- plot_ly(x = Data, type = "histogram", autobinx = F, xbins = compute_bins(Data, input$binsize), marker = m2)%>% 
    layout(
        title='',
        xaxis=list(
          title='# of Flagged Records for one detector in one day', 
          showgrid = F
        ),
        yaxis=list(
          title='Frequency',
          showgrid = T
        ),
        barmode='overlay',
        bargap=0.25
      )
    p
  })

# ByTime   #input$date --------------------------------------------------  
    
  output$fig_BT<- renderPlotly({
    
    Data <- AggDataByTime[which(AggDataByTime$date == input$date),]  
    p <- plot_ly(x = Data$hour, y = Data$flagged, type = "bar")%>% 
      layout(
        title='',
        xaxis=list(
          title='Hour of Day'
        ),
        yaxis=list(
          title='% of flagged records for all detectors in one day'
        )
      )
    p
  })

  

# ByDetector   #input$date, input$detector_ID, input$percent -------------------------------------------------- 
  
  observeEvent(input$SearchDetector_BD,{
    updateSelectInput(session, "detector_ID","Select Detector for Analysis",
                      UniqueValidDetectors,selected = as.numeric(input$SearchDetectorText_BD))
  })
  
  output$fig_BD<- renderPlotly({
    
    Data <- AllDetectorData[which(AllDetectorData$detector_id == input$detector_ID & AllDetectorData$hour != "0" & AllDetectorData$hour != "24"),] 
    Data_percent = Data
    Data_percent[,c(CriteriaCols, "flag")] = Data[,c(CriteriaCols, "flag")] / Data$count
    
    if(input$percent == "Percent") data =  Data_percent
    if(input$percent == "Number") data =  Data
    
    p <- plot_ly(data, x = ~DateTime, y = ~flag, type = "scatter", mode = "lines", name = "flag")%>% 
      add_trace(y = data[,CriteriaCols[1]], name = CriteriaCols[1], mode = 'lines') %>%
      add_trace(y = data[,CriteriaCols[2]], name = CriteriaCols[2], mode = 'lines') %>%
      add_trace(y = data[,CriteriaCols[3]], name = CriteriaCols[3], mode = 'lines') %>%
      add_trace(y = data[,CriteriaCols[4]], name = CriteriaCols[4], mode = 'lines') %>%
      add_trace(y = data[,CriteriaCols[5]], name = CriteriaCols[5], mode = 'lines') %>%
      add_trace(y = data[,CriteriaCols[6]], name = CriteriaCols[6], mode = 'lines') %>%
      add_trace(y = data[,CriteriaCols[7]], name = CriteriaCols[7], mode = 'lines') %>%
      add_trace(y = data[,CriteriaCols[8]], name = CriteriaCols[8], mode = 'lines') %>%
      add_trace(y = data[,CriteriaCols[9]], name = CriteriaCols[9], mode = 'lines') %>%
      add_trace(y = data[,CriteriaCols[10]], name = CriteriaCols[10], mode = 'lines') %>%
      
      layout(
        title='',
        xaxis=list(
          title='Time By Hour'
        ),
        yaxis=list(
          title='# of flagged 30sec lane-level records for one detector',
          range= if (input$percent == "Percent") c(0,1) else c(0,1000)
        )
      )
    p
  })
  
  
# ByThreshold   # -------------------------------------------------- 
  
  Data_threshold<-reactive({
    Data <- subset(AllDetectorData, hour >= 7 & hour <=19) #time filter
    Data$FLAG = if (input$AggToDetector == TRUE) Data$flag_whole_detector else Data$flag # agg to detector or not
    Data$temp=c(1:nrow(Data))
    Data$MonthDay = format(Data$DateTime, "%m%d")
    return (Data)
  })
  
  output$fig_Threshold_0<- renderPlotly({
    if(input$scatterOffThreshold==FALSE){
    Data = Data_threshold()
    p <- plot_ly(Data, x = ~temp, y= ~FLAG, type = "scatter", text =paste("DetectorID:",Data$detector_id, "Date/Time:",Data$DateTime),marker=list(size=3), showlegend=FALSE)%>% 
      add_trace(x =c(1:nrow(Data)), y= rep(input$threshold, nrow(Data)),mode = "lines", name="threshold_line")%>%
      layout(
        title='',
        xaxis=list(
          title='index of combination "detector and hour/day"', 
          showgrid = F
        ),
        yaxis=list(
          title='Number of flagged records (30sec lane-level)',
          showgrid = T
        )
      )
    p
    }
  })  

  
  output$fig_Threshold_1<- renderPlotly({
    
    Data = Data_threshold()
    p <- plot_ly(x = Data$FLAG, type = "histogram",showlegend=FALSE)%>% 
      add_trace(x = c(input$threshold, input$threshold), y= c(0,20000), type="scatter",mode = "lines", name="threshold_line")%>%
      layout(
        title='',
        xaxis=list(
          title='Number of flagged record (lane-level)', 
          showgrid = F
        ),
        yaxis=list(
          title='Frequency',
          showgrid = T
        ),
        barmode='overlay',
        bargap=0.25
      )
    p
  })  
  
  
  output$fig_Threshold_2<- renderPlotly({
    
    Data = Data_threshold()
    Data = subset(Data,FLAG >= input$threshold) #threshold filter
    agg1 = aggregate(Data$FLAG, by = list(MonthDay = Data$MonthDay, detector_id = Data$detector_id), FUN = length)
    agg2 = aggregate(Data$FLAG, by = list(MonthDay = Data$MonthDay, detector_id = Data$detector_id), FUN = sum)
    colnames(agg1)[3] = "flags"; colnames(agg2)[3] = "flags"
    
    p <- plot_ly(agg1, x = as.factor(agg1$detector_id), y = ~flags, type = "bar", color = ~MonthDay, text =paste("DetectorID:",agg1$detector_id), visible="TRUE") %>%
      layout(
        title='Please toggle the legend to select day',
        xaxis <- list(
          title='Index of detector'
        ),
        yaxis=list(
          title='Number of hrs exceeding threshold from 7AM to 7PM'
        )
      )
    p
  })  
  
  output$text_threshold <- renderText({
    content <- paste(sep = " ",
                     "Total Number of Detectors:",
                     length(UniqueValidDetectors))
  })
  
  output$table_threshold<- renderDataTable({
    Data = Data_threshold()
    agg1 = aggregate(Data$FLAG, by = list(MonthDay = Data$MonthDay, detector_id = Data$detector_id), FUN = length)
    colnames(agg1)[3] = "flags"
    agg1
  })
  
  
# ByLocationMap   #input$datetime,  DetectorCenter, Criteria No -------------------------------------------------- 
  
  Data_BL_Combo<-reactive({ # Depends on input$datetime, Criteria No and **ALL** detectors
   Data = subset(AllDetectorData, DateTime == input$datetime)
   DetectorLocFlag =  merge(x = DetectorCenter, y = Data, by.x ="DetectorID",by.y="detector_id") #inner join, all.x=True not included
   DetectorLocFlag$flag_percent =DetectorLocFlag[[input$CriteriaNo]] / DetectorLocFlag$count
   
   pal_percent <- colorNumeric(palette = "Blues",domain = DetectorLocFlag$flag_percent)
   pal_number <- colorNumeric(palette = "Blues",domain = DetectorLocFlag[[input$CriteriaNo]])
   
   if (input$NumberOrPercent == 1) {pal_pal = pal_percent; pal_values = DetectorLocFlag$flag_percent}
   if (input$NumberOrPercent == 0) {pal_pal = pal_number; pal_values = DetectorLocFlag[[input$CriteriaNo]]}
   
   content <- paste(sep = "<br/>",
                    paste("DetectorID:", DetectorLocFlag$DetectorID),
                    paste("DetectorName:", DetectorLocFlag$DetectorName),
                    paste("Number of flagged records:", DetectorLocFlag[[input$CriteriaNo]]),
                    paste("Percent of flagged records:", round(DetectorLocFlag$flag_percent * 100,0), "%"))
   Data_BL_Combo = list(DetectorLocFlag = DetectorLocFlag, pal_pal = pal_pal, pal_values = pal_values, content = content)
   Data_BL_Combo
 }) 

  output$map<- renderLeaflet({ # will not work with clicking markers if map name is "map1", Depends on input$datetime, Criteria No and ALL detectors
    Data_BL = Data_BL_Combo()
    req(Data_BL)
    leaflet(Data_BL$DetectorLocFlag) %>% addTiles() %>%
      addCircleMarkers(~MidLon, ~MidLat, color = Data_BL$pal_pal(Data_BL$pal_values), radius=5, layerId=~DetectorID, stroke = TRUE,fillOpacity = 1) %>%
      addLegend(pal = Data_BL$pal_pal, values = Data_BL$pal_values, labFormat = labelFormat(prefix = '', suffix = '', between = ', '))
  
  })   
  
  observeEvent(input$SearchDetector_BL,{  # different with eventReactive which returns a value instead of action
    updateSelectInput(session, "DetectorNo","Select Detector for Analysis (Or Select From Map)",
                      UniqueValidDetectors,selected = as.numeric(input$SearchDetectorText_BL))
  })
  
  observe({ # update detector id select box based on map selection
    req(input$map_marker_click$id)
    updateSelectInput(session, "DetectorNo","Select Detector for Analysis (Or Select From Map)",
                      UniqueValidDetectors,selected = input$map_marker_click$id
    )
  })
  
  observe({ #zoom in and highlight according to detector id select box, add popup box
    input$DetectorNo #reaction depends on these four inputs
    input$CriteriaNo
    input$datetime
    input$NumberOrPercent
    
    Data_BL = Data_BL_Combo() #extract detector-specific information
    Info = Data_BL$content[which(Data_BL$DetectorLocFlag$DetectorID == input$DetectorNo)]
    site <- subset(DetectorCenter,DetectorID==input$DetectorNo)
    
    isolate({
      new_zoom <- 10
      #bounds <- input$map_bounds
      if(!is.null(input$map_zoom)) new_zoom <- input$map_zoom #maintain map zoom level
      leafletProxy('map') %>%
        removePopup("previous") %>%  #delete previous popup box
        addCircles(lng = site$MidLon[1], lat = site$MidLat[1], radius = 50,layerId = "previous",
                   stroke = TRUE, color = "red", weight = 5, opacity = 1, fill = FALSE) %>%
        addPopups(lng = site$MidLon[1], lat = site$MidLat[1], popup = Info, layerId = "previous") %>%
        setView(lng = site$MidLon[1], lat = site$MidLat[1], zoom = new_zoom)
        #fitBounds(bound$east, bound$north, bound$west, bound$south)
    })
  })
  
  
# ByLocationBarChart   #input$datetime,  input$DetectorNo --------------------------------------------------
  
  output$barchart_BL <- renderPlotly({
    Data <- subset(AllDetectorData, DateTime == input$datetime & detector_id == input$DetectorNo)
    p <- plot_ly(
      x = CriteriaCols,
      y = array(Data[1,CriteriaCols]),
      name = "No. of records meeting any criteria",
      type = "bar"
    ) %>%
      layout(autosize=FALSE,height =300)
    p
  })
  
  
  
  
# ByLocationBarTable   #input$datetime,  input$DetectorNo, input$CriteriaNo --------------------------------------------------  
  
  output$table_BL<- DT::renderDataTable({
    
    time = strptime(input$datetime - 5*3600, "%Y-%m-%d %H:%M:%S") # to fix the time zone error from UTC to EDT
    Dateindex = format(time, "%m%d")
    StartSecond = as.numeric(format(time, "%H")) * 3600 + as.numeric(format(time, "%M")) * 60 + as.numeric(format(time, "%S")) 
    EndSecond = StartSecond + 3600
    
    ByDetectorData <- paste("ByDetectorData",Dateindex,sep="");
    Data = get(ByDetectorData)[,-1]
    
    table <- subset(Data, detector_id == input$DetectorNo & Data[[input$CriteriaNo]] == 1 & timestamp >= StartSecond & timestamp < EndSecond) 
    if(input$CriteriaNo == "flag") return(table)
    if(CriteriaSort[which(CriteriaCols == input$CriteriaNo)] == 0)  table_sorted = table[order(table$detector_id, table$DIRECTION,table$lane_id,table$timestamp),]
    if(CriteriaSort[which(CriteriaCols == input$CriteriaNo)] == 1)  table_sorted = table[order(table$detector_id, table$timestamp,table$DIRECTION,table$lane_id),]
    table_sorted
    
  },
  extensions = 'Buttons',caption="Table for flagged records with selected detector and criteria no.",
  options=list(extensions='Buttons', searching=FALSE, scrollX=400, dom='Bfrtip',buttons=c('csv','excel','print'))
  
  )
  
  # ByLocationReportTable  ----------------------------------------------  
 
  output$table_BL_Report_DL = downloadHandler('Report.csv', content = function(file) {
    write.csv(AllDetectorData_agg, file, row.names = FALSE)
  })
  
  
  output$table_BL_Report<- DT::renderDataTable({
    
    datatable(
    AllDetectorData_agg,
    selection='single',extensions = 'Buttons',rownames=FALSE, caption = 'Summary Table for Dec 1~3, 2016 Sunguide TSS data',
    options=list(
       order=list(list(ncol(AllDetectorData_agg)-1,'desc')),searching=FALSE,pageLength=12, scrollX=400, dom='Bfrtip',
       buttons=list('copy','print'),
       columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(AllDetectorData_agg)-1)))
    )
    )%>% formatPercentage(4:14,0)%>%
     formatStyle('flag_percent', color='red', fontWeight='bold')%>% 
     formatStyle('detector_id', cursor = 'pointer') 
  }
  )
  
  output$text_BL_Report = renderPrint({
    detector_id_row = input$table_BL_Report_cell_clicked$value
    detector_name_row = as.character(DetectorCenter[["DetectorName"]][which(DetectorCenter$DetectorID==toString(detector_id_row))][1])
    
    cat('Click Column Headers to Sort By Criterias:\n\n')
    cat('Click on Detector ID in first column for Detector Configuration:\n\n')
    if (is.null(detector_id_row) || input$table_BL_Report_cell_clicked$col != 0) return()
    cat(detector_id_row ,detector_name_row , 'Other information can be added......',sep = ', ')
  }
  )
  
  # Configuration Table  ----------------------------------------------   
  
  output$table_Config<- DT::renderDataTable({
    datatable(config
    )

  }
  )

# Temp_map ------------------------------------------------------------------  
  output$temp_map<- renderLeaflet({
    leaflet(LinkCenter) %>% addTiles() %>%
      addMarkers(~MidLon, ~MidLat, popup = ~LinkName) 
  })  
  
# Others (simple elements) --------------------------------------------------    
  
  # output$myImage <- renderImage({
  #   list(src = "images/Rules.png",width = 400,height = 300)
  # }, deleteFile = FALSE)
  
  output$CriteriaImage <- renderImage({
     list(src = "images/Rules.png",width = 1100,height = 900)
   }, deleteFile = FALSE)
  
  output$Click_text <- renderText({
    if (is.null(input$map_marker_click$id)) return("Click to select detetcor: ")
    content <- paste(sep = " ",
                     "Detector Selected On Map:",
                     input$map_marker_click$id)
  })
  
  output$table_BD<- renderDataTable({
    Data <- AllDetectorData[which(AllDetectorData$detector_id == input$detector_ID),] 
    Data
  })
  

  
  
  
}

shinyApp(ui, server)



