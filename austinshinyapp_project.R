#Project: Water Qualtiy in Barton Creek Watershed, Austin-Texas

#set working directory
setwd("~/Water_quality_final")

#only Barton Creek Watershed is selected from original Austin groundwater quality file
austin <- read.csv("Austinwater.csv")
austin

#removing NA values from Lat and Lon
austin1<-austin[is.na(austin$austingw.LAT_DD_WGS84)==FALSE,]

#selecting water quality parameters (pH, ammonia,conductivity, flow, turbidity)
pH <- austin1[austin1$austingw.PARAMETER=="pH (Standard units)", c("austingw.LAT_DD_WGS84","austingw.SITE_TYPE","austingw.LON_DD_WGS84","austingw.RESULT","austingw.SAMPLE_DATE")]
pH

ammonia <- austin1[austin1$austingw.PARAMETER=="Ammonia as N (mg/L)", c("austingw.LAT_DD_WGS84","austingw.SITE_TYPE","austingw.LON_DD_WGS84","austingw.RESULT","austingw.SAMPLE_DATE")]
ammonia

conductivity <- austin1[austin1$austingw.PARAMETER=="Conductivity (uS/cm)", c("austingw.LAT_DD_WGS84","austingw.SITE_TYPE","austingw.LON_DD_WGS84","austingw.RESULT","austingw.SAMPLE_DATE")]
conductivity
 
flow <- austin1[austin1$austingw.PARAMETER=="Flow (Cubic feet per second)", c("austingw.LAT_DD_WGS84","austingw.SITE_TYPE","austingw.LON_DD_WGS84","austingw.RESULT","austingw.SAMPLE_DATE")]
flow

turbidity <- austin1[austin1$austingw.PARAMETER=="Turbidity (NTU)", c("austingw.LAT_DD_WGS84","austingw.SITE_TYPE","austingw.LON_DD_WGS84","austingw.RESULT","austingw.SAMPLE_DATE")]
turbidity

watertemp <- austin1[austin1$austingw.PARAMETER=="Water temperature (Deg. Celsius)", c("austingw.LAT_DD_WGS84","austingw.SITE_TYPE","austingw.LON_DD_WGS84","austingw.RESULT","austingw.SAMPLE_DATE")]
watertemp


##plot of water quality parameters with respect to sampling date

library(ggplot2)
#ggplot of pH and sampling date
a1 <- ggplot(pH, aes(x = austingw.RESULT, y = austingw.SAMPLE_DATE)) + geom_point(size = 2.5, col = "blue") + 
      xlab("pH (Standard units)") + ylab("Sampling Date") +   theme_classic() + theme(text=element_text(size=9)) 
a1

#ggplot of ammonia and sampling date
a2 <- ggplot(ammonia, aes(x = austingw.RESULT, y = austingw.SAMPLE_DATE)) + geom_point(size = 2.5, col = "green") +
  xlab("Ammonia as N (mg/L)") + ylab("Sampling Date") +   theme_classic() + theme(text=element_text(size=9))
a2


#ggplot of ammonia and sampling date
a3 <- ggplot(conductivity, aes(x = austingw.RESULT, y = austingw.SAMPLE_DATE)) + geom_point(size = 3, col = "black") +
  xlab("Conductivity (uS/cm)") + ylab("Sampling Date") +   theme_classic() + theme(text=element_text(size=9))
a3


#ggplot of ammonia and sampling date
a4 <- ggplot(turbidity, aes(x = austingw.RESULT, y = austingw.SAMPLE_DATE)) + geom_point(size = 3, col = "brown")  +
  xlab("Turbidity (NTU)") + ylab("Sampling Date") +   theme_classic() + theme(text=element_text(size=9))
a4


#ggplot of ammonia and sampling date
a5 <- ggplot(watertemp, aes(x = austingw.RESULT, y = austingw.SAMPLE_DATE)) + geom_point(size = 3, col = "red") + ggtitle("Water temperature values, 1999-2014") +
  xlab("Water temperature (Deg. Celsius)") + ylab("Sampling Date") + theme_classic() + theme(text=element_text(size=9))
a5


##Combine plot layout for selected parameters, pH, ammonia, conductivity, turbidity, watertemp
library(gridExtra)
library(grid)
plots <- grid.arrange(a1, a2, a3, a4, top=textGrob("Scatter plots of pH, ammonia, conductivity and turbidity values, 1999-2001", 

                                                                                                    gp=gpar(fontsize=20,font=1)))
#saveplots
print(plots)
pdf("~/Water_quality_final/plots.pdf")
dev.off()

#projecting water sample points in Open Street map using leaflet
library(leaflet)
library(htmltools)

#selecting the lat and lon data i.e. spatial data for water sampling points in Barton Creek
df1 <- data.frame(austin1$austingw.LAT_DD_WGS84,austin1$austingw.LON_DD_WGS84,austin1$austingw.SITE_NAME)

#using leaflet
leaflet(df1) %>%
   addTiles() %>%
   addMarkers(~austin1.austingw.LON_DD_WGS84 ,~austin1.austingw.LAT_DD_WGS84,
             popup = ~htmlEscape(austin1$austingw.SITE_NAME))

###############
#using shinyapp to visualize water quality parameters by sample sites

library(ggplot2)
library(dplyr)
library(tidyverse)
library(shiny)

#ui for selecting water quality parameter, sample site in openstreet map using leaflet and loading Austin.gov logo
ui <- fluidPage(
  titlePanel("Water Quality of Barton Creek Watershed, Austin-Texas"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "parameter",
        label = "Select A Parameter",
        choices = unique(austin1$austingw.PARAMETER),
        selected = "PH",
        multiple = FALSE
        ),
      #to insert austin city image file in the shiny app
      #logo.css file is created and saved in www folder to read "png" file
      #open logo.css file to run the code
      fluidRow(
        column(width=5,img(src="austin.logo.png",height="200px",width="200px"),imageOutput("my_image"))
      ),
      #information about data source
      p("Data is provided by", a("austintexas.gov", href = "https://data.austintexas.gov/", target = "_blank"))
      ),
    mainPanel(
      #title for the spatial dataset projected in OpenStreet Map
      h5("Sampling sites in Barton Creek Watershed, Austin"),
      leafletOutput("Austin", height = "300"),
      #plot output name
      plotOutput("boxplot"),
      #summary of selected parameter values
      textOutput("selected_par"),
      verbatimTextOutput("stats")
      )
    )
  )

#server for plotting boxplot, data summary, water sampling points and austinlogo

server <- function(input, output) {
  selectedData <- reactive({a1 <- austin1 %>% filter(austingw.PARAMETER==input$parameter)})
  #boxplot of selected input parameter
  output$boxplot <- renderPlot({
    ggplot(selectedData(),aes(x = austingw.SITE_NAME, y = austingw.RESULT)) + 
      geom_boxplot(colour = 'blue', size = 0.5) +
      xlab("Site name") + ylab("Result") +
      theme(axis.title = element_text(face="bold")) +
      theme(text = element_text(size = 16)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme_bw() +
      coord_flip() +
      theme(axis.line=element_line(colour = "black"))
  })
  #slected input parameter summary
  output$stats <- renderPrint({
    summary(selectedData()$austingw.RESULT)
  })
  #table title for selected input parameter summary
  output$selected_par <- renderText({ 
    paste("Data summary of", input$parameter)
  })
  #selecting spatial data to plot sampling points in openStreet map
  df1 <- data.frame(austin1$austingw.LAT_DD_WGS84,austin1$austingw.LON_DD_WGS84,austin1$austingw.SITE_NAME)
  #spatail data output
  output$Austin <- renderLeaflet({
    leaflet(df1) %>%
      addTiles() %>%
      addMarkers(~austin1.austingw.LON_DD_WGS84 ,~austin1.austingw.LAT_DD_WGS84,
                 popup = ~htmlEscape(austin1$austingw.SITE_NAME))
    
  })
  #ploting austinlogo png file 
  output$m_image <- renderImage({
    list(src="austin.logo.png", height="200px", width="200px")
  },deleteFile = FALSE)
}

shinyApp(ui, server)


