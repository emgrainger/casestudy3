#install packages 
#install.packages("leaflet.extras")
#install.packages("dplyr")
#install.packages("leaflet")
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggplot2")
#install.packages("RColorBrewer")

#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(shinydashboard)
library(leaflet.extras)
library(ggplot2)
library(tidyverse)
library(plsdepot)
library(visreg)
library(caret)
library("RColorBrewer")

#import data
CSOdata = read.csv("CSOdata.csv")
CSOdata = read.csv("CSOdata.csv")
names(CSOdata)[names(CSOdata) == "SPDES.Permit.Number"] <- "Permit"
names(CSOdata)[names(CSOdata) == "Outfall.Number"] <- "Outfall"
names(CSOdata)[names(CSOdata) == "Receiving.Waterbody.Name"] <- "Receiving_Waterbody"
names(CSOdata)[names(CSOdata) == "Discharge.Activation.Type"] <- "Monitoring_Method"
names(CSOdata)[names(CSOdata) == "Number.of.Overflow.Events"] <- "CSO_Activation_Frequency"
names(CSOdata)[names(CSOdata) == "Number.of.Permitted.Outfalls"] <- "Number_Outfalls"
table1= CSOdata[ , c(1,2,6,8,9)] 
table2= CSOdata[ , c(1,2,6,7,8,9, 13,14,15,16)] 


table2$CSO_Activation_Frequency[table2$CSO_Activation_Frequency == "Visit facility website"] = NA
table2$CSO_Activation_Frequency[table2$CSO_Activation_Frequency == "http://cso.savetherain.us/"] = NA
table2$CSO_Activation_Frequency[table2$CSO_Activation_Frequency == "No Data"] = NA
table2$CSO_Activation_Frequency[table2$CSO_Activation_Frequency == "Eliminated"] = 0
table2$CSO_Activation_Frequency[table2$CSO_Activation_Frequency == "Average of <4 overflows annually"] = 3
table2$CSO_Activation_Frequency[table2$CSO_Activation_Frequency == "< 6 annually"] = 5

CSO=table2[ , c(4,6,8)] 
data=table2[,c(9,10,4,6)]
data$CSO_Activation_Frequency=as.numeric(as.character(data$CSO_Activation_Frequency))
data1=table2[,c(3,8,9,10,4,6)]
data1$CSO_Activation_Frequency=as.numeric((data$CSO_Activation_Frequency))
CSO_Model=CSOdata[, c(2,7,8,9, 13)]
CSO_Model$CSO_Activation_Frequency=as.numeric(as.character(CSO_Model$CSO_Activation_Frequency))

library(dplyr)
CSO_Model1 <- CSO_Model %>%
    dplyr::mutate(
        Model = ifelse(Monitoring_Method == "Model", 1, 0)
    )

ui=fluidPage(
    titlePanel("Combined Sewer Overflow (CSO) Monitoring for the State of New York "),
    fluidRow(
               "A combined sewer is a sewer that is constructed in a way in which both storwmater and wastewater of domestic, commercial, and industrial origin are conveyed. 
               A Combined Sewer Activations (CSOs) occurs when the capacity of the wastewater and stormwater exceeds the sewer capacity. This results in raw, untreated sewage discharging to natural waterbodies, as shown in the figure below. 
               Additional information on CSOs can be found on the",
               a("EPA Webiste",
                 href = "https://www3.epa.gov/region1/eco/uep/cso.html/")), column(12, tags$img(height=500, width=800, src="img1.png"),
    fluidRow(
               "The assessment of Combined Sewer Overflows is important because it can inform state funding, engineering efforts, 
               and identify potentially polluted water bodies. By studying this data it is possible to identify locations where significant CSO monitoring, assessment, and/or engineering efforts may be occuring."),
     tags$hr(),
    tags$h3 ("Data for CSOs in New York"),
    fluidRow(
    "Data for this analysis came from", a("Kaggle, an open data source platform.", href="https://www.kaggle.com/new-york-state/nys-combined-sewer-overflows-csos/data"), "The dataset was called the NYS Combined Sewer Overflows (CSOs) and was published by the State of New York. The data was last updated 8 months ago (Version 2).
             The dataset contained 911 rows, 20 columns, 12 discrete columns, and 8 continuous columns. There were a total of 18,200 observations. In order to understand the dataset better, enabling the end-user to filter the 
             information, an interactive Shiny table was created and is presented below. This enables the end user to categorize the data based on the Recieving Water Body, the Monitoring Method, and the CSO Activation Frequency. A search bar is also provided which enables the end-user to directly search the data set.
             For example, if the end-user had a particular Permit that they were interested in they could search for the permit and identify attributes such as the Recieving Water Body, Monitoring Method, and Activation Frequency using the Shiny Table."),
#TABLE FOR VIEWING THE DATA 
fluidRow(
        column(4,
               selectInput("Receiving_Waterbody",
                           "Receiving Waterbody:",
                           c("All",
                             unique(as.character(table1$Receiving_Waterbody))))
        ),
        column(4,
               selectInput("Monitoring_Method",
                           "Monitoring Method:",
                           c("All",
                             unique(as.character(table1$Monitoring_Method))))
        ),
        column(4,
               selectInput("CSO_Activation_Frequency",
                           "CSO Activation Frequency:",
                           c("All",
                             unique(as.character(table1$CSO_Activation_Frequency))))
        )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("table"),
tags$hr(), 
# SCATTER PLOT FOR CSO ACTIVATION VS NUMBER OUTFALLS 
tags$h3 ("Scatterplots for Activation Frequency and Number of Outfalls"),
fluidRow(p(style = "font-family:Garamond",
           "This section presents two graphs comparing Activation Frequency and the Number of Outfalls.
           The first graph presents a static scatterplot of the CSO Activation Frequency and Number of Outfalls for all data points in all counties. 
           As you click on the graph, the x and y values will update below.
           A scatter plot allows to view the data to see if a linear relationship is supported by the data.
           The regression model is based on a single response varaible and one or more predictor variables.
           In this case it was suspected that there may be a relationship between the CSO activation frequencies and the number of outfalls present. Based on the scattergraphs presented, there is not a clear linear relationship.
           This was shown by fitting a simple linear regression to the scattergraph using the function lm(). If a linear regression existed all of the points would be on or close to the black line on the graph. Values close to the line are called fitted values, while values far away from the line are residuals.
           The resulting R2 value was high as the data had many outliers.
           ")),
    plotOutput("plot1", click = "plot_click"),
    verbatimTextOutput("info1"),
    fluidRow(p(style = "font-family:Garamond","This is a comparison of the number of outfalls and the activation frequency by county. Use the drop down menu to select a county of interest.")),
    selectInput("County","County:",c("All",unique(as.character(data1$County)))),
    plotOutput("plot2"),
tags$hr(), 

# LOGISTIC REGRESSION 
tags$h3 ("Probability of Monitoring by Modeling"),
fluidRow(p(style = "font-family:Garamond",
           "The method that is used to identify the annual CSO activation frequencies depends on the regulations and capabilities of the system owner. The three main methods of identifying  activation frequencies identified in the data set are 1. Model, 2. Monitoring, and 3. Observation. 
           A model is a predictive and mathematical calculation that are based on watershed and rainfall information. The system owner will simulate the given rainfall event and/or the typical annual rainfall conditions for the region under the system conditions of that year 
           to predict the number of CSO activation events that occured. The second method, monitoring, is when municipalities have monitoring equipment installed at the outfalls and they collect real time water flow information. The third method is observation, where municipal staff visually check the outfall to determine whether or not an activation has occured.
           Due to budgets and manpower to physically observe locations where CSO activations would occur in places where there are many outfalls and/or the cost of monitoring many outfalls it was suspected that as the number of outfalls increased, the likelihood of using a model would increase. Logistic regression was used to test this theory. Logistic regresssion is used to estimate the probability that an event
           will occur as a function of other variables. A binary classification was done in this case as to whether a model was or was not used. The logistic regression returns a score, estimating the probability that a model was used. The result is shown in the graph below, comparing the number of outfalls and the model's predicted probability that a model was used for identifying the CSO activations.
           The value 1 represents that a model was used. This is shown by the light blue area in the graph. This suggests that as you move towards the right on the graph and the number of outfalls increases, the probability of using a model also increases.
           ")),
plotOutput("prob"),
#MAP OF OUTFALLS 
tags$h3 ("Interactive Map of New York CSO Outfalls"),
fluidRow(p(style = "font-family:Garamond","This is an interactive map that demonstrates the location of CSO outfalls and the Number of Outfalls. By clicking on the box labeled number of outfalls in the top left corner of the map you can interact with the map. 
         Try zooming into NYC to see the significant number of CSO outfalls.")),
mainPanel( 
    leafletOutput(outputId = "mymap"), 
    absolutePanel(top = 60, left = 40, 
                  checkboxInput("Number_Outfalls", "Number of Outfalls", FALSE)))
    ))

server <- function(input, output) {

#DATA TABLE
    output$table = DT::renderDataTable(DT::datatable({
        data1 = table1
        if (input$Receiving_Waterbody != "All") {
            data1<- data1[data1$Receiving_Waterbody == input$Receiving_Waterbody,]
        }
        if (input$CSO_Activation_Frequency != "All") {
            data1 <- data1[data1$CSO_Activation_Frequency == input$CSO_Activation_Frequency,]
        }
        if (input$Monitoring_Method != "All") {
            data1 <- data1[data1$Monitoring_Method == input$Monitoring_Method,]
        }
        data1
    }))

# SCATTERPLOT 
    filtered_data1 <- reactive({
        dplyr::filter(data1, County == input$County)
    })
    output$plot2 <- renderPlot({
        ggplot(filtered_data1(),aes(x=CSO_Activation_Frequency, y=Number_Outfalls))+
            geom_point(size=2)
    })
    output$plot1 <- renderPlot({
        plot(data1$CSO_Activation_Frequency, data1$Number_Outfalls, ylab="Number of Outfalls", xlab="Activation Frequency")
        abline(lm(data1$CSO_Activation_Frequency ~ data1$Number_Outfalls))
        output$info1 <- renderText({
            paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)})
    })

#PROBABILITY PLOT 
    output$prob= renderPlot({cdplot(factor(Model) ~ Number_Outfalls, data=CSO_Model1, main="Probability of Modeling CSO Activations", xlab='Number of Outfalls',ylab='Probability of Model', col=blues9)})
    
#MAP 
    #create colors for map
    pal = colorNumeric(
        palette = c(name=blues9),
        domain =data$Number_Outfalls)

    #create map
    output$mymap <- renderLeaflet({
        leaflet(data) %>% 
            setView(lng = -75, lat = 43, zoom = 6)  %>% 
            addTiles() %>% 
            addMarkers(data = data, lat = ~ Latitude, lng = ~ Longtitude,  clusterOptions=markerClusterOptions())
    })
    
    observe({
        proxy <- leafletProxy("mymap", data = data)
        proxy %>% clearMarkers()
        if (input$Number_Outfalls) {
            proxy %>%  addCircleMarkers(stroke=FALSE, lng=~Longtitude, lat=~Latitude, color=~pal(Number_Outfalls), fillOpacity = 0.2,  label = ~as.character(paste0("Number_Outfalls: ", sep = " ", Number_Outfalls))) %>%
                addLegend("bottomright", pal = pal, values = data$Number_Outfalls,
                          title = "Number of Outfalls",
                          opacity=1)}
        else{
            proxy %>% clearMarkers()%>% clearControls()
        }})
}

shinyApp(ui,server)

