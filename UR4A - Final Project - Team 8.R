# MGMT 59000 - UR4A
# Team 8
# Jocelyn Nelson, Megan Crites, John Carigma

# Call relevant libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(randomForest)
library(corrplot)
library(reshape2)
library(tidyr)

# #Graduate Admissions Dataset
data <- read.csv("Admissions.csv", header = TRUE)
d <- data[,c(9,2:3,5:7)]
model <- readRDS("RFmodel.rds") 

# Load data from csv file from kaggle
admissionsData <- read.csv("Data-Table 1.csv", header=TRUE, stringsAsFactors=TRUE)

# Sort school name column alphabetically
admissionsData <- admissionsData %>% arrange(Name)


# Create vector of score columns
examType <- c("SAT.Critical.Reading.25th.percentile.score",
              "SAT.Critical.Reading.75th.percentile.score", "SAT.Math.25th.percentile.score", 
              "SAT.Math.75th.percentile.score", "SAT.Writing.25th.percentile.score", 
              "SAT.Writing.75th.percentile.score", "ACT.Composite.25th.percentile.score",
              "ACT.Composite.75th.percentile.score")

# Exclusion of NA rows for purposes only of k cluster analysis
admissionsData2 <- admissionsData %>% 
  drop_na(c("SAT.Critical.Reading.25th.percentile.score",
            "SAT.Critical.Reading.75th.percentile.score", "SAT.Math.25th.percentile.score", 
            "SAT.Math.75th.percentile.score", "SAT.Writing.25th.percentile.score", 
            "SAT.Writing.75th.percentile.score", "ACT.Composite.25th.percentile.score",
            "ACT.Composite.75th.percentile.score"))

# Start of main shiny app page
ui <- fluidPage(
  
  # App title ----
  titlePanel("College Admissions"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Undergraduate Summary Table",
                           selectInput(
                             inputId = "tableRows", label = "School Selection",
                             admissionsData$Name),dataTableOutput(outputId = "schoolTable")
                           
                  ),
                  
                  tabPanel("Undergraduate Descriptive Analysis",
                           
                           selectInput(
                             inputId = "histCol", label = "Numerical Data",
                             names(select_if(admissionsData, is.numeric)),
                             selected = names(admissionsData)[[2]]), plotOutput(outputId = "hist"),
                           
                           selectInput(inputId = "userX", label = "Exam Type Selection 1", 
                                       examType, selected = examType[1]),
                           
                           selectInput(inputId = "userY", label = "Exam Type Selection 2", 
                                       examType, selected = examType[3]),
                           
                           sliderInput(inputId = "kIn", label = "Score Value", value = 3, min = 0, max = 50),
                           plotOutput(outputId = "clusterPlot")
                           
                           
                  ),
                  
                  tabPanel("Graduate Admission Prediction", 
                           h3(tableOutput('prediction')),
                           br(),
                           
                           sliderInput(
                             inputId = "GRE" ,
                             label= "Please enter GRE Score ",
                             min=0,
                             max=340,
                             value= 200),
                           
                           sliderInput(
                             inputId = "TOEFL" ,
                             label= "Please enter TOEFL Score ",
                             min=0,
                             max=120,
                             value= 100),
                           
                           sliderInput(
                             inputId = "CGPA" ,
                             label= "Please enter College GPA",
                             min=0,
                             max=10,
                             value= 4),
                           
                           
                           radioButtons(
                             inputId = "SOP" ,
                             label= ("Statement of Purpose"),
                             choices = c(1,2,3,4,5),
                             selected = NULL,
                             choiceNames = NULL,
                             choiceValues = NULL),
                           helpText("1 = Poor · 2 = Fair · 3 = Good · 4 = Very Good · 5 = Excellent"),
                           
                           
                           radioButtons(
                             inputId = "LOR" ,
                             label= ("Letter of Reccomendation"),
                             choices = c(1,2,3,4,5),
                             selected = NULL,
                             choiceNames = NULL,
                             choiceValues = NULL),
                           helpText("1 = Poor · 2 = Fair · 3 = Good · 4 = Very Good · 5 = Excellent"),
                           
                           
                  ),
                  
                  tabPanel("Graduate Admissions Analysis",h4("Summary Stats of Variables"), verbatimTextOutput("sum"),
                           h4("Correlation Between Variables"),
                           plotOutput("Corr"), h4("GRE vs Chance of Admission"), plotOutput("scat2"),
                           
                  )
      )
      
    )
  )
)


server <- function(input, output) {
  
  datasetInput <- reactive({
    
    df <- as.data.frame(matrix(nrow=1, ncol=6))
    names(df) <- c("Chance.of.Admit","GRE.Score","TOEFL.Score","SOP", "LOR","CGPA")
    df[1,] <- as.numeric(c(0,input$GRE,
                           input$TOEFL
                           ,input$SOP
                           ,input$LOR
                           ,input$CGPA
    ))
    prediction <- round(predict(model, df)[1],3)
    prediction <- prediction * 100
    print(paste("Chance of Acceptance:",prediction,"%"))
    
    
  })
  
  output$prediction <- renderText({ datasetInput()
  })
  
  #Descriptive Analytics - Histogram
  output$hist <- renderPlot({
    ggplot(data= admissionsData, aes(x= admissionsData[,input$histCol])) +
      geom_histogram(fill="blue")+
      ggtitle(label= paste0("Histogram of ", input$histCol))
  })
  
  #Descriptive Analytics - Cluster Plot
  scaledClusterData <- reactive({
    data.frame(scale(admissionsData2[ ,c(input$userX, input$userY)]))
  })
  
  kMeansCluster <- reactive({
    kmeans(scaledClusterData(), input$kIn)
  })
  
  output$clusterPlot <- renderPlot ({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(scaledClusterData(),
         col = kMeansCluster()$cluster,
         pch = 20, cex = 3)
    points(kMeansCluster()$centers, pch = 4, cex = 4, lwd = 4)
  })  
  
  
  #Descriptive Analytics - Table output of key information for selected schools
  SchoolData <- reactive({
    admissionsData %>%
      filter(Name == input$tableRows) %>%
      select(Name, Percent.admitted...total, FIPS.state.code, 
             Level.of.institution, Control.of.institution, SAT.Critical.Reading.25th.percentile.score,
             SAT.Critical.Reading.75th.percentile.score, SAT.Math.25th.percentile.score, 
             SAT.Math.75th.percentile.score, SAT.Writing.25th.percentile.score, 
             SAT.Writing.75th.percentile.score)
  })
  
  output$schoolTable <- renderDataTable({
    SchoolData()
  })
  
  
  output$Corr <- renderPlot({
    corrleation <- cor(d)
    corSort <- as.matrix(sort(corrleation[,"Chance.of.Admit"], decreasing = TRUE))
    corHigh <- names(which(apply(corSort,1 , function(x) abs(x) > 0.5 )))
    corrleation <- corrleation[corHigh, corHigh]
    corplot <- corrplot.mixed(corrleation, tl.col="black",
                              tl.pos = "lt")
    
  })
  
  
  
  output$sum <- renderPrint({
    sum <- summary(d)
    print(sum)
  })
  
  output$scat2 <- renderPlot({
    ggplot(d,aes(x=CGPA,y=Chance.of.Admit))+
      geom_point()+
      geom_smooth(method="lm",se=F)+
      labs(x="CGPA",y="Chance of admit",title="CGPA and impact on admissions")
  })
  
  
}


shinyApp(ui = ui, server = server)
