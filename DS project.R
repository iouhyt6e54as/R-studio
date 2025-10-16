#install all packagesS
#install.packages("shiny")
#install.packages("cluster")
#install.packages("readxl")
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("reshape2")
#install.packages("plotrix")


#All libraries
library(shiny)
library(cluster)
library(readxl)
library(arules)
library(arulesViz)
library(tidyverse)
library(dplyr)
library(reshape2)
library(plotrix)

# UI
ui <- fluidPage(
  titlePanel("Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload Your Dataset (Excel for Clustering, CSV for Apriori)", 
                accept = c(".xlsx", ".xls", ".csv")),
      selectInput("analysisType", "Select Analysis Type:", 
                  choices = c("K-Means Clustering", "Apriori Association Rules", "Data Visualization")),
      
      conditionalPanel(
        condition = "input.analysisType == 'K-Means Clustering'",
        numericInput("clusters", "Enter Number of Clusters (2-4):", value = 3, min = 2, max = 4)
      ),
      
      conditionalPanel(
        condition = "input.analysisType == 'Apriori Association Rules'",
        numericInput("minSupport", "Minimum Support (0.001-1):", value = 0.01, min = 0.001, max = 1, step = 0.01),
        numericInput("minConfidence", "Minimum Confidence (0.001-1):", value = 0.8, min = 0.001, max = 1, step = 0.01)
      ),
      
      conditionalPanel(
        condition = "input.analysisType == 'Data Visualization'",
        selectInput("plotType", "Select Visualization Type:", 
                    choices = c("Pie Chart", "Bar Chart (Age)", "Bar Chart (City)", "Histogram"))
      ),
      
      actionButton("run", "Run Analysis")
    ),
    
    mainPanel(
      verbatimTextOutput("summaryOutput"),
      plotOutput("plotOutput"),
      plotOutput("boxplotBefore"),
      plotOutput("boxplotAfter"),
      tableOutput("tableOutput")
    )
  )
)





# server
server <- function(input, output, session) {
  
  # analyse data based on analysis Type
  analysisResult <- eventReactive(input$run, {
    req(input$datafile)
    file <- input$datafile$datapath
    
    if (input$analysisType == "K-Means Clustering") {
      # read and clean clustring data
      raw_data <- read_excel(file)
      data_without_duplicates <- unique(raw_data)
      data_no_na <- na.omit(data_without_duplicates)
      outliers <- boxplot(data_no_na$count, plot = FALSE)$out
      final_data <- data_no_na[-which(data_no_na$count %in% outliers),]
      # Visualize uncleaned data
      # boxplot(data_no_na[, 2:4], main = "Boxplot before Removing Outliers")
      # Visualize cleaned data
      # boxplot(final_data[, 2:4], main = "Boxplot after Removing Outliers")
      
      # aggregate total spending according customer and age
      total_spending <- aggregate(total ~ customer + age, data = final_data, FUN = sum)
      data_scaled <- scale(total_spending[, c("total", "age")])
      
      # run K-Means
      set.seed(123)
      kmeans_result <- kmeans(data_scaled, centers = input$clusters)
      total_spending$cluster <- as.factor(kmeans_result$cluster)
      list(
        type = "clustering", 
        raw_data = raw_data,
        data_no_na = data_no_na, 
        final_data = final_data,  data = total_spending,
        result = kmeans_result
      )
      
      
    } else if (input$analysisType == "Apriori Association Rules") {
      # read transiction data
      transactions <- read.transactions(file, format = "single", sep = ",", cols = c(1, 2))
      
      # run Apriori
      apriori_rules <- apriori(transactions, 
                               parameter = list(supp = input$minSupport, conf = input$minConfidence))
      list(type = "apriori", rules = apriori_rules)
      
    } else if (input$analysisType == "Data Visualization") {
      # Read data
      raw_data <- read_excel(file)
      data_without_duplicates <- unique(raw_data)
      data_no_na <- na.omit(data_without_duplicates)
      list(type = "visualization", data = data_no_na, plotType = input$plotType)
    }
  })
  
  # show results
  output$summaryOutput <- renderPrint({
    req(analysisResult())
    result <- analysisResult()
    if (result$type == "clustering") {
      cat("K-Means Clustering Summary:\n")
      print(result$result)
    } else if (result$type == "apriori") {
      cat("Apriori Rules Summary:\n")
      inspect(result$rules)
    }
  })
  
  output$boxplotBefore <- renderPlot({
    req(analysisResult())
    result <- analysisResult()
    if (result$type == "clustering") {
      boxplot(result$data_no_na[, 2:4], main = "Boxplot Before Removing Outliers")
    }
  })
  
  output$boxplotAfter <- renderPlot({
    req(analysisResult())
    result <- analysisResult()
    if (result$type == "clustering") {
      boxplot(result$final_data[, 2:4], main = "Boxplot After Removing Outliers")
    }
  })
  
  output$plotOutput <- renderPlot({
    req(analysisResult())
    result <- analysisResult()
    
    if (result$type == "clustering") {
      clusplot(result$data, result$result$cluster, main = "K-Means Clustering")
    } else if (result$type == "apriori") {
      plot(result$rules, method = "graph", control = list(type = "items"))
    } else if (result$type == "visualization") {   #visualization
      final_data <- result$data
      
      if (result$plotType == "Pie Chart") {
        cach <- 6142436
        credit <- 6084627
        precentag <- c(50.24, 49.76)
        type <- c("Cash (50.24%)", "Credit (49.76%)")
        pie(precentag, labels = type, main = "Cash & Credit Pie Chart", col = cm.colors(2))
        
      } else if (result$plotType == "Bar Chart (Age)") {
        age <- c(22, 23, 25, 29, 30, 35, 36, 37, 39, 50, 55, 60)
        total_spending <- c(1563238, 751420, 896338, 858431, 795044, 840966, 
                            866690, 1655969, 785008, 797884, 1630497, 785578)
        barplot(total_spending, names.arg = age, col = heat.colors(12), 
                main = "Age vs Total Spending", xlab = "Age", ylab = "Total Spending")
        
      } else if (result$plotType == "Bar Chart (City)") {
        city <- c("Alexandria", "Cairo", "Hurghada", "Dakahlia", "Sohag", 
                  "Port Said", "Giza", "Fayoum", "Gharbia", "Aswan")
        total_spending1 <- c(2509308, 2415823, 1626544, 869425, 866690, 
                             815411, 795044, 792390, 785008, 751420)
        barplot(total_spending1, names.arg = city, col = terrain.colors(10), 
                main = "City vs Total Spending", xlab = "City", ylab = "Total Spending")
        
      } else if (result$plotType == "Histogram") {
        hist(final_data$total, col = "skyblue", border = "blue", 
             main = "Total Spending Histogram", xlab = "Total Spending", ylab = "Frequency")
      }
    }
  })
  
  output$tableOutput <- renderTable({
    req(analysisResult())
    result <- analysisResult()
    if (result$type == "clustering") {
      result$data
    }
  })
}

# Run shiny app
shinyApp(ui = ui, server = server)