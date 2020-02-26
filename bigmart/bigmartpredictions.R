#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("DT")
library(DT)
#install.packages("semantic.dashboard")
#library(semantic.dashboard)
library(GGally)
library(dplyr)
library(stats)
library(randomForest)
#install.packages("plotly")
library(plotly)

Test <- read_csv("/cloud/project/Predict_3Train.csv", na = na_list)
data <-read_csv("/cloud/project/Train_Clean.csv", na = na_list)
data$Item_Weight <- as.numeric(data$Item_Weight)
data_Num <- read_csv("/cloud/project/data_Num.csv")
glimpse(data)
data_Num <- na.omit(data_Num)
c <- cor(data_Num)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Big Mart Data Exploration"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Factors that contribute to sales", tabName = "factors", icon = icon("dashboard")),
      
      menuItem("Random Forest Model and data", tabName = "forest", icon = icon("dashboard")),
      menuItem("Additional EDA", tabName = "eda", icon = icon("signal")),
      menuItem("Data table", tabName = "datatab", icon = icon("table")),
      menuItem("Code for model", tabName = 'codetab',icon = icon("code")),
      menuItem("Code for Shiny", tabName = 'codeshiny',icon = icon("code"))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem("factors",
              h1("Factors that contribute to sales"),
              h3("In this analysis, I leveraged randomForest modeling which is a non-parametric model that can handle skewed data.
                 One of the advantages of using randomForest modeling is that is a fairly accurate and popular learning algorithm that can 
                 handle many different predictors and provides variable importance estimates."),
              
              box(
                plotOutput('varimpplot'), width = 15
              
              ),
              h3("As you can see from the plot above, several factors have been used to estimate their importance in predicting sales. The 3 Variables to pay attention to
                 are..."),
                 h4("-Item MRP"),
              h4("-Outlet Type"),
              h4("-Outlet Establishment Year"),
              h3("It's important to keep in mind that the way randomForest calculated 
                 variable importance can contain bias. This is because some variable types 
                 like continuous features or high-cardinality features are preferred. Though, 
                 testing models with and without these 3 variables resulted in significantly less 
                 accurate models."),
              box(
                plotOutput("a"), width = 15
              )
              ),
      
      tabItem("forest",
                        fluidPage(
                          
                          h2("The randomForest model was trained using non-imputed data. In fact, very little cleaning 
                             was done to achieve this rmse. I've gone through 20 different models that usedMICE/KNN imputation, 
                             oneHOT encoding, manual imputation,  listwise  deletion and creation of additional features.
                             Also, it was known that the data contained NA values, listwise deletion was used to deal with the
                             missing data."),
                          h3("Overall OOB(out of bag) MSE error"),
                          box(plotOutput('forest'),width = 10),
                          h2("The RMSE of the model is 1169.39 and the formula for the model was.."),
                          h3("Item_Outlet_Sales ~
                                   Item_Fat_Content + 
                                   Item_Visibility + Item_Type + 
                                   Item_MRP + Outlet_Establishment_Year +
                                   Outlet_Location_Type + Outlet_Type,
                                 data = Train,
                                 ntree = 300, importance = T"),
                          h3("Detailed information about the code can be found in the coding seciton.")
                          
                          
                          
                        )
              ),
      
      tabItem("eda",
              fluidPage(
                h1("This page contains additional information about the dataset that was used to build the model and why."),
                box(plotOutput("eda_1"),width = 16),
                h2("You'd think that Item Visibility would contribute positively to sales,
                   but here we can see that it dosen't really help sales"),
                box(plotOutput("eda_2"),width = 10),
                box(plotOutput("eda_3"),width = 10),
                box(plotOutput("eda_4"),widh = 10),
                box(selectInput("outletthing","Outlet Filters:",
                                c("Outlet_Type","Outlet_Size","Outlet_Location_Type","Outlet_Identifier","Item_Fat_Content")
                  
                )),
                box(plotOutput('plot2'), width = 11)
              )
      ),
      
      tabItem("datatab",
              fluidPage(
                h1("Here you can look through the entire dataset"),
                DT::dataTableOutput("Table")
              )
      ),
      tabItem("codetab",
              fluidPage(
                h4("")
              )),
      tabItem("codeshiny",
              fluidPage(
                h4("")
              ))
      )
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output) {

  
  # First Tab  
  output$plot2 <- renderPlot({
    ggpairs(data, columns = c("Item_Outlet_Sales","Item_Visibility","Item_Weight","Item_MRP"))
    }
   )
  output$varimpplot <- renderPlot({
    varImpPlot(Forest1_Training)
  })
  output$a<- renderPlotly({
    plot_ly(data, x = ~Item_Outlet_Sales, y = ~Item_MRP, z = ~Outlet_Establishment_Year, color = ~Outlet_Type)
  })
  
  # SECOND TAB
  output$corrplot <- renderPlot({
    corrplot::corrplot(c, method = "circle")
  })

  #THIRD TAB
  output$forest <- renderPlot({
    plot(Forest1_Training)
  })
  #Fourth TAB  
  output$eda_1 <- renderPlot({
    ggplot(data, aes(x = Item_Type, fill = Item_Type))+geom_bar()+theme(axis.text.x = element_blank())
  })
  
  output$eda_2 <- renderPlot({
    ggplot(data, aes(x = data$Item_Outlet_Sales, y = data$Item_Visibility, color = data$Outlet_Type))+
      geom_point()+
      geom_smooth(method = 'lm')
  })
  output$eda_3 <- renderPlot({
    ggplot(data, aes(y = data$Item_Outlet_Sales, x = data$Item_MRP))+
      geom_point()+
      geom_smooth(method = 'lm')
  })
  
  output$eda_4 <- renderPlot({
    ggplot(data, aes(y = Item_Outlet_Sales, x = data[[input$outletthing]], fill = data[[input$outletthing]]))+
      geom_col()+
      theme(axis.text.x = element_blank())
  })
  
  
  # LAST TAB
  output$Table <- renderDataTable(data)
}
# Run the application 
shinyApp(ui = ui, server = server)
