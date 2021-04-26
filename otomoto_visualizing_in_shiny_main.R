#Additional exercises: 
#Add tab with t-SNE
#Add other clustering techniques
#Apply clustering after PCA
#Use different variables for clustering
#Print error rate for clustering
#Plug-in other car to see if there are any differences
#Show linear regression output
#Correct labels in LASSO
#Add validation set

#Exercise 2: Visualizing PCA
set.seed(1337)
library(shiny)
library(dplyr)
library(RColorBrewer)
library(glmnet)
library(xgboost)

passat <- read.csv('audi.csv')
passat <- passat[,c('price', 'mileage', 'year', 'engine')]
passat <- na.omit(passat)
rownames(passat) <- c()
passat_scaled <- scale(passat)
pca <- prcomp(passat, scale. = TRUE)

kmeans_list <- list()
for(i in 1:8){
  kmeans_list[[i]] <- kmeans(passat_scaled, centers = i, nstart = 100)
}
lasso <- glmnet(x=passat_scaled[,2:4], y=passat_scaled[,1])
xgb <- xgboost(data = passat_scaled[,2:4], label = passat_scaled[,1], nrounds = 100, verbose = FALSE)


#PCA is reversible
all(passat == data.frame(apply(t(t(pca$x %*% t(pca$rotation)) * pca$scale + pca$center),2,round)))

#Defining useful auxiliary functions
seq_matrix <- function(from, to, length.out, i){
  diff <- to - from
  from + diff * (i/length.out)
}
#Example
seq_matrix(from = matrix(c(0,0.1,0,0,0,0),ncol=3),matrix(c(1,1,1,1,1,1),ncol=3),10,9)

ui <- fluidPage(
  navbarPage("",
    tabPanel('Principal Component Analysis',
        sidebarPanel(
          sliderInput("animate", "PCA step by step:",
                      min = 0, max = 10,
                      value = 10, step = 1,
                      animate = TRUE),
          checkboxInput("loadings", "Show loadings", FALSE),
          checkboxInput('loading_names', 'Show loading names', FALSE),
          sliderInput('loadings_scale', 'Loadings scale:',
                      min = 1, max = 10,
                      value = 1, step = 0.1),
          hr(),
          checkboxInput("clusters", "Get clusters", FALSE),
          sliderInput('clusters_n', 'Number of clusters:',
                      min = 1, max = 8,
                      value = 2, step = 1)
        ),
        mainPanel(
            plotOutput(outputId = "pca_animated", click='plot_click', height = "600px"),
            verbatimTextOutput("position_info")
      )
    ),
  tabPanel('Hand-made Component Analysis',
    sidebarPanel(
      checkboxInput("loadings2", "Show loadings", FALSE),
      checkboxInput('loading_names2', 'Show loading names', FALSE),
      sliderInput('pc1price', 'PC1 price:',
                  min = -1, max = 1,
                  value = pca$rotation['price',1], step = 0.01),
      sliderInput('pc2price', 'PC2 price:',
                  min = -1, max = 1,
                  value = pca$rotation['price',2], step = 0.01),
      sliderInput('pc1year', 'PC1 year:',
                  min = -1, max = 1,
                  value = pca$rotation['year',1], step = 0.01),
      sliderInput('pc2year', 'PC2 year:',
                  min = -1, max = 1,
                  value = pca$rotation['year',2], step = 0.01),
      sliderInput('pc1mileage', 'PC1 mileage:',
                  min = -1, max = 1,
                  value = pca$rotation['mileage',1], step = 0.01),
      sliderInput('pc2mileage', 'PC2 mileage:',
                  min = -1, max = 1,
                  value = pca$rotation['mileage',2], step = 0.01),
      sliderInput('pc1fuel', 'PC1 engine capacity:',
                  min = -1, max = 1,
                  value = pca$rotation['engine',1], step = 0.01),
      sliderInput('pc2engine', 'PC2 engine capacity:',
                  min = -1, max = 1,
                  value = pca$rotation['engine',2], step = 0.01)
    ),
    mainPanel(
      plotOutput(outputId = "hca", height = "600px")
    )
  ),
  tabPanel('What affects Audi price?',
   mainPanel(
     textOutput("lassotext"),
     plotOutput(outputId = "lasso", height = '600px'),
     hr(),
     textOutput("xgboosttext"),
     plotOutput(outputId = "xgboost", height = '600px')
   )
  )
)
)




#Potential exercise: Adjust further positions (as floats) by acknowledging relative position of text vs lims
pos_adj <- function(x, y, xlim, ylim){
  if(x < 0 & y < 0){
    c(1,1)
  }else if(x > 0 & y > 0){
    c(0,0)
  }else if(x > 0 & y < 0){
    c(0,1)
  }else if(x < 0 & y > 0){
    c(1,0)
  }
}

server <- function(input, output) {
  
  output$pca_animated <- renderPlot({
    if(input$clusters == FALSE){
      plot_col = 1
      plot_pch = 1
    }else{
      plot_col = brewer.pal(n = 8, name = 'Dark2')[kmeans_list[[input$clusters_n]][['cluster']]]
      plot_pch = 19
    }
    loadings_seq <- seq_matrix(from = matrix(rep(1, length(pca$rotation[,1:2])), ncol=2), 
                               to = pca$rotation[,1:2], 
                               length.out = 10, i = input$animate)
    pca_seq <- passat_scaled %*% loadings_seq
    plot(pca_seq, col = plot_col, pch = plot_pch)
    if(input$loadings){
      arrows(x0 = 0, y0 = 0, x1 = loadings_seq[,1]*input$loadings_scale, y1 = loadings_seq[,2]*input$loadings_scale, 
             col = "blueviolet", lwd=2)
    }
    if(input$loading_names){
      for(i in 1:4){
        text(labels = c("Price", "Year", "Mileage", "Engine capacity")[i], 
             adj = pos_adj(loadings_seq[i,1],loadings_seq[i,2]),
             x = loadings_seq[i,1]*input$loadings_scale*1.1, 
             y = loadings_seq[i,2]*input$loadings_scale*1.1,
             offset = 3, col = "blueviolet", cex=1.3)        
      }
    }
  })

#Potential exercise: Set "Click on the plot to see nearest observation values" as default text
  output$position_info <- renderText({
    loadings_seq <- seq_matrix(from = matrix(rep(1, length(pca$rotation[,1:2])), ncol=2), 
                               to = pca$rotation[,1:2], 
                               length.out = 10, i = input$animate)
    pca_seq <- passat_scaled %*% loadings_seq
    null_catch <- function(x) if(is.null(x)) 0 else x
    closest_point_id <- which.min(colSums((t(pca_seq) - c(null_catch(input$plot_click$x), null_catch(input$plot_click$y)))^2))
    paste0("Principal component 1 = ", input$plot_click$x,
           "\nPrincipal component 2 = ", input$plot_click$y,
           "\nNearest observation value:\nPrice = ", passat$price[closest_point_id], 
           "\nYear = ", passat$year[closest_point_id],
           '\nMileage = ', passat$mileage[closest_point_id],
           '\nEngine capacity = ', passat$engine[closest_point_id])
  })
  
  output$hca <- renderPlot({
    loadings_seq <- matrix(c(input$pc1price, input$pc1year, input$pc1mileage, input$pc1engine,
                             input$pc2price, input$pc2year, input$pc2mileage, input$pc2engine), ncol=2)
    pca_seq <- passat_scaled %*% loadings_seq
    plot(pca_seq, xlab='HC1', ylab='HC2', xlim = c(-10,10), ylim = c(-10,10))
    if(input$loadings2){
      arrows(x0 = 0, y0 = 0, x1 = loadings_seq[,1]*6, y1 = loadings_seq[,2]*6, 
             col = "blueviolet", lwd=2)
    }
    if(input$loading_names2){
      for(i in 1:4){
        text(labels = c("Price", "Year", "Mileage", "Engine capacity")[i], 
             adj = pos_adj(loadings_seq[i,1],loadings_seq[i,2]),
             x = loadings_seq[i,1]*6, 
             y = loadings_seq[i,2]*6,
             offset = 3, col = "blueviolet", cex=1.3)        
      }
    }
  })
  
  output$lasso <- renderPlot({
    plot(lasso, xvar='lambda',label=TRUE)
  })
  
  output$xgboost <- renderPlot({
    xgb.plot.importance(xgb.importance(model = xgb))
  })
  
  output$lassotext <- renderText({print("Lasso")})
  
  output$xgboosttext <- renderText({print("XGBoost")})
}

shinyApp(ui = ui, server = server)
