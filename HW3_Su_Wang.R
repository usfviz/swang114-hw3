#setwd('~/Documents/2016-2017_USF/2017/MSAN622_Visualization/HW3/swang114-hw3/')

library(ggplot2)
library(shiny)
library(reshape2)
#install.packages('psych')
library(psych)
#install.packages('GGally')
library(GGally)

DF_raw <- read.csv("dataset_Facebook.csv", sep = ";",header = TRUE)
str(DF_raw)

ui <- fluidPage(
  
  headerPanel(h4('MSAN 622 Assignment 3 - Su Wang')),
  
  sidebarPanel(
    checkboxGroupInput("type", "Types to show:", unique(DF_raw$Type)),
    checkboxGroupInput("col", "Correlations to show:", c("Page.total.likes", "Post.Weekday","comment","like","share", "Total.Interactions"))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Bubble", plotOutput("ggplot", hover = "plot_hover"), uiOutput("hover_info")),
      tabPanel("Scatterplot Matrix", plotOutput("scatter")),
      tabPanel("Parallel Coordinates", plotOutput("parallel"))
    )
  )
)

server <- function(input, output){
  
  #input_year <- reactive({input$year})
  input_type <- reactive({input$type})
  input_col <- reactive({input$col})
  
  output$ggplot <- renderPlot({
    if (is.null(input_type())){
      ggplot() + geom_point(data = DF_raw, 
                   aes( x = comment, y = share, size = like, fill = Type), colour="black", pch=21, alpha=.95) + 
        xlim(0,180) + ylim(0,250) + 
        scale_size(guide = "none") +
        xlab("\n Comment \n (point size represents post likes)") + ylab("Share \n") + 
        ggtitle("Comment and Shares \n") + 
        scale_size_area(max_size=30, guide = "none") + 
        theme(text = element_text(size=15)) + 
        guides(fill = guide_legend(override.aes = list(size=8)))
    }
    else{
      ggplot() + 
        geom_point(data = subset(DF_raw, Type == input_type()), 
                   aes( x = comment, y = share, size = like, fill = Type), alpha=.95, colour="black", pch=21) +
        geom_point(data = subset(DF_raw, Type != input_type()), 
                   aes( x = comment, y = share, size = like, fill = Type), alpha=.02, colour="black", pch=21) +
        xlim(0,180) + ylim(0,250) + 
        scale_size(guide = "none") +
        xlab("\n Comment \n (point size represents post likes)") + ylab("Share \n") +
        ggtitle("Comment and Shares \n") +
        scale_size_area(max_size=30, guide = "none") + 
        theme(text = element_text(size=15)) + 
        guides(fill = guide_legend(override.aes = list(size=8)))
    }
  },
  height = 500, width = 700)
  
  output$hover_info <- renderUI({
    
    hover <- input$plot_hover
    point <- nearPoints(DF_raw, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    left_ <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_ <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px <- hover$range$left + left_ * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_ * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(style = style, p(HTML(paste0("<b>", point$`Type`,"</b>",
                                           "<br> Page Total Likes: ", point$Page.total.likes)))
    )
  })
  
  output$scatter <- renderPlot({
    if (is.null(input_col())){
    pairs.panels(DF_raw[,c(1,5,16,17,18,19)], 
                 method = "pearson", 
                 hist.col = "#00AFBB",
                 density = TRUE,  # show density plots
                 ellipses = TRUE, # show correlation ellipses
                 main = "\n Scatterplot Matrix \n"
    )
    }
    else if(length(c(input_col()))==1){
      pairs.panels(DF_raw[,c(1,5,16,17,18,19)], 
                   method = "pearson", 
                   hist.col = "#00AFBB",
                   density = TRUE,
                   ellipses = TRUE,
                   main = "\n Scatterplot Matrix \n Please select at least two variables!"
      )
    }
    else{
      pairs.panels(DF_raw[,c(input_col())], 
                   method = "pearson", 
                   hist.col = "#00AFBB",
                   density = TRUE,
                   ellipses = TRUE,
                   main = "\n Scatterplot Matrix \n"
      )
    }
  }, height = 500, width = 700)
  
  
  output$parallel <- renderPlot({
    if (is.null(input_type())){
    ggparcoord(data = DF_raw, 
               columns=16:19, 
               groupColumn = "Type",
               shadeBox = NULL,
               scale = "uniminmax")
    }
    else{
      ggparcoord(data = subset(DF_raw, Type == input_type()), 
                 columns=16:19, 
                 groupColumn = "Type",
                 shadeBox = NULL,
                 scale = "uniminmax")
    }
}, height = 500, width = 700)
  
}

shinyApp(ui = ui, server = server)

