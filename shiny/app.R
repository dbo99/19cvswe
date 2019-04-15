#Sys.setenv(TZ="America/Los_Angeles")
setwd("~/Documents/shiny_nohrsc")
#source("read_closecsv.r")
source("df_build.r")



ui <- 
  
  shinyUI(fluidPage(
    
    br(),
    
    sidebarLayout(sidebarPanel(
      
      dateInput('start_date',
                label = 'start day',
                value = Sys.Date()-27), 
  
      dateInput('end_date',
                label = 'end day',
                value = Sys.Date()-20) ,

    
    selectizeInput(
      'nwscode', 'nwsid', choices = unique(df$nwscode), multiple = TRUE),
    
      checkboxGroupInput("parameter", "parameter",                
                   choices = unique(df$paramnam),
                   selected = "water equivalent (swe) [in]") ,
                   
             selectInput('x', 'x', c("date", "dowy")),
            # selectInput('y', 'y', "numval"),
             selectInput('color', 'color', c("location", "param",  "wy", "dowy", "nwscode", "basin", "year") ),
                         
             selectInput('facet_row', 'facet row',
             #c(None='.', names(df))),
             c(None='.', "location", "param",  "wy", "dowy", "nwscode", "basin", "year")),
                           
             selectInput('facet_col', 'facet column',
             #c(None='.', names(df))
             c(None='.', "location", "param", "wy", "dowy", "nwscode", "basin", "year")
                         
                   )        
 
                   ),
                  
  mainPanel(
    
    tabsetPanel(position=c("right"),
    #            tabPanel(strong("plot image (clean)"), 
    #                     br(),
    #                     plotOutput("reg_plot",  height = "750px")) ,
                
                tabPanel(strong("plot image (clean)"), 
                         br(),
                         plotOutput("reg_plot",  height = "750px")) )))
                
    ) 
    )
  

########################
#### server.r
########################


server <- function(input, output) {


output$reg_plot <- renderPlot({
  df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
    filter(nwscode %in% input$nwscode)   %>% 
    filter(paramnam %in% input$parameter) 
  
  p <- ggplot(df, aes_string(x=input$x, y="numval")) + geom_point()
  
  if (input$color != 'None')
    p <- p + aes_string(color=input$color)
  
  facets <- paste(input$facet_row, '~', input$facet_col)
  if (facets != '. ~ .')
    p <- p + facet_grid(facets, scales = "free_y")
  
  print(p)
  
})
}

shinyApp(ui, server)
