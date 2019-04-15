#Sys.setenv(TZ="America/Los_Angeles")
setwd("~/R/proj/nohrsc/shiny")
#setwd("~/Documents/shiny_nohrsc")
#source("read_closecsv.r")
source("df_build_4.15.r")



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
      
      checkboxGroupInput("parameter", "parameter [24-hr avg]",                
                         choices = unique(df$paramnam),
                         selected = "water equivalent (swe) [in]") ,
      
      selectInput('x', 'x', c("date", "dowy")),
      # selectInput('y', 'y', "numval"),
      selectInput('color', 'color', c("none",  "location", "param",  "wy", "dowy", "nwscode", "basin", "year") ),
      
      selectInput('linetype', 'linetype', c("none",  "location", "param",  "wy", "dowy", "nwscode", "basin", "year") ),
      
      selectInput('facet_row', 'row group',
                  #c(None='.', names(df))),
                  c(None='.', "location", "param",  "wy", "dowy", "nwscode", "basin", "year")),
      
      selectInput('facet_col', 'column group',
                  #c(None='.', names(df))
                  c(None='.', "location", "param", "wy", "dowy", "nwscode", "basin", "year")
                  
      )        
      
    ),
    
    mainPanel(
      
      tabsetPanel(position=c("right"),
                  #            tabPanel(strong("plot image (clean)"), 
                  #                     br(),
                  #                     plotOutput("reg_plot",  height = "750px")) ,
                  
                  tabPanel(strong("static plot"), 
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
    
    p <- ggplot(df, aes_string(x=input$x, y="numval")) + geom_line() + geom_point()
    
    if (input$color != 'none')
      p <- p + aes_string(color=input$color)
    
    if (input$linetype != 'none')
      p <- p + aes_string(color=input$linetype)
    
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets, scales = "free_y") +
      theme(strip.text.y = element_text(angle = 0))
    
    print(p)
    
  })
}

shinyApp(ui, server)