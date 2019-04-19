#Sys.setenv(TZ="America/Los_Angeles")
setwd("~/R/proj/nohrsc/shiny")
#setwd("~/Documents/shiny_nohrsc")

source("df_4.19cube.r")


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
        "nws_basin_code", "nws_basin_code", choices = unique(df$nws_basin_code), 
        selected = c("San Joaquin - Friant Dam (FRAC1)"), 
        multiple = TRUE),
      
      #   uiOutput("secondSelection"),
      
      checkboxGroupInput("parameter", "parameter [24-hr avg]",                
                         choices = unique(df$paramnam),
                         selected = "water equivalent (swe) [in]") ,
      
      radioButtons("free_scale", "y scale",                
                   choices = c("free", "fixed"), selected = "fixed", inline = T),
      
      
      
      selectInput('x', 'x-axis', c("date", "dowy")),
      
      
      selectInput('color', 'color', c("none",  "basin_zone", "param",  "wy", "dowy", "nwscode", "basin", "year", "rivgroup"), selected  = "basin_zone" ),
      
      selectInput('linetype', 'line type (eg dashed)', c("none",  "basin_zone", "param",  "wy", "dowy", "nwscode", "basin", "year", "rivgroup"), selected  = "none" ),
      
     # selectInput('group', 'group', c("none",  "basin_zone", "param",  "wy", "dowy", "nwscode", "basin", "year", "rivgroup"), selected  = "none" ),
      # 
      selectInput('facet_row', 'row group',
                  #c(None='.', names(df))),
                  c(none='.', "nws_basin_code", "nwscode", "basin_zone", "param",  "wy",   "basin", "year"),
                  selected = "nws_basin_code"),
      
      selectInput('facet_col', 'column group',
                  #c(None='.', names(df))
                  c(none='.', "nws_basin_code", "nwscode", "basin_zone", "param",  "wy",   "basin", "year"),
                  selected = "param"
                  
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
  
  
  #  df_s <- reactive({
  #    
  #    
  #    df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
  #      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
  #      filter(paramnam %in% input$parameter) %>%
  #      filter(basin_zone %in% input$basin_zone)
  #    
  #    return(df_s)
  #  })
  #  
  
  
  
  # output$secondSelection <- renderUI({
  #   selectInput("basin_zone"  ,"basin zone", choices = as.character(df[df$nws_basin_code==input$nws_basin_code && df$date <= input$end_date,'basin_zone']), multiple = TRUE)
  # })
  # 
  
  output$reg_plot <- renderPlot({
    df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(paramnam %in% input$parameter)# %>%
    # filter(basin_zone %in% input$basin_zone)
    
    p <- ggplot(df, aes_string(x=input$x, y= "numval")) + geom_point(size = 0.5) + geom_line()  + labs(y = NULL, x = NULL) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL))
    
    if (input$color != 'none')
      p <- p + aes_string(color=input$color)
    
    if (input$linetype != 'none')
      p <- p + aes_string(linetype=input$linetype)
    
    #  if (input$group != 'none')
    #    p <- p + aes_string(group=input$group)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    
    if (facets != '. ~ .' &&  input$free_scale == "free"  )
      p <- p + facet_grid(facets, scales = 'free_y') + theme(strip.text.y = element_text(angle = 0)) 
    
    if (facets != '. ~ .' &&  input$free_scale == "fixed"  )
      p <- p + facet_grid(facets) + theme(strip.text.y = element_text(angle = 0)) 
    
    
    
    print(p)
    
  })
}

shinyApp(ui, server)