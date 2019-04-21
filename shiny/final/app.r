#Sys.setenv(TZ="America/Los_Angeles")
#setwd("~/R/proj/nohrsc/shiny")
setwd("~/Documents/shiny_nohrsc/final")
#setwd("~/R/proj/nohrsc/shiny/final")

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
        "nws_basin_code", "nws_basin_code", choices = unique(df$nws_basin_code), 
        selected = c("San Joaquin - Friant Dam (FRAC1)"), 
        multiple = TRUE),
      
      #   uiOutput("secondSelection"),
      
      checkboxGroupInput("parameter", "parameter [24-hr avg]",                
                         choices = unique(df$paramnam),
                         selected = "water equivalent (swe) [in]") ,
      
      radioButtons("free_scale", "y scale",                
                   choices = c("free", "fixed"), selected = "fixed", inline = T),
      
      radioButtons("resctricttodowy", "only dates within day of year range",                
                   choices = c("yes", "no"), selected = "no", inline = T),
      
      
      selectizeInput('x', 'x-axis', c("date", "dowy")),
      
      
      selectizeInput('color', 'color', c("none",  "basin_zone", "param",  "wy", "dowy", "nwscode", "basin", "year", "rivgroup"), selected  = "basin_zone" ),
      
      selectizeInput('linetype', 'line type (eg dashed)', c("none",  "basin_zone", "param",  "wy", "dowy", "nwscode", "basin", "year", "rivgroup"), selected  = "none" ),
      
      # selectInput('group', 'group', c("none",  "basin_zone", "param",  "wy", "dowy", "nwscode", "basin", "year", "rivgroup"), selected  = "none" ),
      # 
      selectizeInput('facet_row', 'plot row group',
                  #c(None='.', names(df))),
                  c(none='.', "nws_basin_code", "nwscode", "basin_zone", "param",  "wy",   "basin", "year"),
                  selected = "nws_basin_code"),
      
      selectizeInput('facet_col', 'plot column group',
                  #c(None='.', names(df))
                  c(none='.', "nws_basin_code", "nwscode", "basin_zone", "param",  "wy",   "basin", "year"),
                  selected = "param"
                  
      )        
      
    ),
    
    mainPanel(
      
      tabsetPanel(position=c("right"),
                              tabPanel(strong("static"), 
                                       br(),
                                       plotOutput("reg_plot",  height = "750px")) ,
                  
                  tabPanel(strong("hoverable"), 
                           br(),
                           #plotOutput("reg_plot",  height = "750px")) )))
                           plotlyOutput("plotly_plot",  height = "750px")) )))
  ) 
  )

########################
#### server.r
########################


server <- function(input, output) {


  
  output$reg_plot <- renderPlot({
   
    
    
    
    startdowy <- dowy %>% filter(date == input$start_date) 
    startdowy <- startdowy$dowy
    enddowy <- dowy %>% filter(date == input$end_date) 
    enddowy <- enddowy$dowy
    
    
    
    if (input$resctricttodowy == "no")
    df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(paramnam %in% input$parameter)
    
    if (input$resctricttodowy == "yes")

      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
        filter(nws_basin_code %in% input$nws_basin_code)   %>% 
        filter(paramnam %in% input$parameter) %>% filter(dowy >= startdowy) %>% filter(dowy <= enddowy)
    
    
    p <- ggplot(df, aes_string(x=input$x, y= "numval")) + geom_point(size = 0.5) + geom_line()  + labs(y = NULL, x = NULL) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) + theme_bw(base_size=18)
   
    if (input$color != 'none')
      p <- p + aes_string(color=input$color)
    
    if (input$linetype != 'none')
      p <- p + aes_string(linetype=input$linetype)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    
    if (facets != '. ~ .' &&  input$free_scale == "free"  )
      p <- p + facet_grid(facets, scales = 'free_y') + theme(strip.text.y = element_text(angle = 0)) 
    
    if (facets != '. ~ .' &&  input$free_scale == "fixed"  )
      p <- p + facet_grid(facets) + theme(strip.text.y = element_text(angle = 0)) 
    
    
    
    print(p)
    
  }) 
    
    
    output$plotly_plot <- renderPlotly({
      
      
      
      startdowy <- dowy %>% filter(date == input$start_date) 
      startdowy <- startdowy$dowy
      enddowy <- dowy %>% filter(date == input$end_date) 
      enddowy <- enddowy$dowy
      
      
      
      if (input$resctricttodowy == "no")
        df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
        filter(nws_basin_code %in% input$nws_basin_code)   %>% 
        filter(paramnam %in% input$parameter)
      
      if (input$resctricttodowy == "yes")
        
        df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
        filter(nws_basin_code %in% input$nws_basin_code)   %>% 
        filter(paramnam %in% input$parameter) %>% filter(dowy >= startdowy) %>% filter(dowy <= enddowy)
      
      
      p <- ggplot(df, aes_string(x=input$x, y= "numval")) + geom_point(size = 0.5) + geom_line()  + labs(y = NULL, x = NULL) +
        scale_y_continuous(sec.axis = dup_axis(name = NULL)) + theme_bw(base_size=18)
      
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
      
      p <- ggplotly(p)
      
      print(p)
      
    })
    
    
}

shinyApp(ui, server)