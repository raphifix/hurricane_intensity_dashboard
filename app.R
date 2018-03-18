## app.R ##
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggjoy)
library(rvest)

########### load data

df <- 
  read_html('http://www.aoml.noaa.gov/hrd/tcfaq/E11.html') %>%
  html_node(xpath = '//table') %>%
  html_table(fill = T)

df <- tail(df, -8)
df <- df[-2,c(1:5)]
names(df) <- df[1, ]
df <- df[-1, ]
df <- head(df, -6)
df <- as.data.frame(sapply(df, as.numeric))

ui <- dashboardPage(
  dashboardHeader(title = "Atlantic Hurricane Intensity: Comparisons Over Time", titleWidth = 600),
  dashboardSidebar(disable = T),
  dashboardBody(
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      '))),
    # Boxes need to be put in a row (or column)
    fluidRow(
      tabBox(width = 8,
             tabPanel('Named Storms', plotOutput("plot1", height = '500px')),
             tabPanel('Hurricanes', plotOutput("plot2", height = '500px')),
             tabPanel('Major Hurricanes', plotOutput("plot3", height = '500px')),
             tabPanel('ACE', plotOutput("plot4", height = '500px'))),
      
      box(width = 4,
        title = "Time Ranges to Compare",
        sliderInput("range", "Range 1:",
                    min = min(df$Year), max = max(df$Year),
                    value = c(min(df$Year),max(df$Year)),
                    sep = '',
                    animate = F),
        sliderInput("range2", "Range 2:",
                    min = min(df$Year), max = max(df$Year),
                    value = c(min(df$Year),max(df$Year)),
                    sep = '',
                    animate = F),
        tags$p('From NOAA:'), tags$p('The Atlantic hurricane database (or HURDAT) extends back to 1851. However, because tropical storms and hurricanes spend much of their lifetime over the open ocean - some never hitting land - many systems were "missed" during the late 19th and early 20th Centuries (Vecchi and Knutson 2008). Starting in 1944, systematic aircraft reconnaissance was commenced for monitoring both tropical cyclones and disturbances that had the potential to develop into tropical storms and hurricanes. This did provide much improved monitoring, but still about half of the Atlantic basin was not covered (Sheets 1990). Beginning in 1966, daily satellite imagery became available at the National Hurricane Center, and thus statistics from this time forward are most complete (McAdie et al. 2009).
For hurricanes striking the USA Atlantic and Gulf coasts, one can go back further in time with relatively reliable counts of systems because enough people have lived along coastlines since 1900.'))
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    data <-
      df_reactive() %>% 
      arrange(desc(Range))
    data$Range[which(data$Range == 1)] <- paste0(input$range[1], '-', input$range[2])
    data$Range[which(data$Range == 2)] <- paste0(input$range2[1], '-', input$range2[2])
    ggplot(data, aes(x = NamedStorms, y = Range, group = Range, fill = Range)) + 
      geom_joy(scale = 5, alpha = 0.75) +
      guides(fill = F) +
      labs(x = '',
           y = '',
           title = paste0('Distribution of Named Storms: ', paste0(input$range[1], '-', input$range[2]), ' vs. ', paste0(input$range2[1], '-', input$range2[2])),
           caption = 'Source: NOAA (http://www.aoml.noaa.gov/hrd/tcfaq/E11.html)') +
      scale_x_continuous(expand = c(0.01, 0)) +
      scale_y_discrete(expand = c(0.01, 0)) +
      theme_joy(grid = F)
  })
  
  df_reactive <- reactive({
    data <- df[which(df$Year >= input$range[1] & df$Year <= input$range[2]),]
    data$Range <- 1
    data2 <- df[which(df$Year >= input$range2[1] & df$Year <= input$range2[2]),]
    data2$Range <- 2
    data <- 
      rbind(data, data2) %>% 
      select(Range, NamedStorms) %>% 
      mutate(Range = as.character(Range))
  })
  
  output$plot2 <- renderPlot({
    data <-
      df_reactive2() %>% 
      arrange(desc(Range))
    data$Range[which(data$Range == 1)] <- paste0(input$range[1], '-', input$range[2])
    data$Range[which(data$Range == 2)] <- paste0(input$range2[1], '-', input$range2[2])
    ggplot(data, aes(x = Hurricanes, y = Range, group = Range, fill = Range)) + 
      geom_joy(scale = 5, alpha = 0.75) +
      guides(fill = F) +
      labs(x = '',
           y = '',
           title = paste0('Distribution of Hurricanes: ', paste0(input$range[1], '-', input$range[2]), ' vs. ', paste0(input$range2[1], '-', input$range2[2])),
           caption = 'Source: NOAA (http://www.aoml.noaa.gov/hrd/tcfaq/E11.html)') +
      scale_x_continuous(expand = c(0.01, 0)) +
      scale_y_discrete(expand = c(0.01, 0)) +
      theme_joy(grid = F)
  })
  
  df_reactive2 <- reactive({
    data <- df[which(df$Year >= input$range[1] & df$Year <= input$range[2]),]
    data$Range <- 1
    data2 <- df[which(df$Year >= input$range2[1] & df$Year <= input$range2[2]),]
    data2$Range <- 2
    data <- 
      rbind(data, data2) %>% 
      select(Range, Hurricanes) %>% 
      mutate(Range = as.character(Range))
  })
  
  output$plot3 <- renderPlot({
    data <-
      df_reactive3() %>% 
      arrange(desc(Range))
    data$Range[which(data$Range == 1)] <- paste0(input$range[1], '-', input$range[2])
    data$Range[which(data$Range == 2)] <- paste0(input$range2[1], '-', input$range2[2])
    ggplot(data, aes(x = MajorHurricanes, y = Range, group = Range, fill = Range)) + 
      geom_joy(scale = 5, alpha = 0.75) +
      guides(fill = F) +
      labs(x = '',
           y = '',
           title = paste0('Distribution of Major Hurricanes: ', paste0(input$range[1], '-', input$range[2]), ' vs. ', paste0(input$range2[1], '-', input$range2[2])),
           caption = 'Source: NOAA (http://www.aoml.noaa.gov/hrd/tcfaq/E11.html)') +
      scale_x_continuous(expand = c(0.01, 0)) +
      scale_y_discrete(expand = c(0.01, 0)) +
      theme_joy(grid = F)
  })
  
  df_reactive3 <- reactive({
    data <- df[which(df$Year >= input$range[1] & df$Year <= input$range[2]),]
    data$Range <- 1
    data2 <- df[which(df$Year >= input$range2[1] & df$Year <= input$range2[2]),]
    data2$Range <- 2
    data <- 
      rbind(data, data2) %>% 
      select(Range, MajorHurricanes) %>% 
      mutate(Range = as.character(Range))
  })
  
  output$plot4 <- renderPlot({
    data <-
      df_reactive4() %>% 
      arrange(desc(Range))
    data$Range[which(data$Range == 1)] <- paste0(input$range[1], '-', input$range[2])
    data$Range[which(data$Range == 2)] <- paste0(input$range2[1], '-', input$range2[2])
    ggplot(data, aes(x = ACE, y = Range, group = Range, fill = Range)) + 
      geom_joy(scale = 5, alpha = 0.75) +
      guides(fill = F) +
      labs(x = '',
           y = '',
           title = paste0('Distribution of ACE: ', paste0(input$range[1], '-', input$range[2]), ' vs. ', paste0(input$range2[1], '-', input$range2[2])),
           subtitle = 'Accumulated Cyclone Energy - An index that combines the numbers of systems,\nhow long they existed and how intense they became.',
           caption = 'Source: NOAA (http://www.aoml.noaa.gov/hrd/tcfaq/E11.html)') +
      scale_x_continuous(expand = c(0.01, 0)) +
      scale_y_discrete(expand = c(0.01, 0)) +
      theme_joy(grid = F)
  })
  
  df_reactive4 <- reactive({
    data <- df[which(df$Year >= input$range[1] & df$Year <= input$range[2]),]
    data$Range <- 1
    data2 <- df[which(df$Year >= input$range2[1] & df$Year <= input$range2[2]),]
    data2$Range <- 2
    data <- 
      rbind(data, data2) %>% 
      select(Range, ACE) %>% 
      mutate(Range = as.character(Range))
  })
}

shinyApp(ui, server)
