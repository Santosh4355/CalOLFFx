#Required Libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(shinydashboard)
library(DT)
library(scales)

#Load the data
df <- read.csv("Predictive modeling_olive fruit fly_2007 to 2024.csv", check.names = TRUE)


df$Date <- make_date(df$Year, df$Month, df$Day)
df$Region <- as.character(df$Region)

weather_cols <- c(
  "Weekly_average_temp_.F.",
  "Weekly_average_prep_.inch.",
  "Weekly_average_RH",
  "Weekly_avg_wind_speed_.mph.",
  "Weekly_avg_soil_temp_.F."
)

for (col in weather_cols) {
  if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
}


readable_theme <- theme_bw(base_size = 17) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.x = element_text(size = 17, face = "bold", margin = margin(t = 12)),
    axis.title.y = element_text(size = 17, face = "bold", margin = margin(r = 12)),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 13),
    legend.position = "bottom",
    legend.box = "vertical",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black", linewidth = 0.6),
    panel.grid.minor = element_blank()
  )


# USER INTERFACE

ui <- dashboardPage(
  dashboardHeader(title = "CalOLFFx Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      
      radioButtons(
        "region_mode", "Select Region:",
        choices = c("SSJV", "SV"),
        selected = "SSJV"
      ),
      
      uiOutput("location_ui"),
      
      dateRangeInput(
        "date_range", "Select Date Range:",
        start = min(df$Date, na.rm = TRUE),
        end = max(df$Date, na.rm = TRUE)
      ),
      
      selectInput(
        "interval", "Aggregation Interval:",
        choices = c("Week" = "week", "Month" = "month", "Year" = "year"),
        selected = "month"
      ),
      
      selectInput(
        "weather_var", "Weather Variable:",
        choices = c(
          "Average Temperature (F)" = "Weekly_average_temp_.F.",
          "Average Precipitation (inch)" = "Weekly_average_prep_.inch.",
          "Average Relative Humidity" = "Weekly_average_RH",
          "Average Wind Speed (mph)" = "Weekly_avg_wind_speed_.mph.",
          "Average Soil Temperature (F)" = "Weekly_avg_soil_temp_.F."
        ),
        selected = "Weekly_average_temp_.F."
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9 !important; }
        .box { border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.08); }
      "))
    ),
    
    fluidRow(
      box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title = "Note",
        "The dashboard includes olive fruit fly data from 2007 to 2024 for southern San Joaquin Valley (SSJV) and from 2011 to 2024
        for Sacramento Valley (SV). Data were selected primarily based on availability and accessibility, and for maintaining similar
        monitoring sites within a region for comparison. Comparisions of OLFF populations between SSJV and SV is likely not possible 
        because different types of traps were used in those regions."
      )
    ),
    
    fluidRow(
      valueBoxOutput("total_olff_box", width = 4),
      valueBoxOutput("male_box", width = 4),
      valueBoxOutput("female_box", width = 4)
    ),
    
    fluidRow(
      box(plotlyOutput("olff_plot", height = 470), width = 12)
    ),
    
    fluidRow(
      box(plotlyOutput("weather_plot", height = 470), width = 12)
    ),
    
    fluidRow(
      box(DTOutput("summary_table"), width = 12)
    )
  )
)


# Server

server <- function(input, output, session) {
  
  region_filtered <- reactive({
    req(input$region_mode)
    {
      df %>% filter(Region == input$region_mode)
    }
  })
  
  output$location_ui <- renderUI({
    locs <- region_filtered() %>%
      distinct(Location) %>%
      arrange(Location) %>%
      pull(Location)
    
    selectInput(
      "location", "Select Location(s):",
      choices = locs,
      selected = head(locs, min(3, length(locs))),
      multiple = TRUE
    )
  })
  
  filtered_data <- reactive({
    req(input$location, input$date_range, input$interval)
    
    region_filtered() %>%
      filter(Location %in% input$location) %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
      mutate(GroupDate = floor_date(Date, unit = input$interval)) %>%
      group_by(Region, Location, GroupDate) %>%
      summarise(
        Total_OLFF      = sum(Total, na.rm = TRUE),
        Total_Males     = sum(Count_males, na.rm = TRUE),
        Total_Females   = sum(Count_females, na.rm = TRUE),
        Avg_Temp_F      = mean(Weekly_average_temp_.F., na.rm = TRUE),
        Avg_Precip_Inch = mean(Weekly_average_prep_.inch., na.rm = TRUE),
        Avg_RH          = mean(Weekly_average_RH, na.rm = TRUE),
        Avg_Wind_MPH    = mean(Weekly_avg_wind_speed_.mph., na.rm = TRUE),
        Avg_Soil_Temp_F = mean(Weekly_avg_soil_temp_.F., na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  summary_data <- reactive({
    req(input$location, input$date_range)
    
    region_filtered() %>%
      filter(Location %in% input$location) %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
      group_by(Region, Location) %>%
      summarise(
        Total_OLFF      = sum(Total, na.rm = TRUE),
        Total_Males     = sum(Count_males, na.rm = TRUE),
        Total_Females   = sum(Count_females, na.rm = TRUE),
        Avg_Temp_F      = mean(Weekly_average_temp_.F., na.rm = TRUE),
        Avg_Precip_Inch = mean(Weekly_average_prep_.inch., na.rm = TRUE),
        Avg_RH          = mean(Weekly_average_RH, na.rm = TRUE),
        Avg_Wind_MPH    = mean(Weekly_avg_wind_speed_.mph., na.rm = TRUE),
        Avg_Soil_Temp_F = mean(Weekly_avg_soil_temp_.F., na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$total_olff_box <- renderValueBox({
    v <- sum(filtered_data()$Total_OLFF, na.rm = TRUE)
    valueBox(comma(v), "Total OLFF Population", color = "olive")
  })
  
  output$male_box <- renderValueBox({
    v <- sum(filtered_data()$Total_Males, na.rm = TRUE)
    valueBox(comma(v), "Total Male OLFF", color = "blue")
  })
  
  output$female_box <- renderValueBox({
    v <- sum(filtered_data()$Total_Females, na.rm = TRUE)
    valueBox(comma(v), "Total Female OLFF", color = "maroon")
  })
  
  output$olff_plot <- renderPlotly({
    p <- ggplot(
      filtered_data(),
      aes(
        x = GroupDate,
        y = Total_OLFF,
        color = Location,
        linetype = Region,
        text = paste0(
          "Region: ", Region,
          "<br>Location: ", Location,
          "<br>Date: ", GroupDate,
          "<br>Total OLFF: ", round(Total_OLFF, 2)
        )
      )
    ) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.6, alpha = 0.85) +
      labs(
        title = "OLFF Population by Location",
        x = "Date",
        y = "Total OLFF",
        color = "Location",
        linetype = "Region"
      ) +
      readable_theme
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.4, yanchor = "top",
          font = list(size = 14)
        ),
        margin = list(l = 100, r = 40, b = 160, t = 70),
        xaxis = list(
          automargin = TRUE, tickfont = list(size = 14),
          title = list(font = list(size = 17)),
          showline = TRUE, linewidth = 1.5, linecolor = "black", mirror = TRUE
        ),
        yaxis = list(
          automargin = TRUE, tickfont = list(size = 14),
          title = list(font = list(size = 17)),
          showline = TRUE, linewidth = 1.5, linecolor = "black", mirror = TRUE
        )
      )
  })
  
 
  output$weather_plot <- renderPlotly({
    req(input$weather_var)
    
    weather_map <- c(
      "Weekly_average_temp_.F." = "Avg_Temp_F",
      "Weekly_average_prep_.inch." = "Avg_Precip_Inch",
      "Weekly_average_RH" = "Avg_RH",
      "Weekly_avg_wind_speed_.mph." = "Avg_Wind_MPH",
      "Weekly_avg_soil_temp_.F." = "Avg_Soil_Temp_F"
    )
    
    weather_label_map <- c(
      "Weekly_average_temp_.F." = "Average Temperature (\u00B0F)",
      "Weekly_average_prep_.inch." = "Average Precipitation (inch)",
      "Weekly_average_RH" = "Average Relative Humidity (%)",
      "Weekly_avg_wind_speed_.mph." = "Average Wind Speed (mph)",
      "Weekly_avg_soil_temp_.F." = "Average Soil Temperature (\u00B0F)"
    )
    
    y_col <- weather_map[[input$weather_var]]
    y_label <- weather_label_map[[input$weather_var]]
    
    plot_data <- filtered_data() %>%
      arrange(Location, GroupDate)
    
    p <- ggplot(
      plot_data,
      aes(
        x = GroupDate,
        y = .data[[y_col]],
        color = Location,
        group = interaction(Location, Region),
        text = paste0(
          "Region: ", Region,
          "<br>Location: ", Location,
          "<br>Date: ", GroupDate,
          "<br>", y_label, ": ", round(.data[[y_col]], 2)
        )
      )
    ) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.2, alpha = 0.6) +
      labs(
        title = "Weather Comparison by Location",
        x = "Date",
        y = y_label,
        color = "Location"
      ) +
      readable_theme
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.4, yanchor = "top",
          font = list(size = 14)
        ),
        margin = list(l = 100, r = 40, b = 160, t = 70),
        xaxis = list(
          automargin = TRUE, tickfont = list(size = 14),
          title = list(font = list(size = 17)),
          showline = TRUE, linewidth = 1.5, linecolor = "black", mirror = TRUE
        ),
        yaxis = list(
          automargin = TRUE, tickfont = list(size = 14),
          title = list(font = list(size = 17)),
          showline = TRUE, linewidth = 1.5, linecolor = "black", mirror = TRUE
        )
      )
  })
  
  output$summary_table <- renderDT({
    datatable(
      summary_data(),
      options = list(scrollX = TRUE, pageLength = 15)
    ) %>%
      formatRound(
        columns = c("Avg_Temp_F", "Avg_Precip_Inch", "Avg_RH", "Avg_Wind_MPH", "Avg_Soil_Temp_F"),
        digits = 2
      )
  })
}

shinyApp(ui = ui, server = server)
