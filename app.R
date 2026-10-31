library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(scales)
library(openxlsx)

# Data and shared helpers
weather_map <- c(
  "Mean temperature (degrees F)" = "Weekly_average_temp_.F.",
  "Mean precipitation (inches)" = "Weekly_average_prep_.inch.",
  "Mean relative humidity (%)" = "Weekly_average_RH",
  "Mean wind speed (mph)" = "Weekly_avg_wind_speed_.mph.",
  "Mean soil temperature (degrees F)" = "Weekly_avg_soil_temp_.F."
)
weather_cols <- unname(weather_map)
count_cols <- c("Total", "Count_males", "Count_females")
metric_cols <- c(count_cols, weather_cols)
labels <- c(
  Region = "Region", Location = "Location", GroupDate = "Period start",
  Records = "Source records", Total = "OLFF captured",
  Count_males = "Male OLFF captured", Count_females = "Female OLFF captured",
  setNames(names(weather_map), weather_cols),
  setNames(paste("Available records:", c("total OLFF", "male OLFF", "female OLFF",
                                         names(weather_map))), paste0(metric_cols, "_n"))
)

df <- read.csv("Predictive modeling_olive fruit fly_2007 to 2024.csv", check.names = TRUE)
required <- c("Year", "Month", "Day", "Region", "Location", metric_cols)
missing_cols <- setdiff(required, names(df))
if (length(missing_cols)) stop("Missing columns: ", paste(missing_cols, collapse = ", "))
df$Date <- make_date(df$Year, df$Month, df$Day)
if (!any(!is.na(df$Date))) stop("No valid dates found.")
df$Region <- as.character(df$Region)
df$Location <- as.character(df$Location)
for (nm in metric_cols) {
  df[[nm]] <- suppressWarnings(as.numeric(as.character(df[[nm]])))
  df[[nm]][!is.finite(df[[nm]])] <- NA_real_
}

safe_sum <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
fmt <- function(x, accuracy = 1) ifelse(is.na(x), "Not available", comma(x, accuracy = accuracy))
pretty_data <- function(x) {
  names(x) <- unname(labels[names(x)])
  x
}
aggregate_data <- function(x, groups) {
  x %>% group_by(across(all_of(groups))) %>% summarise(
    Records = n(), across(all_of(count_cols), safe_sum),
    across(all_of(weather_cols), safe_mean),
    across(all_of(metric_cols), ~ sum(!is.na(.x)), .names = "{.col}_n"),
    .groups = "drop"
  )
}
note <- paste(
  "Data cover 2007–2024 for southern San Joaquin Valley (SSJV) and 2011–2024",
  "for Sacramento Valley (SV). Sites were selected based on data availability",
  "and continuity. Counts should not be directly compared between regions",
  "because different trap types were used. Counts represent captures, not true population estimates."
)
method_note <- paste(
  "Counts sum available source records; weather values are unweighted means of",
  "available source records. Mean precipitation is not accumulated rainfall."
)

# Interface
ui <- fluidPage(
  title = "CalOLFFx Dashboard",
  tags$head(
    tags$script(HTML("document.documentElement.lang = 'en';
      document.addEventListener('DOMContentLoaded', function() {
        var dates = document.querySelectorAll('#date_range input');
        if (dates.length === 2) {
          dates[0].setAttribute('aria-label', 'Start date, year-month-day');
          dates[1].setAttribute('aria-label', 'End date, year-month-day');
        }
      });")),
    tags$style(HTML("
      body { font-size: 17px; color: #17212b; background: #f4f6f9; }
      h1, h2, h3 { line-height: 1.3; }
      a { color: #174e80; text-decoration: underline; }
      .btn-primary { background: #174e80; border-color: #174e80; color: white; }
      :focus { outline: 3px solid #17212b !important; outline-offset: 3px; }
      .skip-link { position: absolute; left: -10000px; }
      .skip-link:focus { left: 12px; top: 12px; z-index: 9999; background: white; padding: 12px; }
      .panel-card { background: white; padding: 20px; margin-bottom: 22px; border: 1px solid #687581; border-radius: 6px; }
      .table-scroll { overflow-x: auto; max-width: 100%; }
      table { width: 100%; border-collapse: collapse; }
      caption { color: #17212b; font-weight: bold; }
      th, td { border: 1px solid #687581; padding: 9px; text-align: left; }
      thead th { background: #e6edf3; }
      .form-control { color: #17212b; border-color: #687581; font-size: 16px; }
      .shiny-output-error-validation { color: #8b1e1e; }
      .help-block { color: #39434d; }
      .plotly { max-width: 100%; }
    "))
  ),
  tags$a(href = "#main-results", class = "skip-link", "Skip to results"),
  tags$header(h1("CalOLFFx Dashboard"), p("Olive fruit fly (OLFF) captures and weather observations")),
  fluidRow(
    column(3,
           tags$aside(`aria-label` = "Dashboard filters", class = "panel-card",
                      h2("Filters"),
                      radioButtons("region_mode", "Region", c("SSJV", "SV"), selected = "SSJV"),
                      uiOutput("location_ui"),
                      dateRangeInput("date_range", "Date range", start = min(df$Date, na.rm = TRUE),
                                     end = max(df$Date, na.rm = TRUE), format = "yyyy-mm-dd"),
                      selectInput("interval", "Aggregation interval",
                                  c("Week" = "week", "Month" = "month", "Year" = "year"),
                                  selected = "month", selectize = FALSE),
                      selectInput("weather_var", "Weather variable", weather_map, selectize = FALSE),
                      downloadButton("download_excel", "Download results (Excel)", class = "btn-primary"),
                      p("The download includes all selected records summarized by location and period, regardless of table page."),
                      tags$div(role = "status", `aria-live` = "polite", `aria-atomic` = "true", textOutput("selection_status"))
           )
    ),
    column(9,
           tags$main(id = "main-results", tabindex = "-1",
                     tags$section(class = "panel-card", h2("About these data"), p(note), p(method_note)),
                     tags$section(class = "panel-card", h2("Selected results"), uiOutput("totals")),
                     tags$section(class = "panel-card", h2("Captures over time"),
                                  uiOutput("capture_summary"),
                                  tags$a(href = "#time-data", "Read the chart data table"),
                                  tags$div(role = "region", `aria-label` = "Capture chart", `aria-describedby` = "capture_summary",
                                           plotlyOutput("olff_plot", height = "460px"))),
                     tags$section(class = "panel-card", h2("Weather over time"),
                                  uiOutput("weather_summary"),
                                  tags$a(href = "#time-data", "Read the chart data table"),
                                  tags$div(role = "region", `aria-label` = "Weather chart", `aria-describedby` = "weather_summary",
                                           plotlyOutput("weather_plot", height = "460px"))),
                     tags$section(class = "panel-card", h2("Location summary"),
                                  numericInput("summary_page", "Location summary page (25 rows per page)", 1, min = 1, step = 1),
                                  uiOutput("summary_table")),
                     tags$section(id = "time-data", tabindex = "-1", class = "panel-card", h2("Chart data"),
                                  p("Includes capture counts and the selected weather variable. All weather variables are included in Excel."),
                                  numericInput("time_page", "Chart data page (25 rows per page)", 1, min = 1, step = 1),
                                  uiOutput("time_table"))
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$location_ui <- renderUI({
    locs <- sort(unique(df$Location[df$Region == input$region_mode & !is.na(df$Location)]))
    tagList(
      tags$fieldset(tags$legend("Locations"),
                    checkboxGroupInput("location", NULL, choices = locs, selected = head(locs, 3)))
    )
  })
  selected_data <- reactive({
    validate(need(length(input$location) > 0, "Select at least one location."))
    req(input$date_range)
    validate(need(length(input$date_range) == 2 && !anyNA(input$date_range), "Enter both dates."),
             need(input$date_range[1] <= input$date_range[2], "The start date must precede the end date."))
    df %>% filter(Region == input$region_mode, Location %in% input$location,
                  Date >= input$date_range[1], Date <= input$date_range[2])
  })
  filtered_data <- reactive({
    selected_data() %>% mutate(GroupDate = floor_date(Date, unit = input$interval, week_start = 7)) %>%
      aggregate_data(c("Region", "Location", "GroupDate")) %>% arrange(Location, GroupDate)
  })
  summary_data <- reactive({
    aggregate_data(selected_data(), c("Region", "Location")) %>% arrange(Location)
  })
  selection_text <- reactive({
    x <- selected_data()
    paste("Region:", input$region_mode, ". Locations:", paste(input$location, collapse = ", "),
          ". Selected dates:", input$date_range[1], "to", input$date_range[2],
          ". Aggregation:", input$interval, ". Source records:", nrow(x), ".")
  })
  output$selection_status <- renderText({
    if (!nrow(selected_data())) return("No records match these filters. Change the locations or dates.")
    selection_text()
  })
  output$totals <- renderUI({
    x <- selected_data()
    validate(need(nrow(x) > 0, "No records match these filters."))
    tagList(p(selection_text()), tags$ul(lapply(count_cols, function(nm) {
      tags$li(paste0(labels[[nm]], ": ", fmt(safe_sum(x[[nm]])),
                     ". Available records: ", sum(!is.na(x[[nm]])), " of ", nrow(x), "."))
    })))
  })
  capture_text <- reactive({
    x <- filtered_data()
    if (!nrow(x)) return("No capture records match these filters.")
    vapply(split(x, x$Location), function(z) {
      ok <- !is.na(z$Total)
      if (!any(ok)) return(paste(z$Location[1], ": capture counts are not available."))
      peak <- max(z$Total, na.rm = TRUE)
      dates <- z$GroupDate[ok & z$Total == peak]
      paste0(z$Location[1], ": ", fmt(safe_sum(z$Total)), " OLFF captured in available records. ",
             "Highest ", input$interval, " total: ", fmt(peak), "; first period starting ", min(dates),
             if (length(dates) > 1) paste0(" (tied across ", length(dates), " periods)") else "", ". ",
             sum(z$Total_n), " of ", sum(z$Records), " source records contain total counts.")
    }, character(1), USE.NAMES = FALSE)
  })
  weather_text <- reactive({
    x <- filtered_data()
    if (!nrow(x)) return("No weather records match these filters.")
    nm <- input$weather_var
    vapply(split(x, x$Location), function(z) {
      y <- z[[nm]]
      if (all(is.na(y))) return(paste(z$Location[1], ":", labels[[nm]], "is not available."))
      paste0(z$Location[1], ": ", labels[[nm]], " ranges from ", fmt(min(y, na.rm = TRUE), .01),
             " to ", fmt(max(y, na.rm = TRUE), .01), " across available ", input$interval,
             " averages. ", sum(z[[paste0(nm, "_n")]]), " of ", sum(z$Records),
             " source records contain this measurement.")
    }, character(1), USE.NAMES = FALSE)
  })
  output$capture_summary <- renderUI(tags$ul(lapply(capture_text(), tags$li)))
  output$weather_summary <- renderUI(tags$ul(lapply(weather_text(), tags$li)))
  
  make_plot <- function(x, metric) {
    validate(need(nrow(x) > 0, "No records match these filters."),
             need(any(!is.na(x[[metric]])), "Measurements are not available for this selection."))
    locs <- sort(unique(x$Location))
    x$Location <- factor(x$Location, levels = locs)
    x$Tooltip <- paste0("Location: ", x$Location, "<br>Period start: ", x$GroupDate,
                        "<br>", labels[[metric]], ": ", fmt(x[[metric]], if (metric == "Total") 1 else .01))
    p <- ggplot(x, aes(x = GroupDate, y = .data[[metric]], color = Location,
                       linetype = Location, shape = Location, group = Location, text = Tooltip)) +
      geom_line(linewidth = .8, na.rm = TRUE) + geom_point(size = 2.4, na.rm = TRUE) +
      scale_linetype_manual(values = rep(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), length.out = length(locs))) +
      scale_shape_manual(values = rep(c(16, 17, 15, 18, 3, 7, 8), length.out = length(locs))) +
      labs(x = "Period start", y = labels[[metric]]) +
      theme_bw(base_size = 16) + theme(legend.position = "bottom",
                                       axis.text = element_text(color = "#17212b"), panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text") %>% layout(
      legend = list(orientation = "h", y = -.3),
      margin = list(l = 85, r = 25, b = 130, t = 30),
      xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)
    ) %>% config(displaylogo = FALSE, scrollZoom = FALSE, responsive = TRUE)
  }
  output$olff_plot <- renderPlotly(make_plot(filtered_data(), "Total"))
  output$weather_plot <- renderPlotly(make_plot(filtered_data(), input$weather_var))
  
  table_view <- function(x, page, title) {
    if (!nrow(x)) return(p("No records match these filters."))
    pages <- ceiling(nrow(x) / 25)
    if (is.null(page) || !is.finite(page)) page <- 1
    page <- max(1, min(pages, as.integer(page)))
    first <- (page - 1) * 25 + 1
    last <- min(page * 25, nrow(x))
    x <- pretty_data(x[first:last, , drop = FALSE])
    cell <- function(v) {
      if (is.na(v)) return("Not available")
      if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
      if (is.numeric(v)) return(fmt(v, .01))
      as.character(v)
    }
    tags$div(class = "table-scroll", tabindex = "0", role = "region", `aria-label` = title,
             tags$table(
               tags$caption(paste0(title, ": rows ", first, "–", last, "; page ", page, " of ", pages, ".")),
               tags$thead(tags$tr(lapply(names(x), function(nm) tags$th(scope = "col", nm)))),
               tags$tbody(lapply(seq_len(nrow(x)), function(i) tags$tr(lapply(seq_along(x), function(j) {
                 if (names(x)[j] == "Location") tags$th(scope = "row", cell(x[[j]][i])) else tags$td(cell(x[[j]][i]))
               }))))
             )
    )
  }
  observeEvent(list(input$region_mode, input$location, input$date_range, input$interval), {
    updateNumericInput(session, "summary_page", value = 1)
    updateNumericInput(session, "time_page", value = 1)
  }, ignoreInit = TRUE)
  observe({
    updateNumericInput(session, "summary_page", max = max(1, ceiling(nrow(summary_data()) / 25)))
    updateNumericInput(session, "time_page", max = max(1, ceiling(nrow(filtered_data()) / 25)))
  })
  output$summary_table <- renderUI({
    table_view(select(summary_data(), Region, Location, Records, all_of(count_cols), all_of(weather_cols)),
               input$summary_page, "Location totals and weather means")
  })
  output$time_table <- renderUI({
    table_view(select(filtered_data(), Region, Location, GroupDate, Records, all_of(count_cols), all_of(input$weather_var)),
               input$time_page, "Values underlying the charts")
  })
  
  output$download_excel <- downloadHandler(
    filename = function() paste0("CalOLFFx_", input$region_mode, "_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      x <- selected_data()
      if (!nrow(x)) {
        showNotification("No records to download. Change the filters.", type = "warning")
        stop("No records match the selected filters.")
      }
      readme <- data.frame(
        Item = c("Dashboard", "Selection", "Observed dates", "Selected weather", "Data context", "Methods", "Missing values", "Navigation"),
        Description = c("CalOLFFx: olive fruit fly captures and weather", selection_text(),
                        paste(min(x$Date), "to", max(x$Date)), labels[[input$weather_var]], note, method_note,
                        "Not available means no valid measurement. Zero represents a recorded or calculated zero. Partial totals include only available records.",
                        "Each worksheet contains a named table with column headers. Time series contains all chart values and all weather variables.")
      )
      sheets <- list("Read me" = readme,
                     "Location summary" = pretty_data(summary_data()),
                     "Time series" = pretty_data(filtered_data()),
                     "Interpretation" = data.frame(Topic = c(rep("Captures", length(capture_text())),
                                                             rep("Weather", length(weather_text()))), Description = c(capture_text(), weather_text())))
      wb <- createWorkbook(creator = "CalOLFFx")
      modifyBaseFont(wb, fontSize = 12, fontName = "Calibri", fontColour = "#17212B")
      header <- createStyle(fontColour = "#FFFFFF", fgFill = "#174E80", textDecoration = "bold", wrapText = TRUE)
      body <- createStyle(wrapText = TRUE, valign = "top")
      decimal <- createStyle(numFmt = "0.00")
      for (i in seq_along(sheets)) {
        nm <- names(sheets)[i]
        dat <- sheets[[i]]
        addWorksheet(wb, nm)
        writeDataTable(wb, nm, dat, tableName = paste0("CalOLFFxTable", i),
                       tableStyle = "TableStyleLight9", headerStyle = header, keepNA = TRUE, na.string = "Not available")
        addStyle(wb, nm, body, rows = seq_len(nrow(dat)) + 1, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
        numeric_cols <- which(vapply(dat, is.numeric, logical(1)))
        if (length(numeric_cols)) addStyle(wb, nm, decimal, rows = seq_len(nrow(dat)) + 1,
                                           cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
        setColWidths(wb, nm, cols = seq_len(ncol(dat)), widths = 24)
        setRowHeights(wb, nm, rows = 1, heights = 65)
        if (nm %in% c("Read me", "Interpretation")) {
          setColWidths(wb, nm, cols = 2, widths = 100)
          setRowHeights(wb, nm, rows = seq_len(nrow(dat)) + 1, heights = 105)
        }
        freezePane(wb, nm, firstRow = TRUE)
      }
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
