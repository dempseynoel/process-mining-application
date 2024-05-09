
# Title: Process Mining Tool
# Description: A Shiny App to allows process mining of event logs in R
# Date: 2022-11-04


# Imports ---------------------------------------------------------------------

source("utility/utility.R")

# UI --------------------------------------------------------------------------

# Sidebar
sidebar <- dashboardSidebar(
  expandOnHover = FALSE,
  minified = FALSE,
  skin = "light",
  
  # Sidebar content
  sliderInput(
    inputId = "trace_frequency",
    label = "Select trace frequency",
    min = 0, 
    max = 100,
    value = 10,
    post = "%",
    ticks = FALSE),
  helpText(
    p("Event trace frequency. This filter uses a percentile cut off, and",
      "will look at those traces which are the most common until the required",
      "percent of traces has been reached.")),
  
  uiOutput("event_presence"),
  
  selectInput(
    inputId = "frequency_profile",
    label = "Select frequency profile",
    choices = PROFILES),
  
  helpText(
    p(tags$b("Absolute:"), "The absolute number of event instances and flows."),
    p(tags$b("Absolute case:"), "The asbolute number of cases behind each event and flow."),
    p(tags$b("Relative:"), "The relative number of instances per event, and the relative outgoing flows for each event."),
    p(tags$b("Relative case:"), "The relative number of cases per event and flow.")),
  
  fileInput(
    inputId = "file_upload",
    label = "Upload eventlog data",
    accept = ".csv")
)

# Body
body <- dashboardBody(
  tags$head(includeCSS("www/theme.css")),
  
  # Value boxes
  fluidRow(
    infoBoxOutput(
      outputId = "cases_filtered"),
    infoBoxOutput(
      outputId = "cases_shown"),
    infoBoxOutput(
      outputId = "activity_count")),
  
  # Process Map
  fluidRow(
    box(
      title = "Activity trace map",
      status = "primary",
      width = 12,
      withSpinner(
        ui_element = svgPanZoomOutput("process_map"),
        type = 3,
        color.background = "#FFFFFF",
        size = 0.25))),
  
  # Activity matrix / dotted chart
  fluidRow(
    box(
      title = "Activity matrix",
      id = "activity_matrix",
      status = "primary",
      width = 6,
      collapsible = TRUE,
      plotOutput("activity_matrix")),
    box(
      title = "Activity dotted chart",
      id = "activity_dotted",
      status = "primary",
      width = 6,
      collapsible = TRUE,
      plotOutput("activity_dotted"))),
  
  # Processing time / activity presence
  fluidRow(
    box(
      title = "Throughput time",
      id = "throughput_time",
      status = "primary",
      width = 6,
      collapsible = TRUE,
      plotOutput("throughput_time")),
    box(
      title = "Activity presence",
      id = "activity_presence",
      status = "primary",
      width = 6,
      collapsible = TRUE,
      plotOutput("activity_presence"))),
  
  # Trace explorer
  fluidRow(
    box(
      title = "Trace explorer",
      id = "data_table",
      status = "primary",
      width = 12,
      collapsible = TRUE,
      dataTableOutput("data_table")))
)

# Overall UI
ui <- dashboardPage(
  preloader = list(html = tagList(spin_1(), "Loading...")),
  title = "",
  dark = NULL, 
  header = dashboardHeader(
    title = dashboardBrand(
      title = strong("Process Mining App"))),
  sidebar = sidebar,
  body = body)

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {
  
  # File upload
  file_or_default <- reactive({
    if (is.null(input$file_upload)) {
      event_log <- default_event_log
    } else {
      file <- input$file_upload
      ext <- file_ext(file$datapath)
      req(file)
      event_log <- read_csv(file$datapath) 
      
      # Validate file
      validate(
        need(ext == "csv", "Please upload a csv file"),
        need(all(names(event_log) %in% REQUIRED_COL_NAMES),
             str_c(
               "Please ensure you only have these three column names in your data: ",
               "'case_id', 'activity_id', 'timestamp'")),
        need(is.POSIXt(event_log$timestamp), 
             str_c("Please ensure the timestamp field is a timestamp with the ", 
                   "following format: YYYY-MM-DD HH:MM:SS")))
      
      event_log <- event_log %>% 
        simple_eventlog(
          case_id = "case_id", 
          activity_id = "activity_id",
          timestamp = "timestamp")
    }
    event_log
  })
  
  # Activity filter
  activity_filter <- reactive({
    if (req(input$event_presence) != "All") {
      
      # Check activities in event_presence against new file, hide charts until all data loaded
      if (req(input$event_presence) %in% file_or_default()$activity_id == FALSE) {
        event_log <- NA
      } else {
        event_log <- file_or_default() %>% 
          filter_activity_presence(req(input$event_presence))
      }
      
    } else {
      event_log <- file_or_default()
    }
    event_log
  })
  
  # Trace filter
  trace_filter <- reactive({
    activity_filter() %>% 
      filter_trace_frequency(percentage = input$trace_frequency / 100)
  })
  
  # Cases filtered
  cases_filtered <- reactive({
    total <- file_or_default() %>% 
      group_by_case() %>% 
      n_cases %>% 
      tally() %>% 
      as.numeric()
    total <- label_number(scale_cut = cut_short_scale(.1))(total)
    value <- activity_filter() %>% 
      group_by_case() %>% 
      n_cases %>% 
      tally() %>% 
      as.numeric()
    value <- label_number(scale_cut = cut_short_scale(.1))(value)
    str_c(value, " of ", total)
  })
  
  # Percentage cases
  cases_shown <- reactive({
    amount <- trace_filter() %>% 
      group_by_case() %>% 
      n_cases %>% 
      tally() %>% 
      as.numeric()
    label_comma()(amount)
  })
  
  # Activity count
  activity_count <- reactive({
    total <- file_or_default() %>% 
      n_activities() %>% 
      as.numeric()
    total <- label_comma()(total)
    amount <- trace_filter() %>% 
      activities() %>% 
      nrow() %>% 
      as.numeric()
    amount <- label_comma()(amount)
    str_c(amount, " of ", total)
  })
  
  # Output UI
  output$event_presence <- renderUI({
    selectInput(
      inputId = "event_presence",
      label = "Select activity presence",
      selected = "All",
      choices = c(
        "All",
        unique(as.character(file_or_default()$activity_id))))
  })
  
  # Output Boxes
  output$cases_filtered <- renderbs4InfoBox({
    # Check if activity_filter() has data - don't render
    req(is.na(activity_filter()) != TRUE)
    bs4InfoBox(
      title = "Cases selected",
      value = cases_filtered(),
      color = "primary",
      fill = TRUE,
      icon = icon("users"))
  })
  
  output$cases_shown <- renderbs4InfoBox({
    # Check if activity_filter() has data - don't render
    req(is.na(activity_filter()) != TRUE)
    bs4InfoBox(
      title = "Cases shown",
      value = cases_shown(),
      color = "primary",
      fill = TRUE,
      icon = icon("hashtag"))
  })
  
  output$activity_count <- renderbs4InfoBox({
    # Check if activity_filter() has data - don't render
    req(is.na(activity_filter()) != TRUE)
    bs4InfoBox(
      title = "Activity count",
      value = activity_count(),
      color = "primary",
      fill = TRUE,
      icon = icon("flag"))
  })
  
  # Process map
  output$process_map <- renderSvgPanZoom({
    # Check if activity_filter() has data - don't render
    req(is.na(activity_filter()) != TRUE)
    dot <- process_map(
      trace_filter(),
      rankdir = "TB",
      type = frequency(
        input$frequency_profile),
      render = FALSE) 
    
    # Change font
    dot[["nodes_df"]][["fontname"]] <- "Source Sans Pro"
    dot[["edges_df"]][["fontname"]] <- "Source Sans Pro"
    
    # Nudge values from edge
    dot[["edges_df"]][["label"]] <- paste("  ", dot[["edges_df"]][["label"]])
    dot[["edges_df"]][["fontsize"]] <- 9
    
    dot %>% 
      generate_dot() %>% 
      grViz() %>% 
      export_svg() %>% 
      svgPanZoom()
  })
  
  # Activity matrix
  output$activity_matrix <- renderPlot({
    # Check if activity_filter() has data - don't render
    req(is.na(activity_filter()) != TRUE)
    if (input$frequency_profile == "absolute") {
      matrix_df <- trace_filter() %>% process_matrix(frequency(value = "absolute"))
      matrix_df$antecedent <- as.character(matrix_df$antecedent)
      matrix_df$consequent <- as.character(matrix_df$consequent)
      
      plot <- ggplot(
        data = matrix_df,
        mapping = aes(x = consequent, y = antecedent)) +
        geom_tile(
          mapping = aes(fill = n),
          colour = "#FFFFFF") +
        geom_text_pilot(
          mapping = aes(label = label_comma()(n)),
          size = 3)
      
    } else if (input$frequency_profile == "absolute_case") {
      matrix_df <- trace_filter() %>% process_matrix(frequency(value = "absolute_case"))
      matrix_df$antecedent <- as.character(matrix_df$antecedent)
      matrix_df$consequent <- as.character(matrix_df$consequent)
      
      plot <- ggplot(
        data = matrix_df,
        mapping = aes(x = consequent, y = antecedent)) +
        geom_tile(
          mapping = aes(fill = n_cases),
          colour = "#FFFFFF") +
        geom_text_pilot(
          mapping = aes(label = label_comma()(n_cases)),
          size = 2.5)
      
    } else if (input$frequency_profile == "relative") {
      matrix_df <- trace_filter() %>% process_matrix(frequency(value = "relative"))
      matrix_df$antecedent <- as.character(matrix_df$antecedent)
      matrix_df$consequent <- as.character(matrix_df$consequent)
      
      plot <- ggplot(
        data = matrix_df,
        mapping = aes(x = consequent, y = antecedent)) +
        geom_tile(
          mapping = aes(fill = rel_n),
          colour = "#FFFFFF") +
        geom_text_pilot(
          mapping = aes(label = label_percent(1)(rel_n)),
          size = 3)
      
    } else {
      matrix_df <- trace_filter() %>% process_matrix(frequency(value = "relative_case"))
      matrix_df$antecedent <- as.character(matrix_df$antecedent)
      matrix_df$consequent <- as.character(matrix_df$consequent)
      
      plot <- ggplot(
        data = matrix_df,
        mapping = aes(x = consequent, y = antecedent)) +
        geom_tile(
          mapping = aes(fill = n_cases),
          colour = "#FFFFFF") +
        geom_text_pilot(
          mapping = aes(label = label_percent(1)(rel_n_cases)),
          size = 3)
    }
    
    plot + labs(
      x = "Consequent", 
      y = "Antecedent") + 
      scale_x_discrete(
        label = function(x) str_trunc(x, 12),
        guide = guide_axis(angle = 90)) +
      scale_y_discrete(
        label = function(x) str_trunc(x, 12)) +
      scale_fill_gradient(
        low = "#99c5de",
        high = "#0170AD") +
      theme_pilot(
        legend_position = "none",
        grid = "vh")
  })
  
  # Dotted chart
  output$activity_dotted <- renderPlot({
    # Check if activity_filter() has data - don't render
    req(is.na(activity_filter()) != TRUE)
    ad <- trace_filter()
    ad$activity_id <- str_trunc(ad$activity_id, 12)
    ad %>%
      dotted_chart(x = "relative") +
      theme_pilot(
        legend_position = "bottom",
        legend_text_size = 8) +
      theme(legend.title = element_blank())
  })
  
  # Throughput time
  output$throughput_time <- renderPlot({
    # Check if activity_filter() has data - don't render
    req(is.na(activity_filter()) != TRUE)
    trace_filter() %>% 
      throughput_time(
        "log",
        units = "days") %>% 
      plot() +
      theme_pilot()
  })
  
  # Activity presence
  output$activity_presence <- renderPlot({
    # Check if activity_filter() has data - don't render
    req(is.na(activity_filter()) != TRUE)
    ap <- trace_filter() %>%
      activity_presence()
    ap$activity_id <- factor(ap$activity_id)
    ap$activity_id <- fct_reorder(ap$activity_id, ap$relative, max)
    
    ggplot(data = ap) +
      geom_col(
        mapping = aes(
          x = relative,
          y = activity_id),
         fill = "#0170AD") +
      labs(
        x = "Relative activity presence",
        y = "Activity") +
      scale_x_continuous(
        labels = label_percent(),
        expand = c(0, 0)) +
      scale_y_discrete(label = function(x) str_trunc(x, 12)) +
      theme_pilot(
        legend_position = "none")
  })
  
  # Trace explorer
  output$data_table <- DT::renderDataTable({
    # Check if activity_filter() has data - don't render
    req(is.na(activity_filter()) != TRUE)
    dt <- trace_filter() %>%
      trace_coverage(level = "trace")
    datatable(
      dt,
      options = list(
        pageLength = 10)) %>% 
      formatPercentage(c("relative", "cum_sum"))
  })
  
}

# Run App ---------------------------------------------------------------------

shinyApp(ui, server)














