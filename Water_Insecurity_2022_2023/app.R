#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


# Load necessary packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)
library(plotly)
library(shinyWidgets)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Water Access Analysis Dashboard"),
  
  # Sidebar with filters
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Geographic Analysis", tabName = "geographic", icon = icon("map")),
      menuItem("Comparison Analysis", tabName = "comparison", icon = icon("chart-line")),
      menuItem("Population Impact", tabName = "population", icon = icon("users")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    ),
    
    # Global filters
    selectInput("year", "Select Year:", 
                choices = c("2022", "2023", "Comparison"),
                selected = "2023"),
    
    selectInput("pop_category", "Population Size:", 
                choices = c("All", "< 50K", "50K-100K", "100K-500K", "500K-1M", "> 1M"),
                selected = "All"),
    
    sliderInput("threshold", "Plumbing Issues Threshold (%):",
                min = 0, max = 100, value = c(0, 100)),
    
    actionButton("show_worst", "Show 10 Worst Areas", 
                 icon = icon("exclamation-triangle")),
    
    actionButton("show_improved", "Show 10 Most Improved", 
                 icon = icon("arrow-up")),
    
    actionButton("reset", "Reset Filters", 
                 icon = icon("sync"))
  ),
  
  # Main dashboard content
  dashboardBody(
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                # Summary statistics boxes
                valueBoxOutput("total_pop_box", width = 3),
                valueBoxOutput("avg_pct_box", width = 3),
                valueBoxOutput("median_pct_box", width = 3),
                valueBoxOutput("change_box", width = 3)
              ),
              fluidRow(
                box(
                  title = "Quick Overview",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("overview_plot", height = "300px")
                )
              ),
              fluidRow(
                box(
                  title = "Key Insights",
                  status = "info",
                  width = 6,
                  textOutput("insights_text")
                ),
                box(
                  title = "Year-over-Year Change",
                  status = "warning", 
                  width = 6,
                  plotlyOutput("yoy_change_plot", height = "250px")
                )
              )
      ),
      
      # Geographic Analysis tab
      tabItem(tabName = "geographic",
              fluidRow(
                box(
                  title = "Geographic Distribution of Plumbing Issues",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 9,
                  leafletOutput("map", height = "600px")
                ),
                box(
                  title = "Regional Summary",
                  status = "info",
                  width = 3,
                  selectInput("region_filter", "Filter by Region:", 
                              choices = c("All", "Northeast", "Midwest", "South", "West"),
                              selected = "All"),
                  plotlyOutput("region_summary", height = "250px"),
                  tableOutput("region_table")
                )
              )
      ),
      
      # Comparison Analysis tab
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "2022 vs 2023 Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  plotlyOutput("scatter_comparison", height = "500px")
                ),
                tabBox(
                  width = 4,
                  tabPanel("Most Improved", 
                           dataTableOutput("improved_table")),
                  tabPanel("Most Deteriorated", 
                           dataTableOutput("deteriorated_table"))
                )
              ),
              fluidRow(
                box(
                  title = "Distribution Comparison",
                  width = 12,
                  plotlyOutput("dist_comparison", height = "300px")
                )
              )
      ),
      
      # Population Impact tab
      tabItem(tabName = "population",
              fluidRow(
                box(
                  title = "Plumbing Issues by Population Size",
                  status = "primary",
                  width = 6,
                  plotlyOutput("boxplot_pop", height = "400px")
                ),
                box(
                  title = "Total Population Affected by Category",
                  status = "warning",
                  width = 6,
                  plotlyOutput("barplot_pop", height = "400px"),
                  checkboxInput("normalize", "Normalize by Population Size", FALSE)
                )
              ),
              fluidRow(
                box(
                  title = "Population Category Statistics",
                  status = "info",
                  width = 12,
                  dataTableOutput("pop_stats_table")
                )
              )
      ),
      
      # Data Explorer tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(
                  title = "Search and Filter Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  textInput("search", "Search by Name:"),
                  dataTableOutput("data_table"),
                  downloadButton("download_data", "Download Filtered Data")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load data (placeholder - replace with your actual data loading)
  water_2022 <- reactive({
    # This would be your actual data loading code
    # For now using a placeholder that simulates the data structure
    data.frame(
      name = paste0("Area", 1:100),
      total_pop = sample(10000:2000000, 100),
      percent_lacking_plumbing = runif(100, 0, 10),
      region = sample(c("Northeast", "Midwest", "South", "West"), 100, replace = TRUE)
    ) %>%
      mutate(pop_category = case_when(
        total_pop < 50000 ~ "< 50K",
        total_pop < 100000 ~ "50K-100K",
        total_pop < 500000 ~ "100K-500K",
        total_pop < 1000000 ~ "500K-1M",
        TRUE ~ "> 1M"
      ))
  })
  
  water_2023 <- reactive({
    # Simulating 2023 data with some changes from 2022
    water_2022() %>%
      mutate(
        percent_lacking_plumbing = pmax(0, percent_lacking_plumbing + rnorm(100, -0.5, 1))  # Some improvements, some deteriorations
      )
  })
  
  # Create comparison dataset
  comparison_data <- reactive({
    water_2022() %>%
      inner_join(water_2023(), by = "name", suffix = c("_2022", "_2023")) %>%
      mutate(
        change_pct = percent_lacking_plumbing_2023 - percent_lacking_plumbing_2022,
        percent_change = (percent_lacking_plumbing_2023 - percent_lacking_plumbing_2022) / 
          percent_lacking_plumbing_2022 * 100
      )
  })
  
  # Filter data based on inputs
  filtered_data <- reactive({
    data <- if(input$year == "2022") {
      water_2022()
    } else if(input$year == "2023") {
      water_2023()
    } else {
      comparison_data()
    }
    
    if(input$pop_category != "All") {
      data <- data %>% filter(pop_category == input$pop_category)
    }
    
    if(input$year != "Comparison") {
      data <- data %>% 
        filter(percent_lacking_plumbing >= input$threshold[1],
               percent_lacking_plumbing <= input$threshold[2])
    }
    
    if(!is.null(input$search) && input$search != "") {
      data <- data %>% filter(grepl(input$search, name, ignore.case = TRUE))
    }
    
    data
  })
  
  # Value boxes
  output$total_pop_box <- renderValueBox({
    if(input$year == "2022") {
      pop <- sum(water_2022()$total_pop, na.rm = TRUE)
      valueBox(
        formatC(pop, format = "d", big.mark = ","),
        "Total Population (2022)",
        icon = icon("users"),
        color = "blue"
      )
    } else if(input$year == "2023") {
      pop <- sum(water_2023()$total_pop, na.rm = TRUE)
      valueBox(
        formatC(pop, format = "d", big.mark = ","),
        "Total Population (2023)",
        icon = icon("users"),
        color = "green"
      )
    } else {
      pop <- sum(water_2023()$total_pop, na.rm = TRUE)
      valueBox(
        formatC(pop, format = "d", big.mark = ","),
        "Total Population (2023)",
        icon = icon("users"),
        color = "purple"
      )
    }
  })
  
  output$avg_pct_box <- renderValueBox({
    if(input$year == "2022") {
      avg <- mean(water_2022()$percent_lacking_plumbing, na.rm = TRUE)
      valueBox(
        paste0(round(avg, 2), "%"),
        "Avg. Lacking Plumbing (2022)",
        icon = icon("tint-slash"),
        color = "yellow"
      )
    } else if(input$year == "2023") {
      avg <- mean(water_2023()$percent_lacking_plumbing, na.rm = TRUE)
      valueBox(
        paste0(round(avg, 2), "%"),
        "Avg. Lacking Plumbing (2023)",
        icon = icon("tint-slash"),
        color = "yellow"
      )
    } else {
      avg <- mean(water_2023()$percent_lacking_plumbing, na.rm = TRUE)
      valueBox(
        paste0(round(avg, 2), "%"),
        "Avg. Lacking Plumbing (2023)",
        icon = icon("tint-slash"),
        color = "yellow"
      )
    }
  })
  
  output$median_pct_box <- renderValueBox({
    if(input$year == "2022") {
      med <- median(water_2022()$percent_lacking_plumbing, na.rm = TRUE)
      valueBox(
        paste0(round(med, 2), "%"),
        "Median Lacking Plumbing (2022)",
        icon = icon("percentage"),
        color = "red"
      )
    } else if(input$year == "2023") {
      med <- median(water_2023()$percent_lacking_plumbing, na.rm = TRUE)
      valueBox(
        paste0(round(med, 2), "%"),
        "Median Lacking Plumbing (2023)",
        icon = icon("percentage"),
        color = "red"
      )
    } else {
      med <- median(water_2023()$percent_lacking_plumbing, na.rm = TRUE)
      valueBox(
        paste0(round(med, 2), "%"),
        "Median Lacking Plumbing (2023)",
        icon = icon("percentage"),
        color = "red"
      )
    }
  })
  
  output$change_box <- renderValueBox({
    avg_2022 <- mean(water_2022()$percent_lacking_plumbing, na.rm = TRUE)
    avg_2023 <- mean(water_2023()$percent_lacking_plumbing, na.rm = TRUE)
    change <- avg_2023 - avg_2022
    change_pct <- (change / avg_2022) * 100
    
    color <- ifelse(change < 0, "green", "red")
    icon_choice <- ifelse(change < 0, "arrow-down", "arrow-up")
    
    valueBox(
      paste0(round(change_pct, 2), "%"),
      "Year-over-Year Change",
      icon = icon(icon_choice),
      color = color
    )
  })
  
  # Overview tab plots
  output$overview_plot <- renderPlotly({
    if(input$year != "Comparison") {
      data <- if(input$year == "2022") water_2022() else water_2023()
      
      p <- ggplot(data, aes(x = percent_lacking_plumbing)) +
        geom_histogram(fill = "#3c8dbc", bins = 30) +
        labs(title = paste("Distribution of Percentage Lacking Plumbing", input$year),
             x = "Percentage", y = "Count") +
        theme_minimal()
    } else {
      p <- ggplot() +
        geom_histogram(data = water_2022(), aes(x = percent_lacking_plumbing, fill = "2022"), alpha = 0.5, bins = 30) +
        geom_histogram(data = water_2023(), aes(x = percent_lacking_plumbing, fill = "2023"), alpha = 0.5, bins = 30) +
        scale_fill_manual(values = c("2022" = "blue", "2023" = "red"), name = "Year") +
        labs(title = "Distribution of Percentage Lacking Plumbing", 
             x = "Percentage", y = "Count") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  output$yoy_change_plot <- renderPlotly({
    change_summary <- comparison_data() %>%
      mutate(improved = change_pct < 0) %>%
      group_by(region, improved) %>%
      summarise(count = n(), .groups = "drop")
    
    p <- ggplot(change_summary, aes(x = region, y = count, fill = improved)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), 
                        labels = c("TRUE" = "Improved", "FALSE" = "Deteriorated"),
                        name = "Status") +
      labs(title = "Areas Improved vs. Deteriorated by Region",
           x = "Region", y = "Number of Areas") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Geographic Analysis tab
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)  # Center on US
    
    # Note: In a real app, you would add markers or polygons here based on actual 
    # geographic data, which is not present in the example dataset
  })
  
  # Comparison Analysis tab
  output$scatter_comparison <- renderPlotly({
    p <- ggplot(comparison_data(), aes(x = percent_lacking_plumbing_2022, 
                                       y = percent_lacking_plumbing_2023,
                                       text = name)) +
      geom_point(aes(color = change_pct), alpha = 0.7) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      scale_color_gradient2(low = "green", mid = "gray", high = "red", midpoint = 0) +
      labs(title = "Percentage Lacking Plumbing: 2022 vs 2023",
           x = "Percentage in 2022",
           y = "Percentage in 2023",
           color = "Change") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$improved_table <- renderDataTable({
    comparison_data() %>%
      filter(change_pct < 0) %>%
      arrange(change_pct) %>%
      head(10) %>%
      select(name, total_pop_2022, percent_lacking_plumbing_2022, 
             percent_lacking_plumbing_2023, change_pct, percent_change) %>%
      mutate(across(where(is.numeric), round, 2)) %>%
      rename("Area" = "name", 
             "Population" = "total_pop_2022", 
             "2022 %" = "percent_lacking_plumbing_2022",
             "2023 %" = "percent_lacking_plumbing_2023",
             "Change" = "change_pct",
             "% Change" = "percent_change")
  }, options = list(pageLength = 5, scrollX = TRUE))
  
  output$deteriorated_table <- renderDataTable({
    comparison_data() %>%
      filter(change_pct > 0) %>%
      arrange(desc(change_pct)) %>%
      head(10) %>%
      select(name, total_pop_2022, percent_lacking_plumbing_2022, 
             percent_lacking_plumbing_2023, change_pct, percent_change) %>%
      mutate(across(where(is.numeric), round, 2)) %>%
      rename("Area" = "name", 
             "Population" = "total_pop_2022", 
             "2022 %" = "percent_lacking_plumbing_2022",
             "2023 %" = "percent_lacking_plumbing_2023",
             "Change" = "change_pct",
             "% Change" = "percent_change")
  }, options = list(pageLength = 5, scrollX = TRUE))
  
  # Population Impact tab
  output$boxplot_pop <- renderPlotly({
    data <- if(input$year == "2022") {
      water_2022()
    } else {
      water_2023()
    }
    
    p <- ggplot(data, aes(x = pop_category, y = percent_lacking_plumbing)) +
      geom_boxplot(fill = "#3c8dbc") +
      labs(title = paste("Plumbing Issues by Population Size (", input$year, ")", sep = ""),
           x = "Population Category", y = "Percentage Lacking Plumbing") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$barplot_pop <- renderPlotly({
    data <- if(input$year == "2022") {
      water_2022()
    } else {
      water_2023()
    }
    
    pop_summary <- data %>%
      group_by(pop_category) %>%
      summarise(
        total_pop = sum(total_pop, na.rm = TRUE),
        avg_pct = mean(percent_lacking_plumbing, na.rm = TRUE),
        affected_pop = sum(total_pop * percent_lacking_plumbing / 100, na.rm = TRUE)
      )
    
    y_var <- if(input$normalize) "avg_pct" else "affected_pop"
    y_label <- if(input$normalize) "Average % Lacking Plumbing" else "Estimated Population Affected"
    
    p <- ggplot(pop_summary, aes(x = pop_category, y = !!sym(y_var), text = pop_category)) +
      geom_bar(stat = "identity", fill = "#dd4b39") +
      labs(title = paste("Impact Analysis by Population Size (", input$year, ")", sep = ""),
           x = "Population Category", y = y_label) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Data Explorer tab
  output$data_table <- renderDataTable({
    filtered_data() %>%
      select(-region) %>%  # Excluding region for simplicity in this example
      mutate(across(where(is.numeric), round, 2))
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("water-access-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Button actions
  observeEvent(input$show_worst, {
    if(input$year == "2022") {
      worst <- water_2022() %>%
        arrange(desc(percent_lacking_plumbing)) %>%
        head(10)
      updateTextInput(session, "search", value = paste(worst$name, collapse = "|"))
    } else if(input$year == "2023") {
      worst <- water_2023() %>%
        arrange(desc(percent_lacking_plumbing)) %>%
        head(10)
      updateTextInput(session, "search", value = paste(worst$name, collapse = "|"))
    }
    updateTabItems(session, "sidebarMenu", "explorer")
  })
  
  observeEvent(input$show_improved, {
    improved <- comparison_data() %>%
      arrange(change_pct) %>%
      head(10)
    updateTextInput(session, "search", value = paste(improved$name, collapse = "|"))
    updateTabItems(session, "sidebarMenu", "explorer")
    updateSelectInput(session, "year", selected = "Comparison")
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "year", selected = "2023")
    updateSelectInput(session, "pop_category", selected = "All")
    updateSliderInput(session, "threshold", value = c(0, 100))
    updateTextInput(session, "search", value = "")
  })
}

# Run the application
shinyApp(ui = ui, server = server)