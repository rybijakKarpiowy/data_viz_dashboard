library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(jsonlite)
library(DT)
library(scales)
library(tidyr)
library(purrr)
library(colorspace)

# Functions to load the data
load_json_data <- function(file_path) {
  tryCatch({
    jsonlite::fromJSON(file_path)
  }, error = function(e) {
    warning(paste("Error loading", file_path, ":", e$message))
    NULL
  })
}

# Data preparation function
# TODO: filter the data at the end so the data table is cleaner
load_sample_data <- function() {
  # Load all the data from JSON files
  users <- load_json_data("generated_data/users.json")
  categories <- load_json_data("generated_data/categories.json")
  subcategories <- load_json_data("generated_data/subcategories.json")
  offers <- load_json_data("generated_data/offers.json")
  variants <- load_json_data("generated_data/variants.json")
  orders <- load_json_data("generated_data/orders.json") 
  order_items <- load_json_data("generated_data/order_items.json")
  discounts <- load_json_data("generated_data/discounts.json")
  
  # Convert date strings to dates
  orders$created_at <- as.POSIXct(orders$created_at)
  offers$created_at <- as.POSIXct(offers$created_at)
  discounts$created_at <- as.POSIXct(discounts$created_at)
  discounts$until <- as.POSIXct(discounts$until)
  
  # Format names
  # Change first letter to capital
  orders$status <- tools::toTitleCase(orders$status)
  # First letter to capital and _ to space
  orders$shipping_method <- tools::toTitleCase(gsub("_", " ", orders$shipping_method))
  # Dpd to DPD
  orders$shipping_method <- gsub("Dpd", "DPD", orders$shipping_method)
  # Rename total to Total
  orders <- orders %>%
    rename("Total value" = total)
  
  # Rename quantity to Quantity and price to Price
  order_items <- order_items %>%
    rename(Quantity = quantity) %>%
    rename(Price = price) %>%
    mutate(was_discounted = ifelse(was_discounted, "Discount", "No discount"))
  
  # Prepare data for category analysis
  subcategories_df <- as.data.frame(subcategories)
  categories_df <- as.data.frame(categories)
  
  # Format order items for analysis
  order_items_df <- as.data.frame(order_items) %>%
    mutate(
      personalization = ifelse(personalization_mask, ifelse(design_file_id, "Drawing + File", "Drawing"), ifelse(design_file_id, "File", "None"))
    ) %>%
    select(-personalization_mask, -design_file_id)
  
  # Join order items with orders for time-based analysis
  order_items_with_dates <- order_items_df %>%
    left_join(orders, by = "order_id")
  
  # Add subcategory information to offers
  offers_df <- as.data.frame(offers)
  offers_with_categories <- offers_df %>%
    rowwise() %>%
    mutate(
      subcategory_names = list(subcategories_df$name[subcategories_df$subcategory_id %in% subcategories]),
      category_ids = list(subcategories_df$category_id[subcategories_df$subcategory_id %in% subcategories])
    )
  
  # Add category names
  offers_with_categories <- offers_with_categories %>%
    rowwise() %>%
    mutate(
      category_names = list(categories_df$name[categories_df$category_id %in% unlist(category_ids)])
    )
  
  # Connect order items with offers and categories
  order_items_with_categories <- order_items_with_dates %>%
    left_join(offers_with_categories %>% select(-price), by = "offer_id")

  
  # Sum up number of items and total value per order
  orders_df <- order_items_with_dates %>%
    group_by(order_id) %>%
    summarise(
      Quantity = sum(Quantity, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(order_id, Quantity) %>%
    left_join(
      select(orders, order_id, created_at, status, shipping_method, "Total value", user_id),
      by = "order_id") %>%
    mutate(
      user_type = ifelse(is.na(user_id), "Anonymous", "User")
    ) %>%
    select(-order_id, -user_id) %>%
    mutate(
      created_at = format(as.POSIXct(created_at), "%Y-%m-%d")
    )
  
  # Calculate convertion rates and total value for subcategories
  subcategories_summary <- order_items_with_categories %>%
    group_by(subcategory_names, offer_id) %>%
    summarise(
      Quantity = sum(Quantity, na.rm = TRUE),
      "Total value" = sum(Quantity * Price, na.rm = TRUE),
      convertion_rate = mean(convertion_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    unnest(c(subcategory_names, offer_id)) %>%
    # find a category name for each subcategory
    left_join(subcategories_df, by = c("subcategory_names" = "name")) %>%
    left_join(categories_df, by = c("category_id" = "category_id")) %>%
    select(-subcategory_id, -category_id) %>%
    rename(category_names = name) %>%
    select(category_names, subcategory_names, offer_id, Quantity, "Total value", convertion_rate)
      
  
  # Calculate convertion rates and total value for offers
  offers_summary <- order_items_with_categories %>%
    group_by(offer_id) %>%
    summarise(
      Quantity = sum(Quantity, na.rm = TRUE),
      "Total value" = sum(Quantity * Price, na.rm = TRUE)
    ) %>%
    left_join(select(
      offers_df,
      c("offer_id", "convertion_rate", "recommended", "name")
    ), by = "offer_id")
  
  # Calculate statistics for variants
  variants_df <- as.data.frame(variants)
  variants_with_data <- order_items_with_categories %>%
    group_by(offer_id, variant_id) %>%
    summarise(
      Quantity = sum(Quantity, na.rm = TRUE),
      "Total value" = sum(Quantity * Price, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      select(offers_df, c("offer_id", "recommended")),
      by = "offer_id"
    ) %>%
    left_join(
      select(variants_df, c("variant_id", "name")),
      by = "variant_id"
    )
  
  all_categories = categories_df$name
  category_colors <- scales::hue_pal()(length(all_categories))
  names(category_colors) <- all_categories
  
  # Return all the prepared datasets
  return(list(
    Orders = orders_df,
    "Order items" = order_items_with_categories,
    Subcategories = subcategories_summary,
    category_colors = category_colors,
    Offers = offers_summary,
    Variants = variants_with_data
  ))
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
    tags$img(src = "https://albox.pl/logo.webp", height = "30px"),
    "Albox Dashboard"
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo {
          background-color: #ffffff !important;
          color: #000000 !important;
          border: 2px solid #000000 !important;
        }
        .small-valuebox .small-box {
          padding-x: 10px;
          display: flex;
          align-items: center;
        }
        .small-valuebox .small-box h3 {
          font-size: 20px; /* reduce value font size */
          margin: 0;
        }
        .small-valuebox .small-box p {
          font-size: 16px; /* reduce subtitle font size */
          margin: 0;
        }
        .small-valuebox .icon-large {
          font-size: 40px;            /* Smaller icon */
          line-height: 40px;          /* Match box height for vertical centering */
          height: 40px;
          display: flex;
          align-items: center;
          justify-content: center;
          position: absolute;
          top: 16px;
          right: 16px;
        }
      "))
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Data Selection",
        status = "primary",
        solidHeader = TRUE,
        column(
          width = 2,
          selectInput(
            "dataType",
            "Select Data Type:",
            choices = c("Orders", "Order items", "Categories", "Offers", "Variants")
          )
        ),
        column(
          width = 2,
          uiOutput("valueTypeUI")  # Dynamic UI for value type based on data type
        ),
        column(
          width = 2,
          conditionalPanel(
            condition = "input.dataType != 'Categories'",
            numericInput(
              "bins",
              "Number of Histogram Bins:",
              value = 10,
              min = 1,
              max = 50
            )
          )
        ),
        column(
          width = 1,
          actionButton("resetFilters", "Remove Filters", class = "btn-danger"),
          style = "margin-top: 25px;"
        ),
        column(
          width = 4,
          div(
            class = "small-valuebox",
            valueBox(
              value = htmlOutput("totalValueThisMonth"),
              subtitle = "Total value of orders this month",
              icon = icon("coins"),
              color = "green",
              width = 8
            )
          )
        )
      )
    ),
    fluidRow(
      box(
        width = 6,
        title = "Data Visualization",
        status = "primary",
        solidHeader = TRUE,
        conditionalPanel(
          condition = "input.dataType == 'Orders' || input.dataType == 'Order items' || input.dataType == 'Offers' || input.dataType == 'Variants'",
          plotOutput("histogram", click = "plot_click", height = "400px")
        ),
        conditionalPanel(
          condition = "input.dataType == 'Categories'",
          plotOutput("categoriesPlot", click = "categories_plot_click", height = "400px")
        )
      ),
      conditionalPanel(
        condition = "input.dataType == 'Orders' || input.dataType == 'Order items' || input.dataType == 'Offers' || input.dataType == 'Variants'",
        # Additional plots, 2 per row
        box(
          width = 3,
          plotlyOutput("additionalPlot1", height = "200px")
        ),
        box(
          width = 3,
          plotlyOutput("additionalPlot2", height = "200px")
        ),
        box(
          width = 3,
          plotlyOutput("additionalPlot3", height = "200px")
        ),
        box(
          width = 3,
          plotlyOutput("additionalPlot4", height = "200px")
        )
      ),
      conditionalPanel(
        condition = "input.dataType == 'Categories'",
        box(
          width = 6,
          plotOutput("subcategoriesPlot", click = "subcategories_plot_click", height = "400px")
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Data Table",
        status = "primary",
        solidHeader = TRUE,
        DTOutput("dataTable")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load data
  data <- reactiveVal(load_sample_data())
  
  # Total value this month
  output$totalValueThisMonth <- renderText({
    dataset <- data()$Orders
    
    # Filter for the current month
    current_month <- format(Sys.Date(), "%Y-%m")
    filtered_data <- dataset %>%
      mutate(month = format(as.Date(created_at), "%Y-%m")) %>%
      filter(month == current_month)
    
    # Calculate total value
    total_value <- sum(filtered_data$"Total value", na.rm = TRUE)
    
    # Format as currency
    formatted_value <- format(total_value, big.mark = ",", scientific = FALSE)
    
    paste0(formatted_value, " zł")
  })
  
  # Track selected bin and filtered data
  selected_bin <- reactiveVal(NULL)
  selected_column <- reactiveVal(NULL)
  filtered_data <- reactiveVal(NULL)
  
  # Dynamic UI for value type based on data type
  output$valueTypeUI <- renderUI({
    data_type <- input$dataType
    
    if (data_type == "Orders") {
      choices <- c("Quantity", "Total value")
    } else if (data_type == "Order items") {
      choices <- c("Quantity", "Price by order item", "Price by quantity")
    } else if (data_type %in% c("Categories", "Offers")) {
      choices <- c("Quantity", "Total value", "convertion_rate")
    } else if (data_type == "Variants") {
      choices <- c("Quantity", "Total value")
    } else {
      choices <- c("Quantity")
    }
    
    selectInput(
      "valueType",
      "Select Value Type:",
      choices = choices,
      selected = ifelse("Quantity" %in% choices, "Quantity", choices[1])
    )
  })
  
  # Get the dataset based on user selection
  get_dataset <- reactive({
    dataset_name <- input$dataType
    if (dataset_name == "Categories") {
      dataset_name <- "Subcategories"
    }
    dataset <- data()[[dataset_name]]
    
    # Handle the case where the dataset is empty or NULL
    if (is.null(dataset) || nrow(dataset) == 0) {
      return(data.frame())
    }
    
    return(dataset)
  })
  
  handle_bar_plot_click <- function(name_col, clicked_x, clicked_y) {
    # Get the data for the plot
    dataset <- get_dataset()
    value_type <- input$valueType
    
    # Prepare the data for plotting
    plot_data <- dataset
    
    # Make sure to group and summarize if needed
    plot_data_ordered <- plot_data %>%
      group_by(.data[[name_col]]) %>%
      summarize(value = sum(.data[[value_type]]), .groups = "drop") %>%
      arrange(desc(value))
    
    # Create a factor with unique levels, ordered by value
    plot_data[[name_col]] <- factor(plot_data[[name_col]],
                                    levels = rev(plot_data_ordered[[name_col]]))
    bar_levels <- levels(plot_data[[name_col]])
    
    # Find which bar was clicked (approximate match)
    if (nrow(plot_data) > 0) {
      # Get label from x-axis position
      y_pos <- round(clicked_y)
      
      if (y_pos >= 1 && y_pos <= nrow(plot_data)) {
        clicked_label <- bar_levels[y_pos]
        
        prev_selected_column = selected_column()
        if (!is.null(prev_selected_column)) {
          # Check if the clicked label is the same as the previously selected label
          if (prev_selected_column == clicked_label) {
            # If it's the same, clear the selection
            selected_column(NULL)
            filtered_data(NULL)
            return()
          }
        }
        
        selected_column(clicked_label)
        
        # Filter the dataset based on the clicked bar
        filtered <- dataset[dataset[[name_col]] == clicked_label, ]
        filtered_data(filtered)
        
        # Reset the bin selection since we're working with a bar chart
        selected_bin(NULL)
      }
    }
  }
  
  # Get the value column based on user selection
  get_value_column <- reactive({
    dataset <- get_dataset()
    value_type <- input$valueType
    
    # Handle empty dataset
    if (nrow(dataset) == 0) {
      return(numeric(0))
    }
    
    # Safely get the column value, returning NA if not found
    if (value_type %in% names(dataset)) {
      return(dataset[[value_type]])
    } else if (value_type == "Price by order item") {
      return(dataset$Price)
    } else {
      # TODO: rename convertion_rate
      if (value_type != "Price by quantity") {
        warning(paste0("Column '", value_type, "' not found in dataset. Returning NA values."))
      }
      return(rep(NA, nrow(dataset)))
    }
  })
  
  # Render the histogram
  output$histogram <- renderPlot({
    dataset <- get_dataset()
    value_type <- input$valueType
    value_column <- get_value_column()
    
    # Skip if there's no data or all values are NA
    if (nrow(dataset) == 0 || all(is.na(value_column)) && value_type != "Price by quantity") {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    # Create histogram data
    if (value_type == "Price by quantity") {
      # Create a histogram of price, where count is a sum of quantities
      value_column <- dataset %>%
        group_by(Price) %>%
        reframe(Quantity = sum(Quantity, na.rm = TRUE)) %>%
        ungroup() %>%
        # for every row create a rep(price, quantity) vector
        mutate(value = map2(Price, Quantity, ~rep(.x, .y))) %>%
        select(value) %>%
        # aggregate them into a single row
        reframe(value = unlist(value))
      value_column <- value_column$value
    }
    
    hist_data <- data.frame(value = value_column)
    
    # Calculate histogram breaks
    range_data <- range(hist_data$value, na.rm = TRUE)
    breaks <- seq(from = range_data[1], to = range_data[2], length.out = input$bins + 1)
    
    # Basic plot with the same breaks that were calculated
    p <- ggplot(hist_data, aes(x = value)) +
      geom_histogram(breaks = breaks, fill = "steelblue", color = "white")
    
    # If there's a selected bin, highlight it
    if (!is.null(selected_bin())) {
      selected <- selected_bin()
      bin_start <- selected$start
      bin_end <- selected$end
      bin_index <- selected$index
      
      # Create subset for just the selected bin
      if (bin_index == 1) {
        # For the first bin, include the left endpoint
        bin_data <- subset(hist_data, value >= bin_start & value <= bin_end)
      } else {
        # For other bins, use half-open interval
        bin_data <- subset(hist_data, value > bin_start & value <= bin_end)
      }
      
      # Add the highlighted bin on top
      p <- p + 
        geom_histogram(data = bin_data, breaks = breaks, fill = "darkred", color = "white")
    }
    
    # Add labels and theme
    p <- p +
      labs(title = paste("Distribution of", input$valueType),
           x = input$valueType,
           y = "Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
    return(p)
  })
  
  # Render the bar plot for categorical data
  output$categoriesPlot <- renderPlot({
    dataset <- get_dataset()
  
    value_type <- input$valueType
    
    # Skip if there's no data
    if (nrow(dataset) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    # For categorical data like categories, subcategories, offers, and variants
    # Get the appropriate name column based on data type
    name_col <- "category_names"
    
    # Ensure the name column exists
    if (!name_col %in% names(dataset)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Name column not found") + 
               theme_void())
    }
    
    # Ensure the value column exists
    if (!value_type %in% names(dataset)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = paste(value_type, "column not found")) + 
               theme_void())
    }
    
    # Prepare the data for plotting
    plot_data <- dataset
    
    # Make sure to group and summarize if needed
    plot_data_ordered <- plot_data %>%
      group_by(.data[[name_col]]) %>%
      summarize(value = sum(.data[[value_type]]), .groups = "drop") %>%
      arrange(desc(value))
    
    # Handle convertion_rate case - calculate mean
    if (value_type == "convertion_rate") {
      plot_data <- plot_data %>%
        select("category_names", "convertion_rate") %>%
        group_by(category_names) %>%
        summarise(convertion_rate = mean(convertion_rate, na.rm = TRUE)) %>%
        ungroup()
      
      plot_data_ordered <- plot_data %>%
        arrange(desc(convertion_rate)) 
    }
    
    # Create a factor with unique levels, ordered by value
    plot_data[[name_col]] <- factor(plot_data[[name_col]],
                                    levels = rev(plot_data_ordered[[name_col]]))
    
    # Create the plot, order by value
    p <- ggplot(plot_data, aes(x = .data[[name_col]],
                               y = .data[[value_type]],
                               fill = .data$category_names
                               )) +
      # apply the color palette
      scale_fill_manual(values = data()$category_colors) +
      geom_bar(stat = "identity") +
      labs(title = paste(value_type, "by", gsub("_names", "", name_col)),
           x = gsub("_names", "", name_col),
           y = value_type) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none"
      ) +
      coord_flip()
    
    if (!is.null(selected_column())) {
      # Highlight the selected column
      selected_label <- selected_column()
      # We don't have access to .data
      # so we need to use the name_col directly
      selected_data <- plot_data[plot_data[[name_col]] == selected_label, ]
      # Get the color for the selected label and change the brightness
      if (name_col == "category_names") {
        selected_fill <- data()$category_colors[selected_label]
      } else {
        # Selected label is a subcategory, not categroy
        selected_fill <- data()$category_colors[selected_data$category_names[1]]
      }
      selected_fill <- darken(selected_fill, amount = 0.4)
      p <- p + 
        geom_bar(data = selected_data, aes(x = .data[[name_col]], y = .data[[value_type]]),
                 stat = "identity", fill = selected_fill)
    }
    
    return(p)
  })
  
  # First additional plot
  output$additionalPlot1 <- renderPlotly({
    dataset <- if (!is.null(filtered_data())) {
      filtered_data()
    } else {
      get_dataset()
    }
    
    # Skip if there's no data
    if (nrow(dataset) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    all_rows_count = nrow(dataset)
    
    # Status pie chart
    # If we are working with orders
    if (input$dataType == "Orders") {
      p_data <- dataset %>%
        group_by(status) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = status)
    } else if (input$dataType == "Order items") {
      # If we are working with order items
      p_data <- dataset %>%
        group_by(personalization) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = personalization)
    } else if (input$dataType == "Offers" || input$dataType == "Variants") {
      p_data <- dataset %>%
        group_by(recommended) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = recommended)
    } else {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    p <- plot_ly(
      data = p_data,
      labels = ~label,
      values = ~percent,
      type = "pie",
      hoverinfo = "label+percent",
      textposition = "none",
      height = 200,
      width = 250
    ) %>% layout(
      title = list(
        text = ifelse(
          input$dataType == "Orders",
          "Order status distribution", 
          ifelse(input$dataType == "Order items",
                 "Order items personalization distribution", 
                 "Recommendation distribution"
                 )
        ),
        font = list(size = 14)
      ),
      margin = list(l = 20, r = 20, b = 50, t = 50)
    )
    
    return(p)
  })
  
  # Second additional plot
  output$additionalPlot2 <- renderPlotly({
    dataset <- if (!is.null(filtered_data())) {
      filtered_data()
    } else {
      get_dataset()
    }
    
    # Skip if there's no data
    if (nrow(dataset) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    all_rows_count = nrow(dataset)
    
    # Shipping method pie chart
    if (input$dataType == "Orders") {
      p_data <- dataset %>%
        group_by(shipping_method) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = shipping_method)
    } else if (input$dataType == "Order items") {
      # If we are working with order items
      p_data <- dataset %>%
        group_by(was_discounted) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = was_discounted)
    } else {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    p <- plot_ly(
      data = p_data,
      labels = ~label,
      values = ~percent,
      type = "pie",
      hoverinfo = "label+percent",
      textposition = "none",
      height = 200,
      width = 250
    ) %>% layout(
      title = list(
        text = ifelse(input$dataType == "Orders", "Order shipping method distribution", "Dicounts distribution"),
        font = list(size = 14)
      ),
      margin = list(l = 10, r = 10, b = 50, t = 50)
    )
    
    return(p)
  })
  
  output$additionalPlot3 <- renderPlotly({
    dataset <- if (!is.null(filtered_data())) {
      filtered_data()
    } else {
      get_dataset()
    }
    
    # Skip if there's no data
    if (nrow(dataset) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    all_rows_count = nrow(dataset)
    
    # User or anonymous pie chart
    if (input$dataType == "Orders") {
      p_data <- dataset %>%
        group_by(user_type) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = user_type)
    } else {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    p <- plot_ly(
      data = p_data,
      labels = ~label,
      values = ~percent,
      type = "pie",
      hoverinfo = "label+percent",
      textposition = "none",
      height = 200,
      width = 250
    ) %>% layout(
      title = list(
        text = "User type distribution",
        font = list(size = 14)
      ),
      margin = list(l = 10, r = 10, b = 50, t = 50)
    )
    
    return(p)
  })
  
  output$additionalPlot4 <- renderPlotly({
    dataset <- if (!is.null(filtered_data())) {
      filtered_data()
    } else {
      get_dataset()
    }
    
    # Skip if there's no data
    if (nrow(dataset) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    # Orders by date pie chart
    if (input$dataType == "Orders") {
      p_data <- dataset %>%
        # This year only
        filter(created_at >= as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"))) %>%
        mutate(month = format(as.Date(created_at), "%Y-%m")) %>%
        group_by(month) %>%
        summarise(value = n()) %>%
        ungroup() %>%
        rename(label = month) %>%
        mutate(label = format(as.Date(paste0(label, "-01")), "%b")) %>%
        # Reorder by months
        mutate(label = factor(label, levels = month.abb)) 
    } else {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    # Bar chart
    p <- plot_ly(
      data = p_data,
      x = ~label,
      y = ~value,
      type = "bar",
      hoverinfo = "text",
      text = ~value,
      textposition = "none",
      height = 200,
      width = 250
    ) %>% layout(
      title = list(
        text = "Number of Orders this year by month",
        font = list(size = 14)
      ),
      margin = list(l = 10, r = 10, b = 50, t = 50),
      # Remove axis titles
      xaxis = list(title = ""),
      yaxis = list(title = "")
    )
    
    return(p)
  })
  
  output$subcategoriesPlot = renderPlot({
    dataset <- get_dataset()
    
    value_type <- input$valueType
    
    # Skip if there's no data
    if (nrow(dataset) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    name_col <- "subcategory_names"
    
    # Ensure the name column exists
    if (!name_col %in% names(dataset)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Name column not found") + 
               theme_void())
    }
    
    # Ensure the value column exists
    if (!value_type %in% names(dataset)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = paste(value_type, "column not found")) + 
               theme_void())
    }
    
    # Prepare the data for plotting
    plot_data <- dataset
    
    # Make sure to group and summarize if needed
    plot_data_ordered <- plot_data %>%
      group_by(.data[[name_col]]) %>%
      summarize(value = sum(.data[[value_type]]), .groups = "drop") %>%
      arrange(desc(value))
    
    # Handle convertion_rate case - calculate mean
    if (value_type == "convertion_rate") {
      subcategories_to_categories <- plot_data %>%
        select("subcategory_names", "category_names") %>%
        unique()
      
      plot_data <- plot_data %>%
        select("subcategory_names", "convertion_rate") %>%
        group_by(subcategory_names) %>%
        summarise(convertion_rate = mean(convertion_rate, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(subcategories_to_categories, by = "subcategory_names")
      
      plot_data_ordered <- plot_data %>%
        arrange(desc(convertion_rate)) 
    }
    
    # Create a factor with unique levels, ordered by value
    plot_data[[name_col]] <- factor(plot_data[[name_col]],
                                    levels = rev(plot_data_ordered[[name_col]]))
    
    # Create the plot, order by value
    p <- ggplot(plot_data, aes(x = .data[[name_col]],
                               y = .data[[value_type]],
                               fill = .data$category_names
    )) +
      # apply the color palette
      scale_fill_manual(values = data()$category_colors) +
      geom_bar(stat = "identity") +
      labs(title = paste(value_type, "by", gsub("_names", "", name_col)),
           x = gsub("_names", "", name_col),
           y = value_type) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none"
      ) +
      coord_flip()
    
    if (!is.null(selected_column())) {
      # Highlight the selected column
      selected_label <- selected_column()
      # We don't have access to .data
      # so we need to use the name_col directly
      selected_data <- plot_data[plot_data[[name_col]] == selected_label, ]
      # Get the color for the selected label and change the brightness
      if (name_col == "category_names") {
        selected_fill <- data()$category_colors[selected_label]
      } else {
        # Selected label is a subcategory, not categroy
        selected_fill <- data()$category_colors[selected_data$category_names[1]]
      }
      selected_fill <- darken(selected_fill, amount = 0.4)
      p <- p + 
        geom_bar(data = selected_data, aes(x = .data[[name_col]], y = .data[[value_type]]),
                 stat = "identity", fill = selected_fill)
    }
    
    return(p)
  })
  
  # Handle plot click
  observeEvent(input$plot_click, {
    dataset <- get_dataset()
    data_type <- input$dataType
    value_type <- input$valueType
    
    # Different handling for histogram vs bar plot
    if (data_type %in% c("Orders", "Order items", "Offers", "Variants")) {
      # Histogram click handling
      
      
      if (value_type == "Price by quantity") {
        # Create a histogram of price, where count is a sum of quantities
        value_column <- dataset %>%
          group_by(Price) %>%
          reframe(Quantity = sum(Quantity, na.rm = TRUE)) %>%
          ungroup() %>%
          # for every row create a rep(price, quantity) vector
          mutate(value = map2(Price, Quantity, ~rep(.x, .y))) %>%
          select(value) %>%
          # aggregate them into a single row
          reframe(value = unlist(value))
        value_column <- value_column$value
      } else {
        value_column <- get_value_column()
      }
      
      # Get the clicked x-value
      clicked_value <- input$plot_click$x
      
      # Handle case with no data
      if (length(value_column) == 0 || all(is.na(value_column))) {
        return()
      }
      
      # Calculate histogram breaks
      range_data <- range(value_column, na.rm = TRUE)
      breaks <- seq(from = range_data[1], to = range_data[2], length.out = input$bins + 1)
      
      # Find which bin was clicked
      for (i in 1:(length(breaks) - 1)) {
        if (clicked_value >= breaks[i] && clicked_value < breaks[i + 1]) {
          prev_selected_bin = selected_bin()
          if (!is.null(prev_selected_bin)) {
            # Check if the clicked bin is the same as the previously selected bin
            if (prev_selected_bin$start == breaks[i] && prev_selected_bin$end == breaks[i + 1]) {
              # If it's the same, clear the selection
              selected_bin(NULL)
              filtered_data(NULL)
              return()
            }
          }
          
          bin_start <- breaks[i]
          bin_end <- breaks[i + 1]
          
          # Store both bin boundaries to ensure exact matching later
          selected_bin(list(
            start = bin_start,
            end = bin_end,
            index = i
          ))
          
          # Apply filter to the dataset - match the exact bin boundaries
          if (value_type == "Price by order item" || value_type == "Price by quantity") {
            value_type <- "Price"
          }
          if (i == 1) {
            # Include the endpoint for the first bin
            filtered <- dataset[dataset[[value_type]] >= bin_start & dataset[[value_type]] <= bin_end, ]
          } else {
            # Standard half-open interval for other bins
            filtered <- dataset[dataset[[value_type]] > bin_start & dataset[[value_type]] <= bin_end, ]
          }
          
          filtered_data(filtered)
          break
        }
      }
      
      # We are working with a histogram, so reset the selected column
      selected_column(NULL)
    }
  })
  
  # define a barplot click function
  observeEvent(input$categories_plot_click, {
    clicked_y <- input$categories_plot_click$y
    clicked_x <- input$categories_plot_click$x
    handle_bar_plot_click("category_names", clicked_x, clicked_y)
  })
  
  observeEvent(input$subcategories_plot_click, {
    clicked_y <- input$subcategories_plot_click$y
    clicked_x <- input$subcategories_plot_click$x
    handle_bar_plot_click("subcategory_names", clicked_x, clicked_y)
  })
  
  # Reset filters when button is clicked
  observeEvent(input$resetFilters, {
  })
  
  # Reset filters when data type, value type or bins are changed
  observeEvent(c(input$dataType, input$valueType, input$bins), {
    selected_bin(NULL)
    selected_column(NULL)
    filtered_data(NULL)
  })
  
  observeEvent(c(input$dataType, input$valueType), {
    # get proposed number of bins with hist()
    if (is.null(input$dataType) || is.null(input$valueType)) {
      return()
    }
    value_column <- get_value_column()
    if (length(value_column) == 0 || all(is.na(value_column))) {
      updateNumericInput(session, "bins", value = 10)
      return()
    }
    breaks <- hist(value_column, plot = FALSE)$breaks
    # Calculate the number of bins based on breaks
    num_bins <- length(breaks) - 1
    updateNumericInput(session, "bins", value = num_bins)
  })
  
  # Render the data table
  output$dataTable <- renderDT({
    # If filtered data exists, show it, otherwise show the full dataset
    display_data <- if (!is.null(filtered_data())) {
      filtered_data()
    } else {
      get_dataset()
    }
    
    # Handle list columns for display - convert them to strings
    display_data <- as.data.frame(display_data)
    for (col in names(display_data)) {
      if (is.list(display_data[[col]])) {
        display_data[[col]] <- sapply(display_data[[col]], function(x) {
          if (length(x) > 0) {
            paste(x, collapse = ", ")
          } else {
            NA
          }
        })
      }
    }
    
    if (input$dataType == "Categories") {
      # Get offer name using offer id
      display_data <- display_data %>%
        left_join(data()$Offers %>% select(offer_id, name), by = "offer_id") %>%
        select(-offer_id) %>%
        mutate(name = paste0("<a href='https://albox.pl/offer/kubek-oslo-wlasny-projekt-45/czarny-86'>", name, "</a>"))
    } else if (input$dataType == "Offers") {
      display_data <- display_data %>%
        mutate(name = paste0("<a href='https://albox.pl/offer/kubek-oslo-wlasny-projekt-45/czarny-86'>", name, "</a>"))
    } else if (input$dataType == "Order items") {
      # Get variant name using variant id
      display_data <- display_data %>%
        select(-order_id, -offer_id, -created_at.x, -created_at.y, -user_id,
               -billing_and_delivery_details_equal, -name, -subcategories,
               -subcategory_names, -category_ids, -category_names) %>%
        left_join(data()$Variants %>% select(variant_id, name), by = "variant_id") %>%
        select(-variant_id) %>%
        mutate(name = paste0("<a href='https://albox.pl/offer/kubek-oslo-wlasny-projekt-45/czarny-86'>", name, "</a>")) %>%
        rename(Variant = name)
    } else if (input$dataType == "Variants") {
      display_data <- display_data %>%
        mutate(name = paste0("<a href='https://albox.pl/offer/kubek-oslo-wlasny-projekt-45/czarny-86'>", name, "</a>"))
    }
    
    datatable(display_data, options = list(pageLength = 10), escape = FALSE);
  })
}

# Run the application
shinyApp(ui = ui, server = server)