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
  
  # Rename convertion_rate to Convertion rate
  offers <- offers %>%
    rename(`Convertion rate` = convertion_rate)
  
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
    left_join(offers_with_categories %>% select(-price, -created_at), by = "offer_id")

  
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
    select(-user_id) %>%
    mutate(
      created_at = format(as.POSIXct(created_at), "%Y-%m-%d")
    ) %>%
    select(
      order_id, created_at, Quantity, "Total value", status, shipping_method, user_type
    ) %>%
    rename(
      "Order ID" = order_id,
      "Status" = status,
      "Created at" = created_at,
      "Shipping method" = shipping_method,
      "User type" = user_type
    )
  
  # Calculate convertion rates and total value for subcategories
  subcategories_summary <- order_items_with_categories %>% 
    select(
      subcategory_names, offer_id, Quantity, Price, `Convertion rate`
    ) %>%
    group_by(subcategory_names, offer_id) %>%
    summarise(
      Quantity = sum(Quantity, na.rm = TRUE),
      "Total value" = sum(Quantity * Price, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      select(offers_df, c("offer_id", "Convertion rate")),
      by = "offer_id"
    )%>%
    unnest(subcategory_names) %>%
    left_join(subcategories_df, by = c("subcategory_names" = "name")) %>%
    left_join(categories_df, by = c("category_id" = "category_id")) %>%
    select(-subcategory_id, -category_id) %>%
    rename(category_names = name) %>%
    select(category_names, subcategory_names, offer_id, Quantity, "Total value", "Convertion rate") %>%
    rename(
      `Category` = category_names,
      `Subcategory` = subcategory_names
    ) %>%
    mutate(
      `Convertion rate` = round(`Convertion rate`, 4)
    )
      
  # Calculate convertion rates and total value for offers
  offers_summary <- order_items_with_categories %>%
    group_by(offer_id) %>%
    summarise(
      Quantity = sum(Quantity, na.rm = TRUE),
      `Total value` = sum(Quantity * Price, na.rm = TRUE)
    ) %>%
    left_join(select(
      offers_df,
      c("offer_id", "Convertion rate", "recommended", "name", "price")
    ), by = "offer_id") %>%
    rename(
      Recommended = recommended,
      `Regular price` = price
      ) %>%
    mutate(
      `Convertion rate` = round(`Convertion rate`, 4)
    )
  
  # Calculate statistics for variants
  variants_df <- as.data.frame(variants)
  variants_with_data <- order_items_with_categories %>%
    group_by(offer_id, variant_id) %>%
    summarise(
      Quantity = sum(Quantity, na.rm = TRUE),
      `Total value` = sum(Quantity * Price, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      select(offers_df, c("offer_id", "recommended", "price")),
      by = "offer_id"
    ) %>%
    left_join(
      select(variants_df, c("variant_id", "name")),
      by = "variant_id"
    )
  
  palette <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728',
              '#9467bd', '#e377c2', '#7f7f7f',
              '#bcbd22', '#17becf')
  
  # Category colors
  all_categories = categories_df$name
  category_colors <- palette[1:length(all_categories)]
  names(category_colors) <- all_categories
  
  # Statuses colors
  all_statuses <- c("Delivered", "Paid", "Cancelled", "Pending", "Shipped")
  status_colors <- palette[1:length(all_statuses)]
  names(status_colors) <- all_statuses
  
  # Shipping methods colors
  all_shipping_methods <- c("Paczkomat Inost", "Kurier DPD", "Kurier Inpost")
  shipping_method_colors <- palette[1:length(all_shipping_methods)]
  names(shipping_method_colors) <- all_shipping_methods
  
  # User type colors
  user_types <- c("User", "Anonymous")
  user_type_colors <- palette[1:length(user_types)]
  names(user_type_colors) <- user_types
  
  # Personalization colors
  personalization_types <- c("None", "File", "Drawing", "Drawing + File")
  personalization_colors <- palette[1:length(personalization_types)]
  names(personalization_colors) <- personalization_types
  
  # Discount colors
  discount_types <- c("No discount", "Discount")
  discount_colors <- palette[1:length(discount_types)]
  names(discount_colors) <- discount_types
  
  # Recommendation colors
  recommendation_types <- c("Not recommended", "Additional carousel",
                            "Landing carousel", "In category")
  recommendation_colors <- palette[1:length(recommendation_types)]
  names(recommendation_colors) <- recommendation_types
  
  
  order_items_summary <- order_items_with_categories %>%
    select(-offer_id, -user_id,
           -billing_and_delivery_details_equal, -name, -subcategories,
           -subcategory_names, -category_ids, -category_names,
           -`Convertion rate`, -shipping_method, -`Total value`,
           -status) %>%
    mutate(created_at = format(as.POSIXct(created_at), "%Y-%m-%d")) %>%
    left_join(variants_df %>% select(variant_id, name), by = "variant_id") %>%
    select(-variant_id) %>%
    rename(
      Discount = was_discounted,
      Personalization = personalization,
      `Created at` = created_at,
      Recommended = recommended,
      `Order ID` = order_id,
      Variant = name
    ) %>%
    select(
      Variant, `Order ID`, `Created at`, Quantity, Price, Discount, Personalization, Recommended
    )
  
  variants_summary <- variants_with_data %>%
    rename(
      `Regular price` = price,
      Recommended = recommended
    ) %>%
    select(-offer_id, -variant_id)
  
  # Return all the prepared datasets
  return(list(
    Orders = orders_df,
    "Order items" = order_items_summary,
    Subcategories = subcategories_summary,
    Offers = offers_summary,
    Variants = variants_summary,
    category_colors = category_colors,
    status_colors = status_colors,
    shipping_method_colors = shipping_method_colors,
    user_type_colors = user_type_colors,
    personalization_colors = personalization_colors,
    discount_colors = discount_colors,
    recommendation_colors = recommendation_colors
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
        /* Remove default shinydashboard styling */
        .main-header .logo {
          background-color: #ffffff !important;
          color: #000000 !important;
          border: none !important;
        }
        
        /* Remove box borders and shadows */
        .box {
          border: none !important;
          box-shadow: none !important;
          background: transparent !important;
        }
        
        .box-header {
          border-bottom: none !important;
          padding-bottom: 0px !important;
        }
        
        .box-body {
          padding-top: 10px !important;
        }
        
        /* Clean content wrapper */
        .content-wrapper {
          background-color: #f8f9fa !important;
        }
        
        .content {
          padding: 15px !important;
        }
        
        /* Simplified value box styling */
        .small-valuebox .small-box {
          border: none !important;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1) !important;
          border-radius: 8px !important;
          padding: 15px !important;
          display: flex;
          align-items: center;
          background: white !important;
        }
        
        .small-valuebox .small-box h3 {
          font-size: 24px;
          font-weight: 600;
          margin: 0;
          color: #2c3e50;
        }
        
        .small-valuebox .small-box p {
          font-size: 14px;
          margin: 0;
          color: #7f8c8d;
          font-weight: 500;
        }
        
        .small-valuebox .icon-large {
          font-size: 32px;
          color: #27ae60;
          position: absolute;
          top: 20px;
          right: 20px;
          opacity: 0.8;
        }
        
        /* Input styling */
        .form-control, .selectize-input {
          border: 1px solid #e9ecef !important;
          border-radius: 6px !important;
          box-shadow: none !important;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #3498db !important;
          box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25) !important;
        }
        
        /* Button styling */
        .btn {
          border: none !important;
          border-radius: 6px !important;
          font-weight: 500 !important;
          transition: all 0.2s ease !important;
        }
        
        .btn-danger {
          background-color: #e74c3c !important;
          color: white !important;
        }
        
        .btn-danger:hover {
          background-color: #c0392b !important;
          transform: translateY(-1px) !important;
        }
        
        /* Plot containers */
        .plot-container {
          background: white;
          border-radius: 8px;
          padding: 15px;
          margin-bottom: 20px;
        }
        
        /* Horizontal plot arrangement */
        .plots-horizontal {
          display: flex;
          gap: 15px;
          align-items: flex-start;
        }
        
        .plots-horizontal > div {
          flex: 1;
        }
        
        /* Table styling */
        .dataTables_wrapper {
          background: white !important;
          border-radius: 8px !important;
          padding: 20px !important;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1) !important;
        }
        
        /* Section headers */
        .section-header {
          font-size: 18px;
          font-weight: 600;
          color: #2c3e50;
          margin-bottom: 15px;
          padding-bottom: 8px;
          border-bottom: 2px solid #3498db;
        }
      "))
    ),
    
    # Data Selection Section
    div(
      class = "section-header",
      "Data Selection & Controls"
    ),
    
    fluidRow(
      style = "display: flex; gap: 15px;",
      column(
        width = 2,
        selectInput(
          "dataType",
          "Data Type:",
          choices = c("Orders", "Order items", "Categories", "Offers", "Variants")
        ),
        # Date selector
        dateRangeInput(
          "dateRange",
          "Date Range:",
          start = "2020-01-01",
          end = Sys.Date(),
          min = "2020-01-01",
          max = Sys.Date(),
          format = "yyyy-mm-dd"
        )
      ),
      column(
        width = 2,
        style = "height: 100%; display: flex; flex-direction: column;
                justify-content: center; gap: 25px;",
        uiOutput("valueTypeUI"),
        actionButton("resetDateFilters", "Clear Date Filters", 
                     class = "btn btn-danger btn-sm",
                     style = "max-width: 150px;")
      ),
      column(
        width = 2,
        conditionalPanel(
          condition = "input.dataType != 'Categories'",
          numericInput(
            "bins",
            "Histogram Bins:",
            value = 10,
            min = 1,
            max = 50
          )
        )
      ),
      column(
        width = 2,
        br()
      ),
      column(
        width = 4,
        div(
          class = "small-valuebox",
          valueBox(
            value = htmlOutput("totalValueThisMonth"),
            subtitle = "Total orders value this month",
            icon = icon("chart-line"),
            color = "green",
            width = 12
          )
        )
      )
    ),
    
    br(),
    
    # Visualization Section
    div(
      class = "section-header",
      "Data Visualization"
    ),
    
    fluidRow(
      column(
        width = 6,
        div(
          class = "plot-container",
          conditionalPanel(
            condition = "input.dataType == 'Orders' || input.dataType == 'Order items' || input.dataType == 'Offers' || input.dataType == 'Variants'",
            plotOutput("histogram", click = "plot_click", height = "450px")
          ),
          conditionalPanel(
            condition = "input.dataType == 'Categories'",
            plotOutput("categoriesPlot", click = "categories_plot_click", height = "400px")
          )
        )
      ),
      
      column(
        width =  6,
        
        conditionalPanel(
          condition = "input.dataType != 'Categories'",
          div(
            class = "plot-container",
            div(
              class = "plots-horizontal",
              div(
                conditionalPanel(
                  condition = "input.dataType != 'Categories'",
                  plotlyOutput("additionalPlot1", height = "200px")
                )
              ),
              div(
                conditionalPanel(
                  condition = "input.dataType != 'Categories' && input.dataType != 'Offers' && input.dataType != 'Variants'",
                  plotlyOutput("additionalPlot2", height = "200px")
                )
              )
            )
          )
        ),
        
        # Additional plots for Orders data type
        conditionalPanel(
          condition = "input.dataType == 'Orders'",
          fluidRow(
            column(
              width = 12,
              div(
                class = "plot-container",
                div(
                  class = "plots-horizontal",
                  div(plotlyOutput("additionalPlot3", height = "200px")),
                  div(plotlyOutput("additionalPlot4", height = "200px"))
                )
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.dataType == 'Categories'",
          div(
            class = "plot-container",
            plotOutput("subcategoriesPlot", click = "subcategories_plot_click", height = "400px")
          )
        )
      )
    ),
    
    br(),
    
    # Data Table Section
    div(
      class = "section-header",
      "Data Table"
    ),
    
    fluidRow(
      column(
        width = 12,
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
      mutate(month = format(as.Date(`Created at`), "%Y-%m")) %>%
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
      choices <- c("Quantity", "Price by order item", "Price by bought product")
    } else if (data_type %in% c("Categories", "Offers")) {
      choices <- c("Quantity", "Total value", "Convertion rate")
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
    
    # Handle convertion_rate case - calculate mean
    if (value_type == "Convertion rate") {
      if (name_col == "Category") {
        plot_data <- plot_data %>%
          select(Category, "Convertion rate", offer_id) %>%
          distinct() %>%
          group_by(Category) %>%
          summarise(`Convertion rate` = mean(`Convertion rate`, na.rm = TRUE)) %>%
          ungroup()
        
        plot_data_ordered <- plot_data %>%
          arrange(desc(`Convertion rate`)) 
      } else if (value_type == "Convertion rate") {
        subcategories_to_categories <- plot_data %>%
          select("Subcategory", "Category") %>%
          unique()
        
        plot_data <- plot_data %>%
          select("Subcategory", "Convertion rate") %>%
          group_by(Subcategory) %>%
          summarise(`Convertion rate` = mean(`Convertion rate`, na.rm = TRUE)) %>%
          ungroup() %>%
          left_join(subcategories_to_categories, by = "Subcategory")
        
        plot_data_ordered <- plot_data %>%
          arrange(desc(`Convertion rate`)) 
      } else {
        warning(paste0("Column '", value_type, "' not found in dataset. Returning NA values."))
        return()
      }
    }
    
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
    if (is.null(value_type)) {
      return(rep(NA, nrow(dataset)))
    }
    if (value_type %in% names(dataset)) {
      return(dataset[[value_type]])
    } else if (value_type == "Price by order item") {
      return(dataset$Price)
    } else {
      if (value_type != "Price by bought product") {
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
    if (is.null(value_type) || (nrow(dataset) == 0 || all(is.na(value_column)) && value_type != "Price by bought product")) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    # Create histogram data
    if (value_type == "Price by bought product") {
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
    
    title = "Distribution of"
    switch(input$dataType,
           "Orders" = switch(value_type,
             "Quantity" = title <- paste(title, "quantity of items sold per order"),
             "Total value" = title <- paste(title, "total value of orders")
           ),
           "Order items" = switch(value_type,
             "Quantity" = title <- paste(title, "quantity of items sold per order item"),
             "Price by order item" = title <- paste(title, "price by order item"),
             "Price by bought product" = title <- paste(title, "price by bought product")
           ),
           "Offers" = switch(value_type,
             "Quantity" = title <- paste(title, "quantity of items sold per offer"),
             "Total value" = title <- paste(title, "total value generated per offer"),
             "Convertion rate" = title <- paste(title, "convertion rate of offers")
           ),
           "Variants" = switch(value_type,
             "Quantity" = title <- paste(title, "quantity of items sold per variant"),
             "Total value" = title <- paste(title, "total value generated per variant")
           )
    )
    
    # Add labels and theme
    p <- p +
      labs(title = title,
           x = input$valueType, 
           y = "Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
    if (value_type == "Total value") {
      # Change the x scale to big number format
      p <- p + 
        scale_x_continuous(labels = scales::comma_format(big.mark = ",")) +
        labs(x = "Total value (in zł)")
    }
    
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
    name_col <- "Category"
    
    # Ensure the name column exists
    if (!name_col %in% names(dataset)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Loading...") + 
               theme_void())
    }
    
    # Ensure the value column exists
    if (is.null(value_type) || !value_type %in% names(dataset)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Loading...") + 
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
    if (value_type == "Convertion rate") {
      plot_data <- plot_data %>%
        select(Category, "Convertion rate", offer_id) %>%
        distinct() %>%
        group_by(Category) %>%
        summarise(`Convertion rate` = mean(`Convertion rate`, na.rm = TRUE)) %>%
        ungroup()
      
      plot_data_ordered <- plot_data %>%
        arrange(desc(`Convertion rate`)) 
    }
    
    # Create a factor with unique levels, ordered by value
    plot_data[[name_col]] <- factor(plot_data[[name_col]],
                                    levels = rev(plot_data_ordered[[name_col]]))
    
    
    title = ""
    switch(value_type,
           "Quantity" = title <- paste(title, "Quantity of items sold per category"),
           "Total value" = title <- paste(title, "Total value generated per category"),
           "Convertion rate" = title <- paste(title, "Convertion rate of categories")
    )

    
    # Create the plot, order by value
    p <- ggplot(plot_data, aes(x = .data[[name_col]],
                               y = .data[[value_type]],
                               fill = .data$Category
                               )) +
      # apply the color palette
      scale_fill_manual(values = data()$category_colors) +
      geom_bar(stat = "identity") +
      labs(title = title,
           x = gsub("_names", "", name_col),
           y = value_type) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, face = "bold"),
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
      if (name_col == "Category") {
        selected_fill <- data()$category_colors[selected_label]
      } else {
        # Selected label is a subcategory, not categroy
        selected_fill <- data()$category_colors[selected_data$Category[1]]
      }
      selected_fill <- darken(selected_fill, amount = 0.4)
      p <- p + 
        geom_bar(
          data = selected_data,
          aes(x = .data[[name_col]],
          y = .data[[value_type]]
          ),
          stat = "identity",
          fill = selected_fill
        )
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
        group_by(Status) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = Status)
    } else if (input$dataType == "Order items") {
      # If we are working with order items
      p_data <- dataset %>%
        group_by(Personalization) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = Personalization)
    } else if (input$dataType == "Offers" || input$dataType == "Variants") {
      p_data <- dataset %>%
        group_by(Recommended) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = Recommended)
    } else {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    marker <- list(
      colors = switch(input$dataType,
                      "Orders" = data()$status_colors[p_data$label],
                      "Order items" = data()$personalization_colors[p_data$label],
                      "Offers" = data()$recommendation_colors[p_data$label],
                      "Variants" = data()$recommendation_colors[p_data$label])
    )
    
    p <- plot_ly(
      data = p_data,
      labels = ~label,
      values = ~percent,
      type = "pie",
      hoverinfo = "label+percent",
      textposition = "none",
      height = 210,
      width = 300,
      marker = marker
    ) %>% layout(
      title = list(
        text = ifelse(
          input$dataType == "Orders",
          "Distribution of order statuses",
          ifelse(input$dataType == "Order items",
                 "Distribution of personalization options",
                 "Distribution of recommendation types"
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
        group_by(`Shipping method`) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = `Shipping method`)
    } else if (input$dataType == "Order items") {
      # If we are working with order items
      p_data <- dataset %>%
        group_by(Discount) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = Discount)
    } else {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    marker = list(
      colors = switch(input$dataType,
                      "Orders" = data()$shipping_method_colors[p_data$label],
                      "Order items" = data()$discount_colors[p_data$label])
    )
    
    p <- plot_ly(
      data = p_data,
      labels = ~label,
      values = ~percent,
      type = "pie",
      hoverinfo = "label+percent",
      textposition = "none",
      height = 210,
      width = 300,
      marker = marker
    ) %>% layout(
      title = list(
        text = ifelse(input$dataType == "Orders",
                      "Distribution of shipping methods",
                      "Distribution of discounts"),
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
        group_by(`User type`) %>%
        summarise(percent = n()/all_rows_count) %>%
        ungroup() %>%
        rename(label = `User type`)
    } else {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") + 
               theme_void())
    }
    
    marker <- list(
      colors = data()$user_type_colors[p_data$label]
    )
    
    p <- plot_ly(
      data = p_data,
      labels = ~label,
      values = ~percent,
      type = "pie",
      hoverinfo = "label+percent",
      textposition = "none",
      height = 210,
      width = 300,
      marker = marker
    ) %>% layout(
      title = list(
        text = "Distribution of user types",
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
        filter(`Created at` >= as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"))) %>%
        mutate(month = format(as.Date(`Created at`), "%Y-%m")) %>%
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
      height = 210,
      width = 300
    ) %>% layout(
      title = list(
        text = paste0("Order counts by month (", format(Sys.Date(), "%Y"), ")"),
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
    
    name_col <- "Subcategory"
    
    if (!name_col %in% names(dataset)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Loading...") + 
               theme_void())
    }
    
    # Ensure the value column exists
    if (is.null(value_type) || !value_type %in% names(dataset)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Loading...") + 
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
    if (value_type == "Convertion rate") {
      subcategories_to_categories <- plot_data %>%
        select("Subcategory", "Category") %>%
        unique()
      
      plot_data <- plot_data %>%
        select("Subcategory", "Convertion rate") %>%
        group_by(Subcategory) %>%
        summarise(`Convertion rate` = mean(`Convertion rate`, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(subcategories_to_categories, by = "Subcategory")
      
      plot_data_ordered <- plot_data %>%
        arrange(desc(`Convertion rate`)) 
    }
    
    # Create a factor with unique levels, ordered by value
    plot_data[[name_col]] <- factor(plot_data[[name_col]],
                                    levels = rev(plot_data_ordered[[name_col]]))
    
    title = ""
    switch (value_type,
            "Quantity" = title <- paste(title, "Quantity of items sold per subcategory"),
            "Total value" = title <- paste(title, "Total value generated per subcategory"),
            "Convertion rate" = title <- paste(title, "Convertion rate of subcategories")
    )
    
    # Create the plot, order by value
    p <- ggplot(plot_data, aes(x = .data[[name_col]],
                               y = .data[[value_type]],
                               fill = .data$Category
    )) +
      # apply the color palette
      scale_fill_manual(values = data()$category_colors) +
      geom_bar(stat = "identity") +
      labs(title = title,
           x = gsub("_names", "", name_col),
           y = value_type) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, face = "bold"),
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
      if (name_col == "Category") {
        selected_fill <- data()$category_colors[selected_label]
      } else {
        # Selected label is a subcategory, not categroy
        selected_fill <- data()$category_colors[selected_data$Category[1]]
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
      
      
      if (value_type == "Price by bought product") {
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
          if (value_type == "Price by order item" || value_type == "Price by bought product") {
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
    handle_bar_plot_click("Category", clicked_x, clicked_y)
  })
  
  observeEvent(input$subcategories_plot_click, {
    clicked_y <- input$subcategories_plot_click$y
    clicked_x <- input$subcategories_plot_click$x
    handle_bar_plot_click("Subcategory", clicked_x, clicked_y)
  })
  
  # Reset filters when button is clicked
  observeEvent(input$resetDateFilters, {
    updateDateRangeInput(session, "dateRange", start = "2020-01-01", end = Sys.Date())
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
        mutate(name = paste0("<a href='https://albox.pl/offer/kubek-oslo-wlasny-projekt-45/czarny-86'>", name, "</a>")) %>%
        rename(Offer = name) %>%
        select(
          Offer,
          Category,
          Subcategory,
          Quantity,
          `Total value`,
          `Convertion rate`
        )
    } else if (input$dataType == "Offers") {
      display_data <- display_data %>%
        mutate(name = paste0("<a href='https://albox.pl/offer/kubek-oslo-wlasny-projekt-45/czarny-86'>", name, "</a>")) %>%
        rename(Offer = name) %>%
        select(
          Offer,
          `Regular price`,
          Quantity,
          `Total value`,
          Recommended,
          `Convertion rate`
        )
    } else if (input$dataType == "Order items") {
      # Get variant name using variant id
      display_data <- display_data %>%
        mutate(Variant = paste0("<a href='https://albox.pl/offer/kubek-oslo-wlasny-projekt-45/czarny-86'>", Variant, "</a>"))
    } else if (input$dataType == "Variants") {
      display_data <- display_data %>%
        mutate(name = paste0("<a href='https://albox.pl/offer/kubek-oslo-wlasny-projekt-45/czarny-86'>", name, "</a>")) %>%
        rename(Variant = name) %>%
        select(
          Variant,
          `Regular price`,
          Quantity,
          `Total value`,
          Recommended
        )
    }
    
    datatable(display_data, options = list(pageLength = 10), escape = FALSE);
  })
}

# Run the application
shinyApp(ui = ui, server = server)