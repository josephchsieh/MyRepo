#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for the storefront
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
      .title-panel {
        background-color: #f8f9fa;
        padding: 20px;
        text-align: center;
        font-family: 'Georgia', sans-serif;
      }
      .title-panel h1 {
        color: #873f2b;
        font-size: 28px;
      }
      .sidebar-panel {
        background-color: #873f2b;
        padding: 15px;
      }
    "))
    ),
    div(class = "title-panel",
        titlePanel("Kepha Coffee Roasters")
    ),
  titlePanel("Coffee Order Form"),
  sidebarLayout(
    sidebarPanel(
      h4("$18 Kenya Kiaguthu Peaberry - Washed", h6("Stone Fruit, Blackberry, Black Tea, Honey")),
      selectInput("size_kenya", "Choose a bag size:", choices = c("8 ounces" = "8 ounces", "16 ounces" = "16 ounces")),
      numericInput("quantity_kenya", "Quantity:", value = 1, min = 1),
      actionButton("add_kenya", "Add to Cart"),
      br(),
      h4("$16 Papua New Guinea Timuza Organic - Washed", h6("Cacao Nibs, Fruit Cake, Vanilla")),
      selectInput("size_png", "Choose a bag size:", choices = c("8 ounces" = "8 ounces", "16 ounces" = "16 ounces")),
      numericInput("quantity_png", "Quantity:", value = 1, min = 1),
      actionButton("add_png", "Add to Cart"),
      br(),
      h4("$14 Guatemala Pura Fruta - Natural", h6("Cacao Nibs, Strawberry, Pound Cake")),
      selectInput("size_guatemala", "Choose a bag size:", choices = c("8 ounces" = "8 ounces", "16 ounces" = "16 ounces")),
      numericInput("quantity_guatemala", "Quantity:", value = 1, min = 1),
      actionButton("add_guatemala", "Add to Cart"),
      br(),
      h4("$12 Brazil Super Fine - Natural", h6("Walnut, Dried Dates, Fudge, Caramel")),
      selectInput("size_brazil", "Choose a bag size:", choices = c("8 ounces" = "8 ounces", "16 ounces" = "16 ounces")),
      numericInput("quantity_brazil", "Quantity:", value = 1, min = 1),
      actionButton("add_brazil", "Add to Cart"),
      br(),
      h2("Shipping"),
      selectInput("select_shipping", "Select Shipping Method", choices = c("Pickup" = "Pickup", "Shipping" = "Shipping")),
      actionButton("add_shipping", "Add Shipping Method")
    ),
    mainPanel(
      tableOutput("cart"),
      textOutput("total")
    )
  )
)


# Define server logic required to draw the cart
server <- function(input, output, session) {
  # Define prices for each product and size
  prices <- list(
    "Kenya Kiaguthu Peaberry - Washed" = c("8 ounces" = 18, "16 ounces" = 36),
    "Papua New Guinea Timuza Organic - Washed" = c("8 ounces" = 16, "16 ounces" = 32),
    "Guatemala Pura Fruta - Natural" = c("8 ounces" = 14, "16 ounces" = 28),
    "Brazil Super Fine - Natural" = c("8 ounces" = 12, "16 ounces" = 22)
  )
  
  # Shipping costs
  shipping_cost <- c("Pickup" = 0, "Shipping" = 7)
  
  cart <- reactiveVal(data.frame(Product = character(0), Size = character(0), Quantity = numeric(0), Price = numeric(0)))
  
  observeEvent(input$add_kenya, {
    new_item <- data.frame(Product = "Kenya Kiaguthu Peaberry - Washed", Size = input$size_kenya, Quantity = input$quantity_kenya, Price = prices[["Kenya Kiaguthu Peaberry - Washed"]][[input$size_kenya]])
    update_cart(new_item)
  })
  
  observeEvent(input$add_png, {
    new_item <- data.frame(Product = "Papua New Guinea Timuza Organic - Washed", Size = input$size_png, Quantity = input$quantity_png, Price = prices[["Papua New Guinea Timuza Organic - Washed"]][[input$size_png]])
    update_cart(new_item)
  })
  
  observeEvent(input$add_guatemala, {
    new_item <- data.frame(Product = "Guatemala Pura Fruta - Natural", Size = input$size_guatemala, Quantity = input$quantity_guatemala, Price = prices[["Guatemala Pura Fruta - Natural"]][[input$size_guatemala]])
    update_cart(new_item)
  })
  
  observeEvent(input$add_brazil, {
    new_item <- data.frame(Product = "Brazil Super Fine - Natural", Size = input$size_brazil, Quantity = input$quantity_brazil, Price = prices[["Brazil Super Fine - Natural"]][[input$size_brazil]])
    update_cart(new_item)
  })
  
  observeEvent(input$add_shipping, {
    current_cart <- cart()
    # Remove any existing shipping entry
    current_cart <- current_cart[current_cart$Product != "Shipping Method", ]
    new_item <- data.frame(Product = "Shipping Method", Size = input$select_shipping, Quantity = 1, Price = shipping_cost[[input$select_shipping]])
    update_cart(new_item, current_cart)
  })
  
  update_cart <- function(new_item, current_cart = NULL) {
    if (is.null(current_cart)) {
      current_cart <- cart()
    }
    item_index <- which(current_cart$Product == new_item$Product & current_cart$Size == new_item$Size)
    
    if (length(item_index) > 0) {
      current_cart$Quantity[item_index] <- current_cart$Quantity[item_index] + new_item$Quantity
    } else {
      current_cart <- rbind(current_cart, new_item)
    }
    
    cart(current_cart)
  }
  
  output$cart <- renderTable({
    cart()
  })
  
  output$total <- renderText({
    total <- sum(cart()$Quantity * cart()$Price)
    paste("Total: $", total)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

