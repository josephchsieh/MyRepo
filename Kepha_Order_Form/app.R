#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Coffee Roastery Order Form Shiny App with Enhanced Styling
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(bslib)

# Define UI
ui <- fluidPage(
  theme = bs_theme(
    bg = "#FDF6EC", 
    fg = "#5D4037", 
    primary = "#795548",
    base_font = font_google("Poppins"),
    heading_font = font_google("Playfair Display"),
    font_scale = 1.0
  ),
  
  useShinyjs(),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      body {
        background-image: linear-gradient(rgba(253, 246, 236, 0.9), rgba(253, 246, 236, 0.9)), 
                          url('https://images.unsplash.com/photo-1447933601403-0c6688de566e?q=80&w=1000');
        background-size: cover;
        background-attachment: fixed;
      }
      .card {
        border-radius: 15px;
        box-shadow: 0 6px 10px rgba(0,0,0,0.1);
        background-color: rgba(255, 255, 255, 0.95);
        margin-bottom: 25px;
        padding: 20px;
        border-top: 3px solid #6F4E37;
      }
      .container {
        max-width: 850px;
      }
      h1.title {
        font-family: 'Playfair Display', sanserif;
        font-weight: 700;
        color: #5D4037;
        text-align: center;
        margin: 30px 0;
        font-size: 36px;
        text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
      }
      h3 {
        color: #6F4E37;
        border-bottom: 1px solid #D7CCC8;
        padding-bottom: 10px;
        margin-top: 25px;
        font-weight: 600;
      }
      .btn-primary {
        background-color: #795548;
        border-color: #5D4037;
        padding: 10px 25px;
        border-radius: 30px;
        font-weight: 500;
        transition: all 0.3s;
      }
      .btn-primary:hover {
        background-color: #5D4037;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      }
      .form-control {
        border-radius: 8px;
        border: 1px solid #D7CCC8;
        padding: 10px;
        background-color: rgba(255, 255, 255, 0.8);
      }
      .form-control:focus {
        border-color: #A1887F;
        box-shadow: 0 0 0 0.2rem rgba(161, 136, 127, 0.25);
      }
      .radio-item, .checkbox-item {
        margin: 10px 0;
      }
      .section-icon {
        margin-right: 10px;
        color: #8D6E63;
      }
      .logo-container {
        text-align: center;
        margin-bottom: 20px;
      }
      .logo {
        width: 120px;
        height: 120px;
        background-color: #6F4E37;
        border-radius: 50%;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        color: white;
        font-size: 24px;
        font-family: 'Playfair Display', serif;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      }
      .bean-card {
        border: 1px solid #D7CCC8;
        border-radius: 12px;
        padding: 15px;
        margin-bottom: 15px;
        background-color: rgba(255, 255, 255, 0.6);
        transition: all 0.3s;
      }
      .bean-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
      }
      .bean-title {
        font-weight: 600;
        color: #5D4037;
        margin-bottom: 10px;
      }
      .bean-origin {
        font-size: 12px;
        color: #8D6E63;
        margin-bottom: 15px;
      }
      .bean-description {
        font-size: 14px;
        margin-bottom: 15px;
        color: #5D4037;
      }
      label {
        font-weight: 500;
        color: #5D4037;
      }
      .required-label::after {
        content: ' *';
        color: #C62828;
      }
      .help-text {
        font-size: 12px;
        color: #8D6E63;
        font-style: italic;
        margin-top: 5px;
      }
    "))
  ),
  
  # Main container
  div(class = "container",
      # Logo and title
      div(h1(class = "title", "Kepha Coffee Order Form")
      ),
      
      # Introduction
      div(class = "card",
          p("Thank you for choosing our  coffee. Please fill out the form below to place your order."),
          p("All our beans are ethically sourced and freshly roasted to order.")
      ),
      
      # Contact Information Section
      div(class = "card",
          h3(tags$i(class = "fa fa-user section-icon"), "Contact Information"),
          div(class = "row",
              div(class = "col-md-4", 
                  textInput("name", span("Name", class = "required-label"), ""),
                  div(class = "help-text", "First, Last")
              ),
              div(class = "col-md-4", 
                  textInput("phone", span("Phone Number", class = "required-label"), ""),
              ),
              div(class = "col-md-4", 
                  textInput("email", span("Email", class = "required-label"), ""),
              )
          )
      ),
      
      # Bean Selection Section  
      div(class = "card",
          h3(tags$i(class = "fa fa-coffee section-icon"), "Bean Selection"),
          p("Select the beans you would like to order:"),
          
          div(class = "row",
              div(class = "col-md-4", 
                  div(class = "bean-card",
                      div(class = "bean-title", "Ethiopia - Washed"),
                      div(class = "bean-origin", "Yirgacheffe Werka Chelchele"),
                      div(class = "bean-description", "Notes of: Black Tea, Dark Chocolate, Orange, Roasted Almond, Vanilla"),
                      numericInput("ethiopia", "Quantity (bags)", 0, min = 0)
                  )
              ),
              div(class = "col-md-4", 
                  div(class = "bean-card",
                      div(class = "bean-title", "Guatemala - Washed"),
                      div(class = "bean-origin", "Cooperative La Asuncion"),
                      div(class = "bean-description", "Notes of: Caramel, Nougat, Milk Chocolate, Red Apple"),
                      numericInput("guatemala", "Quantity (bags)", 0, min = 0)
                  )
              ),
              div(class = "col-md-4", 
                  div(class = "bean-card",
                      div(class = "bean-title", "Costa Rica - Washed"),
                      div(class = "bean-origin", "Esperanza"),
                      div(class = "bean-description", "Notes of: Limeade, Caramel, Macadamia"),
                      numericInput("costa_rica", "Quantity (bags)", 0, min = 0)
                  )
              )
          )
      ),
      
      # Order Options
      div(class = "card",
          h3(tags$i(class = "fa fa-shopping-cart section-icon"), "Order Options"),
          
          # Roast Level Section
          div(class = "row",
              div(class = "col-md-6",
                  strong("Roast Level"),
                  radioButtons("quantity", "What roast level would you like?", 
                               choices = c("Light" = "1", 
                                           "Medium" = "2", 
                                           "Dark" = "3", 
                                           "Other" = "other")),
              ),
          
              # Promo Code Section
              div(class = "col-md-6",
                  strong("Discount"),
                  textInput("promo_code", "Enter Promo Code (if any)", ""),
                  div(class = "help-text", "Enter any special promotional code you have")
              )
          ),
          
          # Other Specifications
          strong("Special Instructions"),
          textAreaInput("specifications", "Additional notes about your order", "", rows = 3),
          div(class = "help-text", "Let us know if you have any special requests for grinding, packaging, etc.")
      ),
      
      # Delivery Method
      div(class = "card",
          h3(tags$i(class = "fa fa-truck section-icon"), "Delivery Method"),
          radioButtons("delivery", "Choose delivery method:", 
                       choices = c("Pickup" = "pickup", 
                                   "Shipping" = "shipping"),
                       inline = TRUE),
          
          # Conditional Panel for Pickup Locations
          conditionalPanel(
            condition = "input.delivery == 'pickup'",
            div(class = "pickup-container",
                h4("Pickup Location"),
                div(class = "help-text", "Select all locations that work for you"),
                checkboxGroupInput("pickup_location", "Select pickup location:",
                                   choices = c("Joseph's home", 
                                               "Ruthie's home", 
                                               "APU", 
                                               "EFCLA", 
                                               "Hillside Church", 
                                               "Other")),
                conditionalPanel(
                  condition = "input.pickup_location.includes('Other')",
                  textInput("other_pickup", "Specify other pickup location:", "")
                )
            )
          ),
          
          # Conditional Panel for Shipping Address
          conditionalPanel(
            condition = "input.delivery == 'shipping'",
            div(class = "shipping-container",
                h4("Shipping Address"),
                textInput("ship_address1", span("Address Line 1", class = "required-label"), ""),
                textInput("ship_address2", "Address Line 2 (optional)", ""),
                div(class = "row",
                    div(class = "col-md-6", textInput("ship_city", span("City", class = "required-label"), "")),
                    div(class = "col-md-3", textInput("ship_state", span("State", class = "required-label"), "")),
                    div(class = "col-md-3", textInput("ship_zip", span("ZIP Code", class = "required-label"), ""))
                )
            )
          )
      ),
      
      # Payment Information
      div(class = "card",
          h3(tags$i(class = "fa fa-credit-card section-icon"), "Payment Information"),
          div(class = "row",
              div(class = "col-md-6",
                  selectInput("payment_method", span("Payment Method:", class = "required-label"),
                              choices = c("Venmo", "Zelle"))
              ),
              div(class = "col-md-6",
                  textInput("payment_username", span("Your Username or Email:", class = "required-label"), ""),
                  div(class = "help-text", "Enter your Venmo username or Zelle email")
              )
          )
      ),
      
      # Submit Section
      div(class = "card text-center",
          p("By submitting this form, you agree to our terms and conditions."),
          actionButton("submit", "Submit Order", class = "btn-primary btn-lg"),
          br(), br(),
          p(class = "help-text", "Questions? Contact us at support@coffeeroastery.com")
      )
  ),
  
  # Order Confirmation Modal
  hidden(
    div(id = "confirmation_modal", class = "modal",
        div(class = "modal-dialog",
            div(class = "modal-content",
                div(class = "modal-header",
                    h4(class = "modal-title", "Order Confirmation")
                ),
                div(id = "confirmation_body", class = "modal-body"),
                div(class = "modal-footer",
                    actionButton("confirm_ok", "OK", class = "btn-primary")
                )
            )
        )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Validate that at least one bag was ordered
  observeEvent(input$submit, {
    total_bags <- input$ethiopia + input$guatemala + input$costa_rica
    
    if (total_bags == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please select at least one bag of coffee.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Check if quantity selection matches actual bean selection
    selected_quantity <- if(input$quantity == "other") input$other_quantity else as.numeric(input$quantity)
    if (total_bags != selected_quantity) {
      showModal(modalDialog(
        title = "Warning",
        paste("You've selected", total_bags, "bags of coffee, but indicated a total quantity of", 
              selected_quantity, "bags. Please ensure these match or update your selection."),
        easyClose = TRUE
      ))
      return()
    }
    
    # For pickup, ensure a location is selected
    if (input$delivery == "pickup" && length(input$pickup_location) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please select a pickup location.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Validate shipping address if shipping is selected
    if (input$delivery == "shipping") {
      if (input$ship_address1 == "" || input$ship_city == "" || 
          input$ship_state == "" || input$ship_zip == "") {
        showModal(modalDialog(
          title = "Error",
          "Please complete all required shipping address fields.",
          easyClose = TRUE
        ))
        return()
      }
    }
    
    # Validate payment information
    if (input$payment_username == "") {
      showModal(modalDialog(
        title = "Error",
        "Please provide your payment information.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Build order summary with attractive styling
    order_summary <- HTML(paste(
      "<div class='confirmation-container' style='background-color: #FDF6EC; padding: 20px; border-radius: 10px;'>",
      "<h4 style='color: #5D4037; border-bottom: 1px solid #D7CCC8; padding-bottom: 10px;'>Order Summary</h4>",
      
      "<div style='margin-bottom: 15px;'>",
      "<p style='margin: 5px 0;'><strong>Name:</strong> ", input$name, "</p>",
      "<p style='margin: 5px 0;'><strong>Contact:</strong> ", input$phone, " | ", input$email, "</p>",
      "</div>",
      
      "<div style='background-color: #EFEBE9; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
      "<h5 style='color: #5D4037; margin-top: 0;'>Order Details:</h5>",
      "<ul style='padding-left: 20px;'>",
      if(input$ethiopia > 0) paste("<li><strong>Ethiopia:</strong> ", input$ethiopia, " bags</li>") else "",
      if(input$guatemala > 0) paste("<li><strong>Guatemala:</strong> ", input$guatemala, " bags</li>") else "",
      if(input$costa_rica > 0) paste("<li><strong>Costa Rica:</strong> ", input$costa_rica, " bags</li>") else "",
      "</ul>",
      "<p style='margin-bottom: 0;'><strong>Total Quantity:</strong> ", 
      if(input$quantity == "other") input$other_quantity else input$quantity, " bags</p>",
      "</div>",
      
      if(input$specifications != "") paste("<div style='margin-bottom: 15px;'><strong>Special Instructions:</strong> ", input$specifications, "</div>") else "",
      if(input$promo_code != "") paste("<div style='margin-bottom: 15px;'><strong>Promo Code:</strong> ", input$promo_code, "</div>") else "",
      
      "<div style='background-color: #EFEBE9; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
      "<p style='margin-top: 0;'><strong>Delivery Method:</strong> ", ifelse(input$delivery == "pickup", "Pickup", "Shipping"), "</p>",
      
      # Pickup or shipping info
      if(input$delivery == "pickup") {
        paste("<p style='margin-bottom: 0;'><strong>Pickup Location:</strong> ", 
              paste(input$pickup_location, collapse = ", "), 
              if("Other" %in% input$pickup_location) paste(" - ", input$other_pickup) else "",
              "</p>")
      } else {
        paste("<p style='margin-bottom: 0;'><strong>Shipping Address:</strong><br>", 
              input$ship_address1, "<br>",
              if(input$ship_address2 != "") paste(input$ship_address2, "<br>") else "",
              input$ship_city, ", ", input$ship_state, " ", input$ship_zip, "</p>")
      },
      "</div>",
      
      "<div style='margin-bottom: 15px;'>",
      "<p style='margin: 5px 0;'><strong>Payment Method:</strong> ", input$payment_method, "</p>",
      "<p style='margin: 5px 0;'><strong>Payment Username:</strong> ", input$payment_username, "</p>",
      "</div>",
      
      "<p style='text-align: center; color: #5D4037; font-weight: 600; margin-top: 20px;'>Thank you for your order! We'll be in touch soon.</p>",
      "</div>"
    ))
    
    # Display order confirmation
    showModal(modalDialog(
      title = HTML("<h3 style='color: #5D4037; margin: 0;'>Order Confirmation</h3>"),
      order_summary,
      footer = tagList(
        downloadButton("download_receipt", "Download Receipt", 
                       style = "background-color: #795548; border-color: #5D4037;"),
        modalButton("Close")
      ),
      size = "m",
      easyClose = TRUE
    ))
    
    # In a real application, this would save the order to a database
    # saveOrder(input)
  })
  
  # Generate receipt for download
  output$download_receipt <- downloadHandler(
    filename = function() {
      paste("coffee-order-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".txt", sep="")
    },
    content = function(file) {
      # Format the text for the receipt
      receipt_text <- paste(
        "Coffee Roastery Order Receipt\n",
        "==============================\n\n",
        "Order Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
        "CUSTOMER INFORMATION\n",
        "-------------------\n",
        "Name: ", input$name, "\n",
        "Phone: ", input$phone, "\n",
        "Email: ", input$email, "\n\n",
        
        "ORDER DETAILS\n",
        "------------\n",
        if(input$ethiopia > 0) paste("Ethiopia: ", input$ethiopia, " bags\n") else "",
        if(input$guatemala > 0) paste("Guatemala: ", input$guatemala, " bags\n") else "",
        if(input$costa_rica > 0) paste("Costa Rica: ", input$costa_rica, " bags\n") else "",
        "\n",
        "Total Quantity: ", if(input$quantity == "other") input$other_quantity else input$quantity, " bags\n",
        if(input$specifications != "") paste("\nSpecial Instructions: ", input$specifications, "\n") else "",
        if(input$promo_code != "") paste("Promo Code: ", input$promo_code, "\n") else "",
        "\n",
        
        "DELIVERY INFORMATION\n",
        "-------------------\n",
        "Method: ", ifelse(input$delivery == "pickup", "Pickup", "Shipping"), "\n",
        
        if(input$delivery == "pickup") {
          paste("Pickup Location: ", 
                paste(input$pickup_location, collapse = ", "), 
                if("Other" %in% input$pickup_location) paste(" - ", input$other_pickup) else "",
                "\n")
        } else {
          paste("Shipping Address:\n", 
                input$ship_address1, "\n",
                if(input$ship_address2 != "") paste(input$ship_address2, "\n") else "",
                input$ship_city, ", ", input$ship_state, " ", input$ship_zip, "\n")
        },
        "\n",
        
        "PAYMENT INFORMATION\n",
        "------------------\n",
        "Payment Method: ", input$payment_method, "\n",
        "Payment Username: ", input$payment_username, "\n\n",
        
        "Thank you for your order!\n",
        "Questions? Contact us at: kephacoffee@gmail.com\n"
      )
      
      writeLines(receipt_text, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)