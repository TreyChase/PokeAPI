library(shiny)
library(DBI)
library(RSQLite)
library(magick)
library(dplyr)

# Source the QMD file for helper functions
source(knitr::purl("pokeapi.qmd", output = tempfile(), quiet = TRUE))

# Connect to the SQLite database
db <- dbConnect(SQLite(), "pokemon.sqlite")

# UI for the Shiny App
ui <- fluidPage(
  titlePanel("Pokémon Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "pokemon_name",
        label = "Select a Pokémon",
        choices = NULL, # Will be dynamically populated
        selected = NULL
      )
    ),
    mainPanel(
      h3(textOutput("pokemon_info")),
      fluidRow(
        column(4, imageOutput("pokemon_sprite")),
        column(4, imageOutput("pokemon_shiny_sprite")),
        column(4, imageOutput("pokemon_art"))
      ),
      plotOutput("pokemon_stats_plot")
    )
  )
)

# Server for the Shiny App
server <- function(input, output, session) {
  # Query Pokémon names from the database
  pokemon_names <- dbGetQuery(db, "SELECT name FROM pokemon_names")
  
  # Update selectInput with Pokémon names
  updateSelectInput(session, "pokemon_name", choices = pokemon_names$name)
  
  # Fetch Pokémon data (use the first 151 Pokémon for now)
  pokemon_data <- fetch_pokemon_attributes(151)
  
  # Display Pokémon information
  output$pokemon_info <- renderText({
    req(input$pokemon_name)
    selected_pokemon <- pokemon_data %>% filter(name == input$pokemon_name)
    paste("Name:", selected_pokemon$name, "| ID:", selected_pokemon$id)
  })
  
  # Helper to fetch or save images
  download_image <- function(url, temp_file) {
    tryCatch({
      download.file(url, temp_file, mode = "wb")
      temp_file
    }, error = function(e) {
      NULL
    })
  }
  
  # Display Pokémon sprites
  output$pokemon_sprite <- renderImage({
    req(input$pokemon_name)
    selected_pokemon <- pokemon_data %>% filter(name == input$pokemon_name)
    temp_file <- tempfile(fileext = ".png")
    src <- download_image(selected_pokemon$sprite, temp_file)
    list(src = src, contentType = "image/png", alt = "Pokémon Sprite")
  }, deleteFile = TRUE)
  
  output$pokemon_shiny_sprite <- renderImage({
    req(input$pokemon_name)
    selected_pokemon <- pokemon_data %>% filter(name == input$pokemon_name)
    temp_file <- tempfile(fileext = ".png")
    src <- download_image(selected_pokemon$shiny_sprite, temp_file)
    list(src = src, contentType = "image/png", alt = "Pokémon Shiny Sprite")
  }, deleteFile = TRUE)
  
  output$pokemon_art <- renderImage({
    req(input$pokemon_name)
    selected_pokemon <- pokemon_data %>% filter(name == input$pokemon_name)
    temp_file <- tempfile(fileext = ".png")
    src <- download_image(selected_pokemon$art, temp_file)
    list(src = src, contentType = "image/png", alt = "Pokémon Art")
  }, deleteFile = TRUE)
  
  # Plot Pokémon base stats
  output$pokemon_stats_plot <- renderPlot({
    req(input$pokemon_name)
    plot_base_stats(input$pokemon_name, pokemon_data)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


