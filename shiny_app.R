# Required Libraries
library(shiny)
library(DBI)
library(RSQLite)
library(magick)
library(dplyr)

# Source the QMD file to 
source(knitr::purl("pokeapi.qmd", output = tempfile(), quiet = TRUE))

# Database path
db_path <- "pokemon_data.sqlite"

# UI for the Shiny App
ui <- fluidPage(
  titlePanel("Pokémon Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "pokemon",
        label = "Search and Select a Pokémon",
        choices = NULL, # Dynamically populated when the app starts
        selected = NULL,
        options = list(placeholder = "Type to search Pokémon by name")
      )
    ),
    mainPanel(
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h3(textOutput("pokemonName")),
            imageOutput("artwork")
          )
        ),
        column(
          width = 8,
          plotOutput("baseStatsPlot", height = "400px")
        )
      ),
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h4("Sprites"),
            fluidRow(
              column(
                width = 6,
                imageOutput("sprite", height = "200px")
              ),
              column(
                width = 6,
                imageOutput("shinySprite", height = "200px")
              )
            )
          )
        )
      )
    )
  )
)

# Server Logic for the Shiny App
server <- function(input, output, session) {
  # Populate the dropdown menu with Pokémon names
  observe({
    conn <- dbConnect(SQLite(), db_path)
    pokemon_choices <- dbGetQuery(conn, "SELECT name FROM pokemon ORDER BY id") %>%
      pull(name)
    dbDisconnect(conn)
    
    updateSelectizeInput(
      session,
      "pokemon",
      choices = pokemon_choices,
      selected = pokemon_choices[1]
    )
  })
  
  # Fetch Pokémon data dynamically based on the selected name
  selected_pokemon_data <- reactive({
    req(input$pokemon) # Ensure a Pokémon is selected
    conn <- dbConnect(SQLite(), db_path)
    pokemon_data <- dbGetQuery(conn, paste0("SELECT * FROM pokemon WHERE name = '", input$pokemon, "'"))
    dbDisconnect(conn)
    return(pokemon_data)
  })
  
  # Render the Pokémon's Name
  output$pokemonName <- renderText({
    pokemon_data <- selected_pokemon_data()
    pokemon_data$name[1]
  })
  
  # Render the Base Stats Plot using plot_base_stats from .qmd
  output$baseStatsPlot <- renderPlot({
    pokemon_data <- selected_pokemon_data()
    plot_base_stats(pokemon_data$name[1], pokemon_data)
  })
  
  # Render the Regular Sprite
  output$sprite <- renderImage({
    pokemon_data <- selected_pokemon_data()
    sprite <- get_pokemon_sprite(pokemon_data$name[1], pokemon_data)
    tmpfile <- tempfile(fileext = ".png")
    magick::image_write(sprite, tmpfile)
    list(src = tmpfile, contentType = "image/png", width = 200, height = 200)
  }, deleteFile = TRUE)
  
  # Render the Shiny Sprite
  output$shinySprite <- renderImage({
    pokemon_data <- selected_pokemon_data()
    shiny_sprite <- get_pokemon_shiny_sprite(pokemon_data$name[1], pokemon_data)
    tmpfile <- tempfile(fileext = ".png")
    magick::image_write(shiny_sprite, tmpfile)
    list(src = tmpfile, contentType = "image/png", width = 200, height = 200)
  }, deleteFile = TRUE)
  
  # Render the Artwork
  output$artwork <- renderImage({
    pokemon_data <- selected_pokemon_data()
    artwork <- get_pokemon_art(pokemon_data$name[1], pokemon_data)
    tmpfile <- tempfile(fileext = ".png")
    magick::image_write(artwork, tmpfile)
    list(src = tmpfile, contentType = "image/png", width = 300, height = 300)
  }, deleteFile = TRUE)
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

