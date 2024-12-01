# Required Libraries
library(shiny)
library(DBI)
library(RSQLite)
library(magick)
library(dplyr)

# Source the QMD file to access plot_base_stats
source(knitr::purl("pokeapi.qmd", output = tempfile(), quiet = TRUE))

# Helper Function: Get Regular Sprite
get_pokemon_sprite <- function(pokemon, pokemon_data) {
  pokemon_sprite <- pokemon_data |>
    filter(name == pokemon) |>
    select(sprite) |>
    pull()
  
  if (is.null(pokemon_sprite) || pokemon_sprite == "") {
    stop("No sprite found for the specified Pokémon.")
  }
  
  img <- tryCatch(
    {
      magick::image_read(pokemon_sprite)
    },
    error = function(e) {
      stop("Failed to read the sprite. Check the file path or URL.")
    }
  )
  
  magick::image_scale(img, "500x500")
}

# Helper Function: Get Shiny Sprite
get_pokemon_shiny_sprite <- function(pokemon, pokemon_data) {
  pokemon_sprite <- pokemon_data |>
    filter(name == pokemon) |>
    select(shiny_sprite) |>
    pull()
  
  if (is.null(pokemon_sprite) || pokemon_sprite == "") {
    stop("No sprite found for the specified Pokémon.")
  }
  
  img <- tryCatch(
    {
      magick::image_read(pokemon_sprite)
    },
    error = function(e) {
      stop("Failed to read the sprite. Check the file path or URL.")
    }
  )
  
  magick::image_scale(img, "500x500")
}

# Helper Function: Get Official Artwork
get_pokemon_art <- function(pokemon, pokemon_data) {
  pokemon_art <- pokemon_data |>
    filter(name == pokemon) |>
    select(art) |>
    pull()
  
  if (is.null(pokemon_art) || pokemon_art == "") {
    stop("No artwork found for the specified Pokémon.")
  }
  
  img <- tryCatch(
    {
      magick::image_read(pokemon_art)
    },
    error = function(e) {
      stop("Failed to read the image. Check the file path or URL.")
    }
  )
  
  magick::image_scale(img, "500x500")
}

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
      ),
      actionButton(inputId = "fetchButton", label = "Fetch Pokémon Info")
    ),
    mainPanel(
      h4(textOutput("pokemonName"), align = "center"), # Subheader for Pokémon name
      fluidRow(
        column(
          width = 4,
          wellPanel(
            div(
              style = "display: flex; justify-content: center; align-items: center; height: 300px;",
              imageOutput("artwork", width = "300px", height = "300px")
            )
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
          div(
            h4("Sprites", align = "center"), # Centered header above sprites
            style = "margin-bottom: 10px;"
          ),
          wellPanel(
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
  
  # Fetch Pokémon data dynamically based on the action button click
  selected_pokemon_data <- eventReactive(input$fetchButton, {
    req(input$pokemon) # Ensure a Pokémon is selected
    conn <- dbConnect(SQLite(), db_path)
    pokemon_data <- dbGetQuery(conn, paste0("SELECT * FROM pokemon WHERE name = '", input$pokemon, "'"))
    dbDisconnect(conn)
    return(pokemon_data)
  })
  
  # Render the Pokémon's Name as a Subheader
  output$pokemonName <- renderText({
    req(selected_pokemon_data())
    pokemon_data <- selected_pokemon_data()
    pokemon_data$name[1]
  })
  
  # Render the Base Stats Plot using plot_base_stats from .qmd
  output$baseStatsPlot <- renderPlot({
    req(selected_pokemon_data())
    pokemon_data <- selected_pokemon_data()
    plot_base_stats(pokemon_data$name[1], pokemon_data)
  })
  
  # Render the Regular Sprite
  output$sprite <- renderImage({
    req(selected_pokemon_data())
    pokemon_data <- selected_pokemon_data()
    sprite <- get_pokemon_sprite(pokemon_data$name[1], pokemon_data)
    tmpfile <- tempfile(fileext = ".png")
    magick::image_write(sprite, tmpfile)
    list(src = tmpfile, contentType = "image/png", width = 200, height = 200)
  }, deleteFile = TRUE)
  
  # Render the Shiny Sprite
  output$shinySprite <- renderImage({
    req(selected_pokemon_data())
    pokemon_data <- selected_pokemon_data()
    shiny_sprite <- get_pokemon_shiny_sprite(pokemon_data$name[1], pokemon_data)
    tmpfile <- tempfile(fileext = ".png")
    magick::image_write(shiny_sprite, tmpfile)
    list(src = tmpfile, contentType = "image/png", width = 200, height = 200)
  }, deleteFile = TRUE)
  
  # Render the Artwork
  output$artwork <- renderImage({
    req(selected_pokemon_data())
    pokemon_data <- selected_pokemon_data()
    artwork <- get_pokemon_art(pokemon_data$name[1], pokemon_data)
    tmpfile <- tempfile(fileext = ".png")
    magick::image_write(artwork, tmpfile)
    list(src = tmpfile, contentType = "image/png", width = 300, height = 300)
  }, deleteFile = TRUE)
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
