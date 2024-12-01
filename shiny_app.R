library(shiny)
library(tidyverse)
library(DBI)
library(RSQLite)
library(ggplot2)
library(magick)

# Accessing the QMD file (pokeapi.qmd)
source(
  knitr::purl("pokeapi.qmd", output = tempfile(), quiet = TRUE)
)

# Database path
db_path <- "pokemon_data.sqlite"

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Pokémon Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "pokemon",
        label = "Search and Select a Pokémon",
        choices = NULL, # Dynamically populate choices later
        selected = NULL,
        options = list(placeholder = "Type to search Pokémon by name or ID")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Base Stats", plotOutput("baseStatsPlot")),
        tabPanel("Sprite", imageOutput("sprite")),
        tabPanel("Shiny Sprite", imageOutput("shinySprite")),
        tabPanel("Artwork", imageOutput("artwork"))
      )
    )
  )
)

# Server for the Shiny app
server <- function(input, output, session) {
  # Dynamically populate the dropdown menu with Pokémon names and IDs
  observe({
    conn <- dbConnect(SQLite(), db_path)
    pokemon_choices <- dbGetQuery(conn, "SELECT id, name FROM pokemon ORDER BY id") %>%
      mutate(display = paste(id, "-", name)) %>%
      pull(display)
    dbDisconnect(conn)
    
    updateSelectizeInput(
      session,
      "pokemon",
      choices = pokemon_choices,
      selected = pokemon_choices[1]
    )
  })
  
  # Reactive: Fetch Pokémon data for the selected Pokémon
  selected_pokemon_data <- reactive({
    req(input$pokemon) # Ensure a Pokémon is selected
    
    # Extract Pokémon name from "ID - Name" format
    pokemon_name <- str_remove(input$pokemon, "^[0-9]+ - ")
    
    # Query Pokémon data from the database
    conn <- dbConnect(SQLite(), db_path)
    pokemon_data <- dbGetQuery(conn, paste0(
      "SELECT * FROM pokemon WHERE name = '", pokemon_name, "'"
    ))
    dbDisconnect(conn)
    
    return(pokemon_data)
  })
  
  # Base stats plot
  output$baseStatsPlot <- renderPlot({
    pokemon_data <- selected_pokemon_data()
    plot_base_stats(pokemon_data$name[1], pokemon_data)
  })
  
  # Sprite image
  output$sprite <- renderImage({
    pokemon_data <- selected_pokemon_data()
    sprite <- get_pokemon_sprite(pokemon_data$name[1], pokemon_data)
    tmpfile <- tempfile(fileext = ".png")
    image_write(sprite, tmpfile)
    list(src = tmpfile, contentType = "image/png", width = 500, height = 500)
  }, deleteFile = TRUE)
  
  # Shiny sprite image
  output$shinySprite <- renderImage({
    pokemon_data <- selected_pokemon_data()
    shiny_sprite <- get_pokemon_shiny_sprite(pokemon_data$name[1], pokemon_data)
    tmpfile <- tempfile(fileext = ".png")
    image_write(shiny_sprite, tmpfile)
    list(src = tmpfile, contentType = "image/png", width = 500, height = 500)
  }, deleteFile = TRUE)
  
  # Artwork image
  output$artwork <- renderImage({
    pokemon_data <- selected_pokemon_data()
    artwork <- get_pokemon_art(pokemon_data$name[1], pokemon_data)
    tmpfile <- tempfile(fileext = ".png")
    image_write(artwork, tmpfile)
    list(src = tmpfile, contentType = "image/png", width = 500, height = 500)
  }, deleteFile = TRUE)
}

# Run the app
shinyApp(ui = ui, server = server)



