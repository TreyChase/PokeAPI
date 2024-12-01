library(shiny)
library(DBI)
library(RSQLite)
library(magick)
library(dplyr)

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
  # Populate the dropdown menu with Pokémon names from the local database
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
  
  # Fetch Pokémon data dynamically from the local database based on the selected name
  selected_pokemon_data <- reactive({
    req(input$pokemon) # Ensure a Pokémon is selected
    conn <- dbConnect(SQLite(), db_path)
    pokemon_data <- dbGetQuery(conn, paste0("SELECT * FROM pokemon WHERE name = '", input$pokemon, "'"))
    dbDisconnect(conn)
    validate(need(nrow(pokemon_data) > 0, "Pokémon data not found in the database."))
    return(pokemon_data)
  })
  
  # Render the Pokémon's Name
  output$pokemonName <- renderText({
    pokemon_data <- selected_pokemon_data()
    pokemon_data$name[1]
  })
  
  # Render the Base Stats Plot using plot_base_stats from the QMD helper function
  output$baseStatsPlot <- renderPlot({
    pokemon_data <- selected_pokemon_data()
    plot_base_stats(pokemon_data$name[1], pokemon_data)
  })
  
  # Render the Regular Sprite from the local file path
  output$sprite <- renderImage({
    pokemon_data <- selected_pokemon_data()
    sprite_path <- pokemon_data$sprite_path[1] # Assuming `sprite_path` is a column in your database
    validate(need(file.exists(sprite_path), "Sprite not found."))
    list(src = sprite_path, contentType = "image/png", width = 200, height = 200)
  }, deleteFile = FALSE)
  
  # Render the Shiny Sprite from the local file path
  output$shinySprite <- renderImage({
    pokemon_data <- selected_pokemon_data()
    shiny_sprite_path <- pokemon_data$shiny_sprite_path[1] # Assuming `shiny_sprite_path` is a column in your database
    validate(need(file.exists(shiny_sprite_path), "Shiny sprite not found."))
    list(src = shiny_sprite_path, contentType = "image/png", width = 200, height = 200)
  }, deleteFile = FALSE)
  
  # Render the Artwork from the local file path
  output$artwork <- renderImage({
    pokemon_data <- selected_pokemon_data()
    artwork_path <- pokemon_data$artwork_path[1] # Assuming `artwork_path` is a column in your database
    validate(need(file.exists(artwork_path), "Artwork not found."))
    list(src = artwork_path, contentType = "image/png", width = 300, height = 300)
  }, deleteFile = FALSE)
}

# Run the Shiny App
shinyApp(ui = ui, server = server)


