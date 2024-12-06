library(shiny)
library(DBI)
library(duckdb)
library(dplyr)
library(httr)
library(jsonlite)
library(DT)
library(purrr)
library(stringr)
library(tools)

# Function to fetch Pokémon moveset
get_pokemon_moveset <- function(pokemon_name) {
  pokemon_name <- tolower(pokemon_name)
  base_url <- "https://pokeapi.co/api/v2/pokemon/"
  url <- paste0(base_url, pokemon_name)
  
  response <- GET(url)
  if (status_code(response) == 200) {
    content <- content(response, as = "text")
    json_data <- fromJSON(content, flatten = TRUE)
    
    if ("moves" %in% names(json_data)) {
      moves <- json_data$moves %>%
        pluck("move.url") %>%
        map_df(~ tryCatch({
          move_response <- GET(.x)
          if (status_code(move_response) == 200) {
            move_content <- content(move_response, as = "text")
            move_data <- fromJSON(move_content, flatten = TRUE)
            
            tibble(
              Move = tools::toTitleCase(move_data$name),
              Type = tools::toTitleCase(move_data$type$name),
              BP = ifelse(is.null(move_data$power), NA_integer_, move_data$power),
              Description = move_data$flavor_text_entries %>%
                filter(language.name == "en") %>%
                slice(1) %>%
                pull(flavor_text) %>%
                str_replace_all("\n", " ")
            )
          } else {
            tibble(
              Move = "N/A",
              Type = "N/A",
              BP = NA_integer_,
              Description = "N/A"
            )
          }
        }, error = function(e) {
          tibble(
            Move = "Error",
            Type = "Error",
            BP = NA_integer_,
            Description = "Error"
          )
        }))
      
      return(moves %>% arrange(Move))
    } else {
      stop("Moveset not found in the response.")
    }
  } else {
    stop(paste("Failed to fetch data: HTTP", status_code(response)))
  }
}

# UI for the Shiny App
ui <- fluidPage(
  titlePanel("Pokémon Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "pokemon_name",
        label = "Select a Pokémon",
        choices = NULL,
        selected = NULL,
        selectize = TRUE
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          div(
            class = "output-container",
            h3(textOutput("pokemon_name_header"))
          ),
          fluidRow(
            column(6, div(class = "bordered-container", imageOutput("pokemon_art"))),
            column(6, div(class = "bordered-container", plotOutput("pokemon_stats_plot")))
          ),
          div(
            style = "margin-top: 30px;",
            h3("Sprites"),
            fluidRow(
              column(4, div(class = "bordered-container", imageOutput("pokemon_sprite"))),
              column(4, div(class = "bordered-container", imageOutput("pokemon_shiny_sprite")))
            )
          )
        ),
        tabPanel(
          "Moveset",
          h3("Moves Learned"),
          DTOutput("pokemon_moveset")
        )
      )
    )
  )
)

# Server for the Shiny App
server <- function(input, output, session) {
  db_path <- "pokemon_data.duckdb"
  conn <- dbConnect(duckdb(), db_path)
  pokemon_names <- dbReadTable(conn, "pokemon") %>%
    mutate(name_with_id = paste(id, "-", name)) %>%
    select(name_with_id, name)
  dbDisconnect(conn)
  
  updateSelectInput(session, "pokemon_name", choices = pokemon_names$name_with_id)
  
  selected_pokemon_data <- reactive({
    req(input$pokemon_name)
    selected_name <- pokemon_names %>%
      filter(name_with_id == input$pokemon_name) %>%
      pull(name)
    
    conn <- dbConnect(duckdb(), db_path)
    pokemon_data <- dbReadTable(conn, "pokemon") %>% filter(name == selected_name)
    dbDisconnect(conn)
    return(pokemon_data)
  })
  
  pokemon_moveset <- reactive({
    req(selected_pokemon_data())
    pokemon_name <- selected_pokemon_data()$name
    tryCatch({
      get_pokemon_moveset(pokemon_name)
    }, error = function(e) {
      tibble(Move = "No moves available", Type = NA, BP = NA, Description = NA)
    })
  })
  
  output$pokemon_moveset <- renderDT({
    req(pokemon_moveset())
    moveset <- pokemon_moveset()
    
    print(moveset)  # Debug: Check the moveset data
    
    datatable(
      moveset,
      options = list(
        pageLength = 10,
        autoWidth = TRUE
      ),
      escape = FALSE
    )
  })
  
  # Display Pokémon name
  output$pokemon_name_header <- renderText({
    req(input$pokemon_name)
    selected_pokemon_data()$name
  })
  
  # Display Pokémon art
  output$pokemon_art <- renderImage({
    req(selected_pokemon_data())
    art_url <- selected_pokemon_data()$art
    temp_file <- tempfile(fileext = ".png")
    download.file(art_url, temp_file, mode = "wb")
    list(src = temp_file, contentType = "image/png", alt = "Pokémon Art")
  }, deleteFile = TRUE)
  
  # Display Pokémon stats plot
  output$pokemon_stats_plot <- renderPlot({
    req(selected_pokemon_data())
    plot_base_stats(selected_pokemon_data()$name, selected_pokemon_data())
  })
  
  # Display Pokémon sprite
  output$pokemon_sprite <- renderImage({
    req(selected_pokemon_data())
    sprite_url <- selected_pokemon_data()$sprite
    temp_file <- tempfile(fileext = ".png")
    download.file(sprite_url, temp_file, mode = "wb")
    list(src = temp_file, contentType = "image/png", alt = "Pokémon Sprite")
  }, deleteFile = TRUE)
  
  # Display Pokémon shiny sprite
  output$pokemon_shiny_sprite <- renderImage({
    req(selected_pokemon_data())
    shiny_sprite_url <- selected_pokemon_data()$shiny_sprite
    temp_file <- tempfile(fileext = ".png")
    download.file(shiny_sprite_url, temp_file, mode = "wb")
    list(src = temp_file, contentType = "image/png", alt = "Pokémon Shiny Sprite")
  }, deleteFile = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)
