library(shiny)
library(bslib)
library(tidyverse)
library(glue)
library(ggridges)

# setwd("C:/Users/Tom/Documents/spotifyr/spotifyr")

# Setup -------------------------------------------------------------------

df <- tibble()
users <- c("Tom", "Harry", "Joe", "Charlotte")

for (user in users) {
  df <- read_tsv(
    file = glue("./data/streaming_data_{str_to_lower(user)}.tsv"),
    show_col_types = FALSE
  ) |>
  mutate(
    user = user,
    ts = date(ymd_hms(ts)),
    track = str_c(track, artist, sep = ", "),
    album = str_c(album, artist, sep = ", ")
  ) |>
  bind_rows(df)
}

# Turn on thematic for theme-matched plots
thematic::thematic_shiny(font = "auto")
theme_set(theme_bw(base_size = 16))

# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "Spotify History Explorer",
  theme = bs_theme(
    bootswatch = "darkly", primary = "#1DB954"),
  sidebar = sidebar(
    selectInput("user", "Listener:",
                users),
    varSelectInput(
      "group_by", "Group by:", 
      df[c("track", "artist", "album")], 
      selected = "artist"
    ),
    sliderInput("year_range", "Years:",
                min = 2010, max = 2024, value = c(2010, 2024), sep = ""
    )
  ),
  card(
      full_screen = TRUE,
      card_header("Top 10"),
      plotOutput("top_10")
  ),
  card(
    full_screen = TRUE,
    card_header("Top 10 over time"),
    plotOutput("top_10_ridges")
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  filtered_data <- reactive({
    df |>
      filter(
        user == input$user,
        year(ts) >= input$year_range[1],
        year(ts) <= input$year_range[2]
      )|>
      summarise(
        plays = n(),
        .by = c(!!input$group_by)
      ) |>
      arrange(desc(plays)) |>
      top_n(10) 
  })
  
  filtered_data_ridges <- reactive({
    df |>
      filter(
        user == input$user,
        year(ts) >= input$year_range[1],
        year(ts) <= input$year_range[2]
      )|>
      inner_join(filtered_data(), by = join_by(!!input$group_by))  
})
  

  top_10_plot <- reactive({
    filtered_data() |>
    ggplot(aes(x = plays, y = fct_reorder(!!input$group_by, plays))) + 
    geom_col(fill = "#1DB954") +
    theme(legend.position = "none") +
    labs(
      y = str_to_title(input$group_by),
      x = "Plays"
    )
  })
  
  top_10_plot_ridges <- reactive({
    filtered_data_ridges() |>
      ggplot(
        aes(
          x = ts, 
          y = fct_reorder(!!input$group_by, plays), 
          fill = fct_reorder(!!input$group_by, plays),  
          colour = fct_reorder(!!input$group_by, plays)
        )
      ) +
      geom_density_ridges() +
      labs(
        y = str_to_title(input$group_by),
        x = NULL
      ) +
      scale_x_date(
        limits = c(min(filtered_data_ridges()$ts), 
                   max(filtered_data_ridges()$ts)
      )) +
      scale_y_discrete(expand = expand_scale(mult = c(0.01, .15))) +
      scale_fill_cyclical(values = c("#adb5bd", "#1DB954")) +
      scale_colour_cyclical(values = c("#adb5bd", "#1DB954"))
  })
  
  output$top_10 <- renderPlot(top_10_plot())
  output$top_10_ridges <- renderPlot(top_10_plot_ridges())
}


# Shiny App ---------------------------------------------------------------

shinyApp(ui, server)