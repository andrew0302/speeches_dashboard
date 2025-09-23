library(tidyverse)
library(here)
library(knitr)
library(grid)
library(patchwork)
library(ggrepel)

# load and process data

source(here("src", "format_data.R"))

# images paths for presidents
img_files <- list.files("www/images/presidents", full.names = TRUE)

# create a lookup table from image paths
img_lookup <- tibble(
  filename = basename(img_files),
  pres_key = tools::file_path_sans_ext(filename),
  path = file.path("images/presidents", basename(img_files))) |> 
  mutate(pres_key = pres_key |> str_to_lower())

# merge into main df
df <- df %>%
  mutate(
    pres_key = president %>%
      str_to_lower() %>%
      str_replace_all("[^a-z]", "_") %>%  
      str_replace_all("_+", "_") %>%
      str_replace_all("^_|_$", "")) %>%
  left_join(img_lookup, by = "pres_key") %>%
  rename(president_img = path)  |>
  mutate(
    party_logo = case_when(
      president_party == "Democratic" ~ "images/logos/Democratic.png",
      president_party == "Republican" ~ "images/logos/Republican.png",
      TRUE ~ NA_character_))

rm(img_files, img_lookup)



ui <- fluidPage(

  
  sidebarLayout(
    
    # Sidebar: portrait, logo, selector
    sidebarPanel(
      selectInput("pres_choice", "Choose a President:", choices = sort(unique(df$president))),
      br(),
      uiOutput("president_img"),
      tags$div(
        style = "text-align:center; font-size: 11px; color: grey; margin-top: 5px;",
        "Portrait and Party Logo from Wikimedia Commons", "(CC-BY-SA/Public Domain)", tags$br(), 
        "Code licensed under the GNU GPL v3. ", tags$br(), 
        "Data from an academic study by Andrew M. Demetriou, licensed under ",
        tags$a(href="https://creativecommons.org/licenses/by/4.0/", "CC-BY 4.0")
      ),
      uiOutput("party_logo")
    ),
    
    # Main content
    mainPanel(
      h3("Average Rating by Political Leaning"),
      plotOutput("president_plot"),
      hr(),
      h3("Overall Results"),
      fluidRow(
        column(6, plotOutput("overview_plot")),
        column(6, plotOutput("mds_plot")))
      )
    )
)

server <- function(input, output, session) {
  
  output$president_img <- renderUI({
    pres_path <- df %>%
      filter(president == input$pres_choice) %>%
      pull(president_img) %>%
      unique()
    
    logo_path <- df %>%
      filter(president == input$pres_choice) %>%
      pull(party_logo) %>%
      unique()
    
    tags$div(
      style = "position: relative; text-align: center; display: inline-block;",
      tags$img(
        src = pres_path,
        height = "300px",
        style = "display: block; margin: auto;"
      ),
      tags$img(
        src = logo_path,
        height = "50px",
        style = "position: absolute; bottom: 10px; right: 10px;"
      ))
  })
  
  
  # Dynamic: faceted bar plot by participant leaning
  output$president_plot <- renderPlot({
    plot_president(df, input$pres_choice)  
  })
  
  plot_df <- df %>%
    group_by(president_party, value) |>
    summarise(
      mean_rating = mean(rating, na.rm = TRUE), 
      sd_rating   = sd(rating, na.rm = TRUE),
      n           = sum(!is.na(rating)),
      .groups = "drop") |>
    mutate(
      se_rating = sd_rating / sqrt(n),
      ci_lower  = mean_rating - 1.96 * se_rating,
      ci_upper  = mean_rating + 1.96 * se_rating)
  
  
  # establish order based on Republican party averages
  order_levels <- plot_df %>%
    filter(president_party == "Republican") %>%
    arrange(mean_rating) %>%
    pull(value)
  
  
  overview_plot <- plot_df |>
    mutate(value = factor(value, levels = order_levels)) %>%
    ggplot(aes(x = value, y = mean_rating, fill = president_party)) +
    geom_col() +
    facet_wrap(~president_party) +
    
    scale_fill_manual(
      values = c(
        "Democratic" = "#3182BD",
        "Republican" = "#DE2D26")) +
    
    scale_y_continuous(limits = c(0, 7)) +
    
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.2,
      position = position_dodge(width = 0.9)) +
    
    geom_text(
      aes(label = round(mean_rating, 2)),
      position = position_stack(vjust = 0.85),
      color = "white",
      size = 3) +
    
    theme_minimal() +
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5)) +
    
    coord_flip() +
    
    labs(
      title = "Party Average by Value \n with 95% Confidence Interval",
      x = NULL,  
      y = "Mean Rating (1–7 Likert Scale)")
  
  # Static: overview plot
  output$overview_plot <- renderPlot({
    overview_plot  # <- object defined earlier
  })
  
  president_profiles <- df %>%
    group_by(president, value) %>%
    summarise(mean_rating = mean(rating, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = value, values_from = mean_rating)
  
  value_matrix <- president_profiles %>% select(-president) %>% as.matrix()
  rownames(value_matrix) <- president_profiles$president
  dist_matrix <- dist(value_matrix, method = "euclidean")
  
  mds_fit <- cmdscale(dist_matrix, k = 2)   
  mds_coords <- as.data.frame(mds_fit) %>%
    rownames_to_column("president") %>%
    left_join(df %>% select(president, president_party) %>% distinct(), by = "president")
  
  mds_plot <- ggplot(mds_coords, aes(x = V1, y = V2, label = president, color = president_party)) +
    geom_point(size = 3) +
    geom_text_repel(aes(label = president), size = 3, max.overlaps = 20) +
    
    scale_color_manual(
      values = c(
        "Democratic" = "#3182BD",
        "Republican" = "#DE2D26")) +
    
    scale_x_continuous(limits = c(-2, 2)) + 
    scale_y_continuous(limits = c(-1, 1)) + 
    
    theme_void() +
    
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5)) +
    
    labs(
      title = "Multidimensional Scaling Map of \nPresidents' Value Profile \nby Euclidean Distance",
      x = "",
      y = "",
      color = "President's Party"
    )
  
  # Static: MDS plot
  output$mds_plot <- renderPlot({
    mds_plot  
  })
}

shinyApp(ui, server)

