
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(reactable)
library(reactablefmtr)
library(echarts4r)
library(mapboxer)
library(dplyr)
library(dtplyr)


isocodes <- vroom::vroom("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv")
data_netflix <- vroom::vroom("data/data_netflix.csv")
actores <- vroom::vroom("data/Actores.csv")
mej_pelis <- vroom::vroom("data/mejores peliculas Netflix.csv")
mej_shows <- vroom::vroom("data/mejores Shows Netflix.csv")


data_netflix <- data_netflix |>
  mutate(genres = stringr::str_replace_all(genres, "\\[|\\]|'", ""),
         production_countries = stringr::str_replace_all(production_countries, "\\[|\\]|'", ""),
         date = lubridate::make_date(release_year, 1,1)) |>
  tidyr::separate(genres, into = c("Genero"), sep = ", ") |>
  tidyr::separate(production_countries, into = c("Principal country"), sep = ", ")




