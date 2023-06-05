
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(reactable)
library(reactablefmtr)
library(echarts4r)
library(mapboxer)
library(dplyr)
library(dtplyr)
library(gmailr)
library(openxlsx)
library(shinymanager)
library(shinyalert)

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



gm_auth_configure(path = "key.json")
gm_auth(email = T, cache = ".secret")

mail <- "eljorgehdz@gmail.com"

credentials <-
  tibble(
    user = c(
      "Invitado",
      "gloria",
      "roberto",
      "hector"
    ),
    password = "invitado01",
    admin = F,
    nombre = c(
      "Invitado",
      "Gloria Perez",
      "Roberto Luna",
      "Hector Pasos"
    ),
    puesto = c(
      "Usuario externo",
      "Representante comercial",
      "Interno de ventas",
      "Gerente de ventas MX"
    ),
    prof_pic = c(
      "https://static.vecteezy.com/system/resources/previews/019/896/008/original/male-user-avatar-icon-in-flat-design-style-person-signs-illustration-png.png",
      "https://dentalia.orionthemes.com/demo-1/wp-content/uploads/2016/10/dentalia-demo-deoctor-5-1-750x750.jpg",
      "https://www.qtrainers.com/upload/profile/160/2020/02/profile_35405e4683b309238.jpg",
      "https://huber.ghostpool.com/wp-content/uploads/avatars/3/596dfc2058143-bpfull.png"
    ))


set_labels(
  language = "es",
  "Please authenticate" = "Identifícate",
  "Username:" = "Usuario:",
  "Password:" = "Contraseña:"
)




