
frontend <-
dashboardPage(
  title = "Shows de Netflix: analytics dashboard",
  #dark = ifelse(lubridate::hour(Sys.time())>18,T,F),
  header = dashboardHeader(
    skin = "dark",
    status = "navy",
    border = TRUE,
    compact = T,
    leftUi = tagList(
      dropdownMenu(
        badgeStatus = "danger",
        type = "messages",
        headerText = "A nivel general",
        messageItem(
          inputId = "triggerAction1",
          message = paste0("Producciones totales: ", nrow(data_netflix)),
          from = "Actual tracking",
          image = "https://loodibee.com/wp-content/uploads/Netflix-N-Symbol-logo.png",
          time = lubridate::today(),
          color = "lime"
        )
      ),
      dropdownMenu(
        badgeStatus = "info",
        type = "tasks",
        headerText = "% Películas con alta calif.",
        taskItem(
          inputId = "triggerAction3",
          text = "Mayor a 90%",
          color = "orange",
          value = round(sum(na.omit(data_netflix$imdb_score >= 8))/
                          length(na.omit(data_netflix$imdb_score))*100, digits = 1)
        )
      ),
      dropdownMenu(type = "notifications", icon = icon("envelope"),
                   headerText = "¿Quieres enviar estos datos por mail?",
                   column(align = "center", width = 12,
                          br(),
                          actionButton("send", "Enviar datos", icon = icon("paper-plane")),
                          br(),br()
                   )
      )
    ),
    rightUi = userOutput("user")
  ),
  sidebar = dashboardSidebar(
    sidebarUserPanel(
      image = "https://img.freepik.com/premium-vector/illustration-negative-film-reel-roll-tapes-movie-cinema-video-logo_468322-1105.jpg",
      name = "Datos al momento"
    ),
    sidebarMenu(
      id = "sidebarmenu",
      sidebarHeader("Menu"),
      menuItem(
        "Info general",
        tabName = "item1",
        icon = icon("youtube")
      ),
      menuItem(
        "Mejores shows",
        tabName = "item2",
        icon = icon("film")
      ),
      menuItem(
        "Mejores películas",
        tabName = "item3",
        icon = icon("video")
      )
    ),
    disable = FALSE,
    width = NULL,
    skin = "dark",
    status = "navy",
    elevation = 4,
    collapsed = T,
    minified = TRUE,
    expandOnHover = TRUE,
    fixed = TRUE,
    id = "sidebar",
    customArea = NULL
  ),
  controlbar = bs4DashControlbar(
    skin = "light",
    overlay = TRUE,
    width = 450,
    type = "pills",
    column(width = 12, align = "center",
           br(),
           p("Filters"),
           chooseSliderSkin("Flat"),
           downloadButton("download", "Descarga los datos", icon = icon("file-excel")),br(),br(),
           pickerInput("prod", "Tipo de producto", choices = unique(data_netflix$type), multiple = T, selected = unique(data_netflix$type)),
           pickerInput("genero", "Genero", choices = unique(data_netflix$Genero), multiple = T, selected = unique(data_netflix$Genero)),
           pickerInput("count", "Pais de origen de produccion", choices = unique(data_netflix$`Principal country`), multiple = T, selected = unique(data_netflix$`Principal country`)),
           sliderInput("calif", "Calificacion mínima", min = min(data_netflix$imdb_score, na.rm = T),
                       max = max(data_netflix$imdb_score, na.rm = T), value = min(data_netflix$imdb_score, na.rm = T)),
           dateRangeInput("fecha", "Periodo", start = min(data_netflix$date), end = max(data_netflix$date)
                          ))
  ),
  footer = dashboardFooter(
    left = a(
      href = "https://www.partrunner.com/en/",
      target = "_blank", "@PartRunner"
    ),
    right = lubridate::year(Sys.Date())
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "item1",
        fluidRow(
          box(
            title = "Shows totales y numero de votos por género",
            closable = F,
            maximizable = T,
            width = 8,
            status = "success",
            solidHeader = F,
            collapsible = TRUE,
            echarts4rOutput("grafico")
          ),
          column(
            width = 4,
            fluidRow( #infoBox(title = "Películas totales", value = scales::comma(length(data_netflix$id[data_netflix$type == "MOVIE"])), subtitle = "Desde 1999", width = 6, color = "navy", fill = T),
              #infoBox(title = "Shows totales", value = 2, subtitle = "Desde 1999", width = 6, color = "navy", fill = T),
              infoBox(title = "Calificación promedio", value = textOutput("cal_prom"), subtitle = textOutput("fecha1"), width = 6, color = "navy", fill = T),
              infoBox(title = "Votos promedio", value = textOutput("prom_votos"), subtitle = textOutput("fecha2"), width = 6, color = "navy", fill = T),
              box(width = 12,echarts4rOutput("step", height = 250), title = "Cuantil: distribución de calificación por %")),
            reactableOutput("tabla")
          )
        ),
        fluidRow(
          box(width = 7, mapboxerOutput("mapa"), title = "Distribución mundial del producciones de shows"),
          box(width = 5, echarts4rOutput("map"), title = "Géneros más atractivos"),
          box(width = 12, echarts4rOutput("trend"), title = "Tendencias de producción de shows a lo largo de los años")
        )
      ),
      tabItem(
        tabName = "item2",
        fluidRow(
          box(
            title = "Numero de shows por mejor calificación",
            closable = F,
            maximizable = T,
            width = 4,
            status = "success",
            solidHeader = F,
            collapsible = TRUE,
            echarts4rOutput("mej_shows", height = 900)
          ),
          box(width = 8, reactableOutput("main_show"), title = "Calificacion vs número de temporadas")
        )
      )
    )
  ),
  fullscreen = T,
  help = F,

  scrollToTop = T
)


secure_app(frontend,choose_language = FALSE,
           tags_top =
             tags$div(
               tags$img(
                 src = "https://1000logos.net/wp-content/uploads/2017/05/Netflix-Logo-2006.png", width = 100
               )
             ),
           theme = shinythemes::shinytheme("darkly"),
           background  = "linear-gradient(rgba(0, 0, 0, 0.5),
                    rgba(0, 0, 0, 0.5)),
                    url('https://cdn.pixabay.com/photo/2017/09/04/09/37/cinema-strip-2713352_1280.jpg')
                    no-repeat center fixed;
                    -webkit-background-size: cover;
                    -moz-background-size: cover;
                    -o-background-size: cover;
                    background-size: cover;")





