



function(input, output, session){

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  output$user <- renderUser({

    req(res_auth)

    dashboardUser(
      name = res_auth$nombre,
      image = res_auth$prof_pic,
      title = res_auth$puesto,
      subtitle = if_else(res_auth$nombre == "Invitado", "Invitado", "Colaborador")

      #footer = p("The footer", class = "text-center")
    )
  })


  observeEvent(input$send,{

    req(data())

    shinyalert(
      title = "¿Quieres enviar estos datos a tu correo?", type = "input",
      text = "¡Introdúcelo aquí!",
      closeOnEsc = FALSE,
      imageUrl = "https://1000logos.net/wp-content/uploads/2017/05/Netflix-Logo-2006.png",
      closeOnClickOutside = FALSE,
      showConfirmButton = TRUE,
      confirmButtonCol = "#99CEFF",
      callbackR = function(value) {

       withProgress(message = "Enviando correo",{

         incProgress(0.3)
         writexl::write_xlsx(data(), path = "datatabla.xlsx")
         incProgress(0.3)
         gm_mime() |>
           gm_to(value) |>
           gm_from(mail) |>
           gm_subject("Analytics dashboard: Netflix") |>
           gm_html_body(paste0(
             "<p> Buenas tardes ",value," </p>

    <p> Se te envía la información solicitada y filtrada del tablero actual. </p>
    <br>"
           )) |>
           gm_attach_file("datatabla.xlsx") |>
           gm_send_message()
         incProgress(0.3)
         shinyalert(paste("Correo enviado con éxito"),
                    imageUrl = "https://1000logos.net/wp-content/uploads/2017/05/Netflix-Logo-2006.png",
                    closeOnEsc = TRUE,
                    showConfirmButton = TRUE,
                    confirmButtonCol = "#99CEFF"
         )

       })


      }
    )
  })



  data <- reactive({
    data_netflix |>
      filter(type %in% input$prod & Genero %in% input$genero
             & `Principal country` %in% input$count & imdb_score >= input$calif
             & date >= input$fecha[1] & date <= input$fecha[2])
  })


  totales <- reactive({

    req(data())

    data() |>
      group_by(Genero) |>
      summarise(
        `Shows totales` = n(),
        `Votos promedio` = mean(imdb_votes, na.rm = T)
      ) |>
      filter(Genero != "" & is.na(Genero) == F) |>
      arrange(desc(`Shows totales`))

  })


  output$grafico <- renderEcharts4r({

    totales() |>
      e_charts(Genero, dispose = F) |>
      e_bar(`Shows totales`) |>
      e_bar(`Votos promedio`, y_index = 1) |>
      e_tooltip(trigger = "axis") |>
      e_theme("auritus")

  })

  quantile_data <- reactive({
    req(data())

    quantile(data()$imdb_score, na.rm = T, probs = seq(0, 1, 1/4)) |>
      as.data.frame() |>
      setNames("valor") |>
      tibble::rownames_to_column("porcentaje")

  })

  output$step <- renderEcharts4r({

    quantile_data() |>
      e_charts(porcentaje, dispose = F) |>
      e_bar(valor, name = "Cuantil") |>
      e_tooltip(trigger = "axis") |>
      e_theme("auritus")

  })

  radar_data <- reactive({

    req(data())

    data() |>
      group_by(Genero) |>
      summarise(total = sum(imdb_votes, na.rm = T)) |>
      mutate(total = round(total/sum(total)*350)) |>
      top_n(10, total)

  })

  output$map <- renderEcharts4r({

    radar_data() |>
      e_charts(Genero, dispose = F) |>
      e_radar(total) |>
      e_tooltip(trigger = "item", confine = T) |>
      e_theme("auritus")

  })

  map_data <- reactive({

    req(data())

    data() |>
      group_by(`Principal country`) |>
      summarise(
        producciones = n()
      ) |>
      mutate(producciones = producciones/sum(producciones)*80) |>
      filter(`Principal country` != "") |>
      left_join(isocodes |>
                  rename(
                    lat = `Latitude (average)`,
                    lng = `Longitude (average)`
                  ) |>
                  select(`Alpha-2 code`, lat, lng),
                by = c("Principal country" = "Alpha-2 code"))


  })


  output$mapa <- renderMapboxer({




    map_data() |>
      as_mapbox_source() |>
      mapboxer(
        style = if_else(input$dark_mode == F, "mapbox://styles/mapbox/light-v10","mapbox://styles/mapbox/dark-v11"),
        token = "pk.eyJ1IjoiYWFyb242NjYiLCJhIjoiY2o2M2NmZmJ4MWc0ZDJxbnI3dmZ5OXB2MSJ9.T3trJ5Tu66Kw-w-1ELpzBA"
      ) |>
      set_view_state( -105.133208, 21.4326077, 1,pitch = 20) |>
      add_circle_layer(
        circle_opacity = 0.4,
        circle_color = "red",
        circle_radius = c("get", "producciones")
      )

  })

  trend_data <- reactive({

    req(data())

    data() |>
      group_by(date) |>
      summarise(n = n(),
                calif = mean(imdb_score, na.rm = T))

  })

  output$trend <- renderEcharts4r({

    trend_data() |>
      e_charts(date, dispose = F) |>
      e_area(n, symbol = "none") |>
      e_tooltip(trigger = "axis", confine = T) |>
      e_line(calif, y_index = 1, symbol = "none") |>
      e_theme("auritus")

  })

  mej_shows_data <- reactive({

    mej_shows |>
      group_by(MAIN_GENRE) |>
      summarise(
        n = n(),
        calif = mean(NUMBER_OF_VOTES, na.rm = T)) |>
      arrange(n)

  })


  output$mej_shows <- renderEcharts4r({

    mej_shows_data() |>
      e_charts(MAIN_GENRE) |>
      e_bar(n) |>
      #e_bar(calif, y_index = 1) |>
      e_flip_coords() |>
      e_tooltip(trigger = "axis", confine = T) |>
      e_theme("auritus")


  })


  output$main_show <- renderReactable({

    mej_shows |>
      select(TITLE, NUMBER_OF_SEASONS, SCORE) |>
      na.omit() |>
      reactable(
        theme = reactablefmtr::nytimes(background_color = "transparent"),
        columns = list(
          SCORE = colDef(name = "Calificacion",
                         cell = data_bars(
                           data = mej_shows,
                           fill_color = '#226ab2',
                           background = '#FFFFFF',
                           bar_height = 7,
                           #number_fmt = scales::percent,
                           text_position = 'outside-end',
                           max_value = 13,
                           #icon = 'circle',
                           fill_opacity = 1,
                           icon_color = '#226ab2',
                           icon_size = 15,
                           text_color = '#226ab2',
                           round_edges = TRUE
                         )

          ),
          NUMBER_OF_SEASONS = colDef(name = "Temporadas",
                                     cell = data_bars(
                                       data = mej_shows,
                                       fill_color = 'red',
                                       background = '#FFFFFF',
                                       bar_height = 7,
                                       #number_fmt = scales::percent,
                                       text_position = 'outside-end',
                                       max_value = 13,
                                       #icon = 'circle',
                                       fill_opacity = 1,
                                       icon_color = '#226ab2',
                                       icon_size = 15,
                                       text_color = 'red',
                                       round_edges = TRUE
                                     )

          )
        )
      )

  })


  output$cal_prom <- renderText({

    round(mean(data()$imdb_score[], na.rm = T), digits = 1)

  })

  output$prom_votos <- renderText({

    scales::comma(round(mean(data()$imdb_votes[], na.rm = T), digits = 1))

  })


  output$fecha1 <- renderText({

      paste0("Desde ", min(data()$date))

  })

  output$fecha2 <- renderText({

    paste0("Desde ", min(data()$date))

  })


  output$download <- downloadHandler(
    filename = function() {
      paste0("data peliculas",lubridate::today(), ".xlsx")
    },
    content = function(file) {
      withProgress(message = "Generando documento",{

        incProgress(1/16)
        wb = createWorkbook()
        incProgress(1/16)
        addWorksheet(wb, sheetName = "Data general",gridLines = F,
                     tabColour = RColorBrewer::brewer.pal(12,"Set3")[1],
                     zoom = 120)
        incProgress(1/16)
        writeData(wb, sheet = "Data general", x = data(), withFilter = T,
                  borderStyle = "medium",
                  borderColour = "#000000",
                  borders = "columns",
                  headerStyle = createStyle(
                    fontColour = "#ffffff", fgFill = "#4F80BD",
                    halign = "center", valign = "center", textDecoration = "bold",
                    border = "TopBottomLeftRight"
                  ))
        incProgress(1/16)
        addWorksheet(wb, sheetName = "Total vs votos",gridLines = F,
                     tabColour = RColorBrewer::brewer.pal(12,"Set3")[2],
                     zoom = 120)
        incProgress(1/16)
        writeData(wb, sheet = "Total vs votos", x = totales(), withFilter = T,
                  borderStyle = "medium",
                  borderColour = "#000000",
                  borders = "columns",
                  headerStyle = createStyle(
                    fontColour = "#ffffff", fgFill = "#4F80BD",
                    halign = "center", valign = "center", textDecoration = "bold",
                    border = "TopBottomLeftRight"
                  ))
        incProgress(1/16)
        addWorksheet(wb, sheetName = "Mapa datos",gridLines = F,
                     tabColour = RColorBrewer::brewer.pal(12,"Set3")[3],
                     zoom = 120)
        incProgress(1/16)
        writeData(wb, sheet = "Mapa datos", x = map_data(), withFilter = T,
                  borderStyle = "medium",
                  borderColour = "#000000",
                  borders = "columns",
                  headerStyle = createStyle(
                    fontColour = "#ffffff", fgFill = "#4F80BD",
                    halign = "center", valign = "center", textDecoration = "bold",
                    border = "TopBottomLeftRight"
                  ))
        incProgress(1/16)
        addWorksheet(wb, sheetName = "Radar datos",gridLines = F,
                     tabColour = RColorBrewer::brewer.pal(12,"Set3")[4],
                     zoom = 120)
        incProgress(1/16)
        writeData(wb, sheet = "Radar datos", x = radar_data(), withFilter = T,
                  borderStyle = "medium",
                  borderColour = "#000000",
                  borders = "columns",
                  headerStyle = createStyle(
                    fontColour = "#ffffff", fgFill = "#4F80BD",
                    halign = "center", valign = "center", textDecoration = "bold",
                    border = "TopBottomLeftRight"
                  ))
        incProgress(1/16)
        addWorksheet(wb, sheetName = "Tendencias",gridLines = F,
                     tabColour = RColorBrewer::brewer.pal(12,"Set3")[5],
                     zoom = 120)
        incProgress(1/16)
        writeData(wb, sheet = "Tendencias", x = trend_data(), withFilter = T,
                  borderStyle = "medium",
                  borderColour = "#000000",
                  borders = "columns",
                  headerStyle = createStyle(
                    fontColour = "#ffffff", fgFill = "#4F80BD",
                    halign = "center", valign = "center", textDecoration = "bold",
                    border = "TopBottomLeftRight"
                  ))
        incProgress(1/16)
        addWorksheet(wb, sheetName = "Mejores shows",gridLines = F,
                     tabColour = RColorBrewer::brewer.pal(12,"Set3")[6],
                     zoom = 120)
        incProgress(1/16)
        writeData(wb, sheet = "Mejores shows", x = mej_shows_data(), withFilter = T,
                  borderStyle = "medium",
                  borderColour = "#000000",
                  borders = "columns",
                  headerStyle = createStyle(
                    fontColour = "#ffffff", fgFill = "#4F80BD",
                    halign = "center", valign = "center", textDecoration = "bold",
                    border = "TopBottomLeftRight"
                  ))
        incProgress(1/16)
        addWorksheet(wb, sheetName = "Quantile",gridLines = F,
                     tabColour = RColorBrewer::brewer.pal(12,"Set3")[7],
                     zoom = 120)
        incProgress(1/16)
        writeData(wb, sheet = "Quantile", x = quantile_data(), withFilter = T,
                  borderStyle = "medium",
                  borderColour = "#000000",
                  borders = "columns",
                  headerStyle = createStyle(
                    fontColour = "#ffffff", fgFill = "#4F80BD",
                    halign = "center", valign = "center", textDecoration = "bold",
                    border = "TopBottomLeftRight"
                  ))
        incProgress(1/16)
        saveWorkbook(wb,file)

      })
    }
  )



}
