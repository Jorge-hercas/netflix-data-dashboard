



function(input, output, session){


  data <- reactive({
    data_netflix |>
      filter(type %in% input$prod & Genero %in% input$genero
             & `Principal country` %in% input$count & imdb_score >= input$calif
             & date >= input$fecha[1] & date <= input$fecha[2])
  })



  output$grafico <- renderEcharts4r({

    data() |>
      group_by(Genero) |>
      summarise(
        `Shows totales` = n(),
        `Votos promedio` = mean(imdb_votes, na.rm = T)
      ) |>
      filter(Genero != "" & is.na(Genero) == F) |>
      arrange(desc(`Shows totales`)) |>
      e_charts(Genero, dispose = F) |>
      e_bar(`Shows totales`) |>
      e_bar(`Votos promedio`, y_index = 1) |>
      e_tooltip(trigger = "axis") |>
      e_theme("auritus")

  })


  output$step <- renderEcharts4r({

    quantile(data()$imdb_score, na.rm = T, probs = seq(0, 1, 1/4)) |>
      as.data.frame() |>
      setNames("valor") |>
      tibble::rownames_to_column("porcentaje") |>
      e_charts(porcentaje, dispose = F) |>
      e_bar(valor, name = "Cuantil") |>
      e_tooltip(trigger = "axis") |>
      e_theme("auritus")

  })

  output$map <- renderEcharts4r({

    data() |>
      group_by(Genero) |>
      summarise(total = sum(imdb_votes, na.rm = T)) |>
      mutate(total = round(total/sum(total)*350)) |>
      top_n(10, total) |>
      e_charts(Genero, dispose = F) |>
      e_radar(total) |>
      e_tooltip(trigger = "item", confine = T) |>
      e_theme("auritus")

  })


  output$mapa <- renderMapboxer({

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
                by = c("Principal country" = "Alpha-2 code")) |>
      as_mapbox_source() |>
      mapboxer(
        style = "mapbox://styles/mapbox/light-v10",
        token = "pk.eyJ1IjoiYWFyb242NjYiLCJhIjoiY2o2M2NmZmJ4MWc0ZDJxbnI3dmZ5OXB2MSJ9.T3trJ5Tu66Kw-w-1ELpzBA"
      ) |>
      set_view_state( -105.133208, 21.4326077, 1,pitch = 20) |>
      add_circle_layer(
        circle_opacity = 0.4,
        circle_color = "red",
        circle_radius = c("get", "producciones")
      )

  })

  output$trend <- renderEcharts4r({

    data() |>
      group_by(date) |>
      summarise(n = n(),
                calif = mean(imdb_score, na.rm = T)) |>
      e_charts(date, dispose = F) |>
      e_area(n, symbol = "none") |>
      e_tooltip(trigger = "axis", confine = T) |>
      e_line(calif, y_index = 1, symbol = "none") |>
      e_theme("auritus")

  })


  output$mej_shows <- renderEcharts4r({

    mej_shows |>
      group_by(MAIN_GENRE) |>
      summarise(
        n = n(),
        calif = mean(NUMBER_OF_VOTES, na.rm = T)
      ) |>
      arrange(n) |>
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
      #mutate(SCORE = SCORE/10)
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

}
