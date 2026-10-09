library(DT)

x <- tribble(
~abbr, ~name,
'SO₂', 'Sulphur dioxide',
'NOₓ', 'Nitrogen oxides',
'NH₃', 'Ammonia',
'PM₂.₅', 'Primary particulate matter',
'NMVOCs', 'Non-methane volatile organic compounds'
)

paste(
  lapply(x$abbr,\(x) paste0('<h1>',x,'</h1>')),
  lapply(x$name,\(x) paste0('<h3>',x,'</h3>')),
collapse = '<br>') %>% HTML() %>% browsable()

sources <- matrix(ncol = 6,byrow = T,
                                              c("Sector",
                                              "PM₂.₅",
                                                "NOₓ",
                                                "SO₂",
                                                "NH₃",
                                              "NMVOC",
                                  "Energy industries",
                                               "3.3%",
                                              "20.4%",
                                              "37.3%",
                                               "0.1%",
                                               "0.5%",
          "Manufacturing industries and construction",
                                              "16.1%",
                                              "15.6%",
                                              "21.6%",
                                               "0.7%",
                                               "2.4%",
                               "Industrial processes",
                                              "12.9%",
                                               "0.1%",
                                               "4.8%",
                                               "1.3%",
                                              "54.1%",
  "Residential and small-scale commercial combustion",
                                              "43.1%",
                                              "10.3%",
                                              "25.5%",
                                               "0.8%",
                                               "6.2%",
                                        "Agriculture",
                                               "4.0%",
                                               "0.8%",
                                                "N/A",
                                              "87.6%",
                                              "14.4%",
                                     "Road transport",
                                              "12.4%",
                                              "33.6%",
                                               "0.7%",
                                               "1.5%",
                                               "3.9%",
                                 "Fugitive emissions",
                                               "1.1%",
                                               "0.2%",
                                               "1.4%",
                                               "0.1%",
                                              "15.8%",
                                 "Non-road transport",
                                               "3.6%",
                                              "16.8%",
                                               "8.3%",
                                                 "0%",
                                               "1.6%")
  ) %>% as.tibble() %>% set_names(c('Sector','PM₂.₅','NOₓ','SO₂','NH₃','NMVOC')) %>% 
  filter(row_number()!=1) %>% 
  DT::datatable(
    filter    = 'none',
    class     = 'table table-sm table-borderless',
    rownames  = FALSE,
    options   = list(
      ordering  = FALSE,
      searching = FALSE,
      paging    = FALSE,
      info      = FALSE,
      dom       = 't'
    )
  )


# ── PM₂.₅ source contributions ─────────────────────────────────────────────
# Icons: place 20×20 px PNGs in www/icons/ named as the keys below.
# e.g. www/icons/Energy.png, www/icons/Manufacturing.png, …

pm25_sources <- tibble(
  key    = c("Energy", "Manufacturing", "Industrial",
             "Residential", "Agriculture", "Road",
             "Fugitive", "NonRoad"),
  sector = c("Energy industries", "Manufacturing & construction",
             "Industrial processes", "Residential combustion",
             "Agriculture", "Road transport",
             "Fugitive emissions", "Non-road transport"),
  pm25   = c(3.3, 16.1, 12.9, 43.1, 4.0, 12.4, 1.1, 3.6)
) |>
  arrange(pm25)  # ascending so largest bar is at top after coord flip

# Build the rich-text 'rich' list: one entry per category key
icon_rich <- setNames(
  lapply(pm25_sources$key, \(k) list(
    height          = 24,
    width           = 24,
    align           = "right",
    backgroundColor = list(image = paste0("icons/", k, ".png"))
  )),
  pm25_sources$key
)

pm25_chart <- pm25_sources |>
  e_charts(key) |>
  e_bar(
    pm25,
    legend      = FALSE,
    barMaxWidth = 18,
    itemStyle   = list(
      color        = "#000000",
      borderRadius = c(0, 4, 4, 0)
    ),
    label = list(show = FALSE)
  ) |>
  e_flip_coords() |>
  e_y_axis(
    axisLine  = list(show = FALSE),
    axisTick  = list(show = FALSE),
    splitLine = list(show = FALSE),
    axisLabel = c(
      list(
        show     = TRUE,
        interval = 0,
        margin   = 20,
        # show only the image token — no text beneath it
        formatter = htmlwidgets::JS(
          "function(value){ return '{' + value + '| }'; }"
        )
      ),
      icon_rich   # named list of rich styles, one per key
    )
  ) |>
  e_x_axis(show = FALSE) |>
  e_legend(show = FALSE) |>
  e_grid(
    containLabel = TRUE,
    top    = 8,
    bottom = 8,
    left   = 8,
    right  = 24
  ) |>
  e_tooltip(
    trigger   = "item",
    formatter = htmlwidgets::JS(
      # look up the human-readable sector name from the bound data
      "function(p){ return p.name + '<br/><b>' + p.value + '%</b>'; }"
    )
  )

pm25_chart
