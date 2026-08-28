# ==============================================================================
# MODULE: ANALYTICS & REPORTS
# Herd distributions + individual animal vs herd comparison
# ==============================================================================

mod_analytics_ui <- function(id) {

  ns <- NS(id)

  div(
    class = "container-fluid my-2",

    # --------------------------------------------------------------------------
    # FILTERS
    # --------------------------------------------------------------------------
    layout_columns(
      col_widths = c(6, 6),

      selectInput(
        ns("pheno_metric"),
        "Select Metric to Visualize:",
        choices = c(
          "Milk Yield" = "milk_yield",
          "Fat %" = "fat",
          "Breed Distribution" = "breed",
          "Gender Distribution" = "gender",
          "Status Distribution" = "status"
        ),
        selected = "milk_yield"
      ),

      uiOutput(ns("animal_picker_ui"))
    ),

    # --------------------------------------------------------------------------
    # CHARTS
    # --------------------------------------------------------------------------
    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Herd Phenotype Distribution"),
        plotlyOutput(
          ns("plot_pheno"),
          height = "400px"
        )
      ),

      card(
        card_header("Individual Animal Performance vs Herd"),
        plotlyOutput(
          ns("plot_ind"),
          height = "400px"
        )
      )
    )
  )
}


# ==============================================================================
# SERVER
# ==============================================================================

mod_analytics_server <- function(id, records_data) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns


    # ==========================================================================
    # 1. ANIMAL PICKER
    # ==========================================================================

    output$animal_picker_ui <- renderUI({

      df <- records_data()

      if (is.null(df) || nrow(df) == 0) {

        return(
          p(
            class = "small text-muted",
            "No animals available."
          )
        )
      }

      choices <- setNames(
        df$Animal_Code,
        paste0(
          df$Animal_Code,
          " - ",
          df$Animal_Name
        )
      )

      selectInput(
        ns("analytics_animal"),
        "Select Animal to Compare:",
        choices = choices
      )
    })


    # ==========================================================================
    # 2. HERD DATA
    # ==========================================================================

    pheno_plot_data <- reactive({

      req(input$pheno_metric)

      df <- records_data()

      # ------------------------------------------------------------------------
      # MILK YIELD
      # ------------------------------------------------------------------------

      if (input$pheno_metric == "milk_yield") {

        tryCatch({

          dbGetQuery(
            db_pool,
            "
            SELECT
                a.animal_code AS \"Animal_Code\",
                a.animal_name AS \"Animal_Name\",
                m.morning_yield,
                m.evening_yield,
                (
                    COALESCE(m.morning_yield, 0)
                    +
                    COALESCE(m.evening_yield, 0)
                ) AS milk_yield,
                m.record_date
            FROM milk_yield_records m

            INNER JOIN lactation l
                ON m.lactation_id = l.lactation_id

            INNER JOIN animals a
                ON l.animal_id = a.animal_id

            WHERE m.status IS DISTINCT FROM 'Rejected'

            ORDER BY m.record_date
            "
          )

        }, error = function(e) {

          message(
            "Milk Yield query error: ",
            conditionMessage(e)
          )

          data.frame(
            Animal_Code = character(),
            Animal_Name = character(),
            morning_yield = numeric(),
            evening_yield = numeric(),
            milk_yield = numeric(),
            record_date = as.Date(character())
          )
        })


      # ------------------------------------------------------------------------
      # FAT %
      # ------------------------------------------------------------------------

      } else if (input$pheno_metric == "fat") {

        tryCatch({

          dbGetQuery(
            db_pool,
            "
            SELECT
                a.animal_code AS \"Animal_Code\",
                a.animal_name AS \"Animal_Name\",
                p.value AS fat_value,
                p.record_date

            FROM phenotypes p

            INNER JOIN traits t
                ON p.trait_id = t.trait_id

            INNER JOIN animals a
                ON p.animal_id = a.animal_id

            WHERE t.trait_id = 2
              AND p.status IS DISTINCT FROM 'Rejected'

            ORDER BY p.record_date
            "
          )

        }, error = function(e) {

          message(
            "Fat % query error: ",
            conditionMessage(e)
          )

          data.frame(
            Animal_Code = character(),
            Animal_Name = character(),
            fat_value = numeric(),
            record_date = as.Date(character())
          )
        })


      # ------------------------------------------------------------------------
      # BREED / GENDER / STATUS
      # ------------------------------------------------------------------------

      } else {

        df
      }
    })


    # ==========================================================================
    # 3. HERD DISTRIBUTION PLOT
    # ==========================================================================

    output$plot_pheno <- renderPlotly({

      metric <- input$pheno_metric

      d <- pheno_plot_data()

      if (is.null(d) || nrow(d) == 0) {

        return(
          plotly_empty(
            type = "scatter",
            mode = "markers"
          ) %>%
            layout(
              title = "No data available for this metric yet"
            )
        )
      }


      # ------------------------------------------------------------------------
      # MILK YIELD
      # ------------------------------------------------------------------------

      if (metric == "milk_yield") {

        plot_ly(
          d,
          x = ~milk_yield,
          type = "histogram"
        ) %>%
          layout(
            title = "Milk Yield Distribution",
            xaxis = list(
              title = "Daily Milk Yield (L)"
            ),
            yaxis = list(
              title = "Number of Records"
            )
          )


      # ------------------------------------------------------------------------
      # FAT %
      # ------------------------------------------------------------------------

      } else if (metric == "fat") {

        plot_ly(
          d,
          x = ~fat_value,
          type = "histogram"
        ) %>%
          layout(
            title = "Fat Percentage Distribution",
            xaxis = list(
              title = "Fat (%)"
            ),
            yaxis = list(
              title = "Number of Records"
            )
          )


      # ------------------------------------------------------------------------
      # BREED
      # ------------------------------------------------------------------------

      } else if (metric == "breed") {

        tab <- as.data.frame(
          table(
            d$Breed,
            useNA = "ifany"
          )
        )

        names(tab) <- c(
          "Breed",
          "Count"
        )

        plot_ly(
          tab,
          x = ~Breed,
          y = ~Count,
          type = "bar"
        ) %>%
          layout(
            title = "Breed Distribution",
            xaxis = list(
              title = "Breed"
            ),
            yaxis = list(
              title = "Number of Animals"
            )
          )


      # ------------------------------------------------------------------------
      # GENDER
      # ------------------------------------------------------------------------

      } else if (metric == "gender") {

        tab <- as.data.frame(
          table(
            d$Gender,
            useNA = "ifany"
          )
        )

        names(tab) <- c(
          "Gender",
          "Count"
        )

        plot_ly(
          tab,
          x = ~Gender,
          y = ~Count,
          type = "bar"
        ) %>%
          layout(
            title = "Gender Distribution",
            xaxis = list(
              title = "Gender"
            ),
            yaxis = list(
              title = "Number of Animals"
            )
          )


      # ------------------------------------------------------------------------
      # STATUS
      # ------------------------------------------------------------------------

      } else if (metric == "status") {

        tab <- as.data.frame(
          table(
            d$Status,
            useNA = "ifany"
          )
        )

        names(tab) <- c(
          "Status",
          "Count"
        )

        plot_ly(
          tab,
          labels = ~Status,
          values = ~Count,
          type = "pie"
        ) %>%
          layout(
            title = "Animal Status Distribution"
          )
      }
    })


    # ==========================================================================
    # 4. INDIVIDUAL ANIMAL VS HERD
    # ==========================================================================

    output$plot_ind <- renderPlotly({

      req(input$analytics_animal)

      metric <- input$pheno_metric

      d <- pheno_plot_data()


      # ------------------------------------------------------------------------
      # DISTRIBUTION-ONLY METRICS
      # ------------------------------------------------------------------------

      if (metric %in% c("breed", "gender", "status")) {

        return(
          plotly_empty(
            type = "scatter",
            mode = "markers"
          ) %>%
            layout(
              title =
                "Individual comparison is available for Milk Yield or Fat %"
            )
        )
      }


      if (is.null(d) || nrow(d) == 0) {

        return(
          plotly_empty(
            type = "scatter",
            mode = "markers"
          ) %>%
            layout(
              title = "No data available yet"
            )
        )
      }


      # ------------------------------------------------------------------------
      # SELECT VALUE COLUMN
      # ------------------------------------------------------------------------

      value_col <- if (
        metric == "milk_yield"
      ) {
        "milk_yield"
      } else {
        "fat_value"
      }


      if (!value_col %in% names(d)) {

        return(
          plotly_empty(
            type = "scatter",
            mode = "markers"
          ) %>%
            layout(
              title = "Required data column is unavailable"
            )
        )
      }


      # ------------------------------------------------------------------------
      # HERD AVERAGE
      # ------------------------------------------------------------------------

      herd_values <- d[[value_col]]

      herd_avg <- mean(
        herd_values,
        na.rm = TRUE
      )


      # ------------------------------------------------------------------------
      # SELECTED ANIMAL VALUE
      # ------------------------------------------------------------------------

      animal_values <- d[
        d$Animal_Code == input$analytics_animal,
        value_col
      ]

      animal_values <- animal_values[
        !is.na(animal_values)
      ]


      if (length(animal_values) == 0) {

        return(
          plotly_empty(
            type = "scatter",
            mode = "markers"
          ) %>%
            layout(
              title = paste(
                "No",
                if (metric == "milk_yield") {
                  "milk yield"
                } else {
                  "fat percentage"
                },
                "records available for",
                input$analytics_animal
              )
            )
        )
      }


      animal_avg <- mean(
        animal_values,
        na.rm = TRUE
      )


      # ------------------------------------------------------------------------
      # COMPARISON DATA
      # ------------------------------------------------------------------------

      comp <- data.frame(

        Category = c(
          "Selected Animal",
          "Herd Average"
        ),

        Value = c(
          animal_avg,
          herd_avg
        )
      )


      # ------------------------------------------------------------------------
      # COMPARISON CHART
      # ------------------------------------------------------------------------

      plot_ly(
        comp,
        x = ~Category,
        y = ~Value,
        type = "bar"
      ) %>%
        layout(
          title = paste0(
            input$analytics_animal,
            " vs Herd Average"
          ),
          xaxis = list(
            title = ""
          ),
          yaxis = list(
            title =
              if (metric == "milk_yield") {
                "Daily Milk Yield (L)"
              } else {
                "Fat (%)"
              }
          )
        )
    })
  })
}
