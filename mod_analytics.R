# ==============================================================================
# MODULE: ANALYTICS & REPORTS
# Metric picker (milk yield / fat / breed / sex / status) + herd distribution
# plot + individual-animal-vs-herd comparison plot.
# ==============================================================================

mod_analytics_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "container-fluid my-2",
    layout_columns(
      col_widths = c(6, 6),
      selectInput(ns("pheno_metric"), "Select Metric to Visualize:",
                  choices = c("Milk Yield" = "milk_yield",
                              "Fat %" = "fat",
                              "Breed Distribution" = "breed",
                              "Sex Distribution" = "sex",
                              "Status Distribution" = "status")),
      uiOutput(ns("animal_picker_ui"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Herd Phenotype Distribution"), plotlyOutput(ns("plot_pheno"))),
      card(card_header("Individual Animal Performance vs Herd"), plotlyOutput(ns("plot_ind")))
    )
  )
}

# records_data: reactive df of animals
mod_analytics_server <- function(id, records_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$animal_picker_ui <- renderUI({
      df <- records_data()
      if (nrow(df) == 0) return(p(class = "small text-muted", "No animals yet."))
      choices <- setNames(df$Animal_Code, paste0(df$Animal_Code, " - ", df$Animal_Name))
      selectInput(ns("analytics_animal"), "Select Animal to Compare:", choices = choices)
    })

    pheno_plot_data <- reactive({
      req(input$pheno_metric)
      df <- records_data()

      if (input$pheno_metric == "milk_yield") {
        tryCatch(
          dbGetQuery(db_pool, "
            SELECT
              a.animal_code AS \"Animal_Code\",
              m.morning_yield,
              m.evening_yield,
              (m.morning_yield + m.evening_yield) AS milk_yield,
              m.record_date
            FROM milk_yield_records m
            JOIN lactations l
              ON m.lactation_id = l.lactation_id
            JOIN animals a
              ON l.animal_id = a.animal_id
            WHERE m.status IS DISTINCT FROM 'Rejected'"),
          error = function(e) data.frame(
            Animal_Code = character(),
            morning_yield = numeric(),
            evening_yield = numeric(),
            milk_yield = numeric(),
            record_date = as.Date(character())
          )
        )
      } else if (input$pheno_metric == "fat") {
        tryCatch(
          dbGetQuery(db_pool, "
            SELECT a.animal_code AS \"Animal_Code\", p.value AS fat_value
            FROM phenotypes p
            JOIN traits t ON p.trait_id = t.trait_id
            JOIN animals a ON p.animal_id = a.animal_id
            WHERE t.trait_name ILIKE '%fat%'"),
          error = function(e) data.frame(Animal_Code = character(), fat_value = numeric())
        )
      } else {
        df  # breed / sex / status come straight from records_data()
      }
    })

    output$plot_pheno <- renderPlotly({
      metric <- input$pheno_metric
      d <- pheno_plot_data()

      if (nrow(d) == 0) {
        return(plotly_empty(type = "scatter", mode = "markers") %>%
                 layout(title = "No data available for this metric yet"))
      }

      if (metric == "milk_yield") {
        plot_ly(d, x = ~milk_yield, type = "histogram",
                marker = list(color = "#00A676")) %>%
          layout(title = "Milk Yield Distribution (Litres)",
                 xaxis = list(title = "Milk Yield (L)"), yaxis = list(title = "Count"))

      } else if (metric == "fat") {
        plot_ly(d, x = ~fat_value, type = "histogram",
                marker = list(color = "#FFB020")) %>%
          layout(title = "Fat % Distribution",
                 xaxis = list(title = "Fat %"), yaxis = list(title = "Count"))

      } else if (metric == "breed") {
        tab <- as.data.frame(table(d$Breed))
        plot_ly(tab, x = ~Var1, y = ~Freq, type = "bar",
                marker = list(color = "#2F8CFF")) %>%
          layout(title = "Breed Distribution", xaxis = list(title = "Breed"), yaxis = list(title = "Count"))

      } else if (metric == "sex") {
        tab <- as.data.frame(table(d$Sex))
        plot_ly(tab, x = ~Var1, y = ~Freq, type = "bar",
                marker = list(color = "#17C3B2")) %>%
          layout(title = "Sex Distribution", xaxis = list(title = "Sex"), yaxis = list(title = "Count"))

      } else if (metric == "status") {
        tab <- as.data.frame(table(d$Status))
        plot_ly(tab, x = ~Var1, y = ~Freq, type = "pie") %>%
          layout(title = "Record Status Distribution")
      }
    })

    output$plot_ind <- renderPlotly({
      req(input$analytics_animal)
      metric <- input$pheno_metric
      d <- pheno_plot_data()

      if (metric %in% c("breed", "sex", "status")) {
        return(plotly_empty(type = "scatter", mode = "markers") %>%
                 layout(title = "Select Milk Yield or Fat % to compare an individual animal"))
      }

      value_col <- if (metric == "milk_yield") "milk_yield" else "fat_value"
      if (!value_col %in% names(d) || nrow(d) == 0) {
        return(plotly_empty(type = "scatter", mode = "markers") %>%
                 layout(title = "No data available yet"))
      }

      herd_avg <- mean(d[[value_col]], na.rm = TRUE)
      animal_val <- d[[value_col]][d$Animal_Code == input$analytics_animal]
      animal_val <- if (length(animal_val) == 0) NA else mean(animal_val, na.rm = TRUE)

      comp <- data.frame(
        Category = c("Selected Animal", "Herd Average"),
        Value = c(animal_val, herd_avg)
      )

      plot_ly(comp, x = ~Category, y = ~Value, type = "bar",
              marker = list(color = c("#FF5A5F", "#00A676"))) %>%
        layout(title = paste0(input$analytics_animal, " vs Herd Average"),
               yaxis = list(title = if (metric == "milk_yield") "Milk Yield (L)" else "Fat %"))
    })
  })
}
