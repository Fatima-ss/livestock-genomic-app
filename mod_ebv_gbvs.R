# ==============================================================================
# MODULE: EBV / GEBV MANAGEMENT
# Lets Supervisor/Admin select specific animals, choose a genomic prediction
# engine (PBLUP / SSBLUP / GBLUP), run the pipeline against their existing
# phenotype + genotype + pedigree data, preview results, and save them back
# into animals.gebv_value / best_model / gebv_calculation_date.
#
# NOTE: "Run Engine" currently produces a PLACEHOLDER result. The real
# Docker/BLUPF90 pipeline is not yet exposed over an API (Plumber step is a
# later milestone). This module is being built UI-first so the flow — select,
# load data, run, preview, save — is fully wired and provably correct. Once
# the pipeline is exposed over HTTP, only the run_engine_placeholder()
# function below needs to be swapped for a real API call.
# ==============================================================================

mod_ebv_gebv_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("tab_ebv_gebv_ui"))
}

# records_data: reactive df of animals (shared, built in app.R)
# auth: shared reactiveValues (logged_in, role, full_name, ...)
# bump_refresh: function to invalidate records_data elsewhere
# log_activity: function to append to the dashboard activity feed
mod_ebv_gebv_server <- function(id, records_data, auth, bump_refresh, log_activity) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Role gate + main layout ---------------------------------------------
    output$tab_ebv_gebv_ui <- renderUI({
      req(auth$logged_in)
      if (!auth$role %in% c("supervisor", "admin")) {
        return(card(
          class = "border-danger my-3 text-center p-4",
          h4("Access Restricted", class = "text-danger fw-bold"),
          p("Only 'Farm Supervisor' or 'System Admin' can run EBV/GEBV predictions.")
        ))
      }

      layout_columns(
        col_widths = c(7, 5),

        card(
          card_header("Animal List — Select Animals to Run"),
          p(class = "small text-muted mb-2",
            "Select one or more animals below. Status shows whether each animal already has a GEBV/EBV value. Selecting an animal that already has a result will overwrite it with the engine you choose."),
          DTOutput(ns("animal_select_table"))
        ),

        div(
          card(
            card_header("Run Configuration"),
            uiOutput(ns("selection_summary_ui")),
            selectInput(ns("engine_choice"), "Prediction Engine:",
                        choices = c("PBLUP" = "PBLUP", "SSBLUP" = "SSBLUP", "GBLUP" = "GBLUP"),
                        selected = "SSBLUP"),
            p(class = "small text-muted",
              "PBLUP uses pedigree only. SSBLUP combines pedigree + genotypes (single-step). GBLUP uses genotypes only — selected animals without genotype records may fail this engine."),
            actionButton(ns("check_data_btn"), "1. Check Data Availability", class = "btn w-100 mb-2", style = "background:#2F8CFF; color:#fff;"),
            uiOutput(ns("data_check_ui")),
            actionButton(ns("run_engine_btn"), "2. Load Data & Run Engine", class = "btn w-100 mb-2", style = "background:#FF5A5F; color:#fff;"),
            uiOutput(ns("run_status_ui"))
          ),
          card(
            card_header("Preview Results (not yet saved)"),
            DTOutput(ns("results_preview_table")),
            uiOutput(ns("save_section_ui"))
          )
        )
      )
    })

    # ---- Animal selection table -----------------------------------------------
    output$animal_select_table <- renderDT({
      df <- records_data()
      if (nrow(df) == 0) {
        return(datatable(data.frame(Message = "No animals found."), rownames = FALSE))
      }
      view_df <- data.frame(
        Animal_Code = df$Animal_Code,
        Animal_Name = df$Animal_Name,
        Gender = df$Gender,
        GEBV_Status = ifelse(is.na(df$GEBV_Value), "Not Calculated", "Calculated"),
        Current_GEBV = round(df$GEBV_Value, 3),
        Current_Model = ifelse(is.na(df$Best_Model), "-", df$Best_Model),
        Last_Calculated = ifelse(is.na(df$GEBV_Calculation_Date), "-", as.character(df$GEBV_Calculation_Date)),
        stringsAsFactors = FALSE
      )
      datatable(
        view_df, rownames = FALSE, selection = "multiple",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    selected_animals <- reactive({
      df <- records_data()
      rows <- input$animal_select_table_rows_selected
      if (is.null(rows) || length(rows) == 0) return(df[0, , drop = FALSE])
      df[rows, , drop = FALSE]
    })

    output$selection_summary_ui <- renderUI({
      sel <- selected_animals()
      if (nrow(sel) == 0) {
        return(div(class = "alert alert-secondary py-1 small mb-2", "No animals selected yet — pick rows from the list on the left."))
      }
      div(class = "alert alert-info py-1 small mb-2",
          paste0(nrow(sel), " animal(s) selected: ", paste(sel$Animal_Code, collapse = ", ")))
    })

    # ---- Step 1: check phenotype/genotype/pedigree availability ---------------
    data_check_result <- reactiveVal(NULL)

    observeEvent(input$check_data_btn, {
      sel <- selected_animals()
      if (nrow(sel) == 0) {
        showNotification("Select at least one animal first.", type = "warning")
        return()
      }

      animal_ids <- sel$animal_id
      pheno_counts <- tryCatch({
        dbGetQuery(db_pool, "
          SELECT animal_id, COUNT(*) AS n_pheno
          FROM phenotypes WHERE animal_id = ANY($1) GROUP BY animal_id",
          params = list(animal_ids))
      }, error = function(e) data.frame(animal_id = integer(), n_pheno = integer()))

      geno_counts <- tryCatch({
        dbGetQuery(db_pool, "
          SELECT animal_id, COUNT(*) AS n_geno
          FROM genotypes WHERE animal_id = ANY($1) GROUP BY animal_id",
          params = list(animal_ids))
      }, error = function(e) data.frame(animal_id = integer(), n_geno = integer()))

      pop_counts <- tryCatch({
        dbGetQuery(db_pool, "
          SELECT animal_id FROM training_population WHERE animal_id = ANY($1)",
          params = list(animal_ids))
      }, error = function(e) data.frame(animal_id = integer()))

      check_df <- data.frame(
        Animal_Code = sel$Animal_Code,
        animal_id = sel$animal_id,
        Has_Pedigree = ifelse(!is.na(sel$Sire_ID) | !is.na(sel$Dam_ID), "Yes", "No"),
        Has_Phenotype = ifelse(sel$animal_id %in% pheno_counts$animal_id, "Yes", "No"),
        Has_Genotype = ifelse(sel$animal_id %in% geno_counts$animal_id, "Yes", "No"),
        In_Training_Population = ifelse(sel$animal_id %in% pop_counts$animal_id, "Yes", "No"),
        stringsAsFactors = FALSE
      )
      data_check_result(check_df)
    })

    output$data_check_ui <- renderUI({
      res <- data_check_result()
      if (is.null(res)) return(NULL)

      engine <- input$engine_choice
      warnings <- character(0)
      if (engine == "GBLUP" && any(res$Has_Genotype == "No")) {
        warnings <- c(warnings, paste0("Missing genotypes for: ",
                      paste(res$Animal_Code[res$Has_Genotype == "No"], collapse = ", "),
                      " — GBLUP requires genotype data for every selected animal."))
      }
      if (engine %in% c("PBLUP", "SSBLUP") && any(res$Has_Pedigree == "No")) {
        warnings <- c(warnings, paste0("Missing pedigree link for: ",
                      paste(res$Animal_Code[res$Has_Pedigree == "No"], collapse = ", ")))
      }

      tagList(
        renderTable(res[, setdiff(names(res), "animal_id")], striped = TRUE, spacing = "xs")(),
        if (length(warnings) > 0) {
          div(class = "alert alert-warning py-1 small mt-1", lapply(warnings, p, class = "mb-1"))
        } else {
          div(class = "alert alert-success py-1 small mt-1", "Data looks sufficient for the selected engine.")
        }
      )
    })

    # ---- Step 2: run engine (placeholder until Plumber API exists) -----------
    run_engine_placeholder <- function(animal_row, engine) {
      # PLACEHOLDER LOGIC ONLY — replace this function body with a real
      # Plumber API call once the Docker pipeline is exposed over HTTP.
      # Kept deterministic-ish (seeded by animal_id) so re-runs during UI
      # testing don't look random/confusing.
      set.seed(animal_row$animal_id)
      list(
        gebv_value = round(rnorm(1, mean = 0, sd = 1.2), 3),
        best_model = engine,
        gebv_calculation_date = Sys.Date()
      )
    }

    run_results <- reactiveVal(NULL)

    observeEvent(input$run_engine_btn, {
      sel <- selected_animals()
      if (nrow(sel) == 0) {
        showNotification("Select at least one animal first.", type = "warning")
        return()
      }

      engine <- input$engine_choice
      results_list <- lapply(seq_len(nrow(sel)), function(i) {
        r <- run_engine_placeholder(sel[i, ], engine)
        data.frame(
          animal_id = sel$animal_id[i],
          Animal_Code = sel$Animal_Code[i],
          Animal_Name = sel$Animal_Name[i],
          New_GEBV = r$gebv_value,
          Engine_Used = r$best_model,
          Calculated_On = as.character(r$gebv_calculation_date),
          stringsAsFactors = FALSE
        )
      })
      results_df <- do.call(rbind, results_list)
      run_results(results_df)

      log_activity(paste0(auth$full_name, " ran ", engine, " for ", nrow(sel), " animal(s) [placeholder engine]"))
      output$run_status_ui <- renderUI({
        div(class = "alert alert-success py-1 small mt-1",
            paste0(engine, " completed for ", nrow(sel), " animal(s). Review the preview, then save to DB.",
                   " (Placeholder result — real pipeline not yet connected.)"))
      })
    })

    output$results_preview_table <- renderDT({
      res <- run_results()
      if (is.null(res) || nrow(res) == 0) {
        return(datatable(data.frame(Message = "No results yet — run the engine first."), rownames = FALSE))
      }
      datatable(res[, setdiff(names(res), "animal_id")], rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE, dom = "t"))
    })

    # ---- Step 3: save results back to DB --------------------------------------
    output$save_section_ui <- renderUI({
      res <- run_results()
      if (is.null(res) || nrow(res) == 0) return(NULL)
      tagList(
        hr(),
        actionButton(ns("save_results_btn"), "3. Save Results to Database",
                     class = "btn w-100", style = "background:#00A676; color:#fff;"),
        uiOutput(ns("save_status_ui"))
      )
    })

    observeEvent(input$save_results_btn, {
      res <- run_results()
      req(res, nrow(res) > 0)

      saved <- 0
      failed <- 0
      for (i in seq_len(nrow(res))) {
        ok <- tryCatch({
          dbExecute(db_pool, "
            UPDATE animals
            SET gebv_value = $1, best_model = $2, gebv_calculation_date = $3, updated_at = CURRENT_TIME
            WHERE animal_id = $4",
            params = list(res$New_GEBV[i], res$Engine_Used[i], res$Calculated_On[i], res$animal_id[i]))
          TRUE
        }, error = function(e) FALSE)
        if (ok) saved <- saved + 1 else failed <- failed + 1
      }

      log_activity(paste0(auth$full_name, " saved GEBV results for ", saved, " animal(s)",
                           if (failed > 0) paste0(" (", failed, " failed)") else ""))

      output$save_status_ui <- renderUI({
        if (failed == 0) {
          div(class = "alert alert-success py-1 small mt-2", paste0(saved, " record(s) saved to the database."))
        } else {
          div(class = "alert alert-warning py-1 small mt-2",
              paste0(saved, " saved, ", failed, " failed — check database constraints."))
        }
      })

      run_results(NULL)
      data_check_result(NULL)
      bump_refresh()
    })
  })
}