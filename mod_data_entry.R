# ==============================================================================
# MODULE: DATA ENTRY CONSOLE
# 1. Animal Registration (existing records table + single + bulk CSV)
# 2. Phenotype Entry (existing records table + add form)
# 3. Population Management (existing records table + training/seed import)
# 4. Genotype File Ingestion (existing records table + VCF / PLINK)
# ==============================================================================

mod_data_entry_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("tab_data_entry_ui"))
}

# records_data: reactive df of animals
# auth: shared reactiveValues
# bump_refresh, log_activity: shared functions
# refresh_trigger: reactiveVal used to force choice-lists to recompute
mod_data_entry_server <- function(id, records_data, auth, bump_refresh, log_activity, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Lookup choice lists -------------------------------------------------
    species_choices <- reactive({
      tryCatch({
        df <- dbGetQuery(db_pool, "SELECT species_id, species_name FROM species ORDER BY species_name")
        setNames(df$species_id, df$species_name)
      }, error = function(e) c())
    })

    breed_choices <- reactive({
      tryCatch({
        df <- dbGetQuery(db_pool, "SELECT breed_id, breed_name FROM breeds ORDER BY breed_name")
        choices <- setNames(df$breed_id, df$breed_name)
        if (!any(grepl("buffalo", names(choices), ignore.case = TRUE))) {
          choices <- c(choices, setNames("buffalo_placeholder", "Buffalo"))
        }
        choices
      }, error = function(e) c())
    })

    observeEvent(input$species_id, {
      req(input$species_id)
      df <- tryCatch(
        dbGetQuery(db_pool, "SELECT breed_id, breed_name FROM breeds WHERE species_id = $1 ORDER BY breed_name", params = list(as.integer(input$species_id))),
        error = function(e) data.frame()
      )
      updateSelectInput(session, "breed_id", choices = setNames(df$breed_id, df$breed_name))
    }, ignoreInit = FALSE)

    sire_choices <- reactive({
      refresh_trigger()
      tryCatch({
        df <- dbGetQuery(db_pool, "
          SELECT animal_code, animal_name FROM animals
          WHERE gender = 'Male' AND animal_code IS NOT NULL ORDER BY animal_code")
        if (nrow(df) == 0) return(c("None available" = ""))
        c("None (optional)" = "", setNames(df$animal_code, paste0(df$animal_code, " - ", df$animal_name)))
      }, error = function(e) c("None available" = ""))
    })

    dam_choices <- reactive({
      refresh_trigger()
      tryCatch({
        df <- dbGetQuery(db_pool, "
          SELECT animal_code, animal_name FROM animals
          WHERE gender = 'Female' AND animal_code IS NOT NULL ORDER BY animal_code")
        if (nrow(df) == 0) return(c("None available" = ""))
        c("None (optional)" = "", setNames(df$animal_code, paste0(df$animal_code, " - ", df$animal_name)))
      }, error = function(e) c("None available" = ""))
    })

    trait_choices <- reactive({
      tryCatch({
        df <- dbGetQuery(db_pool, "SELECT trait_id, trait_name FROM traits ORDER BY trait_name")
        setNames(df$trait_id, df$trait_name)
      }, error = function(e) c())
    })

    next_passport_id_rv <- reactiveVal(NULL)
    observeEvent(list(refresh_trigger()), {
      next_passport_id_rv(generate_next_passport_id())
    }, ignoreNULL = FALSE)

    output$next_passport_id_out <- renderText({
      req(next_passport_id_rv())
      next_passport_id_rv()
    })

    # ---- Existing-records queries (drive the tables shown above each form) --
    existing_animals <- reactive({
      refresh_trigger()
      tryCatch({
        dbGetQuery(db_pool, "
          SELECT a.animal_code AS \"Animal Code\",
                 a.animal_name AS \"Name\",
                 COALESCE(b.breed_name, '') AS \"Breed\",
                 a.gender AS \"Gender\",
                 a.date_of_birth AS \"DOB\",
                 COALESCE(a.herd_id::text, '') AS \"Herd\",
                 a.status AS \"Status\"
          FROM animals a
          LEFT JOIN breeds b ON b.breed_id = a.breed_id
          ORDER BY a.created_at DESC")
      }, error = function(e) data.frame())
    })

    existing_phenotypes <- reactive({
      refresh_trigger()
      tryCatch({
        dbGetQuery(db_pool, "
          SELECT a.animal_code AS \"Animal\",
                 t.trait_name AS \"Trait\",
                 p.value AS \"Value\",
                 p.record_date AS \"Date\",
                 CASE WHEN p.is_milk_trait THEN 'Yes' ELSE 'No' END AS \"Milk\",
                 p.status AS \"Status\"
          FROM phenotypes p
          JOIN animals a ON a.animal_id = p.animal_id
          JOIN traits t ON t.trait_id = p.trait_id
          ORDER BY p.record_date DESC")
      }, error = function(e) data.frame())
    })

    existing_population <- reactive({
      refresh_trigger()
      tryCatch({
        dbGetQuery(db_pool, "
          SELECT a.animal_code AS \"Animal\",
                 a.animal_name AS \"Name\",
                 COALESCE(tp.herd_id::text, '') AS \"Herd\",
                 COALESCE(tp.farm_id::text, '') AS \"Farm\",
                 tp.status AS \"Status\"
          FROM training_population tp
          JOIN animals a ON a.animal_id = tp.animal_id
          ORDER BY tp.training_population_id DESC")
      }, error = function(e) data.frame())
    })

    existing_genotypes <- reactive({
      refresh_trigger()
      tryCatch({
        dbGetQuery(db_pool, "
          SELECT a.animal_code AS \"Animal\",
                 COALESCE(g.platform, '') AS \"Platform\",
                 COALESCE(g.chip_name, '') AS \"Chip\",
                 COALESCE(g.marker_count::text, '') AS \"Markers\",
                 COALESCE(g.format, '') AS \"Format\",
                 g.status AS \"Status\"
          FROM genotypes g
          JOIN animals a ON a.animal_id = g.animal_id
          ORDER BY g.genotype_id DESC")
      }, error = function(e) data.frame())
    })

    output$existing_animals_table <- DT::renderDT({
      DT::datatable(existing_animals(), options = list(pageLength = 5, scrollX = TRUE),
                     rownames = FALSE, filter = "top")
    })

    output$existing_phenotypes_table <- DT::renderDT({
      DT::datatable(existing_phenotypes(), options = list(pageLength = 5, scrollX = TRUE),
                     rownames = FALSE, filter = "top")
    })

    output$existing_population_table <- DT::renderDT({
      DT::datatable(existing_population(), options = list(pageLength = 5, scrollX = TRUE),
                     rownames = FALSE, filter = "top")
    })

    output$existing_genotypes_table <- DT::renderDT({
      DT::datatable(existing_genotypes(), options = list(pageLength = 5, scrollX = TRUE),
                     rownames = FALSE, filter = "top")
    })

    # ---- Main tab UI (role-gated) --------------------------------------------
    output$tab_data_entry_ui <- renderUI({
      req(auth$logged_in)
      if (!auth$role %in% c("operator", "supervisor", "admin")) {
        return(card(
          class = "border-danger my-3 text-center p-4",
          h4("Access Restricted", class = "text-danger fw-bold"),
          p("You need 'Data Entry Operator', 'Farm Supervisor', or 'System Admin' privileges to register records.")
        ))
      }

      tagList(
        tags$style(HTML("
          .data-entry-tabs .nav-link { font-weight:600; color:#5f6b6f; border:none; border-bottom:2px solid transparent; padding:10px 16px; }
          .data-entry-tabs .nav-link.active { color:#008a63 !important; background:transparent !important; border-bottom:2px solid #00A676; }
          .data-entry-tabs .nav-link:hover { color:#008a63; }
          .data-entry-panel { padding: 18px 4px 4px; }
          .data-entry-section-heading { font-weight:700; font-size:0.85rem; color:#008a63; text-transform:uppercase; letter-spacing:.03em; margin:4px 0 10px; }
        ")),
        div(
          class = "data-entry-tabs",
          navset_tab(
            id = ns("data_entry_tabs"),

            nav_panel(
              title = tagList(icon("paw"), "Animals"),
              div(
                class = "data-entry-panel",

                div(class = "data-entry-section-heading", "Existing Animal Records"),
                DT::DTOutput(ns("existing_animals_table")),
                hr(),

                div(class = "data-entry-section-heading", "Register New Animal"),
                layout_columns(
                  col_widths = c(6, 6),
                  div(
                    h5("Individual Registration", class = "fw-bold", style = "color:#00A676;"),
                    p(class = "small text-muted", "Animal Code is generated automatically from animals.animal_code. Required database fields are collected here; Sire and Dam are linked through existing animal codes."),
                    textInput(ns("animal_name"), "Animal Name:", value = ""),
                    selectInput(ns("species_id"), "Species:", choices = species_choices()),
                    selectInput(ns("breed_id"), "Breed:", choices = breed_choices()),
                    selectInput(ns("gender"), "Gender:", choices = c("Female", "Male")),
                    dateInput(ns("dob"), "Date of Birth:", value = Sys.Date()),
                    numericInput(ns("first_lactation_age"), "First Lactation Age:", value = NA, min = 0),
                    textInput(ns("ear_tag_no"), "Ear Tag No.:", value = ""),
                    textInput(ns("microchip_no"), "Microchip No.:", value = ""),
                    textInput(ns("birth_place"), "Birth Place:", value = ""),
                    numericInput(ns("herd_id"), "Herd ID:", value = NA, min = 1, step = 1),
                    selectInput(ns("sire_passport_id"), "Sire (animal_code, optional):", choices = sire_choices()),
                    selectInput(ns("dam_passport_id"), "Dam (animal_code, optional):", choices = dam_choices()),
                    actionButton(ns("save_btn"), "Submit Record for Validation", class = "btn w-100 my-2", style = "background:#00A676; color:#fff;"),
                    uiOutput(ns("save_msg"))
                  ),
                  div(
                    h5("Bulk Animal Registration", class = "fw-bold", style = "color:#2F8CFF;"),
                    fileInput(ns("bulk_file"), "Upload Bulk CSV Dataset:", accept = ".csv"),
                    actionButton(ns("upload_btn"), "Process Bulk Import", class = "btn w-100 mb-3", style = "background:#2F8CFF; color:#fff;"),
                    uiOutput(ns("bulk_msg")),
                    hr(),
                    h6("CSV columns supported:"),
                    tags$ul(class = "small text-muted",
                      tags$li("animal_code (optional; generated automatically if blank)"),
                      tags$li("animal_name, species_id, breed_id, gender, date_of_birth"),
                      tags$li("first_lactation_age, ear_tag_no, microchip_no, birth_place, herd_id"),
                      tags$li("sire_animal_code and dam_animal_code are optional links to existing animals"),
                      tags$li("status is controlled by the application and starts as Pending Validation")
                    )
                  )
                )
              )
            ),

            nav_panel(
              title = tagList(icon("clipboard-list"), "Phenotypes"),
              div(
                class = "data-entry-panel",

                div(class = "data-entry-section-heading", "Existing Phenotype Records"),
                DT::DTOutput(ns("existing_phenotypes_table")),
                hr(),

                div(class = "data-entry-section-heading", "Add Phenotype Record"),
                p(class = "small text-muted", "Save one phenotype record using the exact phenotypes table fields: animal_id, trait_id, value, record_date, remarks, is_milk_trait, status, and optional lactation_id."),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  uiOutput(ns("pheno_animal_picker_ui")),
                  uiOutput(ns("pheno_trait_picker_ui")),
                  numericInput(ns("pheno_value"), "Value:", value = NA)
                ),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  dateInput(ns("pheno_record_date"), "Record Date:", value = Sys.Date()),
                  uiOutput(ns("pheno_lactation_picker_ui")),
                  checkboxInput(ns("pheno_is_milk_trait"), "Is Milk Trait?", value = FALSE)
                ),
                textAreaInput(ns("pheno_remarks"), "Remarks:", value = "", rows = 2),
                actionButton(ns("save_pheno_btn"), "Save Phenotype Record", class = "btn w-100 my-2", style = "background:#17C3B2; color:#fff;"),
                uiOutput(ns("pheno_save_msg"))
              )
            ),

            nav_panel(
              title = tagList(icon("users"), "Population"),
              div(
                class = "data-entry-panel",

                div(class = "data-entry-section-heading", "Training / Seed Population"),
                DT::DTOutput(ns("existing_population_table")),
                hr(),

                div(class = "data-entry-section-heading", "Import Training Population"),
                p(class = "small text-muted", "Add existing animals to training_population. This section does not create a second animal record; it links existing animals through animal_id and stores the optional herd_id/farm_id and status."),
                fileInput(ns("seed_pop_file"), "Upload Training / Seed Population CSV:", accept = ".csv"),
                actionButton(ns("upload_seed_btn"), "Import Training Population", class = "btn w-100 mb-2", style = "background:#2F8CFF; color:#fff;"),
                uiOutput(ns("seed_pop_msg")),
                tags$p(class = "small text-muted mb-0", "CSV: animal_code is required; herd_id and farm_id are optional; status defaults to Active.")
              )
            ),

            nav_panel(
              title = tagList(icon("dna"), "Genotypes"),
              div(
                class = "data-entry-panel",

                div(class = "data-entry-section-heading", "Existing Genotype Records"),
                DT::DTOutput(ns("existing_genotypes_table")),
                hr(),

                div(class = "data-entry-section-heading", "Add Genotype Information"),
                p(class = "small text-muted", "Store genotype-file metadata using the exact genotypes table fields. The database schema does not contain raw_data or uploaded_at, so those old fields are not used."),
                selectInput(ns("genotype_animal"), "Animal Code:", choices = NULL),
                layout_columns(
                  col_widths = c(4, 4, 4),
                  textInput(ns("genotype_platform"), "Platform:", value = ""),
                  textInput(ns("genotype_chip_name"), "Chip Name:", value = ""),
                  numericInput(ns("genotype_marker_count"), "Marker Count:", value = NA, min = 0)
                ),
                textAreaInput(ns("genotype_remarks"), "Remarks:", value = "", rows = 2),
                fileInput(ns("genotype_file"), "Upload Genotype File (VCF / PLINK):", accept = c(".vcf", ".ped", ".map")),
                actionButton(ns("preview_genotype_btn"), "Preview 3-Record Sample", class = "btn w-100 mb-2", style = "background:#FFB020; color:#1A1A1A;"),
                uiOutput(ns("genotype_preview_ui")),
                actionButton(ns("commit_genotype_btn"), "Save Genotype Metadata", class = "btn w-100 mt-2", style = "background:#00A676; color:#fff;"),
                uiOutput(ns("genotype_commit_msg"))
              )
            )
          )
        )
      )
    })

    # ---- 1. ANIMAL REGISTRATION ----------------------------------------------
    observeEvent(input$save_btn, {
      req(auth$logged_in, input$animal_name, input$species_id, input$breed_id,
          input$gender, input$dob, input$herd_id)

      animal_code <- next_passport_id_rv()
      req(animal_code)

      ok <- tryCatch({
        resolve_id <- function(code) {
          code <- trimws(code %||% "")
          if (code == "") return(NA_integer_)
          r <- dbGetQuery(db_pool, "SELECT animal_id FROM animals WHERE animal_code = $1 LIMIT 1", params = list(code))
          if (nrow(r) == 1) r$animal_id[1] else NA_integer_
        }

        sire_id <- resolve_id(input$sire_passport_id)
        dam_id  <- resolve_id(input$dam_passport_id)

        dbExecute(db_pool, "
          INSERT INTO animals
            (animal_code, animal_name, species_id, breed_id, gender, date_of_birth,
             first_lactation_age, sire_id, dam_id, ear_tag_no, microchip_no,
             birth_place, herd_id, status, created_at)
          VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, NOW())",
          params = list(
            animal_code,
            input$animal_name,
            as.integer(input$species_id),
            as.integer(input$breed_id),
            input$gender,
            as.character(input$dob),
            if (is.na(input$first_lactation_age)) NA_integer_ else as.integer(input$first_lactation_age),
            sire_id,
            dam_id,
            if (trimws(input$ear_tag_no) == "") NA_character_ else input$ear_tag_no,
            if (trimws(input$microchip_no) == "") NA_character_ else input$microchip_no,
            if (trimws(input$birth_place) == "") NA_character_ else input$birth_place,
            as.integer(input$herd_id),
            "Pending Validation"
          )
        )
        TRUE
      }, error = function(e) { message(conditionMessage(e)); FALSE })

      if (!ok) {
        output$save_msg <- renderUI({ div(class = "alert alert-danger py-1 mt-2 mb-0 small", "Could not save — check required fields and database constraints.") })
        return()
      }

      bump_refresh()
      log_activity(paste0(auth$full_name, " registered animal '", animal_code, "'"))
      output$save_msg <- renderUI({ div(class = "alert alert-success py-1 mt-2 mb-0 small", paste0("Animal '", animal_code, "' registered — pending validation.")) })
      showNotification("New Animal Record submitted for Supervisor Validation!", type = "message")
      updateTextInput(session, "animal_name", value = "")
      updateNumericInput(session, "first_lactation_age", value = NA)
      updateTextInput(session, "ear_tag_no", value = "")
      updateTextInput(session, "microchip_no", value = "")
      updateTextInput(session, "birth_place", value = "")
      updateSelectInput(session, "sire_passport_id", choices = sire_choices(), selected = "")
      updateSelectInput(session, "dam_passport_id", choices = dam_choices(), selected = "")
    })

    observeEvent(input$upload_btn, {
      req(auth$logged_in, input$bulk_file)
      df <- tryCatch(read.csv(input$bulk_file$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) {
        output$bulk_msg <- renderUI({ div(class = "alert alert-danger py-1 mt-2 mb-0 small", "Could not read that CSV file.") })
        return()
      }

      required_csv <- c("animal_name", "species_id", "breed_id", "gender", "date_of_birth", "herd_id")
      missing_csv <- setdiff(required_csv, names(df))
      if (length(missing_csv) > 0) {
        output$bulk_msg <- renderUI({ div(class = "alert alert-danger py-1 mt-2 mb-0 small", paste0("Missing required CSV column(s): ", paste(missing_csv, collapse = ", "))) })
        return()
      }

      resolve_animal_id <- function(code) {
        if (is.null(code) || is.na(code) || trimws(as.character(code)) == "") return(NA_integer_)
        r <- dbGetQuery(db_pool, "SELECT animal_id FROM animals WHERE animal_code = $1 LIMIT 1", params = list(trimws(as.character(code))))
        if (nrow(r) == 1) r$animal_id[1] else NA_integer_
      }

      start_id <- generate_next_passport_id()
      start_num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", start_id)))
      if (is.na(start_num)) start_num <- 1

      inserted <- 0; failed <- 0
      for (i in seq_len(nrow(df))) {
        row <- df[i, , drop = FALSE]
        supplied_code <- if ("animal_code" %in% names(df)) as.character(row$animal_code[1]) else ""
        animal_code <- if (!is.na(supplied_code) && trimws(supplied_code) != "") trimws(supplied_code) else sprintf("LSK-%06d", start_num + i - 1)
        sire_id <- if ("sire_animal_code" %in% names(df)) resolve_animal_id(row$sire_animal_code[1]) else NA_integer_
        dam_id  <- if ("dam_animal_code" %in% names(df)) resolve_animal_id(row$dam_animal_code[1]) else NA_integer_

        ok <- tryCatch({
          dbExecute(db_pool, "
            INSERT INTO animals
              (animal_code, animal_name, species_id, breed_id, gender, date_of_birth,
               first_lactation_age, sire_id, dam_id, ear_tag_no, microchip_no,
               birth_place, herd_id, status, created_at)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, NOW())",
            params = list(
              animal_code,
              row$animal_name[1],
              as.integer(row$species_id[1]),
              as.integer(row$breed_id[1]),
              row$gender[1],
              as.character(row$date_of_birth[1]),
              if ("first_lactation_age" %in% names(df) && !is.na(row$first_lactation_age[1])) as.integer(row$first_lactation_age[1]) else NA_integer_,
              sire_id,
              dam_id,
              if ("ear_tag_no" %in% names(df) && !is.na(row$ear_tag_no[1]) && trimws(as.character(row$ear_tag_no[1])) != "") as.character(row$ear_tag_no[1]) else NA_character_,
              if ("microchip_no" %in% names(df) && !is.na(row$microchip_no[1]) && trimws(as.character(row$microchip_no[1])) != "") as.character(row$microchip_no[1]) else NA_character_,
              if ("birth_place" %in% names(df) && !is.na(row$birth_place[1]) && trimws(as.character(row$birth_place[1])) != "") as.character(row$birth_place[1]) else NA_character_,
              as.integer(row$herd_id[1]),
              "Pending Validation"
            )
          )
          TRUE
        }, error = function(e) { message(conditionMessage(e)); FALSE })
        if (ok) inserted <- inserted + 1 else failed <- failed + 1
      }

      bump_refresh()
      log_activity(paste0(auth$full_name, " bulk-imported ", inserted, " animals"))
      output$bulk_msg <- renderUI({
        div(class = if (failed == 0) "alert alert-success py-1 mt-2 mb-0 small" else "alert alert-warning py-1 mt-2 mb-0 small",
            paste0(inserted, " imported", if (failed > 0) paste0(", ", failed, " failed (bad data or constraint).") else "."))
      })
    })

    # ---- 2. PHENOTYPE ENTRY ---------------------------------------------------
    output$pheno_animal_picker_ui <- renderUI({
      df <- records_data()
      if (nrow(df) == 0) return(p(class = "small text-muted", "No animals available yet."))
      choices <- setNames(df$Animal_Code, paste0(df$Animal_Code, " - ", df$Animal_Name))
      selectInput(ns("pheno_animal"), "Animal Code:", choices = choices)
    })

    output$pheno_trait_picker_ui <- renderUI({
      selectInput(ns("pheno_trait"), "Trait:", choices = trait_choices())
    })

    output$pheno_lactation_picker_ui <- renderUI({
      req(input$pheno_animal)
      animal_row <- tryCatch(
        dbGetQuery(db_pool, "SELECT animal_id FROM animals WHERE animal_code = $1 LIMIT 1", params = list(input$pheno_animal)),
        error = function(e) data.frame()
      )
      if (nrow(animal_row) != 1) return(selectInput(ns("pheno_lactation_id"), "Lactation ID (optional):", choices = c("None" = "")))
      df <- tryCatch(
        dbGetQuery(db_pool, "SELECT lactation_id, start_date, end_date, status FROM lactations WHERE animal_id = $1 ORDER BY start_date DESC", params = list(animal_row$animal_id[1])),
        error = function(e) data.frame()
      )
      if (nrow(df) == 0) return(selectInput(ns("pheno_lactation_id"), "Lactation ID (optional):", choices = c("None" = "")))
      labels <- paste0(df$lactation_id, " — ", df$start_date, " to ", ifelse(is.na(df$end_date), "Open", df$end_date), " (", df$status, ")")
      selectInput(ns("pheno_lactation_id"), "Lactation ID (optional):", choices = c("None" = "", setNames(df$lactation_id, labels)))
    })

    observeEvent(input$save_pheno_btn, {
      req(auth$logged_in, input$pheno_animal, input$pheno_trait, input$pheno_record_date)
      if (is.na(input$pheno_value)) {
        output$pheno_save_msg <- renderUI({ div(class = "alert alert-danger py-1 mt-2 mb-0 small", "Enter a value.") })
        return()
      }

      animal_row <- tryCatch(
        dbGetQuery(db_pool, "SELECT animal_id FROM animals WHERE animal_code = $1 LIMIT 1", params = list(input$pheno_animal)),
        error = function(e) data.frame()
      )
      if (nrow(animal_row) != 1) {
        output$pheno_save_msg <- renderUI({ div(class = "alert alert-danger py-1 mt-2 mb-0 small", "Animal code was not found.") })
        return()
      }

      lactation_id <- if (is.null(input$pheno_lactation_id) || input$pheno_lactation_id == "") NA_integer_ else as.integer(input$pheno_lactation_id)
      remarks <- if (is.null(input$pheno_remarks) || trimws(input$pheno_remarks) == "") NA_character_ else input$pheno_remarks

      ok <- tryCatch({
        dbExecute(db_pool, "
          INSERT INTO phenotypes
            (animal_id, trait_id, value, record_date, remarks, is_milk_trait, status, lactation_id)
          VALUES ($1, $2, $3, $4, $5, $6, $7, $8)",
          params = list(
            animal_row$animal_id[1],
            as.integer(input$pheno_trait),
            input$pheno_value,
            as.character(input$pheno_record_date),
            remarks,
            isTRUE(input$pheno_is_milk_trait),
            "Pending Validation",
            lactation_id
          )
        )
        TRUE
      }, error = function(e) { message(conditionMessage(e)); FALSE })

      if (!ok) {
        output$pheno_save_msg <- renderUI({ div(class = "alert alert-danger py-1 mt-2 mb-0 small", "Could not save phenotype record — check the database constraints.") })
        return()
      }
      log_activity(paste0(auth$full_name, " logged a phenotype for ", input$pheno_animal))
      output$pheno_save_msg <- renderUI({ div(class = "alert alert-success py-1 mt-2 mb-0 small", "Phenotype record saved — pending validation.") })
      updateNumericInput(session, "pheno_value", value = NA)
      updateTextAreaInput(session, "pheno_remarks", value = "")
    })

    # ---- 3. POPULATION MANAGEMENT --------------------------------------------
    observeEvent(input$upload_seed_btn, {
      req(auth$logged_in, input$seed_pop_file)
      df <- tryCatch(read.csv(input$seed_pop_file$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) {
        output$seed_pop_msg <- renderUI({ div(class = "alert alert-danger py-1 mt-2 mb-0 small", "Could not read that CSV file.") })
        return()
      }
      if (!"animal_code" %in% names(df)) {
        output$seed_pop_msg <- renderUI({ div(class = "alert alert-danger py-1 mt-2 mb-0 small", "CSV must contain an animal_code column.") })
        return()
      }

      inserted <- 0; failed <- 0
      for (i in seq_len(nrow(df))) {
        code <- trimws(as.character(df$animal_code[i]))
        if (is.na(code) || code == "") { failed <- failed + 1; next }
        animal_row <- tryCatch(
          dbGetQuery(db_pool, "SELECT animal_id FROM animals WHERE animal_code = $1 LIMIT 1", params = list(code)),
          error = function(e) data.frame()
        )
        if (nrow(animal_row) != 1) { failed <- failed + 1; next }

        herd_id <- if ("herd_id" %in% names(df) && !is.na(df$herd_id[i]) && trimws(as.character(df$herd_id[i])) != "") as.integer(df$herd_id[i]) else NA_integer_
        farm_id <- if ("farm_id" %in% names(df) && !is.na(df$farm_id[i]) && trimws(as.character(df$farm_id[i])) != "") as.integer(df$farm_id[i]) else NA_integer_
        status <- if ("status" %in% names(df) && !is.na(df$status[i]) && trimws(as.character(df$status[i])) != "") as.character(df$status[i]) else "Active"

        ok <- tryCatch({
          dbExecute(db_pool, "
            INSERT INTO training_population (animal_id, herd_id, farm_id, status)
            VALUES ($1, $2, $3, $4)",
            params = list(animal_row$animal_id[1], herd_id, farm_id, status))
          TRUE
        }, error = function(e) FALSE)
        if (ok) inserted <- inserted + 1 else failed <- failed + 1
      }

      bump_refresh()
      log_activity(paste0(auth$full_name, " imported ", inserted, " training-population records"))
      output$seed_pop_msg <- renderUI({
        div(class = if (failed == 0) "alert alert-success py-1 mt-2 mb-0 small" else "alert alert-warning py-1 mt-2 mb-0 small",
            paste0(inserted, " training-population records imported", if (failed > 0) paste0(", ", failed, " failed.") else "."))
      })
    })

    # ---- 4. GENOTYPE FILE INGESTION ------------------------------------------
    observe({
      df <- records_data()
      choices <- if (nrow(df) == 0) c() else setNames(df$Animal_Code, paste0(df$Animal_Code, " - ", df$Animal_Name))
      updateSelectInput(session, "genotype_animal", choices = choices)
    })

    genotype_lines <- reactiveVal(NULL)

    observeEvent(input$preview_genotype_btn, {
      req(input$genotype_file)
      ext <- tolower(tools::file_ext(input$genotype_file$name))
      if (!ext %in% c("vcf", "ped", "map")) {
        output$genotype_preview_ui <- renderUI({
          div(class = "alert alert-danger py-1 mt-2 mb-0 small",
              "Only VCF (.vcf) or PLINK (.ped/.map) files are accepted.")
        })
        return()
      }

      lines <- tryCatch(readLines(input$genotype_file$datapath, warn = FALSE), error = function(e) character())
      genotype_lines(lines)

      data_lines <- if (ext == "vcf") {
        lines[!grepl("^##", lines) & !grepl("^#CHROM", lines)]
      } else {
        lines
      }

      sample_lines <- head(data_lines[nchar(trimws(data_lines)) > 0], 3)

      if (length(sample_lines) == 0) {
        output$genotype_preview_ui <- renderUI({
          div(class = "alert alert-warning py-1 mt-2 mb-0 small", "No data records found to preview.")
        })
        return()
      }

      output$genotype_preview_ui <- renderUI({
        tagList(
          h6(paste0(length(sample_lines), "-Record Preview (", toupper(ext), ")"), class = "fw-bold mt-2"),
          lapply(seq_along(sample_lines), function(i) {
            fields <- strsplit(sample_lines[i], "\t|\\s+")[[1]]
            card(
              class = "mb-2",
              card_header(paste0("Sample ", i)),
              tags$pre(style = "white-space:pre-wrap; font-size:12px;", sample_lines[i]),
              p(class = "small text-muted mb-0",
                if (ext == "vcf" && length(fields) >= 5) {
                  paste0("CHROM=", fields[1], "  POS=", fields[2], "  ID=", fields[3],
                         "  REF=", fields[4], "  ALT=", fields[5],
                         " — this line defines one genomic variant/trait call.")
                } else {
                  paste0(length(fields), " fields detected on this line.")
                })
            )
          })
        )
      })
    })

    observeEvent(input$commit_genotype_btn, {
      req(auth$logged_in, input$genotype_animal, input$genotype_file, genotype_lines())
      lines <- genotype_lines()
      if (is.null(lines) || length(lines) == 0) {
        output$genotype_commit_msg <- renderUI({
          div(class = "alert alert-danger py-1 mt-2 mb-0 small", "Preview the file first before committing.")
        })
        return()
      }

      ext <- tolower(tools::file_ext(input$genotype_file$name))
      animal_row <- tryCatch(
        dbGetQuery(db_pool, "SELECT animal_id FROM animals WHERE animal_code = $1 LIMIT 1", params = list(input$genotype_animal)),
        error = function(e) data.frame()
      )
      if (nrow(animal_row) != 1) {
        output$genotype_commit_msg <- renderUI({ div(class = "alert alert-danger py-1 mt-2 mb-0 small", "Animal code was not found.") })
        return()
      }

      marker_count <- if (is.null(input$genotype_marker_count) || is.na(input$genotype_marker_count)) NA_integer_ else as.integer(input$genotype_marker_count)
      platform <- if (is.null(input$genotype_platform) || trimws(input$genotype_platform) == "") NA_character_ else input$genotype_platform
      chip_name <- if (is.null(input$genotype_chip_name) || trimws(input$genotype_chip_name) == "") NA_character_ else input$genotype_chip_name
      remarks <- if (is.null(input$genotype_remarks) || trimws(input$genotype_remarks) == "") NA_character_ else input$genotype_remarks

      ok <- tryCatch({
        dbExecute(db_pool, "
          INSERT INTO genotypes
            (animal_id, platform, chip_name, marker_count, genotype_file, remarks, format, status)
          VALUES ($1, $2, $3, $4, $5, $6, $7, $8)",
          params = list(
            animal_row$animal_id[1], platform, chip_name, marker_count,
            input$genotype_file$name, remarks, toupper(ext), "Pending Validation"
          )
        )
        TRUE
      }, error = function(e) { message(conditionMessage(e)); FALSE })

      if (!ok) {
        output$genotype_commit_msg <- renderUI({
          div(class = "alert alert-danger py-1 mt-2 mb-0 small", "Could not save genotype metadata — check the database constraints.")
        })
        return()
      }
      log_activity(paste0(auth$full_name, " committed genotype metadata for ", input$genotype_animal))
      output$genotype_commit_msg <- renderUI({
        div(class = "alert alert-success py-1 mt-2 mb-0 small", paste0("Genotype metadata saved for ", input$genotype_animal, " — pending validation."))
      })
    })

  })
}