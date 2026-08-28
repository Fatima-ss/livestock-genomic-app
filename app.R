# ==============================================================================
# LIVESTOCK GENOMIC INFORMATION MANAGEMENT SYSTEM
# Modularized Shiny app — global.R loads packages/DB/helpers and sources every
# file in modules/. This file only builds the navbar shell and wires modules
# together through a small set of shared reactives.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. USER INTERFACE (UI)
# ------------------------------------------------------------------------------

source("global.R")
ui <- tagList(
  useShinyjs(),
  mod_login_ui("login"),

  page_navbar(
    title = tagList(
      span("LIVESTOCK GENOMIC INFORMATION MANAGEMENT SYSTEM", class = "navbar-brand-title"),
      span(
        class = "navbar-live-status",
        span(class = "live-dot"),
        "Live Herd Activity · Last synced: ",
        textOutput("live_clock", inline = TRUE)
      )
    ),

    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#00A676",
      secondary = "#2F8CFF",
      success = "#17C3B2",
      info = "#2F8CFF",
      warning = "#FFB020",
      danger = "#FF5A5F",
      bg = "#FFFFFF",
      fg = "#1A1A1A"
    ),

    nav_panel(
      title = "Dashboard",
      value = "dashboard_tab",
      icon = bs_icon("house-door-fill"),
      mod_dashboard_ui("dashboard")
    ),

    nav_panel(
      title = "Animal Directory & Passport",
      value = "animal_directory_tab",
      icon = bs_icon("card-checklist"),
      mod_directory_ui("directory")
    ),

    nav_panel(
      title = "Data Entry Console",
      value = "data_entry_tab",
      icon = bs_icon("plus-circle-fill"),
      mod_data_entry_ui("data_entry")
    ),

    nav_panel(
      title = "Analytics & Reports",
      value = "analytics_tab",
      icon = bs_icon("bar-chart-line-fill"),
      mod_analytics_ui("analytics")
    ),

    nav_panel(
  title = "EBV / GEBV",
  value = "ebv_gebv_tab",
  icon = bs_icon("graph-up-arrow"),
  mod_ebv_gebv_ui("ebv_gebv")
),

    nav_panel(
      title = "Admin & GEBV Engine",
      value = "admin_tab",
      icon = bs_icon("cpu-fill"),
      mod_admin_ui("admin")
    )
  )
)

# ------------------------------------------------------------------------------
# 2. SERVER LOGIC
# ------------------------------------------------------------------------------

server <- function(input, output, session) {

  # (onStop line removed from here)

  # ---- Shared reactives used by more than one module -----------------------
  refresh_trigger <- reactiveVal(0)
  bump_refresh <- function() { refresh_trigger(refresh_trigger() + 1) }

  records_data <- reactive({
    refresh_trigger()
    tryCatch({
      dbGetQuery(db_pool, "
        SELECT a.animal_id,
               a.animal_code AS \"Animal_Code\",
               a.animal_name AS \"Animal_Name\",
               s.species_name AS \"Species\",
               b.breed_name AS \"Breed\",
               a.gender AS \"Gender\",
               a.date_of_birth AS \"Date_of_Birth\",
               a.first_lactation_age AS \"First_Lactation_Age\",
               a.ear_tag_no AS \"Ear_Tag_No\",
               a.microchip_no AS \"Microchip_No\",
               a.birth_place AS \"Birth_Place\",
               a.herd_id AS \"Herd_ID\",
               a.sire_id AS \"Sire_ID\",
               a.dam_id AS \"Dam_ID\",
               a.gebv_value AS \"GEBV_Value\",
               a.gebv_calculation_date AS \"GEBV_Calculation_Date\",
               a.best_model AS \"Best_Model\",
               a.status AS \"Status\"
        FROM animals a
        LEFT JOIN breeds b ON a.breed_id = b.breed_id
        LEFT JOIN species s ON a.species_id = s.species_id
        ORDER BY a.created_at DESC"
      )
    }, error = function(e) {
      showNotification(paste("Could not load animals:", conditionMessage(e)), type = "error")
      data.frame(
        animal_id = integer(), Animal_Code = character(), Animal_Name = character(),
        Species = character(), Breed = character(), Gender = character(),
        Date_of_Birth = as.Date(character()), First_Lactation_Age = numeric(),
        Ear_Tag_No = character(), Microchip_No = character(), Birth_Place = character(),
        Herd_ID = integer(), Sire_ID = integer(), Dam_ID = integer(),
        GEBV_Value = numeric(), GEBV_Calculation_Date = character(),
        Best_Model = character(), Status = character()
      )
    })
  })

  activity_log <- reactiveVal(data.frame(Time = character(), Event = character(), stringsAsFactors = FALSE))

  log_activity <- function(msg) {
    new_row <- data.frame(Time = format(Sys.time(), "%H:%M:%S"), Event = msg, stringsAsFactors = FALSE)
    updated <- rbind(new_row, isolate(activity_log()))
    if (nrow(updated) > 8) updated <- updated[1:8, , drop = FALSE]
    activity_log(updated)
  }

  output$live_clock <- renderText({
    invalidateLater(1000, session)
    format(Sys.time(), "%H:%M:%S")
  })

  output$activity_feed_ui <- renderUI({
    log <- activity_log()
    if (nrow(log) == 0) {
      return(p(class = "small text-muted mb-0", "No recent activity yet — actions will appear here as they happen."))
    }
    tagList(lapply(seq_len(nrow(log)), function(i) {
      div(class = "activity-item",
          div(class = "activity-dot"),
          div(
            div(style = "font-family: monospace; font-size: 11.5px; color:#69756D;", log$Time[i]),
            div(style = "font-size: 13px;", log$Event[i])
          )
      )
    }))
  })

  # ---- Authentication module (returns shared `auth` reactiveValues) --------
  auth <- mod_login_server("login", log_activity = log_activity)

  # ---- Feature modules --------------------------------------------------------
  mod_dashboard_server("dashboard", records_data = records_data)

  mod_directory_server("directory",
                        records_data = records_data,
                        auth = auth,
                        bump_refresh = bump_refresh,
                        log_activity = log_activity)

  mod_data_entry_server("data_entry",
                         records_data = records_data,
                         auth = auth,
                         bump_refresh = bump_refresh,
                         log_activity = log_activity,
                         refresh_trigger = refresh_trigger)


  mod_analytics_server("analytics", records_data = records_data)

  mod_ebv_gebv_server("ebv_gebv",
                     records_data = records_data,
                     auth = auth,
                     bump_refresh = bump_refresh,
                     log_activity = log_activity)

  mod_admin_server("admin", auth = auth, log_activity = log_activity)
}

# ------------------------------------------------------------------------------
# 3. RUN APPLICATION
# ------------------------------------------------------------------------------
onStop(function() {
  poolClose(db_pool)
})

shinyApp(ui = ui, server = server)
