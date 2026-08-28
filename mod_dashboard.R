# ==============================================================================
# MODULE: DASHBOARD
# Hero banner, KPI value boxes, Live Herd Activity feed (latest event),
# and the institutional role matrix. Read-only — no auth gating needed.
# ==============================================================================

mod_dashboard_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML("
        .hero-banner {
          background: linear-gradient(135deg, #E8FFF4 0%, #E6F1FF 100%);
          border: 1px solid #CFF3E6;
          border-radius: 12px;
          padding: 30px;
          box-shadow: 0 4px 14px rgba(0,166,118,0.10);
        }
        .role-card {
          border-top: 4px solid #00A676;
          background: #ffffff;
          border-radius: 8px;
          padding: 15px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        }
        .live-dot {
          width: 10px; height: 10px; border-radius: 50%;
          background: #00A676; display: inline-block; margin-right: 8px;
          animation: pulse 1.6s infinite;
        }
        @keyframes pulse {
          0%   { box-shadow: 0 0 0 0 rgba(0,166,118,0.55); }
          70%  { box-shadow: 0 0 0 9px rgba(0,166,118,0); }
          100% { box-shadow: 0 0 0 0 rgba(0,166,118,0); }
        }
        .activity-item {
          display: flex; gap: 10px; align-items: flex-start;
          padding: 8px 0; border-bottom: 1px solid #EEF1EF;
        }
        .activity-item:last-child { border-bottom: none; }
        .activity-dot {
          width: 8px; height: 8px; border-radius: 50%;
          background: #17C3B2; margin-top: 6px; flex-shrink: 0;
        }
        .hero-photo {
          width: 100%; max-height: 220px; object-fit: cover;
          border-radius: 10px; border: 2px solid #00A676;
          box-shadow: 0 4px 10px rgba(0,0,0,0.1);
        }
        .navbar-brand-title {
          font-size: 1.15rem;
          font-weight: 600;
          white-space: nowrap;
        }
        .navbar-live-status {
          display: inline-flex;
          align-items: center;
          margin-left: 18px;
          padding-left: 16px;
          border-left: 1px solid rgba(255,255,255,0.35);
          font-size: 0.82rem;
          font-weight: 500;
          white-space: nowrap;
          opacity: 0.96;
        }
        .hero-map {
          width: 100%;
          height: 280px;
          border-radius: 10px;
          overflow: hidden;
          border: 2px solid #00A676;
          box-shadow: 0 4px 10px rgba(0,0,0,0.10);
        }
        .hero-title-row {
          margin-bottom: 18px;
        }
        @media (max-width: 992px) {
          .navbar-live-status { margin-left: 8px; padding-left: 8px; }
          .navbar-brand-title { font-size: 0.95rem; }
        }
      "))
    ),

    div(
      class = "container-fluid my-2",

      div(
        class = "hero-banner mb-4",

        # --- Title appears ONCE, full width, at the very top ---
        div(
          class = "hero-title-row",
          h1("System Overview", class = "text-success fw-bold")
        ),

        # --- Overview (description + badges), full width — map removed ---
        p(class = "lead text-secondary",
          "A centralized, multi-role management framework for multi-farm monitoring, validated phenotype entry, and automated Genomic Estimated Breeding Value (GEBV) evaluation."),
        div(
          class = "mt-3",
          span(class = "badge fs-6 me-2", style = "background:#00A676;", "Role-Based Access Control"),
          span(class = "badge fs-6 me-2", style = "background:#17C3B2;", "Animal Directory"),
          span(class = "badge fs-6 me-2", style = "background:#17C3B2;", "Genomic Prediction Modeling"),
          span(class = "badge fs-6", style = "background:#2F8CFF;", "EBV | GEBV Evaluations")
        )
      ),

      layout_columns(
        fill = FALSE,
        value_box(
          title = "Active Herd Count",
          value = textOutput(ns("kpi_total_count")),
          showcase = bs_icon("journal-text"),
          theme = "primary"
        ),
        value_box(
          title = "Average 305-Day Lactation Milk Yield",
          value = textOutput(ns("kpi_avg_milk")),
          showcase = bs_icon("droplet-fill"),
          theme = "info"
        ),
        value_box(
          title = "Average Fat Content",
          value = textOutput(ns("kpi_avg_fat")),
          showcase = bs_icon("percent"),
          theme = "success"
        ),
        value_box(
          title = "Total Lactation Records",
          value = textOutput(ns("kpi_lactation_count")),
          showcase = bs_icon("card-checklist"),
          theme = "warning"
        )
      ),

      br(),

      div(
        class = "role-card mb-4",
        div(
          class = "d-flex align-items-center justify-content-between mb-2",
          h5(bs_icon("activity"), " Live Herd Activity", class = "fw-bold mb-0", style = "color:#00A676;"),
          span(
            span(class = "live-dot"),
            span("Live", class = "small text-muted fw-semibold")
          )
        ),
        uiOutput(ns("live_activity_feed"))
      ),

      br(),

      h4("Institutional Role Matrix", class = "fw-bold text-dark mb-3"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        div(
          class = "role-card",
          h5(bs_icon("pencil-square"), " Data Entry Operator", class = "fw-bold", style = "color:#00A676;"),
          tags$ul(
            class = "text-muted small ps-3",
            tags$li("Add new animal records"),
            tags$li("Enter milk & fat phenotypes"),
            tags$li("Upload animal photo"),
            tags$li("Upload raw genotype files")
          )
        ),
        div(
          class = "role-card",
          h5(bs_icon("check-circle-fill"), " Farm Supervisor", class = "fw-bold", style = "color:#FFB020;"),
          tags$ul(
            class = "text-muted small ps-3",
            tags$li("Full system access"),
            tags$li("Validate & audit records"),
            tags$li("Manage users"),
            tags$li("Run GEBV predictions")
          )
        ),
        div(
          class = "role-card",
          h5(bs_icon("building"), " Farm Manager", class = "fw-bold", style = "color:#2F8CFF;"),
          tags$ul(
            class = "text-muted small ps-3",
            tags$li("Multi-farm oversight"),
            tags$li("View executive reports"),
            tags$li("Monitor herd performance"),
            tags$li("Approve pending records")
          )
        ),
        div(
          class = "role-card",
          h5(bs_icon("shield-lock-fill"), " System Admin", class = "fw-bold", style = "color:#FF5A5F;"),
          tags$ul(
            class = "text-muted small ps-3",
            tags$li("Full system access"),
            tags$li("User management (Create/Delete)"),
            tags$li("Run GEBV predictions"),
            tags$li("System configuration")
          )
        )
      )
    )
  )
}

# records_data: reactive returning the animals data frame (from the main server)
mod_dashboard_server <- function(id, records_data) {
  moduleServer(id, function(input, output, session) {

    output$kpi_total_count <- renderText({ nrow(records_data()) })

    output$kpi_avg_milk <- renderText({
      r <- tryCatch(dbGetQuery(db_pool, "SELECT AVG(morning_yield + evening_yield) AS m FROM milk_yield_records"), error = function(e) NULL)
      if (is.null(r) || is.na(r$m[1])) "N/A" else paste0(round(r$m[1], 1), " L")
    })

    output$kpi_avg_fat <- renderText({
      r <- tryCatch(dbGetQuery(db_pool, "
        SELECT AVG(p.value) AS m FROM phenotypes p
        JOIN traits t ON p.trait_id = t.trait_id
        WHERE t.trait_name ILIKE '%fat%'"), error = function(e) NULL)
      if (is.null(r) || nrow(r) == 0 || is.na(r$m[1])) "N/A" else paste0(round(r$m[1], 2), " %")
    })

    output$kpi_lactation_count <- renderText({
      r <- tryCatch(dbGetQuery(db_pool, "SELECT COUNT(*) AS n FROM milk_yield_records"), error = function(e) NULL)
      if (is.null(r) || nrow(r) == 0 || is.na(r$n[1])) "N/A" else format(r$n[1], big.mark = ",")
    })

    # --- Live Herd Activity feed: latest animal, phenotype & milk-yield events ---
    live_activity <- reactive({
      tryCatch({
        dbGetQuery(db_pool, "
          SELECT * FROM (
            SELECT 'New animal registered' AS action,
                   animal_id::text AS ref, created_at
            FROM animals
            UNION ALL
            SELECT 'Milk yield recorded' AS action,
                   record_id::text AS ref, created_at
            FROM milk_yield_records
            UNION ALL
            SELECT 'Phenotype entry added' AS action,
                   phenotype_id::text AS ref, created_at
            FROM phenotypes
          ) feed
          ORDER BY created_at DESC
          LIMIT 1")
      }, error = function(e) data.frame(action = character(), ref = character(), created_at = character()))
    })

    output$live_activity_feed <- renderUI({
      df <- live_activity()

      if (nrow(df) == 0) {
        return(p(class = "text-muted small mb-0", "No recent activity to show yet."))
      }

      tagList(
        lapply(seq_len(nrow(df)), function(i) {
          div(
            class = "activity-item",
            div(class = "activity-dot"),
            div(
              span(df$action[i], class = "fw-semibold"),
              span(paste0(" · #", df$ref[i]), class = "text-muted small"),
              br(),
              span(format(as.POSIXct(df$created_at[i]), "%d %b %Y, %I:%M %p"),
                   class = "text-muted small")
            )
          )
        })
      )
    })
  })
}