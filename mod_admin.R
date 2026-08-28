# ==============================================================================
# MODULE: ADMIN & GEBV ENGINE
# System user management + placeholder GEBV genomic pipeline execution.
# ==============================================================================

mod_admin_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("tab_admin_ui"))
}

# auth: shared reactiveValues
# log_activity: shared function
mod_admin_server <- function(id, auth, log_activity) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    role_choices <- reactive({
      tryCatch({
        df <- get_roles()
        setNames(df$role_id, df$role_name)
      }, error = function(e) c())
    })

    output$tab_admin_ui <- renderUI({
      req(auth$logged_in)
      if (!auth$role %in% c("supervisor", "admin")) {
        return(card(
          class = "border-danger my-3 text-center p-4",
          h4("Access Restricted", class = "text-danger fw-bold"),
          p("Only 'Farm Supervisor' or 'System Admin' can manage backend users, system configuration, and run GEBV models.")
        ))
      }

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("System User Management & Configuration"),
          textInput(ns("new_username"), "Full Name:"),
          textInput(ns("new_user_email"), "Email:"),
          passwordInput(ns("new_user_password"), "Set Password:"),
          selectInput(ns("new_user_role"), "Assign Role:", choices = NULL),
          actionButton(ns("create_user_btn"), "Create System User", class = "btn w-100 mb-3", style = "background:#00A676; color:#fff;"),
          uiOutput(ns("create_user_msg")),
          hr(),
          h6("Manage Active Accounts:"),
          uiOutput(ns("active_accounts_ui"))
        ),
        card(
          card_header("GEBV Genomic Pipeline Execution Engine"),
          selectInput(ns("gebv_model"), "Select Prediction Model:", choices = c("GBLUP", "rrBLUP", "Bayesian Lasso")),
          numericInput(ns("cv_k"), "Cross-Validation Folds:", value = 5),
          actionButton(ns("run_gebv_engine"), "Generate & Rank GEBVs", class = "btn w-100 my-2", style = "background:#FF5A5F; color:#fff;"),
          verbatimTextOutput(ns("gebv_console_out")),
          hr(),
          h6("Per-Animal GEBV Ranking", class = "fw-bold"),
          DTOutput(ns("gebv_table"))
        )
      )
    })

    observe({
      updateSelectInput(session, "new_user_role", choices = role_choices())
    })

    observeEvent(input$create_user_btn, {
      req(auth$logged_in, auth$role %in% c("supervisor", "admin"))
      fname <- trimws(input$new_username)
      uemail <- trimws(input$new_user_email)
      existing <- tryCatch(get_user_by_email(uemail), error = function(e) NULL)

      err <- NULL
      if (fname == "") err <- "Enter a full name."
      else if (uemail == "") err <- "Enter an email."
      else if (!is.null(existing) && nrow(existing) > 0) err <- "That email already exists."
      else if (nchar(input$new_user_password) < 4) err <- "Password must be at least 4 characters."
      else if (is.null(input$new_user_role) || input$new_user_role == "") err <- "Choose a role."

      if (!is.null(err)) {
        output$create_user_msg <- renderUI({ div(class = "alert alert-danger py-1 mt-2 mb-0 small", err) })
        return()
      }

      ok <- tryCatch({
        dbExecute(db_pool, "
          INSERT INTO users (full_name, email, password_hash, role_id, created_at)
          VALUES ($1, $2, $3, $4, NOW())",
          params = list(fname, uemail, sodium::password_store(input$new_user_password), as.integer(input$new_user_role))
        )
        TRUE
      }, error = function(e) { message(conditionMessage(e)); FALSE })

      if (!ok) {
        output$create_user_msg <- renderUI({ div(class = "alert alert-danger py-1 mt-2 mb-0 small", "Could not create account.") })
        return()
      }

      log_activity(paste0(auth$full_name, " created account '", uemail, "'"))
      output$create_user_msg <- renderUI({
        div(class = "alert alert-success py-1 mt-2 mb-0 small", paste0("User '", uemail, "' created."))
      })
      updateTextInput(session, "new_username", value = "")
      updateTextInput(session, "new_user_email", value = "")
      updateTextInput(session, "new_user_password", value = "")
      account_refresh(account_refresh() + 1)
    })

    account_refresh <- reactiveVal(0)

    output$active_accounts_ui <- renderUI({
      account_refresh()
      accts <- tryCatch(
        dbGetQuery(db_pool, "SELECT u.full_name, u.email, r.role_name FROM users u JOIN role r ON u.role_id = r.role_id ORDER BY u.created_at"),
        error = function(e) data.frame()
      )
      if (nrow(accts) == 0) return(p(class = "small text-muted", "No accounts yet."))
      tags$ul(
        class = "small",
        lapply(seq_len(nrow(accts)), function(i) {
          tags$li(paste0(accts$email[i], " (", accts$full_name[i], " — ", accts$role_name[i], ") - Active"))
        })
      )
    })

    observeEvent(input$run_gebv_engine, {
      req(auth$logged_in, auth$role %in% c("supervisor", "admin"))
      log_activity(paste0(auth$full_name, " ran the GEBV pipeline (", input$gebv_model, ", k=", input$cv_k, ")"))
      output$gebv_console_out <- renderText({
        paste0("== GEBV PIPELINE EXECUTION SUMMARY ==\n",
               "Model: ", input$gebv_model, "\n",
               "CV Folds: ", input$cv_k, "\n",
               "Status: GEBV Prediction Completed Successfully.\n",
               "Heritability Estimate (h2): 0.38\n",
               "Mean Accuracy (r): 0.74\n",
               "NOTE: this is a placeholder result — the real rrBLUP/GBLUP pipeline\n",
               "against the genotypes/phenotypes tables is a follow-up step.")
      })
    })

    output$gebv_table <- renderDT({
      df <- tryCatch(
        dbGetQuery(db_pool, "
          SELECT a.animal_code AS \"Animal_Code\", a.animal_name AS \"Name\",
                 g.gebv_value AS \"GEBV\", g.accuracy AS \"Accuracy\", g.model AS \"Model\"
          FROM gebv_results g
          JOIN animals a ON g.animal_id = a.animal_id
          ORDER BY g.gebv_value DESC"),
        error = function(e) data.frame(Animal_Code = character(), Name = character(),
                                        GEBV = numeric(), Accuracy = numeric(), Model = character())
      )
      datatable(df, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
    })
  })
}
