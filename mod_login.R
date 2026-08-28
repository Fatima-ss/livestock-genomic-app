# ==============================================================================
# MODULE: LOGIN / SIGNUP
# Livestock Genomic Information Management System
# ==============================================================================

# Compatibility for existing app.R / modules.
if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
}

if (!exists("useShinyjs", mode = "function", inherits = FALSE)) {
  useShinyjs <- function(...) {
    if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::useShinyjs(...)
    else invisible(NULL)
  }
}

# ------------------------------------------------------------------------------
# ROLE -> PERMISSION MAP
# The returned permissions control navigation AND functionality.
# IMPORTANT: the main app should also use auth$can_access() on the server side
# before executing protected operations.
# ------------------------------------------------------------------------------
LGMS_ROLE_PERMISSIONS <- list(
  admin = c(
    "dashboard", "animals", "genomics", "breeding", "milk_analytics",
    "reports", "users", "settings", "admin"
  ),
  manager = c(
    "dashboard", "animals", "genomics", "breeding", "milk_analytics",
    "reports"
  ),
  supervisor = c(
    "dashboard", "animals", "genomics", "breeding", "milk_analytics",
    "reports"
  ),
  operator = c(
    "dashboard", "animals", "milk_analytics"
  )
)

lgms_permissions_for_role <- function(role_name) {
  role_name <- tryCatch(
    normalize_role(role_name),
    error = function(e) tolower(trimws(as.character(role_name %||% "")))
  )

  LGMS_ROLE_PERMISSIONS[[role_name]] %||% character(0)
}

# Optional server-side helper for use from app.R/modules:
#   req(auth$can_access("animals"))
#   lgms_require_permission(auth, "animals")
lgms_require_permission <- function(auth, permission) {
  if (is.null(auth) || !isTRUE(auth$logged_in)) {
    shiny::req(FALSE)
  }
  shiny::req(
    is.function(auth$can_access) &&
      isTRUE(auth$can_access(permission))
  )
  invisible(TRUE)
}

# ==============================================================================
# UI
# ==============================================================================

mod_login_ui <- function(id) {

  ns <- NS(id)

  tagList(

    # --------------------------------------------------------------------------
    # JavaScript: reliable login/signup switching + role based navigation.
    # --------------------------------------------------------------------------
    tags$script(HTML("
      if (!window.__lgmsLoginHandlersInstalled) {
        Shiny.addCustomMessageHandler('lgms-hide', function(id) {
          var el = document.getElementById(id);
          if (el) {
            el.style.display = 'none';
            el.setAttribute('aria-hidden', 'true');
          }
        });

        Shiny.addCustomMessageHandler('lgms-show', function(id) {
          var el = document.getElementById(id);
          if (el) {
            el.style.display = 'flex';
            el.removeAttribute('aria-hidden');
          }
        });

        Shiny.addCustomMessageHandler('lgms-show-block', function(id) {
          var el = document.getElementById(id);
          if (el) {
            el.style.display = 'block';
            el.removeAttribute('aria-hidden');
          }
        });

        Shiny.addCustomMessageHandler('lgms-run-js', function(js) {
          try { eval(js); } catch (e) { console.error(e); }
        });

        Shiny.addCustomMessageHandler('lgms-apply-role', function(x) {
          try {
            var role = (x && x.role) ? String(x.role).toLowerCase() : 'operator';
            var permissions = (x && Array.isArray(x.permissions)) ?
              x.permissions : [];

            document.querySelectorAll('[data-lgms-permission]').forEach(function(el) {
              var permission = el.getAttribute('data-lgms-permission');
              var allowed = permissions.indexOf(permission) !== -1;
              el.style.display = allowed ? '' : 'none';
              el.setAttribute('aria-hidden', allowed ? 'false' : 'true');
            });

            var admin = document.querySelector('a[data-value=\"admin_tab\"]');
            if (admin && admin.closest('li')) {
              admin.closest('li').style.display =
                permissions.indexOf('admin') !== -1 ? '' : 'none';
            }

            document.body.setAttribute('data-lgms-role', role);
          } catch (e) {
            console.error('LGMS role navigation error:', e);
          }
        });

        window.__lgmsLoginHandlersInstalled = true;
      }
    ")),

    # --------------------------------------------------------------------------
    # FULL-SCREEN LOGIN OVERLAY
    # --------------------------------------------------------------------------
    div(
      id = ns("loginOverlay"),
      class = "lgms-login-overlay",
      style = "
        position:fixed;
        inset:0;
        width:100%;
        height:100%;
        z-index:99999;
        display:flex;
        overflow:auto;
        background:#f6faf7;
        font-family:'Segoe UI',Arial,sans-serif;
      ",

      # LEFT PANEL
      div(
        class = "lgms-left-panel",

        div(
          class = "lgms-login-card",

          # Logo / title
          div(
            class = "lgms-brand",
            bs_icon(
              "shield-lock-fill",
              size = "2.7rem",
              style = "color:#16a36f;"
            ),
            tags$h1(
              "Livestock Genomic Information Management System",
              class = "lgms-title"
            ),
            tags$p(
              "Smarter breeding decisions for Pakistan's livestock farms",
              class = "lgms-tagline"
            )
          ),

          # LOGIN VIEW
          div(
            id = ns("loginView"),
            class = "lgms-form-view",
            style = "display:block;",

            tags$h2("Welcome Back", class = "lgms-heading"),
            tags$p(
              "Please login to your account",
              class = "lgms-description"
            ),

            div(
              class = "lgms-field",
              tags$label("Email"),
              textInput(
                ns("login_user"),
                label = NULL,
                placeholder = "Enter your email",
                width = "100%"
              )
            ),

            div(
              class = "lgms-field",
              tags$label("Password"),
              passwordInput(
                ns("login_pass"),
                label = NULL,
                placeholder = "Enter your password",
                width = "100%"
              )
            ),

            actionButton(
              ns("login_btn"),
              "Log In",
              class = "btn lgms-login-btn",
              style = "width:100%;"
            ),

            uiOutput(ns("login_error")),

            tags$div(
              class = "lgms-divider",
              tags$span("OR")
            ),

            tags$p(
              class = "lgms-create-text",
              "Don't have an account? ",
              actionLink(
                ns("show_signup"),
                "Create Account",
                class = "lgms-link"
              )
            )
          ),

          # SIGNUP VIEW
          div(
            id = ns("signupView"),
            class = "lgms-form-view",
            style = "display:none;",

            tags$h2("Create Your Account", class = "lgms-heading"),
            tags$p(
              "Join the livestock management system",
              class = "lgms-description"
            ),

            div(
              class = "lgms-field",
              tags$label("Full Name"),
              textInput(
                ns("signup_fullname"),
                label = NULL,
                placeholder = "Enter your full name",
                width = "100%"
              )
            ),

            div(
              class = "lgms-field",
              tags$label("Email"),
              textInput(
                ns("signup_user"),
                label = NULL,
                placeholder = "Enter your email",
                width = "100%"
              )
            ),

            div(
              class = "lgms-field",
              tags$label("Password"),
              passwordInput(
                ns("signup_pass"),
                label = NULL,
                placeholder = "Choose a password",
                width = "100%"
              )
            ),

            div(
              class = "lgms-field",
              tags$label("Confirm Password"),
              passwordInput(
                ns("signup_pass2"),
                label = NULL,
                placeholder = "Confirm your password",
                width = "100%"
              )
            ),

            actionButton(
              ns("signup_btn"),
              "Create Account",
              class = "btn lgms-signup-btn",
              style = "width:100%;"
            ),

            uiOutput(ns("signup_error")),

            tags$p(
              class = "lgms-create-text lgms-back-text",
              "Already have an account? ",
              actionLink(
                ns("show_login"),
                "Back to Log In",
                class = "lgms-link"
              )
            )
          ),

          # FOOTER
          div(
            class = "lgms-footer",
            tags$div(
              bs_icon(
                "shield-check",
                size = "0.9rem",
                style = "color:#16a36f;margin-right:5px;"
              ),
              "Secure • Reliable • Efficient"
            ),
            tags$div(
              "Pakistan Livestock Network",
              class = "lgms-footer-brand"
            )
          )
        )
      ),

      # RIGHT PANEL
      div(
        class = "lgms-right-panel",

        div(class = "lgms-circle lgms-circle-one"),
        div(class = "lgms-circle lgms-circle-two"),

        div(
          class = "lgms-visual-card",

          tags$img(
            src = "WhatsApp_Image_2026-08-23_at_11_15_15_PM.jpeg",
            alt = "Pakistan livestock and herd",
            class = "lgms-hero-image",
            onerror = paste0(
              "this.style.display='none';",
              "var fallback=document.getElementById('",
              ns("imageFallback"),
              "');",
              "if(fallback){fallback.style.display='flex';}"
            )
          ),

          div(
            id = ns("imageFallback"),
            class = "lgms-image-fallback",
            style = "display:none;",

            bs_icon(
              "geo-alt-fill",
              size = "3rem",
              style = "color:#168b61;margin-bottom:12px;"
            ),

            tags$h3("Pakistan Livestock Network"),
            tags$p(
              "Place the livestock/Pakistan image in the www folder."
            )
          )
        ),

        tags$div(
          class = "lgms-right-label",
          bs_icon(
            "geo-alt-fill",
            size = "0.9rem",
            style = "margin-right:5px;"
          ),
          "Pakistan Livestock Network"
        )
      )
    ),

    # PAGE CSS — same design/colours.
    tags$style(
      HTML(
        paste0(
          "
          #", ns("loginOverlay"), " { box-sizing:border-box; }
          #", ns("loginOverlay"), " *,
          #", ns("loginOverlay"), " *::before,
          #", ns("loginOverlay"), " *::after { box-sizing:border-box; }

          .lgms-left-panel {
            width:46%; min-width:480px; min-height:100vh; display:flex;
            align-items:center; justify-content:center; padding:28px 40px;
            background:radial-gradient(circle at 20% 10%, rgba(22,163,111,.055), transparent 34%), #ffffff;
          }

          .lgms-login-card {
            width:100%; max-width:440px; padding:34px 38px 24px;
            border:1px solid #e5ece7; border-radius:22px; background:rgba(255,255,255,.98);
            box-shadow:0 18px 45px rgba(22,71,49,.10); text-align:center;
          }

          .lgms-brand { margin-bottom:24px; }

          .lgms-title {
            margin:10px 0 2px; color:#145a43; font-size:23px; line-height:1.25; font-weight:700;
          }

          .lgms-subtitle { color:#145a43; font-size:15px; font-weight:600; margin-top:2px; }

          .lgms-tagline {
            color:#7a827e; font-size:12.5px; line-height:1.45; margin:9px 0 0;
          }

          .lgms-heading {
            color:#145a43; font-size:24px; font-weight:700; margin:6px 0 4px;
          }

          .lgms-description {
            color:#7a827e; font-size:13px; margin:0 0 20px;
          }

          .lgms-field { text-align:left; margin-bottom:13px; }

          .lgms-field label {
            display:block; color:#50615a; font-size:12px; font-weight:600;
            margin:0 0 6px 2px;
          }

          #", ns("login_user"), ",
          #", ns("login_pass"), ",
          #", ns("signup_fullname"), ",
          #", ns("signup_user"), ",
          #", ns("signup_pass"), ",
          #", ns("signup_pass2"), " {
            width:100% !important; height:45px !important; margin:0 !important;
            border:1px solid #d8e2dc !important; border-radius:9px !important;
            background:#fbfdfc !important; color:#25352e !important;
            padding:10px 13px !important; font-size:13px !important;
            box-shadow:none !important; outline:none !important; transition:all .2s ease;
          }

          #", ns("login_user"), ":focus,
          #", ns("login_pass"), ":focus,
          #", ns("signup_fullname"), ":focus,
          #", ns("signup_user"), ":focus,
          #", ns("signup_pass"), ":focus,
          #", ns("signup_pass2"), ":focus {
            border-color:#16a36f !important; background:#ffffff !important;
            box-shadow:0 0 0 3px rgba(22,163,111,.10) !important;
          }

          .lgms-login-btn, .lgms-signup-btn {
            height:45px !important; margin-top:4px !important; border:0 !important;
            border-radius:9px !important;
            background:linear-gradient(135deg,#13a56e,#07945f) !important;
            color:#ffffff !important; font-size:14px !important; font-weight:700 !important;
            box-shadow:0 7px 16px rgba(22,163,111,.20) !important;
            transition:transform .15s ease, box-shadow .15s ease;
          }

          .lgms-login-btn:hover, .lgms-signup-btn:hover {
            color:#ffffff !important; transform:translateY(-1px);
            box-shadow:0 10px 20px rgba(22,163,111,.26) !important;
          }

          .lgms-login-btn:active, .lgms-signup-btn:active { transform:translateY(0); }

          .lgms-divider {
            display:flex; align-items:center; gap:10px; margin:19px 0 15px;
            color:#a1aaa5; font-size:10px; font-weight:600;
          }

          .lgms-divider::before, .lgms-divider::after {
            content:''; flex:1; height:1px; background:#e8eeeb;
          }

          .lgms-create-text { color:#7b8580; font-size:12px; margin:0; }

          .lgms-link {
            color:#0c9b68 !important; font-weight:700; text-decoration:none !important;
          }

          .lgms-link:hover {
            color:#087d54 !important; text-decoration:underline !important;
          }

          .lgms-back-text { margin-top:17px !important; }

          .lgms-footer {
            border-top:1px solid #edf1ef; margin-top:25px; padding-top:15px;
            color:#7f8984; font-size:10.5px; line-height:1.7;
          }

          .lgms-footer-brand {
            color:#145a43; font-size:10.5px; font-weight:700;
          }

          .lgms-right-panel {
            position:relative; width:54%; min-height:100vh; overflow:hidden;
            display:flex; align-items:center; justify-content:center;
            background:linear-gradient(135deg,#f3fbeF 0%,#e4f3d8 48%,#d4eaca 100%);
          }

          .lgms-circle {
            position:absolute; border-radius:50%; pointer-events:none;
          }

          .lgms-circle-one {
            width:650px; height:650px; top:-250px; right:-170px;
            background:rgba(255,255,255,.38);
          }

          .lgms-circle-two {
            width:520px; height:520px; bottom:-250px; left:-190px;
            background:rgba(22,163,111,.065);
          }

          .lgms-visual-card {
            position:relative; z-index:2; width:88%; height:82%; min-height:430px;
            display:flex; align-items:center; justify-content:center;
            border-radius:28px; padding:24px; background:rgba(255,255,255,.22);
            border:1px solid rgba(255,255,255,.50);
          }

          .lgms-hero-image {
            width:100%; height:100%; object-fit:contain; display:block;
            filter:drop-shadow(0 15px 22px rgba(43,80,54,.12));
          }

          .lgms-image-fallback {
            width:100%; height:100%; align-items:center; justify-content:center;
            flex-direction:column; text-align:center; color:#145a43;
          }

          .lgms-image-fallback h3 { margin:0 0 7px; font-size:21px; }

          .lgms-image-fallback p {
            max-width:300px; margin:0; color:#6c7972; font-size:12px; line-height:1.5;
          }

          .lgms-right-label {
            position:absolute; z-index:5; right:28px; bottom:22px;
            color:#145a43; font-size:11px; font-weight:700; opacity:.68;
          }

          #", ns("login_error"), ", #", ns("signup_error"), " { text-align:left; }

          #", ns("login_error"), " .alert, #", ns("signup_error"), " .alert {
            border-radius:8px; font-size:11px; margin-top:9px !important;
            margin-bottom:0 !important;
          }

          @media (max-width:1000px) {
            .lgms-left-panel { width:100%; min-width:0; padding:22px; }
            .lgms-right-panel { display:none !important; }
            .lgms-login-card { max-width:460px; }
          }

          @media (max-width:520px) {
            .lgms-left-panel { padding:14px; }
            .lgms-login-card { padding:26px 20px 20px; border-radius:17px; }
            .lgms-title { font-size:20px; }
            .lgms-heading { font-size:22px; }
          }
          "
        )
      )
    )
  )
}

# ==============================================================================
# SERVER
# ==============================================================================

mod_login_server <- function(id, log_activity = function(...) invisible(NULL)) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    auth <- reactiveValues(
      logged_in = FALSE,
      role = NULL,
      username = NULL,
      full_name = NULL,
      user_id = NULL,
      permissions = character(0)
    )

    # --------------------------------------------------------------------------
    # Central permission checker.
    # Use from the rest of the application:
    #   req(auth$can_access("animals"))
    # --------------------------------------------------------------------------
    auth$can_access <- function(feature) {
      isTRUE(auth$logged_in) &&
        !is.null(feature) &&
        feature %in% auth$permissions
    }

    # --------------------------------------------------------------------------
    # LOGIN
    # --------------------------------------------------------------------------
    observeEvent(input$login_btn, {

      email <- trimws(input$login_user %||% "")
      password <- input$login_pass %||% ""

      if (!nzchar(email) || !nzchar(password)) {
        output$login_error <- renderUI({
          div(
            class = "alert alert-warning py-1 px-2 mt-2 mb-0 small",
            "Please enter both email and password."
          )
        })
        return()
      }

      result <- tryCatch(
        get_user_by_email(email),
        error = function(e) {
          message("Login database error: ", conditionMessage(e))
          NULL
        }
      )

      ok <- FALSE

      if (!is.null(result) && nrow(result) == 1) {
        ok <- tryCatch(
          sodium::password_verify(
            result$password_hash[1],
            password
          ),
          error = function(e) {
            message("Password verification error: ", conditionMessage(e))
            FALSE
          }
        )
      }

      if (!ok) {
        output$login_error <- renderUI({
          div(
            class = "alert alert-danger py-1 px-2 mt-2 mb-0 small",
            "Invalid email or password."
          )
        })
        return()
      }

      # ------------------------------------------------------------------------
      # AUTHENTICATED USER STATE
      # ------------------------------------------------------------------------
      auth$logged_in <- TRUE
      auth$role <- tryCatch(
        normalize_role(result$role_name[1]),
        error = function(e) tolower(trimws(as.character(result$role_name[1])))
      )
      auth$username <- result$email[1]
      auth$full_name <- result$full_name[1]
      auth$user_id <- result$users_id[1]
      auth$permissions <- lgms_permissions_for_role(auth$role)

      output$login_error <- renderUI(NULL)

      # Hide login screen.
      session$sendCustomMessage(
        "lgms-hide",
        ns("loginOverlay")
      )

      # Apply role to navigation elements marked with:
      # data-lgms-permission="animals"
      session$sendCustomMessage(
        "lgms-apply-role",
        list(
          role = auth$role,
          permissions = auth$permissions
        )
      )

      # Existing admin-tab compatibility.
      session$sendCustomMessage(
        "lgms-run-js",
        if ("admin" %in% auth$permissions) {
          '$(\'a[data-value="admin_tab"]\').closest("li").show();'
        } else {
          '$(\'a[data-value="admin_tab"]\').closest("li").hide();'
        }
      )

      tryCatch(
        log_activity(
          user_id = auth$user_id,
          action = "LOGIN",
          details = paste0("Successful login; role=", auth$role)
        ),
        error = function(e) {
          message("Activity log error: ", conditionMessage(e))
        }
      )

      showNotification(
        paste0(
          "Welcome, ",
          ifelse(
            nzchar(auth$full_name %||% ""),
            auth$full_name,
            auth$username
          ),
          "!"
        ),
        type = "message"
      )
    })

    # --------------------------------------------------------------------------
    # LOGOUT
    # --------------------------------------------------------------------------
    observeEvent(input$logout_btn, {

      tryCatch(
        log_activity(
          user_id = auth$user_id,
          action = "LOGOUT",
          details = "User logged out"
        ),
        error = function(e) invisible(NULL)
      )

      auth$logged_in <- FALSE
      auth$role <- NULL
      auth$username <- NULL
      auth$full_name <- NULL
      auth$user_id <- NULL
      auth$permissions <- character(0)

      updateTextInput(session, "login_user", value = "")
      updateTextInput(session, "login_pass", value = "")

      output$login_error <- renderUI(NULL)
      output$signup_error <- renderUI(NULL)

      # Remove role from page and hide all permission-controlled navigation.
      session$sendCustomMessage(
        "lgms-apply-role",
        list(role = "", permissions = character(0))
      )

      session$sendCustomMessage("lgms-show", ns("loginOverlay"))
      session$sendCustomMessage("lgms-show-block", ns("loginView"))
      session$sendCustomMessage("lgms-hide", ns("signupView"))
    })

    # --------------------------------------------------------------------------
    # SHOW SIGNUP
    # --------------------------------------------------------------------------
    observeEvent(input$show_signup, {

      output$login_error <- renderUI(NULL)
      output$signup_error <- renderUI(NULL)

      session$sendCustomMessage("lgms-hide", ns("loginView"))
      session$sendCustomMessage("lgms-show-block", ns("signupView"))
    })

    # --------------------------------------------------------------------------
    # BACK TO LOGIN
    # --------------------------------------------------------------------------
    observeEvent(input$show_login, {

      output$login_error <- renderUI(NULL)
      output$signup_error <- renderUI(NULL)

      session$sendCustomMessage("lgms-hide", ns("signupView"))
      session$sendCustomMessage("lgms-show-block", ns("loginView"))
    })

    # --------------------------------------------------------------------------
    # SIGNUP
    # New accounts receive the operator role by default.
    # --------------------------------------------------------------------------
    observeEvent(input$signup_btn, {

      uname <- trimws(input$signup_user %||% "")
      fname <- trimws(input$signup_fullname %||% "")
      pass1 <- input$signup_pass %||% ""
      pass2 <- input$signup_pass2 %||% ""

      err <- NULL

      if (fname == "") {
        err <- "Please enter your full name."
      } else if (uname == "") {
        err <- "Please enter your email."
      } else if (
        !grepl(
          "^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$",
          uname
        )
      ) {
        err <- "Please enter a valid email address."
      } else if (nchar(pass1) < 4) {
        err <- "Password must be at least 4 characters."
      } else if (pass1 != pass2) {
        err <- "Passwords do not match."
      }

      existing <- NULL

      if (is.null(err)) {
        existing <- tryCatch(
          get_user_by_email(uname),
          error = function(e) NULL
        )

        if (!is.null(existing) && nrow(existing) > 0) {
          err <- "That email is already registered."
        }
      }

      if (!is.null(err)) {
        output$signup_error <- renderUI({
          div(
            class = "alert alert-danger py-1 px-2 mt-2 mb-0 small",
            err
          )
        })
        return()
      }

      ok <- tryCatch({

        default_role <- dbGetQuery(
          db_pool,
          "SELECT role_id
           FROM role
           WHERE lower(role_name) LIKE '%operator%'
           LIMIT 1"
        )

        default_role_id <- if (
          nrow(default_role) == 1
        ) {
          default_role$role_id[1]
        } else {
          NA_integer_
        }

        if (is.na(default_role_id)) {
          stop("No operator role exists in the role table.")
        }

        dbExecute(
          db_pool,
          "INSERT INTO users
             (full_name, email, password_hash, role_id, created_at)
           VALUES
             ($1, $2, $3, $4, NOW())",
          params = list(
            fname,
            uname,
            sodium::password_store(pass1),
            default_role_id
          )
        )

        TRUE

      }, error = function(e) {

        message("Signup database error: ", conditionMessage(e))
        FALSE
      })

      if (!ok) {

        output$signup_error <- renderUI({
          div(
            class = "alert alert-danger py-1 px-2 mt-2 mb-0 small",
            "Could not create account — please try again."
          )
        })

        return()
      }

      output$signup_error <- renderUI({
        div(
          class = "alert alert-success py-1 px-2 mt-2 mb-0 small",
          "Account created! You can log in now."
        )
      })

      updateTextInput(session, "login_user", value = uname)
      updateTextInput(session, "signup_user", value = "")
      updateTextInput(session, "signup_fullname", value = "")
      updateTextInput(session, "signup_pass", value = "")
      updateTextInput(session, "signup_pass2", value = "")

      session$sendCustomMessage("lgms-hide", ns("signupView"))
      session$sendCustomMessage("lgms-show-block", ns("loginView"))
    })

    # Return authentication state to app.R.
    return(auth)
  })
}