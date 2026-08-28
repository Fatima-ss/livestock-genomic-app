cat("DEBUG 1: starting global.R\n")
library(shiny)
cat("DEBUG 2: shiny loaded\n")
library(shinyjs)
cat("DEBUG 3: shinyjs loaded\n")
library(bslib)
cat("DEBUG 4: bslib loaded\n")
library(bsicons)
cat("DEBUG 5: bsicons loaded\n")
library(DT)
cat("DEBUG 6: DT loaded\n")
library(plotly)
cat("DEBUG 7: plotly loaded\n")
library(DBI)
cat("DEBUG 8: DBI loaded\n")
library(RPostgres)
cat("DEBUG 9: RPostgres loaded\n")
library(pool)
cat("DEBUG 10: pool loaded\n")
library(sodium)
cat("DEBUG 11: sodium loaded\n")
library(leaflet)
cat("DEBUG 12: leaflet loaded\n")
# Standard lactation period (days) used across dashboard/report calculations
LACTATION_PERIOD_DAYS <- 305

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# ------------------------------------------------------------------------------
# DATABASE CONNECTION (PostgreSQL)
# Credentials come from .Renviron (never hardcode them here).
# ------------------------------------------------------------------------------

app_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA)
if (is.na(app_dir) || app_dir == "") app_dir <- getwd()
renviron_path <- file.path(app_dir, ".Renviron")
if (file.exists(renviron_path)) {
  readRenviron(renviron_path)
} else if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

required_vars <- c("PG_HOST", "PG_PORT", "PG_DB", "PG_USER", "PG_PASSWORD")
missing_vars <- required_vars[Sys.getenv(required_vars) == ""]
if (length(missing_vars) > 0) {
  stop(
    "Missing required environment variable(s): ", paste(missing_vars, collapse = ", "),
    ". Make sure a '.Renviron' file with these values exists in the app's root folder: ",
    app_dir
  )
}

pg_port <- suppressWarnings(as.integer(Sys.getenv("PG_PORT")))
if (is.na(pg_port)) {
  stop("PG_PORT in .Renviron is not a valid number: '", Sys.getenv("PG_PORT"), "'")
}

db_pool <- dbPool(
  RPostgres::Postgres(),
  host     = Sys.getenv("PG_HOST"),
  port     = pg_port,
  dbname   = Sys.getenv("PG_DB"),
  user     = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASSWORD"),
  sslmode  = "require"
)

# ------------------------------------------------------------------------------
# SHARED HELPER FUNCTIONS (used by more than one module)
# ------------------------------------------------------------------------------

normalize_role <- function(role_name) {
  rn <- tolower(role_name)
  if (grepl("admin", rn)) return("admin")
  if (grepl("manager", rn)) return("manager")
  if (grepl("supervisor", rn)) return("supervisor")
  return("operator")
}

sanitize_input <- function(x) {
  x <- gsub("<[^>]*>", "", x)
  x <- trimws(x)
  x
}

get_user_by_email <- function(email) {
  dbGetQuery(db_pool, "
    SELECT u.users_id, u.full_name, u.email, u.password_hash, r.role_name
    FROM users u JOIN role r ON u.role_id = r.role_id
    WHERE lower(u.email) = lower($1)
    LIMIT 1", params = list(email))
}

get_roles <- function() {
  dbGetQuery(db_pool, "SELECT role_id, role_name FROM role ORDER BY role_id")
}

generate_next_passport_id <- function() {
  last <- tryCatch(
    dbGetQuery(db_pool, "SELECT animal_code FROM animals WHERE animal_code IS NOT NULL ORDER BY animal_id DESC LIMIT 1"),
    error = function(e) data.frame()
  )
  if (nrow(last) == 0 || is.na(last$animal_code[1]) || last$animal_code[1] == "") {
    return("LSK-000001")
  }
  num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", last$animal_code[1])))
  if (is.na(num)) num <- 0
  sprintf("LSK-%06d", num + 1)
}

# ------------------------------------------------------------------------------
# LOAD MODULES
# ------------------------------------------------------------------------------
cat("DEBUG app_dir =", app_dir, "\n")
module_files <- list.files(file.path(app_dir, "modules"), pattern = "\\.R$", full.names = TRUE)
cat("DEBUG modules found =", length(module_files), "\n")
for (f in module_files) {
  tryCatch(source(f), error = function(e) cat("ERROR in", f, ":", conditionMessage(e), "\n"))
}