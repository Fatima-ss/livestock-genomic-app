# ==============================================================================
# MODULE: ANIMAL DIRECTORY & PASSPORT
# Displays the full animals table. Admin role can double-click to edit
# a small set of whitelisted columns.
# ==============================================================================

mod_directory_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(uiOutput(ns("directory_header_ui"), inline = TRUE)),
    DTOutput(ns("table_records"))
  )
}

# records_data: reactive df of animals
# auth: shared reactiveValues (logged_in, role, full_name, ...)
# bump_refresh: function to invalidate records_data elsewhere
# log_activity: function to append to the dashboard activity feed
mod_directory_server <- function(id, records_data, auth, bump_refresh, log_activity) {
  moduleServer(id, function(input, output, session) {

    output$directory_header_ui <- renderUI({
      if (isTRUE(auth$logged_in) && auth$role == "admin") {
        "Live Herd Database Directory (Double-Click Any Cell to Edit)"
      } else {
        "Live Herd Database Directory (View Only — editing requires Admin role)"
      }
    })

    output$table_records <- renderDT({
      df <- records_data()
      is_admin <- isTRUE(auth$logged_in) && auth$role == "admin"
      datatable(df[, setdiff(names(df), "animal_id")], rownames = FALSE, editable = is_admin,
                options = list(pageLength = 8, scrollX = TRUE))
    })

    editable_cols <- c(Animal_Name = "animal_name", Gender = "gender",
                       Date_of_Birth = "date_of_birth", Ear_Tag_No = "ear_tag_no", Status = "status")

    observeEvent(input$table_records_cell_edit, {
      req(auth$logged_in)
      if (auth$role != "admin") {
        showNotification("Only Admin can edit the Animal Directory.", type = "warning")
        bump_refresh()
        return()
      }
      info <- input$table_records_cell_edit
      df <- records_data()
      display_cols <- setdiff(names(df), "animal_id")
      col_name <- display_cols[info$col + 1]
      animal_id <- df$animal_id[info$row]

      if (!(col_name %in% names(editable_cols))) {
        showNotification("That field can't be edited — Passport ID and other reference fields are locked.", type = "warning")
        bump_refresh()
        return()
      }

      db_col <- editable_cols[[col_name]]
      ok <- tryCatch({
        dbExecute(db_pool, paste0("UPDATE animals SET ", db_col, " = $1, updated_at = CURRENT_TIME WHERE animal_id = $2"),
                  params = list(info$value, animal_id))
        TRUE
      }, error = function(e) FALSE)

      if (ok) log_activity(paste0(auth$full_name, " edited ", col_name, " for animal_id ", animal_id))
      else showNotification("Could not save that edit.", type = "error")
      bump_refresh()
    })
  })
}
