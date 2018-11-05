library(shiny)

shinyTeam <- readLines("members.txt")
shinyApp(
  ui =  fluidPage(
    h1("Shiny Standup Order"),
    selectizeInput(
      "team_names",
      "Names: ",
      shinyTeam,
      selected = shinyTeam,
      multiple = TRUE
    ),
    verbatimTextOutput("team_order"),
    checkboxInput(
      "auto_open", "Automatically Open Zoom",
      value = TRUE
    ),
    checkboxInput(
      "open_zoom", "Open Standup Zoom: ",
      value = FALSE
    ),
    uiOutput("zoom_iframe")
  ),
  server = function(input, output, session) {

    auto_open_cutoff_sec <- 30

    standup_time <- function() {
      lubridate::ymd_hms(paste(Sys.Date(), "17-00-00"), tz = "UTC")
    }
    now <- function() {
      Sys.time()
    }

    time_diff <- reactive({
      invalidateLater(1000)
      timediff <- base::difftime(standup_time(), now(), units = "secs")
      timediff
    })

    should_open <- reactive({
      abs(as.numeric(time_diff())) < auto_open_cutoff_sec
    })

    auto_label <- reactive({
      if (input$auto_open) {
        if (should_open()) {
          "Automatically Open Zoom: Opening!"
        } else {
          ret <- paste0(
            "Automatically Open Zoom: ~ "
            # ,
            # format(
            #   standup_time(),
            #   format = "%I:%M:%S %p",
            #   tz = lubridate::tz(Sys.time())
            # )
          )

          mins <- as.numeric(floor(time_diff() / 60))
          if (time_diff() < 60 && time_diff() >= 0) {

            ret <- paste0(ret, mins, " mins")
          } else {
            hours <- round(mins / 60) %% 24
            ret <- paste0(ret, hours, " hours")
          }

          ret
        }
      } else {
        "Automatically Open Zoom"
      }
    })

    observe({
      if (input$auto_open && (!input$open_zoom)) {
        if (should_open()) {
          updateCheckboxInput(session, "open_zoom", value = TRUE)
        }
      }
    })

    observe({
      updateCheckboxInput(session, "auto_open", label = auto_label())
    })

    output$team_order <- renderText({
      set.seed(as.integer(Sys.Date()))
      paste(
        seq_along(input$team_names), ". ", sample(input$team_names),
        sep = "", collapse = "\n"
      )
    })

    output$zoom_iframe <- renderUI({
      if (!input$open_zoom) return(NULL)
      tags$iframe(
        width = "500px", height = "500px",
        src = "https://bit.ly/shiny-standup"
        # src = "https://bit.ly/barret-zoom"
      )
    })
  }
)
