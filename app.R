library(shiny)
library(promises)
# library(reactlog)
# options(shiny.reactlog = TRUE)


shinyTeam <- readLines("members.txt")
shinyApp(
  ui = fillPage(
    fillRow(
      flex=c(2,10),
      fillCol(
        flex = NA,
        style="padding:10px;",
        titlePanel("Shiny Standup Order"),
        uiOutput("team_order"),
        # checkboxInput(
        #   "auto_open", "Automatically Open Zoom",
        #   value = TRUE
        # ),
        actionButton(
          "open_zoom", "Open Standup Zoom",
          value = FALSE
        ),
        uiOutput("zoom_iframe")
      ),
      fillCol(
        uiOutput("dashboard", style = "top: 0; bottom: 0; left: 0; right: 0; position: absolute;")
      )
    )
  ),
  server = function(input, output, session) {

    cutoffs <- c(-10, 60 * 5)

    standup_time <- function() {
      lubridate::ymd_hms(paste(Sys.Date(), "12-15-00"), tz = "America/New_York")
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
      time_val <- as.numeric(time_diff())
      time_val >= cutoffs[1] && time_val <= cutoffs[2]
    })

    auto_label <- reactive({
      req(input$auto_open)
      if (input$auto_open) {
        if (should_open()) {
          "Automatically Open Zoom: Opening!"
        } else {
          ret <- "Automatically Open Zoom: ~ "

          mins <- as.numeric(floor(time_diff() / 60))
          if (mins <= 2 && mins >= 0) {
            ret <- paste0(ret, as.numeric(floor(time_diff())) + cutoffs[1], " s")
          } else if (mins < 90 && mins >= 0) {
            ret <- paste0(ret, mins, " mins")
          } else {
            hours <- (round(mins / 60) - 1) %% 24 + 1
            ret <- paste0(ret, hours, " hour", if (hours > 1) "s")
          }

          ret
        }
      } else {
        "Automatically Open Zoom"
      }
    })

    auto_open_zoom_fn <- reactive({
      # invlidate every 4 hours
      invalidateLater(1000 * 60 * 60 * 4)

      has_called <- FALSE
      function() {
        if (has_called) return()
        has_called <<- TRUE
        open_zoom_fn()
      }
    })

    observe({
      if (all(input$auto_open, should_open())) {
        # reactive plus function call
        auto_open_zoom_fn()()
      }
    })

    observe({
      updateCheckboxInput(session, "auto_open", label = auto_label())
    })

    output$team_order <- renderUI({
      # update every hour
      invalidateLater(1000 * 60 * 60 * 1)

      set.seed(as.integer(Sys.Date()))
      team <- sample(shinyTeam)
      do.call(
        htmltools::tags$ol,
        c(lapply(team, tags$li), list(style = "padding-inline-start: 20px;"))
      )
    })

    open_zoom_fn <- function() {
      output$zoom_iframe <- renderUI({
        tags$iframe(
          style="overflow: scroll;",
          width = "100px", height = "100px",
          src = "https://zoom.us/j/2341255846?pwd=emZUL3dESDNDLy9iYUFGV1FGQUR4QT09"
        )
      })

      # close iframe N seconds later
      later::later(function() {
        output$zoom_iframe <- NULL
      }, 15)
    }
    # if button is clicked, open zoom
    observeEvent(input$open_zoom, {
      open_zoom_fn()
    })

    output$dashboard <- renderUI({
      promise_resolve(TRUE) %...>% {
        Sys.sleep(0.25)
      } %...>% {
        tags$iframe(
          style="width: 100%; height: 100%; border: lightgrey; border-style: dashed; border-width: thin;",
          # src="https://beta.rstudioconnect.com/content/b1e2e5de-9f9a-4282-a235-0d4e0478cccf/"
          src = "https://rstudio.github.io/shinycoreci/results"
        )
      }
    })


  }
)
