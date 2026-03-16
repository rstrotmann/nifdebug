#' Interactive time metric comparison plot
#'
#' This function creates an interactive Shiny plot to compare different time
#' metrics (TIME, TAFD, TAD, NTIME) against each other. This is useful for
#' debugging and verifying the consistency of time-related fields in a NIF data
#' set. The x- and y-axis time metrics can be selected interactively via
#' dropdown menus.
#'
#' @param nif A nif object.
#' @inheritParams nif::plot.nif
#'
#' @return A Shiny app object.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   debug_time_plot(examplinib_poc_nif)
#'   debug_time_plot(examplinib_poc_nif, color = "ANALYTE")
#'   debug_time_plot(examplinib_poc_nif, analyte = "RS2023", facet = NULL)
#' }
debug_time_plot <- function(
    nif,
    analyte = NULL,
    dose = NULL,
    color = NULL,
    facet = "DOSE",
    min_time = NULL,
    max_time = NULL,
    size = 2,
    alpha = 0.5,
    log = FALSE,
    title = "Time metric relation",
    legend = TRUE,
    scales = "fixed",
    ...
) {
  # input validation
  nif:::validate_nif(nif)
  nif:::validate_argument(analyte, "character", allow_null = TRUE)
  nif:::validate_argument(dose, "numeric", allow_null = TRUE)
  nif:::validate_argument(color, "character", allow_null = TRUE)
  nif:::validate_argument(facet, "character", allow_null = TRUE)
  nif:::validate_argument(min_time, "numeric", allow_null = TRUE)
  nif:::validate_argument(max_time, "numeric", allow_null = TRUE)
  nif:::validate_argument(size, "numeric")
  nif:::validate_argument(alpha, "numeric")
  nif:::validate_argument(log, "logical")
  nif:::validate_argument(title, "character")
  nif:::validate_argument(legend, "logical")
  nif:::validate_argument(scales, "character")

  plot_data_set <- nif:::make_plot_data_set(
    nif, analyte, dose, time = "TAFD", color, min_time, max_time,
    facet = facet
  )

  plot_data <- plot_data_set$data |>
    filter(.data$EVID == 0)

  analyte_values <- unique(plot_data$ANALYTE)
  if (!is.null(analyte)) {
    analyte_values <- intersect(analyte_values, analyte)
  }
  if (length(analyte_values) == 0) {
    stop("No matching analyte values found in data.")
  }

  time_choices <- intersect(
    c("TIME", "TAFD", "TAD", "NTIME"), names(plot_data)
  )
  if (length(time_choices) < 2) {
    stop("At least two time metric fields are required in the data set!")
  }

  default_y <- ifelse("TAFD" %in% time_choices, "TAFD", time_choices[2])

  ui <- shiny::fluidPage(
    title = "Time metric comparison",
    shiny::fluidRow(
      style = "padding: 10px;",
      shiny::column(
        3,
        shiny::selectInput(
          "x_time", label = "x-axis time metric",
          choices = time_choices, selected = time_choices[1]
        )
      ),
      shiny::column(
        3,
        shiny::selectInput(
          "y_time", label = "y-axis time metric",
          choices = time_choices, selected = default_y
        )
      ),
      shiny::column(
        6,
        shiny::checkboxGroupInput(
          "analytes", "Analytes",
          choices = analyte_values,
          selected = analyte_values,
          inline = TRUE
        )
      )
    ),
    shiny::hr(),
    shiny::plotOutput("time_plot", height = "600px")
  )

  server <- function(input, output, session) {
    plot_data_r <- shiny::reactive({
      d <- plot_data
      if (!is.null(input$analytes) && length(input$analytes) > 0) {
        d <- d[d$ANALYTE %in% input$analytes, , drop = FALSE]
      }
      d
    })

    output$time_plot <- shiny::renderPlot({
      shiny::req(input$x_time, input$y_time)

      d <- plot_data_r()

      p <- d |>
        ggplot2::ggplot(ggplot2::aes(
          x = .data[[input$x_time]],
          y = .data[[input$y_time]],
          color = .data$COLOR
        )) +
        ggplot2::geom_point(size = size, alpha = alpha, na.rm = TRUE)

      if (!is.null(plot_data_set$facet)) {
        if (length(unique(d$FACET)) > 1) {
          p <- p + ggplot2::facet_wrap(~FACET, scales = scales)
        }
      }

      if (isTRUE(log)) {
        p <- p +
          ggplot2::scale_x_log10() +
          ggplot2::scale_y_log10()
      }

      p <- p +
        ggplot2::labs(
          x = input$x_time,
          y = input$y_time,
          color = nice_enumeration(plot_data_set$color)
        ) +
        ggplot2::theme_bw(16) +
        ggplot2::theme(
          legend.position = ifelse(
            isTRUE(legend) & length(plot_data_set$color) > 0, "bottom", "none"
          )
        ) +
        ggplot2::ggtitle(title) +
        watermark(cex = 1.5)

      p
    })
  }

  shiny::shinyApp(ui, server)
}
