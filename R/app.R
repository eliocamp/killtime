#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

info_icon <- function(text) {
    shiny::tags$i(
        class = "fa fa-info-circle",
        style = "color: #3689e6",
        title = text,
    )
}


ui <- function(type = c("lt", "ld")) {
    type <- type[1]

    if (type == "lt") {
        title <- "Claculadora de Tiempo Letal"
        percentil <- "Percentil de tiempo letal"
        x_lab <- "Tiempo"
        y_lab <- "% de animales muertos\n(corregido por mortalidad en grupo control)"
        transformations <- c(Logit = "logit",
                             "Complementary Log-log" = "cll",
                             Probit = "probit",
                             "No transformation" = "identity")

    } else {
        title <- "Claculadora de Dosis Letal"
        percentil <- "Percentil de dosis letal"
        x_lab <- "Dosis"
        y_lab <- "% de animales muertos"
        transformations <- c(Logit = "logit",
                             "Complementary Log-log" = "cll",
                             Probit = "probit")
    }







    mime_types <- c("text/csv", "application/vnd.ms-excel",
                    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    miniUI::miniPage(
        miniUI::gadgetTitleBar(title, left = NULL,
                               right = miniUI::miniTitleBarButton("done", "Salir")),
        miniUI::miniTabstripPanel(
            miniUI::miniTabPanel("Archivo", icon = shiny::icon("table"),
              miniUI::miniContentPanel(
                  shiny::fluidPage(
                      shiny::fluidRow(
                          shiny::column(6, shiny::fileInput("file_input",
                                                            "Elegir archivo (csv o excel)",
                                                            accept = mime_types,
                                                            width = "100%")),
                          shiny::column(6, shiny::uiOutput("sheet_select"))
                      ),
                      shiny::fluidRow(
                          shiny::column(12, shiny::h4("Previsualización de datos:"), shiny::dataTableOutput("raw_data"))
                      )
                  )
              )
              ),
            miniUI::miniTabPanel("Datos", icon = shiny::icon("table"),
                miniUI::miniContentPanel(
                    shiny::fluidPage(
                        shiny::fluidRow(
                            shiny::column(2, shiny::uiOutput("time_dropdown")),
                            shiny::column(3, shiny::uiOutput("experiment_dropdown")),
                            shiny::column(ifelse(type == "lt", 1, 2), shiny::uiOutput("exp_n")),
                            if (type == "lt") shiny::column(3, shiny::uiOutput("control_dropdown")),
                            if (type == "lt") shiny::column(1, shiny::uiOutput("control_n")),
                            shiny::column(ifelse(type == "lt", 2, 3), shiny::selectInput("trans_y",
                                                                list("Transformación",
                                                                     info_icon("Tipo de transformación a aplicar a los datos")),
                                                                transformations))
                            ),
                        shiny::fluidRow(
                            shiny::column(12,
                              shiny::h4("Datos seleccionados:"),
                              shiny::dataTableOutput("all_data"))
                            )
                        )
                    )
            ),
            miniUI::miniTabPanel("Resultado y plot", icon =  shiny::icon("chart-line"),
              miniUI::miniContentPanel(
                  shiny::fluidPage(
                      shiny::fluidRow(
                          shiny::column(4, shiny::numericInput("lt_level", percentil,
                                                               50, min = 0, max = 100)),
                          shiny::column(4, shiny::numericInput("lt_alpha",
                                                               list("Alfa",
                                                                    info_icon("Alfa para el intervalo de confianza")),
                                                               0.05, min = 0.001, max = 0.5)),
                          shiny::column(4, shiny::verbatimTextOutput("LT_text")),
                          shiny::tags$style(type='text/css', "#LT_text { width:100%; margin-top: 25px;}")
                          ),
                      shiny::fluidRow(
                          shiny::column(12, shiny::imageOutput("plot_saved", width = "auto", height = "auto")),
                          ),
                      shiny::fluidRow(
                          shiny::column(width = 6,
                            shiny::textAreaInput("x_lab", "Etiqueta x", x_lab, rows = 1)),
                          shiny::column(width = 6,
                            shiny::textAreaInput("y_lab", "Etiqueta y", y_lab)),
                      ),
                      shiny::fluidRow(
                          shiny::column(width = 4, shiny::numericInput("width", "Ancho (px)", value = 3200)),
                          shiny::column(width = 4, shiny::numericInput("height", "Alto (px)", value = 1500)),
                          shiny::column(width = 4, shiny::numericInput("text_size", "Tamaño del texto", 13, min = 0)),
                      ),
                      shiny::fluidRow(
                          shiny::column(12, shiny::downloadButton("save_plot", "Guardar plot"))
                      )
                  )
              )
            )
        )
    )
}


# Define server logic required to draw a histogram
server <- function(type = c("lt", "ld")) {
    type <- type[1]
    function(input, output) {

        output$sheet_select <- shiny::renderUI({
            if (!is.null(input$file_input)) {
                file_extension <- tools::file_ext(input$file_input$datapath)
                is_excel <- !is.na(readxl::excel_format(input$file_input$datapath, guess = TRUE))

                if (is_excel) {
                    sheets <- readxl::excel_sheets(input$file_input$datapath)
                } else {
                    sheets <- NULL
                }

                if (is.null(sheets)) {
                    NULL
                }

                shiny::selectInput("sheet_list", list("Hoja", info_icon("Seleccionar la hoja donde están los datos")),
                                   choices = sheets)

            } else {
                NULL
            }
        })

        all_data <- shiny::reactive({
            if (!is.null(input$file_input)) {
                file_extension <- tools::file_ext(input$file_input$datapath)
                is_excel <- !is.na(readxl::excel_format(input$file_input$datapath, guess = TRUE))
                if (is_excel) {
                    data <- try(readxl::read_excel(input$file_input$datapath,
                                                   sheet = input$sheet_list),
                                silent = TRUE)
                } else if (file_extension == "csv") {
                    data <- try(as.data.frame(data.table::fread(input$file_input$datapath)),
                                silent = TRUE)
                } else {
                    data <- data.frame()
                }
                if (!inherits(data, "try-error")) {
                    colnames(data) <- make.names(colnames(data), unique = TRUE)
                    data
                }

            } else {
                data.frame()
            }
        })

        columns <- reactive({
            colnames(all_data())
        })





        output$raw_data <- shiny::renderDataTable(all_data())
        output$all_data <- shiny::renderDataTable(data())

        data_columns <- reactive({
            c(input$time_col,
              input$exp_col,
              input$control_col)
        })


        data <- shiny::reactive({
            if (nrow(all_data() != 0)) {
                data <- all_data()[, data_columns()]
                colnames(data) <- c("Tiempo", "Grupo tratamiento", "Grupo control")
            } else {
                data <- all_data()
            }

            data
        }

        )

        if (type == "lt") {


            output$time_dropdown <- shiny::renderUI(

                shiny::selectInput("time_col", list("Tiempo", info_icon("Seleccionar la columna de tiempo")),
                                   columns(), selected = columns()[1])

            )

            output$experiment_dropdown <- shiny::renderUI(
                shiny::selectInput("exp_col", list("Grupo tratamiento",
                                                   info_icon("Seleccionar la columna con el grupo tratamiento")),
                                   columns(), selected = columns()[2])
            )

            output$exp_n <- shiny::renderUI(
                shiny::numericInput("exp_n", list("N", info_icon("Cantidad total de animales en el grupo tratamiento")),
                                    value = max(all_data()[[input$exp_col]]))
            )

            output$control_dropdown <- shiny::renderUI(
                shiny::selectInput("control_col", list("Grupo control",
                                                       info_icon("Seleccionar la columna con el grupo control")),
                                   columns(), selected = columns()[3])
            )

            output$control_n <- shiny::renderUI(
                shiny::numericInput("control_n", list("N", info_icon("Cantidad total de animales en el grupo control")),
                                    value = max(all_data()[[input$exp_col]]))
            )


            model <- shiny::reactive(lt_fit(times = data()[["Tiempo"]],
                                               dead_experiment = data()[["Grupo tratamiento"]],
                                               dead_control = data()[["Grupo control"]],
                                               n_experiment =input$exp_n,
                                               n_control = input$control_n,
                                               trans_y = input$trans_y))
            plot <- shiny::reactive({
                plot.killtime_lt_fit(model(),
                                     p = input$lt_level/100,
                                     alpha_lt = input$lt_alpha,
                                     x_lab = input$x_lab,
                                     y_lab = input$y_lab) +
                    ggplot2::theme_minimal(base_size = input$text_size)
            })

            LT_text <- shiny::reactive({
                p <-  input$lt_level/100
                LT <- lethal_time(model(), p = p, input$lt_alpha)

                pretty_LT(LT, p)
            })
        } else if (type == "ld") {

            output$time_dropdown <- shiny::renderUI(

                shiny::selectInput("time_col", list("Dosis", info_icon("Seleccionar la columna de dosis")),
                                   columns(), selected = columns()[1])

            )

            output$experiment_dropdown <- shiny::renderUI(
                shiny::selectInput("exp_col", list("Muertos",
                                                   info_icon("Seleccionar la columna con los animales muertos")),
                                   columns(), selected = columns()[2])
            )

            output$exp_n <- shiny::renderUI(
                shiny::numericInput("exp_n", list("N", info_icon("Cantidad total de animales")),
                                    value = max(all_data()[[input$exp_col]]))
            )

            output$control_dropdown <- shiny::renderUI(NULL)

            output$control_n <- shiny::renderUI(NULL)

            model <- shiny::reactive(ld_fit(dose = data()[["Tiempo"]],
                                     dead = data()[["Grupo tratamiento"]],
                                     n = input$exp_n,
                                     trans_y = input$trans_y))
            plot <- shiny::reactive({
                plot.killtime_ld_fit(model(),
                                     p = input$lt_level/100,
                                     alpha_ld = input$lt_alpha,
                                     x_lab = input$x_lab,
                                     y_lab = input$y_lab) +
                    ggplot2::theme_minimal(base_size = input$text_size)
            })

            LT_text <- shiny::reactive({
                p <-  input$lt_level/100
                LT <- lethal_dose(model(), p = p, input$lt_alpha)

                pretty_LD(LT, p)
            })
        }





        output$plot <- shiny::renderPlot(plot())





        output$plot_saved <- shiny::renderImage({
            plot_file <- tempfile(fileext = ".png")
            ggplot2::ggsave(plot = plot(), plot_file,
                            width = input$width/300,
                            height = input$height/300, dpi = 300)
            list(src = plot_file,
                 alt = "plot",
                 width = "100%",
                 height = "auto")
        }, deleteFile = TRUE)

        output$LT_text <- shiny::renderText(LT_text())


        output$save_plot <- shiny::downloadHandler(
            filename = function() {
                paste0("LT", input$lt_level, ".png")
            },
            content = function(file) {
                ggplot2::ggsave(plot = plot(), file,
                                width = input$width/300,
                                height = input$height/300, dpi = 300)
            }
        )

        shiny::observeEvent(input$done , shiny::stopApp())

    }

}

# Run the application

#' @export
lethal_time <- function(options = list()) {
    run_app(type = "lt", options = options)
}


run_app <- function(type = c("lt", "ld"), options = list()) {
    options$width <- "2000px"
    type <- type[1]
    # vwr = shiny::dialogViewer('MyAppName', width = 1600, height = 1200)
    # shiny::runGadget(ui(type), server = server(type), viewer = vwr)
    shiny::shinyApp(ui = ui(type),
                    server = server(type),
                    options = options)
}
