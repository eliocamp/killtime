type <- "lt"
# vwr = shiny::dialogViewer('MyAppName', width = 1600, height = 1200)
# shiny::runGadget(ui(type), server = server(type), viewer = vwr)
shiny::shinyApp(ui = killtime:::ui(type),
                server = killtime:::server(type),
                options = options)
