# De https://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r/5076821
has_IP <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}


internet_test <- function(has_to_work = FALSE) {
  internet <- has_IP()

  if (!internet & has_to_work) {
    cat("Es necesario descargar paquetes y la computadora no est\u00e1 conectada a internet.\nCon\u00e9ctese a interent y trate nuevamente Aprete enter para cerrar.")
    invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))
    stop()
  } else {
    return(internet)
  }
}

install_stuff <- !requireNamespace("remotes", quietly = TRUE) | !requireNamespace("killtime", quietly = TRUE)

if (install_stuff) {
  internet_test(TRUE)

  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = "https://cloud.r-project.org")
  }

  if (!requireNamespace("killtime", quietly = TRUE)) {
    message("Instalando paquete\n (esto s\u00f3lo es necesario la primera vez)")
    remotes::install_github("eliocamp/killtime", upgrde = "always")
  }
}

internet <- internet_test(FALSE)
if (internet) {
  local_sha <- utils::packageDescription("killtime")$RemoteSha
  remote_sha <- remotes::remote_sha(remotes::github_remote("eliocamp/killtime"))

  if (!identical(local_sha, remote_sha)) {
    cat("Existe una nueva versi\u00f3n. ¿Actualizar? (si/no): ")
    response <- invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))

    if (response != "no") {
      message("Actualizando...")
      remotes::update_packages("killtime", upgrade = "always")
    }

  }
}



shiny::runApp(killtime::lethal_time_ui(), launch.browser = TRUE)
