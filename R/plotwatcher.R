#' Interactive plot visualizer
#'
#' Allows the user to browse a list of ggplot2 plots either one by one, or all together using patchwork.
#'
#' @param plotlist A list of ggplot2 plot objects.
#'
#' @return Displays plots interactively. Does not return a value.
#' @export
#'
#' @importFrom ggplot2 theme
#' @importFrom patchwork wrap_plots
#'
#' @examples plotwatcher(plotlist)
plotwatcher <- function(plotlist) {
  cat("Welcome to plotwatcher, choose between following:")
  option <- readline(prompt = "Select an option: \n(1) Visualize plot by plot\n(2) Visualize plots together\n(3) Exit\n")
  i <- 1

  while (option != "3") {
    if (option == "1") {
      #Option (1)
      repeat {
        cat("Visualizing plot by plot\n")
        print(plotlist[[i]])  # Mostrar el gráfico actual
        plotindex <- readline(prompt = "Choose an option: \n(1) Next plot\n(2) Previous plot\n(3) Exit\n")

        if (plotindex == "1" && i < length(plotlist)) {
          i <- i + 1  # Next plot
        } else if (plotindex == "2" && i > 1) {
          i <- i - 1  # Go back to previous plot
        } else if (plotindex == "3") {
          cat("Exiting plot by plot visualizer\n")
          break  # Terminate function
        } else {
          cat("Invalid option or end of plot list\n")
          break # Go back to main menu
        }
      }
    } else if (option == "2") {
      cat("Visualizing all plots together\n")
      library(patchwork)
      combined <- patchwork::wrap_plots(lapply(plotlist, function(p) p + theme(legend.position = "none")))
      print(combined)

      break  # Terminate function
    } else {
      cat("Invalid option. Please try again\n")
    }
    option <- readline(prompt = "Select an option: \n(1) Visualize plot by plot\n(2) Visualize plots together\n(3) Exit\n")
  }
  cat("Exiting the function.\n")
}
