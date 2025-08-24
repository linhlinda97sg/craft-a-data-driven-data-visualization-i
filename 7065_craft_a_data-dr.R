# Craft a Data-Driven Data Visualization Integrator

# Load necessary libraries
library(tidyverse)
library.ggplot2)
library(plotly)

# Define a data model class
DataVisualizationIntegrator <- R6::R6Class("DataVisualizationIntegrator",
  private = list(
    .data = NULL,
    .visualizations = list()
  ),
  
  public = list(
    initialize = function(data) {
      private$.data = data
    },
    
    add_viz = function(viz_type, viz_params) {
      private$.visualizations <- c(private$.visualizations, list(list(viz_type, viz_params)))
    },
    
    generate_visualizations = function() {
      for (viz in private$.visualizations) {
        viz_type <- viz[[1]]
        viz_params <- viz[[2]]
        
        if (viz_type == "bar_chart") {
          p <- ggplot2::ggplot(private$.data, aes_string(x = viz_params$x, y = viz_params$y)) + 
            geom_bar(stat = "identity")
          print(p)
        } else if (viz_type == "scatter_plot") {
          p <- ggplot2::ggplot(private$.data, aes_string(x = viz_params$x, y = viz_params$y)) + 
            geom_point()
          print(p)
        } else if (viz_type == "interactive_chart") {
          p <- plotly::ggplotly(private$.data, aes_string(x = viz_params$x, y = viz_params$y))
          print(p)
        }
      }
    }
  )
)

# Example usage
data <- data.frame(x = c(1, 2, 3, 4, 5), y = c(2, 4, 6, 8, 10))

viz_integrator <- DataVisualizationIntegrator$new(data)
viz_integrator$add_viz("bar_chart", list(x = "x", y = "y"))
viz_integrator$add_viz("scatter_plot", list(x = "x", y = "y"))
viz_integrator$add_viz("interactive_chart", list(x = "x", y = "y"))

viz_integrator$generate_visualizations()