% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beNicePlotFunction.R
\name{BeNiceAnotateThatPlot}
\alias{BeNiceAnotateThatPlot}
\title{BeNiceAnotateThatPlot}
\usage{
BeNiceAnotateThatPlot(dataPlot, anotation = "", anotationType = "right",
  titleAnotation = NULL, titlePlot = NULL, plotIt = T,
  fontFamily = "Calibri", ...)
}
\description{
Takes an already built ggplot and add text to the right of the main plot, or as a legend underneath.
}
\examples{
\dontrun{
# Dummy data generation
x = runif(9, 0, 125)
data = as.data.frame(x)
data$y = runif(9, 0, 125) 

# Dummy plot generation
superPlot = ggplot(data, aes(x, y)) +
        geom_point()

# Anotate that dummy plot
annotedPlot = BeNiceAnotateThatPlot(superPlot, anotation = "The beach is pretty nice\\nIsn't it ?", titleAnotation = "Title", titlePlot = "Super Nice Title")
}
}

