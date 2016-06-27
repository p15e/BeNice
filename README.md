# BeNice
Set of R functions to improve Rmd outputs and default figures.

# Functions 

## BeNiceAnotateThatPlot
Takes an already built ggplot and add text to the right of the main plot, or as a legend underneath.

Use:
###### Dummy data generationx = runif(9, 0, 125)
data = as.data.frame(x)
data$y = runif(9, 0, 125)

###### Dummy plot generation
superPlot = ggplot(data, aes(x, y)) +
        geom_point()

###### Anotate that dummy plot
annotedPlot = BeNiceAnotateThatPlot(superPlot, anotation = "The beach is pretty nice\nIsn't it ?", titleAnotation = "Title", titlePlot = "Super Nice Title")


## BeNiceGetTheme
General utility function to get a specific theme