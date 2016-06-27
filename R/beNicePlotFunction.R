#' BeNiceAnotateThatPlot
#'
#' Takes an already built ggplot and add text to the right of the main plot, or as a legend underneath.
#'
#' @examples 
#' \dontrun{
#' # Dummy data generation
#' x = runif(9, 0, 125)
#' data = as.data.frame(x)
#' data$y = runif(9, 0, 125) 
#' 
#' # Dummy plot generation
#' superPlot = ggplot(data, aes(x, y)) +
#'         geom_point()
#' 
#' # Anotate that dummy plot
#' annotedPlot = BeNiceAnotateThatPlot(superPlot, anotation = "The beach is pretty nice\nIsn't it ?", titleAnotation = "Title", titlePlot = "Super Nice Title")
#' }
#' @export

BeNiceAnotateThatPlot = function (dataPlot,
                                    anotation = "",
                                    anotationType = "right",
                                    titleAnotation = NULL,
                                    titlePlot = NULL,
                                    plotIt = T,
                                    fontFamily = 'Calibri',
                                    ...) {
        
        # set theme of the textplot (blank)
        themeComplete = BeNiceGetTheme("completeBlank")
        
        # depending on the anotationType position of the text is different
        switch(anotationType,
               right = {
                       
                       textData = data.frame(x=0, y=40, group = NA, lab=anotation, fontSize = 8, face = 'plain')
                       if (!is.null(titleAnotation)) {
                               textData[["y"]] = textData[["y"]] - 2
                               titleData = data.frame(x=0, y=40, group = NA, lab=titleAnotation, fontSize = 8, face = 'bold')
                               textData = rbind(textData, titleData)
                       }
                       
                       textPlot = ggplot(textData, aes_string(x = "x", y = "y")) +
                               geom_blank() +
                               geom_text(aes(x, y,
                                             label=lab,
                                             size = fontSize,
                                             fontface = face),
                                         vjust = "top",
                                         hjust = "left",
                                         family = fontFamily, show.legend = F) +
                               themeComplete + xlim(0,20) + ylim(0,40)
                       
                       
                       arrangedGrobs = arrangeGrob(dataPlot, textPlot, ncol=2, widths = c(2/3,1/3), top=titlePlot)
                       
               },
               legend = {
                       
                       textData = data.frame(x=0, y=10, group = NA, lab=anotation, fontSize = 8, face = 'plain')
                       if (!is.null(titleAnotation)) {
                               textData[["y"]] = textData[["y"]] - 2
                               titleData = data.frame(x=0, y=10, group = NA, lab=titleAnotation, fontSize = 8, face = 'bold')
                               textData = rbind(textData, titleData)
                       }
                       
                       textPlot = ggplot(textData, aes_string(x = "x", y = "y")) +
                               geom_blank() +
                               geom_text(aes(x, y,
                                             label=lab,
                                             size = fontSize,
                                             fontface = face),
                                         vjust = "top",
                                         hjust = "left",
                                         family = fontFamily, show.legend = F) +
                               themeComplete + xlim(0,40) + ylim(0,10)
                       
                       
                       arrangedGrobs = arrangeGrob(dataPlot, textPlot, ncol=1, heights = c(4/5,1/5), top=titlePlot)
               },
               {stop("anotationType invalid")})
        
        if (plotIt)
                grid.arrange(arrangedGrobs)
        
        return(arrangedGrobs)
}

##### THEMES ####
#' BeNiceGetTheme
#' 
#' General utility function to get a specific theme
#' 
#' @export
#' 

BeNiceGetTheme = function (theme = "completeBlank") {
        # Define themes - this would be cleaner if declared in a BeNice object - need only at init
        themeCompleteBlank = theme_bw() +
                theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),legend.position="none",
                      panel.background=element_blank(),panel.border=element_blank(),
                      panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank(),plot.background=element_blank())
        
        # Create a theme dictionary
        themeList = list(completeBlank = themeCompleteBlank)
        
        # Check if the theme asked for is in the dictionnary
        if (!(theme%in%names(themeList))) 
                stop("theme is invalid")
        
        # Return thee that was asked for
        return(themeList[[theme]])
        
}