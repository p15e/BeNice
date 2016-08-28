#' BeNiceGroupLineAndBarPlot
#' 
#' @export
#' 

BeNiceGroupLineAndBarPlot = function (dataXYZGLine = NULL, summarySETable = NULL, legendLabels = NULL, legendBorders = "n",
                                      groupVariable, variablesLine, variablesBar,
                                      ticksLine = NULL, ticksBar = NULL,
                                      labelLine = "", labelBar = "", plotTitle = "",
                                      groupTicksLabels = NULL, labelGroup = "Group",
                                      separateLineAndBarColors = F) {
        if (is.null(dataXYZGLine) && is.null(summarySETable)) {
                stop("dataXYGLine or summarySETable need to be set  (one of them)");
        }
        
        if (!is.null(dataXYZGLine)) {
                ## create summarySE table from data
                meltedData = melt(dataXYZGLine, id.vars = groupVariable)
                summarySETable = summarySE(dataXYZGLine, measurevar = "value", groupvars = c("variable", "group"), na.rm=T)
        } 
        summarySETable = as.data.table(summarySETable)
        ## separate the two plot data
        lineDT = summarySETable[variable%in%variablesLine, c("variable", groupVariable, "value", "ci"), with=F ]
        
        
        ## get the range
        maxLine = max(lineDT[, value], na.rm = T)
        minLine = min(lineDT[, value], na.rm = T)
        rangeLine = maxLine - minLine
        
        
        
        
        ## compute the axis limits
        safeBorders = 0.1
        
        if (is.null(ticksLine)) {
                maxLineBorder = maxLine + safeBorders * rangeLine
                minLineBorder = minLine - safeBorders * rangeLine
                ticksLine = seq(signif(minLineBorder,1), signif(maxLineBorder,1), signif(rangeLine/5,1))  
        } else {
                rangeTicks = range(ticksLine)[2]-range(ticksLine)[1]
                maxLineBorder = max(ticksLine) + safeBorders * rangeTicks
                minLineBorder = min(ticksLine) - safeBorders * rangeTicks
        }
        
        
        
        ## groups
        groupsVector = unique(summarySETable[, group])
        if (is.null(groupTicksLabels)) {
                groupTicksLabels = groupsVector  
        }
        
        ticksGroup = seq(1.5, 1.5+(NROW(groupsVector)-1), 1)
        
        
        # bar parameters
        width = 0.25
        linewidth = 2
        barWidthPerVar = 2*width/NROW(variablesBar)
        qualitativeColors = c('#1f78b4','#33a02c','#e31a1c','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
        if ((NROW(variablesBar)==1)&&(separateLineAndBarColors==F)) {
                colorVariablesBar = "black" 
        } else {
                if (separateLineAndBarColors) {
                        colorVariablesBar = qualitativeColors[(NROW(variablesLine)+1):(NROW(variablesLine)+NROW(variablesBar))]   
                } else {
                        colorVariablesBar = qualitativeColors[1:NROW(variablesBar)] 
                }
                
        }
        if (NROW(variablesLine)==1) {
                colorVariablesLine = "black" 
        } else {
                colorVariablesLine = qualitativeColors[1:NROW(variablesLine)] 
        }
        
        
        
        ## Plot 1: RTs on first y-axis, errors on second y-axis
        
        plotsebargraph = function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
                w = wiskwidth/2
                segments(x0 = loc, x1 = loc, y0 = value, y1 = value + sterr, col = color, 
                         lwd = linewidth)
                segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
                         col = color, lwd = linewidth)  # upper whiskers
        }
        
        plotsegraph = function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
                w = wiskwidth/2
                segments(x0 = loc, x1 = loc, y0 = value - sterr, y1 = value + sterr, col = color, 
                         lwd = linewidth)
                segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
                         col = color, lwd = linewidth)  # upper whiskers
                segments(x0 = loc - w, x1 = loc + w, y0 = value - sterr, y1 = value - sterr, 
                         col = color, lwd = linewidth)  # lower whiskers
        }
        
        ## begin plots
        
        par(cex.main = 1.5, mar = c(5, 6, 4, 6) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
            font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
        # mpg = c(3, 1, 0) is default. first = axis labels!; middle = tick labels mar
        # = c(5, 4, 4, 2) + 0.1 is default
        
        digitsize <- 1.2
        x <- c(1, 2, 3, 4)
        plot(x, c(-10, -10, -10, -10), type = "p", ylab = labelLine, 
             xlab = " ", cex = 1.5, ylim = c(minLineBorder, maxLineBorder), xlim = c(1, NROW(groupsVector)+1), lwd = 2, pch = 5, col="white",
             axes = F, main = " ")
        
        axis(1, at = ticksGroup, labels = groupTicksLabels)
        mtext(labelGroup, side = 1, line = 3, cex = 1.5, font = 2)
        axis(2, at = ticksLine)
        
        title(plotTitle)
        if (!is.null(legendLabels)) {
                legend('topright', legendLabels , 
                       lty=1, lwd=2, col=c(colorVariablesLine, colorVariablesBar)[1:NROW(legendLabels)], bty=legendBorders, cex=1)
        }
        
        
        for (i in 1:NROW(variablesLine)) {
                values = lineDT[variable == variablesLine[i], value]
                ci = lineDT[variable == variablesLine[i], ci]
                topText = (values + ci)*1.1
                print(points(ticksGroup, values, cex = 1.5, lwd = 2, pch = 19-i, col=colorVariablesLine[i]))
                plot.errbars = plotsegraph(ticksGroup, values, lineDT[variable == variablesLine[i], ci], 0.1, col=colorVariablesLine[i])
                print(plot.errbars)
                print(lines(ticksGroup, values , lwd = 2, type = "c", col=colorVariablesLine[i]))
                if (NROW(variablesLine)==1) {
                        for(j in 1:NROW(values)) {
                                print(text(ticksGroup[j], topText[j] , paste(signif(values[j],2)), adj = 0.5, cex = digitsize))
                        }   
                }
                
        }
        
        ## begin bar plot
        if (!is.null(variablesBar)) { 
                barDT = summarySETable[variable%in%variablesBar, c("variable", groupVariable, "value", "ci"), with=F ]
                maxBar = max(barDT[, value], na.rm = T)
                minBar = min(barDT[, value], na.rm = T)
                rangeBar = maxBar - minBar
                
                if (is.null(ticksBar)) {
                        maxBarBorder = maxBar + safeBorders * rangeBar
                        minBarBorder = minBar - safeBorders * rangeBar
                        ticksBar = seq(signif(minBarBorder,1), signif(maxBarBorder,1), signif(rangeBar/5,2))
                } else {
                        rangeTicks = range(ticksBar)[2]-range(ticksBar)[1]
                        maxBarBorder = max(ticksBar) + safeBorders * rangeTicks
                        minBarBorder = min(ticksBar) - safeBorders * rangeTicks
                }
                
                par(new = TRUE, mar = c(5.8, 6, 25, 6) + 0.1)
                
                x <- c(1, 2, 3, 4)
                plot(x, c(-10, -10, -10, -10), type = "p", ylab = " ", xlab = " ", cex = 1.5,
                     ylim = c(minBarBorder, maxBarBorder), xlim = c(1, NROW(groupsVector)+1), lwd = 2, axes = FALSE, main = " ", col="white")
                axis(4, at = ticksBar, las = 1)
                grid::grid.text(labelBar, 0.97, 0.5, rot = 270, gp = grid::gpar(cex = 1.5,
                                                                                font = 2))
                for (i in 1:NROW(variablesBar)) {
                        values = barDT[variable == variablesBar[i], value]
                        ci = barDT[variable == variablesBar[i], ci]
                        
                        for(j in 1:NROW(values)) {
                                
                                x0 = ticksGroup[j] - width + barWidthPerVar*(i-1)
                                x1 = ticksGroup[j] - width + barWidthPerVar*i
                                y0 = minBarBorder
                                y1 = values[j]
                                toPlot = segments(x0, y0, x0, y1, lwd = linewidth, col=colorVariablesBar[i]) +
                                        segments(x0, y1, x1, y1, lwd = linewidth, col=colorVariablesBar[i]) +
                                        segments(x1, y1, x1, y0, lwd = linewidth, col=colorVariablesBar[i]) +
                                        segments(x1, y0, x0, y0, lwd = linewidth, col=colorVariablesBar[i])
                                print(toPlot)
                                if (NROW(variablesBar)==1) {
                                        print(text(ticksGroup[j], minBarBorder+ (maxBarBorder-minBarBorder)*0.15, paste(signif(values[j],2)), col = colorVariablesBar[i], adj = 0.5, cex = digitsize))
                                }
                        }
                        
                        
                        loc.errbars = ticksGroup - width + barWidthPerVar*(i-0.5)
                        plot.errbars = plotsebargraph(loc.errbars, values, ci, 0.2, color = colorVariablesBar[i])  # 0.2 = wiskwidth
                        print(plot.errbars)
                        
                        
                        
                        
                }
        }
        
        
}


#' BeNiceCorrelationPlot
#'
#' Inspired by http://shinyapps.org/apps/RGraphCompendium/index.php#preface
#' 
#' @param DataXY Data.frame holds X and Y columns
#' @param type For now only simple
#'
#'@export

BeNiceCorrelationPlot = function (dataXY, type="simple", xLabel = "X", yLabel = "Y", anotation = "") {
        X = dataXY$X
        Y = dataXY$Y
        maxX = max(X, na.rm = T)
        maxY = max(Y, na.rm = T)
        minX = min(X, na.rm = T)
        minY = min(Y, na.rm = T)
        rangeX = maxX - minX
        rangeY = maxY - minY
        safeBorders = 0.1
        
        maxX = maxX + safeBorders * rangeX
        maxY = maxY + safeBorders * rangeY
        minX = minX - safeBorders * rangeX
        minY = minY - safeBorders * rangeY
        
        # package plotrix is needed for function "ablineclip""
        op = par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
        plot(X, Y, col = "black", pch = 21, bg = "grey", cex = 2,
             xlim = c(minX,maxX), ylim = c(minY,maxY), ylab = "", xlab = "", axes = FALSE)
        axis(1)
        axis(2) 
        reg1 = lm(Y ~ X)
        ablineclip(reg1, lwd = 2,x1 = minX, x2 = maxX) 
        par(las = 0)
        mtext(xLabel, side = 1, line = 2.5, cex = 1.5)
        mtext(yLabel, side = 2, line = 3.7, cex = 1.5)
        text(0.65*maxX, .80*maxY, paste("r = ", signif(cor(X,Y, use = "complete.obs"),3)), cex = 1.5)
}

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

#' BeNiceBetaPlotFromFit
#'
#' 
#' @param type string Set the type of interval shown on the graph either 0.95 (confint) confidence intervals or Std. Error (stde)
#' 
BeNiceBetaConfIntPlotFromFit = function (fitObject, type="confint", relabel = NULL, withoutIntercept = T, ...) {
        
        summaryObject = summary(fitObject)
        # extract the coefficient depending on the summaryObject - as a matrix
        if (class(summaryObject$coefficients) == "list") {
                # this is a betareg summary object
                meanCoefs = summaryObject$coefficients$mean
        } else {
                # we hope its a glm object then ! *to add more of
                meanCoefs = summaryObject$coefficients
        }
        
        
        
        # without intercept
        if (withoutIntercept)
                meanCoefs = meanCoefs[2:NROW(meanCoefs),]
        
        # value
        values = meanCoefs[,1]
        
        # Confint or StdE
        groups = rownames(meanCoefs)
        
        if (type == "confint") {
                confIntervals = confint(fitObject, groups, level=0.95)
                lower = confIntervals[,1]
                upper = confIntervals[,2]
        } else {
                # lower
                lower = values - meanCoefs[,2]
                
                # upper
                upper = values + meanCoefs[,2]   
        }
        
        
        
        # Relabel groups
        if (!is.null(relabel)&&NROW(relabel)==NROW(groups)){
                groups = relabel
        }
        
        
        
        
        BenicePlotConfint(data.table(group=groups,value=values,lower=lower,upper=upper), ...)
}

#' BeNiceGetBetaAndConfintFromFit
#' 
#' 

BeNiceGetBetaAndConfintFromFit = function (fitObject, type="confint", withoutIntercept = T) {
        summaryObject = summary(fitObject)
        # extract the coefficient depending on the summaryObject - as a matrix
        if (class(summaryObject$coefficients) == "list") {
                # this is a betareg summary object
                meanCoefs = summaryObject$coefficients$mean
        } else {
                # we hope its a glm object then ! *to add more of
                meanCoefs = summaryObject$coefficients
        }
        
        
        
        # without intercept
        if (withoutIntercept)
                meanCoefs = meanCoefs[2:NROW(meanCoefs),]
        
        # value
        values = meanCoefs[,1]
        
        # Confint or StdE
        coefNames = rownames(meanCoefs)
        
        if (type == "confint") {
                confIntervals = confint(fitObject, coefNames, level=0.95)
                lower = confIntervals[,1]
                upper = confIntervals[,2]
        } else {
                # lower
                lower = values - meanCoefs[,2]
                
                # upper
                upper = values + meanCoefs[,2]   
        }
        
        return(data.table(group=coefNames,value=values,lower=lower,upper=upper))
}


#' BenicePlotConfint
#'
#' Plots confidence interval
#'
BenicePlotConfint = function (data, groupVariable = "group", 
                              interestVariable = "value", lowerVariable = "lower", 
                              upperVariable = "upper", evolutionVariable = NULL,
                              labelInterestVariable = "",labelGroupVariable = "", title = NULL) {
        
        theme = theme_bw()
        data = as.data.table(data)
        
        # ALL <- data.frame(study = as.character(x$Study1), HPD.median = bf$HPD.median, HPD.upper = bf$HPD.upper, 
        #                   HPD.lower = bf$HPD.lower, logBF10 = bf$logBF10, logBF10_clamped = sapply(bf$logBF10, 
        #                                                                                            function(lbf) {
        #                                                                                                    clamp(lbf, log(1/110), log(110))
        #                                                                                            }), test = as.character(x$Testtype), number = as.integer(x[, 1]))
        # 
        # rm(x, bf, clamp)
        
        # reorder factor levels based on another variable (HPD.mean)
        # ALL$study.ES_order <- reorder(ALL$study, ALL$HPD.median, mean)
        # data = data[order(-get(interestVariable))]
        if(is.null(evolutionVariable)) {
                data[[groupVariable]] <- factor(data[[groupVariable]], levels = data[[groupVariable]][order(data[[interestVariable]])]) 
                toPlot = ggplot(data, aes_string(x = groupVariable, y = interestVariable, ymin = lowerVariable, 
                                                 ymax = upperVariable)) +
                        geom_pointrange() + 
                        coord_flip() + 
                        geom_hline(yintercept = 0, linetype = "dotted") + 
                        theme + theme(axis.text=element_text(size=12),
                                      axis.title=element_text(size=14,face="bold")) +
                        ylab(labelInterestVariable) + xlab(labelGroupVariable)
        } else {
                # data[[groupVariable]] <- factor(data[[groupVariable]], levels = data[[groupVariable]][order(data[[interestVariable]])]) 
                toPlot = ggplot(data, aes_string(x = evolutionVariable, y = interestVariable, ymin = lowerVariable, 
                                                 ymax = upperVariable, group=groupVariable)) +  
                        geom_line(position = position_dodge(width = 0.7), aes_string(color=groupVariable))+
                        geom_pointrange(position = position_dodge(width = 0.7), aes_string(color=groupVariable)) + 
                        geom_hline(yintercept = 0, linetype = "dotted") + 
                        theme +
                        ylab(labelInterestVariable) + xlab(labelGroupVariable) 
        }
        
        
        
        
        
        
        if (!is.null(title)) {
                toPlot = toPlot + ggtitle(title)
        }
                                                     
        print(toPlot)
}

#' summarySE
#'
#' Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#'
#' @param data a data frame.
#' @param measurevar the name of a column that contains the variable to be summariezed.
#' @param groupvars a vector containing names of columns that contain grouping variables.
#' @param na.rm a boolean that indicates whether to ignore NA's.
#' @param conf.interval the percent range of the confidence interval, default is 95%.
#'
#' @export
#'
summarySE = function(data=NULL,
                     measurevar,
                     groupvars=NULL,
                     na.rm=FALSE,
                     conf.interval=.95,
                     .drop=TRUE) {
        
        # New version of length which can handle NA's: if na.rm==T, don't count them
        length2 <- function (x, na.rm=FALSE) {
                if (na.rm) sum(!is.na(x))
                else       length(x)
        }
        
        # This does the summary. For each group's data frame, return a vector with
        # N, mean, and sd
        datac <- ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                               c(N    = length2(xx[[col]], na.rm=na.rm),
                                 mean = mean   (xx[[col]], na.rm=na.rm),
                                 sd   = sd     (xx[[col]], na.rm=na.rm)
                               )
                       },
                       measurevar
        )
        
        # Rename the "mean" column
        datac <- rename(datac, c("mean" = measurevar))
        
        datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
        
        # Confidence interval multiplier for standard error
        # Calculate t-statistic for confidence interval:
        # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
        ciMult <- qt(conf.interval/2 + .5, datac$N-1)
        datac$ci <- datac$se * ciMult
        
        return(datac)
}