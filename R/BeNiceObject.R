#' BeNice class object
#' 
#' Objects that holds the data and dynamically fetch relevant columns for plotting
#' 
#' 

BeNiceObject <- R6Class(classname = "BeNiceObject",
                    
                    private = list(
                            
                            # Methods
                            formatData = function (rename=F) {
                                    
                            }
                    ),
                    
                    public = list(
                            # Fields
                            data = NULL,
                            idColumn = NULL,
                            xColumn = NULL,
                            yColumn = NULL,
                            setForPlot = NULL,
                            
                            initialize() {},
                            setData(data, x = NULL, y = NULL, id = NULL) {
                                    if ("data.frame"%in%class(data)) {
                                            stop("data must be of class data.frame")
                                    }
                                    
                                    public$data = data
                                    public$idColumn = id
                                    public$xColumn = x
                                    public$yColumn = y
                                    
                                    if ((!is.null(x))&&(!is.null(y))) {
                                            if (columnInData(x)&&columnInData(y)) {
                                                    public$setForPlot = T
                                            }
                                    }
                                 
                                    
                                    
                            }
                            
                            columnInData(column) {
                                    if (column%in%colnames(public$data)) {
                                            return(T)
                                    } else {
                                            return(F)
                                    }
                            }
                            )
)