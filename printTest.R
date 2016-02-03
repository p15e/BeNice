

# - add variable name and units (age = 12+-sd years)
print_sem <- function(X, digits = 2){
        if (is.data.frame(X)){
                n <- length(X[1,])
                pp <- list()
                for (ii in 1:n){
                        pp[[ii]] <- print_sem_v(X[,ii], digits = digits)
                }
        } else {
                print_sem_v(X, digits = digits)
        }
}



sem <- function(x){
        m = mean(x)
        n = length(x)
        e = sd(x)/sqrt(n)
        data.frame(mean=m,sem=e, n=e)
}

print_sem_v <- function(x,digits=3){
        k <- digits
        m <- format(round(mean(x), k), nsmall=k)
        s <- sd(x)/sqrt(length(x))
        s <- format(round(s, k), nsmall=k)
        print(paste0(m, '\u00B1', s))
}




# function to print out result from correlation test:
print.cor.test <- function(rr, ndigits = 2, format = "long"){
        
        
        
        
        
        # check p value;
        if (round(rr$p.value, ndigits) == 0){
                ptext = paste0("p<", (10^-ndigits)) 
        } else {
                ptext = paste0("p=", round(rr$p.value, ndigits)) 
        }
        
        if (format == "long"){
                ss <- paste0("(r=", round(rr$estimate, ndigits), "[", round(rr$conf.int[1], ndigits),",", round(rr$conf.int[1], ndigits), "], t=", round(rr$statistic, ndigits), ", df=", rr$parameter, ", ", ptext, ")")
        } else {
                ss <- paste0("(r=", round(rr$estimate, ndigits),", ", ptext, ")")
        }
        ss
}



### Print t-test:
print.t.test <- function(tt, ndigits = 2, format = "long", units = ""){
        # -0.13 % [-0.16 -0.09] (t = -7.2, df = 93, p-value = 1.9e-10).
        
        
        # check p value;
        if (round(tt$p.value, ndigits) == 0){
                ptext = paste0("p<", (10^-ndigits)) 
        } else {
                ptext = paste0("p=", round(tt$p.value, ndigits)) 
        }
        
        
        # I'm not printing the difference between the two?
        # diff(tt$estimate)
        ttext <-paste0("t=", round(tt$statistic, ndigits))
        dftext <- paste0("df=", round(tt$parameter, ndigits))
        
        mdiff <- paste0("mean difference = ", round(tt$estimate, ndigits), units)
        mdiff.ci <- paste0("[", round(tt$conf.int[1], ndigits), ", ",round(tt$conf.int[2], ndigits) , "]")
        
        
        
        if (format == "long"){
                ss <- paste0("(", 
                             mdiff, " ", mdiff.ci, ", ", ttext, ", ", dftext, ", ", ptext, ")")
        } else {
                ss <- paste0("(", ttext, ", ", ptext, ")")
        }
        ss
}


# print.t.test(tt, units="%", format = "long")
# print.t.test(tt, units="%", format = "short")

# to report that all values are > than ...
pBiggerThan <- function(pvalues, ndigits = 2){
        floor(min(pvalues)*10^ndigits)/10^ndigits        
}





# function to remove multiple spaces
trim <- function(x) return(gsub("^ *|(?<= ) | *$", "", x, perl=T))


principal2df <- function(pc){
        nFactors <- pc$factors
        nVar <- length(pc$values)
        
        pc.out <- capture.output(pc)
        # 
        # pc.out[1]
        # suggest caption:
        # tt <- paste0(pc.out[1], '. ', pc.out[2], '. ', pc.out[3], '.')
        
        
        nr.start <- 5
        nr.end <- nr.start+nVar - 1 #25
        nc.start <- 1
        nc.end <- nFactors + 1
        
        
        pc.form <- pc.out[nr.start:nr.end]
        # pc.form[1]
        pc.form <- paste0(pc.form, ';')
        
        lines <- gsub(" ", ";", trim(pc.form))
        con <- textConnection(lines)
        data <- read.csv(con, sep=";", header=F)
        close(con)
        
        data <- as.data.frame(data)
        
        df <- data[,1:nc.end]
        names(df) <- c('', paste0('Comp.', 1:7))
        df
}






principal.print <- function(pcdf, lowerValue=0.2, upperValue=.4){
        
        if (is.data.frame(pcdf)){
                
        } else {
                pc.out <- capture.output(pcdf)
                # suggest caption:
                cap <- paste0(pc.out[1], '. ', pc.out[2], '. ', pc.out[3], '.')
                
                pcdf <- principal2df(pcdf)
        }
        
        
        # pc.table <- xtable(pcdf)
        
        
        
        
        # threshold output by cor value:
        varNames <- pcdf[,1]
        coefValues <- pcdf[,-1]
        
        if (lowerValue < 0 | lowerValue > 1){
                browser()       
        }
        if (upperValue < 0 | upperValue > 1 | upperValue <= lowerValue){
                browser()       
        }
        
        # remove lower values
        if (lowerValue > 0){
                coefValueCHR <- coefValues
                coefValueCHR[abs(coefValues) < lowerValue] <- ''
        }
        
        
        
        # bold high values
        if (upperValue > 0){
                coefValueCHR[abs(coefValues) >= upperValue] <- paste0('**', coefValueCHR[abs(coefValues) >= upperValue], '**')
                
        }
        
        
        # df <- cbind(varNames, coefValueCHR)
        # pc.table <- xtable(df)
        xtable(cbind(varNames, coefValueCHR))
}




p2stars <- function(pv){
        s <- rep('', length(pv))
        s[pv <= .1] <- '.'
        s[pv <= .05] <- '*'
        s[pv <= .01] <- '**'
        s[pv <= .001] <- '***'
        s
}

beta.print <- function(beta.fit){
        require(xtable)
        aa <- as.data.frame(summary(beta.fit)$coefficients$mean)[-1,]
        aa$stars <- p2stars(aa$`Pr(>|z|)`)
        names(aa) <- c("coef.", "se.", "z", "p", " ")
        xtable(aa)       
}



### COMPUTATIONS
# if there are any 0 or 1 this code will bug.
# transfor variables:
prior.beta <- function(yy, br=0.001){
        yy * (1-2*br) + br
}

