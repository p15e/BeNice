

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



