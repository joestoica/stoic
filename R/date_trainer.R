x = data.frame(elapsed = rep(0, 1), ans = rep(0,1))

date_trainer <- function() {
    date = sample(seq(as.Date('1800/01/01'), as.Date('2199/12/31'), by="day"), 1)
    print(date)
    start = Sys.time()
    readline()
    stop = Sys.time()
    
    elapsed = as.numeric(round(stop - start, 2))
    day = weekdays(date)
    
    print(paste0("Day: ", day))
    
    ans = readline("Correct? t/f")
    
    if(x[1,1] == 0){
        x[1,1] = elapsed
        x[1,2] = ans      
        return(x)
    } else{
        return(x = rbind(x, c(elapsed, ans)))
    }
}


date_trainer()




