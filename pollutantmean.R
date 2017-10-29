pollutantmean <- function(directory, pollutant, id=1:332) {

    # READ DATA FROM FILES
    path <- getwd()
    #setwd(directory)
    datalist <- as.list(id)
    index0 <- 0
    for(i in id) {
        idstring <- str_pad(i, 3, pad = "0")
        pdata = read.csv(paste(path,"/specdata/", idstring,'.csv',sep=''))
        if (index0==0) { 
            data <- pdata 
            index0 <- 1
        }
        else { 
            data <- rbind(data, pdata)
        }
    }
    #setwd(path)

    # READ POLLUTANT DATA FROM DATAFRAME
    colnames <- dimnames(data)[[2]]
    #print(colnames)
    icol <- match(pollutant, colnames)
    #print(icol)
    #print(data[icol])
    mean(as.numeric(data[[icol]]), na.rm = TRUE)
}