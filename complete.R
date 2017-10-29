complete <- function(directory, id=1:332) {

    # READ DATA FROM FILES
    path <- getwd()
    x <- matrix(1,length(id),2)
    x[,1] <- id
    index0 <- 1
    for(i in id) {
        #print(c("id",i))
        idstring <- str_pad(i, 3, pad = "0")
        pdata = read.csv(paste(path,"/specdata/", idstring,'.csv',sep=''))

        # COUNT NA'S       
        rs <- rowSums(data.matrix(pdata))
        rs2 = rs[!is.na(rs)]
        x[index0,2] <- length(rs2)
        index0 <- index0 + 1
    }
    dimnames(x) <- list(c(id),c("id","nobs"))
    as.data.frame(x)
}