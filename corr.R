corr <- function(directory, threshold = 0) {

    # READ DATA FROM FILES
    id <- 1:332
    path <- getwd()
    x <- matrix(1,length(id),2)
    x[,1] <- id
    corrs <- rep(NA,length(id))
    index0 <- 1
    for(i in id) {
        idstring <- str_pad(i, 3, pad = "0")
        pdata = read.csv(paste(path,"/specdata/", idstring,'.csv',sep=''))

        # COUNT NA'S       
        rs <- rowSums(data.matrix(pdata))
        rs2 = rs[!is.na(rs)]
        x[index0,2] <- length(rs2)

        # COMPUTE CORRELATION
        if (x[index0,2] >= threshold) {
            #print(idstring)
            #print(x[index0,2])
            #print(pdata[[2]])
            c1 <- as.numeric(pdata[[2]])
            c2 <- as.numeric(pdata[[3]])
            #print(cor(c1,c2, use="complete.obs"))
            if (x[index0,2] > 0 ) {
                corrs[index0] <- cor(c1,c2, use="complete.obs")
            }
            else { corrs[index0] <- NA }
        }
        index0 <- index0 + 1
    }
    corrs[!is.na(corrs)]
}