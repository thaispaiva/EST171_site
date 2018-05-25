## Code to combine portfolio historical data
## Thais Paiva - 05/2018

## stock symbols
id <- c("IXBK", "NBI", "IXK", "IXF", "IXID", "IXIS", "IXUT", "IXTR", "FVX", "TYX")

setwd("static/datasets/NASDAQ/")  # change working directory

# change system locale for months in english
temp = Sys.setlocale("LC_TIME"); Sys.setlocale("LC_TIME", "english" )   

## load data
quotes = vector("list", length(id))
names(quotes) = id
for(i in 1:length(id)){
  # read file
  file = paste0(id[i],".csv")
  quotes[[i]] = read.csv(file, stringsAsFactors=FALSE)
  
  # treat special formats
  quotes[[i]][,1] = as.Date(quotes[[i]][,1], format="%b %d, %Y")
  quotes[[i]][,2:5] = lapply(quotes[[i]][,2:5], function(x) as.numeric(gsub(",", "", x)) )
  
  # check if ok
  if( any(is.na(quotes[[i]])) )
    cat("NA on file ", i,"! \n")
  else
    cat("file ",i," ok \n") 
}

## reference dates
data = quotes[[1]][,1]

## create data.frame
nasdaq = as.data.frame(matrix(0,length(data),length(id)+1))
nasdaq[,1] = data
names(nasdaq) = c("Date", id)

## save only price values for reference dates
for(i in 1:length(id)){
  nasdaq[,i+1] = quotes[[i]][quotes[[i]][,1]%in%data, 2]
}

## save data.frame in a csv file
write.csv(nasdaq, file="nasdaq.csv", row.names=FALSE)

## back to original configuration!
Sys.setlocale("LC_TIME", temp)  

