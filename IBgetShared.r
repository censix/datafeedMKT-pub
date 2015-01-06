#################
# reads the file and converts the data to xts. Does not do any checks!!
#################
library(mmap)
library(xts)
source('IBmakeVarlengthStruct.r')


#allshares<-dir(IBdatafeedShareDir, pattern=glob2rx('*.bin'))

IBgetShared <- function(filename) {
	infoname <- sub('\\.bin','.info',filename)
	#'somename.bin' and 'somename.info'
	if (!file.exists(filename)) return(NULL)
	if (!file.exists(infoname)) return(NULL)  

	#extract2XTS<- function(x) na.trim( .xts( as.matrix(cbind(x$BidSize, x$BidPrice, x$AskPrice, x$AskSize, x$Last, x$LastSize, x$Volume,x$Open,x$High,x$Low)), x$timestamp), sides='right', is.na='all')
	#c("BidSize", "BidPrice", "AskPrice", "AskSize", "Last", "LastSize", "Volume","Open","High","Low")

	# This function is agnostic to the number of columns in a datarow :))
	THEextractFUN<- function(x) na.trim( .xts( do.call(cbind,x[-1]) , x[[1]]), sides='right', is.na='all')

	info <- read.table(infoname)
	thesymbols <- as.character(info[,1])
	numsymbols <- length(thesymbols)
	atom.lst=c(
		list(double())   #timestamp
		,rep( list(double(), double(), double(), double(), double(), double(), double(),double(),double(),double())  #other columns
			, numsymbols)
	)   
	ss <- IBmakeVarlengthStruct(atom.lst, 1L)
	m <- mmap(filename, ss , extractFUN=THEextractFUN)  

	tmpxts<-m[]
 
	# Assign labels
	xx <- thesymbols
	yy <- c('BidSize', 'BidPrice', 'AskPrice', 'AskSize', 'Last', 'LastSize', 'Volume','Open','High','Low')
	colnames(tmpxts)<- paste(rep(xx,each=length(yy)), rep(yy, length(xx)), sep='.')
  
	munmap(m) #free up resources
	if (!is.xts(tmpxts)) return(NULL)
	if (nrow(tmpxts)<1) return(NULL)
	return(tmpxts)
}

