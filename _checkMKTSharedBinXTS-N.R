#################
# read all the *.bin files from directory, converts the data to xts and assigns it to the symbol derived from the filename.
#################
library(mmap)
library(xts)
#library(quantmod) #only needed for plotting

source('IBmakeVarlengthStruct.r')

source('datafeed-config.r')

allshares<-dir(IBdatafeedShareDir, pattern=glob2rx('*.bin'))
allinfo<-dir(IBdatafeedShareDir, pattern=glob2rx('*.info'))
stopifnot(length(allshares)==length(allinfo))

#~ 
#~ THEextractFUN<- function(x) na.trim( .xts( as.matrix(
#~ 	cbind(x$BidSize, x$BidPrice, x$AskPrice, x$AskSize, x$Last, x$LastSize, x$Volume,x$Open,x$High,x$Low
#~ 		,x$BidSize2, x$BidPrice2, x$AskPrice2, x$AskSize2, x$Last2, x$LastSize2, x$Volume2,x$Open2,x$High2,x$Low2
#~ 	))
#~ 	, x$timestamp), sides='right', is.na='all')
#~ 

# This function is agnostic to the number of columns in a datarow :))
THEextractFUN<- function(x) na.trim( .xts( do.call(cbind,x[-1]) , x[[1]]), sides='right', is.na='all')


#for (tmpfile in allshares) {
for (idx in 1:length(allshares)) {
	streamfile <- allshares[idx]
	infofile <- allinfo[idx]
	stopifnot( strsplit(streamfile,'\\.')[[1]][1]==strsplit(infofile,'\\.')[[1]][1] )
      #tmpfile <- 'xxxxxxxxx.bin'
#~    m <- mmap(paste(IBdatafeedShareDir,'/',tmpfile,sep=''), 
#~ 		struct(double()
#~ 			,double(), double(), double(), double(), double(), double(), double(),double(),double(),double() 
#~ 			,double(), double(), double(), double(), double(), double(), double(),double(),double(),double()
#~ 		)
#~ 		, extractFUN=THEextractFUN)
   
   # Get the stream's symbols from the .info file text
   	info <- read.table(paste(IBdatafeedShareDir,'/',infofile,sep=''))
	thesymbols <- as.character(info[,1])
    numsymbols <- length(thesymbols)
	atom.lst=c(
		list(double())   #timestamp
		,rep( list(double(), double(), double(), double(), double(), double(), double(),double(),double(),double())  #other columns
			, numsymbols)
	)   
	ss <- IBmakeVarlengthStruct(atom.lst, 1L)
	m <- mmap(paste(IBdatafeedShareDir,'/',streamfile,sep=''), ss , extractFUN=THEextractFUN)
	#print(last(m[]))  #Gets us the last non-NA timestamp entry!!
	tmpxts<-m[]
	
#~ 	colnames(tmpxts)<-c("BidSize", "BidPrice", "AskPrice", "AskSize", "Last", "LastSize", "Volume","Open","High","Low"
#~ 	,"BidSize2", "BidPrice2", "AskPrice2", "AskSize2", "Last2", "LastSize2", "Volume2","Open2","High2","Low2")

	# Assign labels
	xx <- thesymbols
	yy <- c('BidSize', 'BidPrice', 'AskPrice', 'AskSize', 'Last', 'LastSize', 'Volume','Open','High','Low')
	colnames(tmpxts)<- paste(rep(xx,each=length(yy)), rep(yy, length(xx)), sep='.')
				
	# Store in .GlobalEnv
	assign(paste( symbol<-paste(thesymbols, collapse='_') ,'_xts',sep=''), tmpxts, envir=.GlobalEnv)
	#plot(tmpxts[, grep("Last$", colnames(tmpxts),ignore.case = TRUE)[1] ] )
    #plot(tmpxts[, grep("Last$", colnames(tmpxts),ignore.case = TRUE)[2] ] )
    print(symbol) ;print(periodicity(tmpxts))
    
    munmap(m) #free up resources
	
}

#
