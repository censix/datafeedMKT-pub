#Create an independent R process for reading marketdata and
#for sharing it via a mmap structure in the desired aggregation>=5secs.


# Any error happening in this tryCatch.. will be a FATAL error. We send a WARNING email with the error text
tryCatch({

require(mmap)
require(xts)
require(IBrokers)

source('datafeed-config.r')

source('IBmakeVarlengthStruct.r')
source('IBgetShared.r')
source('IBappendXtsToHistory.r')

source('IBemailNotify.r')
source('IBsmsNotify.r')
# if true, notification emails will be sent. Implemented as stub only
ibNotificationEmail <- TRUE
# if true, notification sms will be sent. Implemented as stub only
ibNotificationSMS <- FALSE

# Cleanup of non-intact files
# Scan folder for existing .bin and .info files. Remove orphans, then append .bin file content to xts history,
# dissected by symbols ! then DELETE all .bin and .info files.
allpreviousshares<-dir(IBdatafeedShareDir, pattern=glob2rx('*.bin'))
allpreviousinfo<-dir(IBdatafeedShareDir, pattern=glob2rx('*.info'))
if ( max(length(allpreviousshares),length(allpreviousinfo))>0 ) {
	intact <- intersect( sub('\\.info','',allpreviousinfo) , sub('\\.bin','',allpreviousshares))
	if (length(intact) < max(length(allpreviousshares),length(allpreviousinfo))  ) { #There are orphan files. remove them.
		for (thisid in unique(c(sub('\\.bin','',allpreviousshares),sub('\\.info','',allpreviousinfo)))  ) {
			if (!(thisid %in% intact)) {
				cat('Orphan file found. This should not happen! Deleting ...',thisid,'\n')
				file.remove(paste(IBdatafeedShareDir,'/', paste(thisid,'.bin',sep='') ,sep=''))
				file.remove(paste(IBdatafeedShareDir,'/', paste(thisid,'.info',sep='') ,sep=''))
			}
		}
	}
}

# Append existing files to xts history, then remove them
allpreviousshares<-dir(IBdatafeedShareDir, pattern=glob2rx('*.bin'))
allpreviousinfo<-dir(IBdatafeedShareDir, pattern=glob2rx('*.info'))
if ( max(length(allpreviousshares),length(allpreviousinfo))>0 ) {
	for (idx in 1:length(allpreviousshares)) {
		streamfile <- allpreviousshares[idx]
		infofile <- allpreviousinfo[idx]
		streamid <- sub('\\.bin','',streamfile)
		# Dissect into symbols. PENDING
		# Append to history,as stream. 
		tmpxts<-IBgetShared( paste(IBdatafeedShareDir,'/', streamfile ,sep='') )
		if (!is.null(tmpxts)) {
			#Append previously recorded content as xts to (existing?) stream.rdata file in IBdatafeedHistoryDir
			#Check and warn if we have data overlaps, missing columns, etc.
			print(paste('Appending or creating history for ',streamid))
			IBappendXtsToHistory(
				  newSeriesxts = tmpxts,
				  rdatafile = paste(IBdatafeedHistoryDir,'/',streamid,'.rdata',sep=''),
				  currentSymbol = streamid )		
		}
		rm(tmpxts)
		#
		print(paste('Removing',streamid))
		file.remove(paste(IBdatafeedShareDir,'/', streamfile ,sep=''))
		file.remove(paste(IBdatafeedShareDir,'/', infofile ,sep=''))	
	}
}


# Select from contracts UNIVERSE only what is necessary for the streams!!
stopifnot( length(IBstreamsymbolSpecs)>0  )
IBcontractSpecs.UNIVERSE <- IBcontractSpecs
IBcontractSpecs <- IBcontractSpecs[ unique(unlist(IBstreamsymbolSpecs)) ]
stopifnot( length(IBcontractSpecs)==length( unique(unlist(IBstreamsymbolSpecs)) ) )


# Create the contracts and prepare the request for marketdata. Note that stocks and currencies need to be handled differently!
contractList<-fileList<-shmemList<-agrcountList<-keepcsvhistoryList<-keepoutputhistoryList  <- list()
#tickerList<- 1:length(IBcontractSpecs)
for (ii in 1:length(IBcontractSpecs)) {
   if (IBcontractSpecs[[ii]]$type=='currency') {
      contractList[[ii]]  <- twsCurrency( symbol=IBcontractSpecs[[ii]]$symbol ,currency=IBcontractSpecs[[ii]]$currency)  # Currency type contract
      #fileList[[ii]]      <- paste(IBdatafeedRawDir,'/',IBcontractSpecs[[ii]]$symbol,IBcontractSpecs[[ii]]$currency,'-raw.csv',sep='')
      #shmemList[[ii]]     <- paste(IBdatafeedShareDir,'/',IBcontractSpecs[[ii]]$symbol,IBcontractSpecs[[ii]]$currency,'-', IBcontractSpecs[[ii]]$exchange,'-shared.bin',sep='')
   } else if (IBcontractSpecs[[ii]]$type=='stock') {
      contractList[[ii]]  <- twsSTK( symbol=IBcontractSpecs[[ii]]$symbol ,    # Stock type contract
                                     exch=IBcontractSpecs[[ii]]$exchange ,
                                     primary=IBcontractSpecs[[ii]]$primary ,  #added 1 june 2011
                                     currency=IBcontractSpecs[[ii]]$currency )
      #fileList[[ii]]      <- paste(IBdatafeedRawDir,'/',IBcontractSpecs[[ii]]$symbol,'-raw.csv',sep='')
      #shmemList[[ii]]     <- paste(IBdatafeedShareDir,'/',IBcontractSpecs[[ii]]$symbol,'-', IBcontractSpecs[[ii]]$exchange,'-shared.bin',sep='')
  } else if (IBcontractSpecs[[ii]]$type=='future') {
      contractList[[ii]]  <- twsFuture( symbol=IBcontractSpecs[[ii]]$symbol ,    # future type contract
                                     exch=IBcontractSpecs[[ii]]$exchange ,
                                     expiry=IBcontractSpecs[[ii]]$expiry,
                                     primary=IBcontractSpecs[[ii]]$primary ,
                                     currency=IBcontractSpecs[[ii]]$currency )
      #fileList[[ii]]      <- paste(IBdatafeedRawDir,'/',IBcontractSpecs[[ii]]$symbol,'-raw.csv',sep='')
      #shmemList[[ii]]     <- paste(IBdatafeedShareDir,'/',IBcontractSpecs[[ii]]$symbol,'-', IBcontractSpecs[[ii]]$exchange,'-shared.bin',sep='')
   } else stop('Unsupported type in contract definition. Has to be stock or currency')

   if ((IBcontractSpecs[[ii]]$outputbarsizeseconds %% 5) != 0  |
        IBcontractSpecs[[ii]]$outputbarsizeseconds < 5  |
        IBcontractSpecs[[ii]]$outputbarsizeseconds >= (60*60*24) ) stop('Invalid outputbarsize. has to be greater than and a multiple of 5. cannot exceed 60*60*24')
   agrcountList[[ii]] <- as.integer( IBcontractSpecs[[ii]]$outputbarsizeseconds %/% 5 )  # multiple of 5sec that we aggregate
   keepcsvhistoryList[[ii]] <- IBcontractSpecs[[ii]]$keepcsvhistory
   #if (!keepcsvhistoryList[[ii]]) unlink(fileList[[ii]])
   keepoutputhistoryList[[ii]] <- IBcontractSpecs[[ii]]$keepoutputhistory
#~    if (keepoutputhistoryList[[ii]]) {
	  #print('keepoutputhistory on a per-symbol basis is not supported at the moment.')
#~     }
   
}

numSingleContracts <- length(contractList)  #number of non-multisymbol contracts

if (length(IBstreamsymbolSpecs)>0) {
	# create fake contracts for the multisymbol instances and raw and shmem files for each instance
	# create the cross-update map from single streams to multisymbol streams
	#xupdate.mat <- matrix(NA,nrow=length(unlist(IBmultisymbolSpecs)), ncol=4)
	xupdate.mat <- matrix(NA,nrow=length(unlist(IBstreamsymbolSpecs)), ncol=4)
	xupdate.ridx <- 0
	ibcs.count <- numSingleContracts
	#for (thisset in IBstreamsymbolSpecs) {
	for (thisset.idx in 1:length(IBstreamsymbolSpecs)) {
		thisset <- IBstreamsymbolSpecs[[thisset.idx]]
		if (length(names(IBstreamsymbolSpecs))>0) {
			streamfile.id <- names(IBstreamsymbolSpecs)[thisset.idx]
			stopifnot( length(grep('\\.[bin|info]',streamfile.id))==0 ) # streamid cannot contain these reserved suffixes
		} else  streamfile.id <- tempfile(pattern = "", tmpdir = '', fileext = '')		
		streamfile <- paste(IBdatafeedShareDir,'/',streamfile.id,'.bin',sep='')
		streamfileinfo <- paste(IBdatafeedShareDir,'/',streamfile.id,'.info',sep='')
		streamfileinfo.content <- ''		
		ibcs.count <- ibcs.count + 1
		contractList[[ibcs.count]]  <- thisset
		shmemList[[ibcs.count]]  <- streamfile
		opbs <- NULL
		for (el in thisset) {
			if (is.null(opbs)) opbs<-IBcontractSpecs[[el]]$outputbarsizeseconds #must all be the same
			else if (opbs!=IBcontractSpecs[[el]]$outputbarsizeseconds) stop('outputbarsizeseconds #must all be the same for multisymbol stream')
			#shmemList[[ibcs.count]] <- paste(shmemList[[ibcs.count]], '_', IBcontractSpecs[[el]]$symbol,'-', IBcontractSpecs[[el]]$currency,'-', IBcontractSpecs[[el]]$exchange, sep='')
			#streamfileinfo.content <- paste(streamfileinfo.content, '_', IBcontractSpecs[[el]]$symbol,'-', IBcontractSpecs[[el]]$currency,'-', IBcontractSpecs[[el]]$exchange, sep='')
			streamfileinfo.content <- paste(streamfileinfo.content, IBcontractSpecs[[el]]$symbol,'\t', IBcontractSpecs[[el]]$currency,'\t', IBcontractSpecs[[el]]$exchange, '\n', sep='')
			# cross-update map: if we have 19 single contracts and (m1,m3), (m1,m2) as multisymbol streams
			# and i.e.  m1=1, m2=2, m3=3, then we have:
			# xupdate.mat <-
			#   m1, 20, 1, 2
			#   m1, 21, 1, 2
			#   m3, 20, 2, 2
			#   m2, 21, 2, 2
			xupdate.ridx <- xupdate.ridx + 1
			xupdate.mat[xupdate.ridx,] <- c(which(el==names(IBcontractSpecs)), ibcs.count, which(el==thisset), length(thisset))
		}
		#shmemList[[ibcs.count]]  <- paste(shmemList[[ibcs.count]],'-shared.bin',sep='')
		# Write stream information to info file:
		cat(streamfileinfo.content, file=streamfileinfo)
	}
}


# Eternal loop. this enables us to cold-restart the reqMktdata() should it exit because of specific
# errors that require a restart of this command. reqMktdata() is only
# allowed to exit because we use our modified twsCALLBACKdatafeed.
ibShareFilesWipe <- TRUE #At the beginning of the trading day, we wipe the shared mmap files, but when
                         #restarting from interruptions during the day, we don't!!!
while(TRUE) {

# Verify all contracts, to avoid FODIC problem
tmpContractList <- contractList[1:numSingleContracts]
tmpTimeout <- unclass(Sys.time()) # timeout init
while(length(tmpContractList)>0) {
   # Do reqContractDetails() here to avoid the 'First Order of the Day Is Cancelled' FODIC problem
   # For Futures, here we pick the front month contract, in case that expiry='' or has an invalid value
   ibFODICworkaroundDone <- FALSE # we haven't yet done anthing to avoid the 'First Order of the Day Is Cancelled' (FODIC) problem.
   if (is.list(tmpContractList[[1]])) { #skip the multisymbols. they are not lists, just arrays
	   while(!ibFODICworkaroundDone) {
		  if (unclass(Sys.time())-tmpTimeout > 2*60*60) stop('Timeout while doing reqContractDetails() in initial contract list verification loop') #fatal timeout after 2h.
		  #if (unclass(Sys.time())-tmpTimeout > 15*60) stop('Timeout while doing reqContractDetails() in initial contract list verification loop')  #fatal timeout after 15 min.
		  tmp<-list()
		  tmp<-reqContractDetails(conn=ibConnection, Contract=tmpContractList[[1]], verbose=TRUE)
	   #print(tmp)
		  if (tmpContractList[[1]]$sectype=='FUT') {
			if (length(tmp)>0) {
				if ( length(tmp)>1  ) {
					#multiple contractDetails returned. Take the first. Assume it is the front month. modify master contract list
					warning('Multiple futures contracts returned. taking the first one. Asuming it is the front month contract')
					if ( !inherits(tmp[[1]]$contract , 'twsContract') ) stop('Multiple futures contractDetials returned. But something is wrong')
	print(tmp[[1]]$contract)
					#contractList[[ length(contractList)-length(tmpContractList)+1 ]] <- tmp[[1]]$contract #BUG
					contractList[[ numSingleContracts-length(tmpContractList)+1 ]] <- tmp[[1]]$contract
				}
				ibFODICworkaroundDone <- TRUE
			} else {
				Sys.sleep(15)
			}
		  } else {
			if (length(tmp)>0) { 
				ibFODICworkaroundDone <- TRUE 
				#Sys.sleep(1)  #This delay may be needed if we have many >10 contracts, i.e. stocks
			} else {
				Sys.sleep(15)
			}
		  }
	   }
   }
   tmpContractList[[1]] <- NULL
   Sys.sleep(1)
}
print('All contracts verified - datafeed good to go')


# Define the main callback handler
source('IBeWrapper.Mktdata.SHARED.MULTISYMBOL100.r')
#myWrapper <- IBeWrapper.Mktdata.SHARED.MULTISYMBOL(Contracts=contractList, ShareFiles=shmemList, Aggregation=agrcountList, ShareFilesWipe=ibShareFilesWipe, xupdate.mat=xupdate.mat )
#myWrapper <- IBeWrapper.Mktdata.SHARED.MULTISYMBOL(Contracts=contractList[1:numSingleContracts], ShareFiles=shmemList[1:numSingleContracts], Aggregation=5L, ShareFilesWipe=ibShareFilesWipe, xupdate.mat=NULL )
myWrapper <- IBeWrapper.Mktdata.SHARED.MULTISYMBOL(Contracts=contractList, ShareFiles=shmemList, Aggregation=5L, ShareFilesWipe=ibShareFilesWipe, xupdate.mat=xupdate.mat )


#print(contractList[1:numSingleContracts])
#print(tickerList)
#print(fileList[1:numSingleContracts])


#use a single call to reqMktdata(...) and specify the _list_ of contracts 
reqMktData(conn=ibConnection, 
	Contract=contractList[1:numSingleContracts], 
	#Contract=contractList, 
	tickGenerics = "236",     #"100,101,104,106,165,221,225,236"
    snapshot = FALSE, 
    eventWrapper = myWrapper,
    CALLBACK=twsCALLBACKdatafeed
    )


#This call only returns if we get a specific type of error. We don't want this to happen too often
#These types of error require the reqMktData() to be reissued, otherwise data will not flow
#despite it being available.
Sys.sleep(2*60) #Wait 2 minutes before we attempt a restart
ibShareFilesWipe <- FALSE #We want to preserve previous data, so no wiping
} #End of eternal loop


},

error = function(e) {
  #There was a FATAL error. We dispatch an alert and quit.
  print(e)
  if (ibNotificationEmail & exists('IBemailMsg',mode='function')) IBemailMsg(msgBODY=paste('FATAL: DATAFEED abort!',as.character(e)), msgHEAD=paste('FATAL: DATAFEED abort! Check Error Message'))
  if (ibNotificationSMS & exists('IBsmsMsg',mode='function')) IBsmsMsg(msgBODY=paste('FATAL: DATAFEED abort!! Check Error Message'))
  quit(save='no')
}

) #end of tryCatch



