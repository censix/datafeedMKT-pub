

#Contracts has to be a list(..) of twsContracts


IBeWrapper.Mktdata.SHARED.MULTISYMBOL <- function(Contracts=list(), ShareFiles=list(), Aggregation=5L, ShareFilesWipe=FALSE, xupdate.mat=NULL) {
  verbose <- 0 # only for init
  eW <- eWrapper(NULL)

  if (!(length(Contracts)>0)) stop('Contract list cannot be empty')
  n<-length(Contracts)
  stopifnot( (is.integer(Aggregation) & (1<Aggregation)) )  #barsize (in seconds) is the same for all contracts!
  #Initalize n different mmap xts structures type'double' on disk

  # c("BidSize", "BidPrice", "AskPrice", "AskSize", "Last", "LastSize", "Volume","Open","High","Low")
  
  #Set number of rows and calculate xts size in bytes (on disk)
  numrows<-18000 # set to 18000 ~= 17280 * 5sec to cover 24 hours. The rest is safety padding
  #Create disk files and mappings
  numcontracts <- 0
  numstreams <- 0
  mmappings<-list()
  for (id in 1:n) {
	if (is.list(Contracts[[id]])) { # symbol contract == list()	
		numcontracts <- numcontracts + 1
		
#~ 		tmpx <- xts(matrix(data=NA_real_, nrow=numrows, ncol=10), Sys.time()+1:numrows)
#~ 		sizeinbytes<-length(coredata(tmpx)) * nbytes(struct(double())) + length(.index(tmpx)) * nbytes(struct(double()))
#~ 		rm(tmpx)
#~ 		#filename: SYMBOL-EXCHANGE-mktdata.bin
#~ 		#tmpfname <- paste(ShareDir,'/',Contracts[[id]]$symbol,'-', Contracts[[id]]$exch,'-mktdata.bin',sep='')
#~ 		tmpfname <- ShareFiles[[id]]
#~ 		if (!file.exists(tmpfname) | ShareFilesWipe) { writeBin(raw(sizeinbytes),tmpfname) }
#~ 		mmappings[[id]] <- mmap(tmpfname, struct(timestamp=double(), BidSize=double(), BidPrice=double(), AskPrice=double(), AskSize=double(), Last=double(), LastSize=double(),  Volume=double(),  Open=double(),  High=double(),  Low=double() ))
#~ 		if (ShareFilesWipe) {
#~ 		   # Initialize values - brute
#~ 		   mmappings[[id]][,1] <- NA
#~ 		   mmappings[[id]][,2] <- NA
#~ 		   mmappings[[id]][,3] <- NA
#~ 		   mmappings[[id]][,4] <- NA
#~ 		   mmappings[[id]][,5] <- NA
#~ 		   mmappings[[id]][,6] <- NA
#~ 		   mmappings[[id]][,7] <- NA
#~ 		   mmappings[[id]][,8] <- NA
#~ 		   mmappings[[id]][,9] <- NA
#~ 		   mmappings[[id]][,10] <- NA
#~ 		   mmappings[[id]][,11] <- NA		   		   		   
#~ 		   ## Write full xts data by column .....
#~ 		   #m[,1] <- .index(x)
#~ 		   #m[,2] <- coredata(x)[,1]
#~ 		   #m[,3] <- coredata(x)[,2]
#~ 		   #m[,4] <- coredata(x)[,3]
#~ 		   #m[,5] <- coredata(x)[,4]
#~ 		   # maybe it would be good to have some kind of  consistency check on the
#~ 		   # client side, where we compare the write/read data to an xts stored in
#~ 		   # a .rdata file ??
#~ 		}

    } else {  # multi symbol definition , including single symbols!
		if (length(Contracts[[id]])>100) stop('more than 100 symbols in a multisymbol stream are not supported yet.')
		numstreams <- numstreams + 1
		numcolumns <- 10*length(Contracts[[id]])
		tmpx <- xts(matrix(data=NA_real_, nrow=numrows, ncol=numcolumns), Sys.time()+1:numrows)
		sizeinbytes<-length(coredata(tmpx)) * nbytes(struct(double())) + length(.index(tmpx)) * nbytes(struct(double()))
		rm(tmpx)
		#filename: 43274Fsdrc.bin
		tmpfname <- ShareFiles[[id]]
		if (!file.exists(tmpfname) | ShareFilesWipe) { writeBin(raw(sizeinbytes),tmpfname) }
		
		#all.LABELS <- c('BidSize', 'BidPrice', 'AskPrice', 'AskSize', 'Last', 'LastSize',  'Volume',  'Open',  'High',  'Low' )			
		# Make a struct() of the required length
		numsymbols <- length(Contracts[[id]])
		atom.lst=c(
			list(double())   #timestamp
				,rep(
				#BIG# 1.37 Mb per symbol 100% size.  double() == real64()
				  list(double(), double()   ,double()   ,double() ,double()   ,double()  ,double()  ,double() ,double() ,double() )  
				#OK#  1.0 Mb  per symbol  80% sixe
				  #list(int32()  ,real64()   ,real64()   ,int32()  ,real64()   ,int32()   ,real64()  ,real64() ,real64() , int32() )
				#TIGHT?#  0.6 Mb  per symbol  45% size   
				  #list(uint24() ,real32()   ,real32()   ,uint24() ,real32()   ,uint24()  ,real32()  ,real32() ,real32() , uint24() )			
				, numsymbols)
		)   
		ss <- IBmakeVarlengthStruct(atom.lst, 1L)		
		stopifnot(is.struct(ss))
		if (verbose>0) cat('length of struct:',length(ss),'\n')
		# Create the mapping
		mmappings[[id]] <- mmap(tmpfname, ss )		
		if (ShareFilesWipe) {
		   # Initialize values - brute
		   mmappings[[id]][,1] <- NA
		   for (icol in 2:(numcolumns+1)) mmappings[[id]][,icol] <- NA
		   ## Write full xts data by column .....
		   #m[,1] <- .index(x)
		   #m[,2] <- coredata(x)[,1]
		   #m[,3] <- coredata(x)[,2]
		   #m[,4] <- coredata(x)[,3]
		   #m[,5] <- coredata(x)[,4]
		   # maybe it would be good to have some kind of  consistency check on the
		   # client side, where we compare the write/read data to an xts stored in
		   # a .rdata file ??
		}
    }
  }
  #store mappings in closure
  eW$assign.Data("mmappings",mmappings)
  #store number of contracts, etc in closure
  eW$assign.Data("numcontracts",numcontracts)
  eW$assign.Data('xupdate.mat', xupdate.mat)
  eW$assign.Data('numstreams', numstreams)

	# Initialize in-memory data buffer
    eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_,
        10), ncol = 10), 0), .Dimnames = list(NULL, c("BidSize",
        "BidPrice", "AskPrice", "AskSize", "Last", "LastSize",
        "Volume","Open","High","Low")))), n)) # instead of 'n' this should be 'numcontracts'

    eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) {
        tickType = msg[3]
        msg <- as.numeric(msg)
        id <- msg[2]
        data <- eW$get.Data("data")
        attr(data[[id]], "index") <- as.numeric(nowtime<-Sys.time())
        nr.data <- NROW(data[[id]])
        if (tickType == .twsTickType$BID) {
            data[[id]][nr.data, 1:2] <- msg[5:4]
        }
        else if (tickType == .twsTickType$ASK) {
            data[[id]][nr.data, 3:4] <- msg[4:5]
        }
        else if (tickType == .twsTickType$LAST) {
            data[[id]][nr.data, 5] <- msg[4]
        }
        else if (tickType == .twsTickType$HIGH) {
            data[[id]][nr.data, 9] <- msg[4]
        }
        else if (tickType == .twsTickType$LOW) {
            data[[id]][nr.data, 10] <- msg[4]
        }
        else if (tickType == .twsTickType$OPEN) {
            data[[id]][nr.data, 8] <- msg[4]
        }
        eW$assign.Data("data", data)
        c(curMsg, msg)
        #eW$makeDataRowIfTime()
    }
    
    eW$tickSize <- function(curMsg, msg, timestamp, file, ...) {
        data <- eW$get.Data("data")
        tickType = msg[3]
        msg <- as.numeric(msg)
        id <- as.numeric(msg[2])
        attr(data[[id]], "index") <- as.numeric(nowtime<-Sys.time())
        nr.data <- NROW(data[[id]])
        if (tickType == .twsTickType$BID_SIZE) {
            data[[id]][nr.data, 1] <- msg[4]
        }
        else if (tickType == .twsTickType$ASK_SIZE) {
            data[[id]][nr.data, 4] <- msg[4]
        }
        else if (tickType == .twsTickType$LAST_SIZE) {
            data[[id]][nr.data, 6] <- msg[4]
        }
        else if (tickType == .twsTickType$VOLUME) {
            data[[id]][nr.data, 7] <- msg[4]
        }
        eW$assign.Data("data", data)
        c(curMsg, msg)
        #?eW$makeDataRowIfTime()
    }

   eW$assign.Data("gridtimeoflastrow.int.sec", 0)  #initialize
   eW$assign.Data("barsize.int.sec", as.integer(Aggregation))  #initialize


   eW$makeDataRowIfTime <- function(nowtime) {
	 verbose <- 0  # 0: nothing, 1:basic, 2:all
	 gridtimeoflastrow.int.sec <- eW$get.Data("gridtimeoflastrow.int.sec")
	 barsize.int.sec <- eW$get.Data("barsize.int.sec")
	 if ( (gridtime<-unclass(nowtime)%/%barsize.int.sec*barsize.int.sec) > gridtimeoflastrow.int.sec ) {
		# Reset the timestamp
		eW$assign.Data("gridtimeoflastrow.int.sec", gridtime)
		#BEGIN - For all id's write the new row to shared memory as struct type 'double' with 11 columns.
		data <- eW$get.Data("data")
		mmappings <- eW$get.Data("mmappings")
		#print(mmappings[[id]][,1])
		#      #get the ii index of the first free (NA) timestamp of m[], assuming that we do not have an extraction function??
		#      #Faster: we could store this index in the closure somewhere and just increment it,
		#      #  instead of searching for it every time.
		remembered.iinext <- NA
		numcontracts <- eW$get.Data("numcontracts")
		vapply(1:numcontracts, function(id) {
		
#~ 		  tstmps<-mmappings[[id]][,1]
#~ 			#print( is.na( tstmps[[1]][1] ) )  #first element
#~ 		  iinext<-match(NA, tstmps[[1]] )  #Only works if we don't have leading NAs or interrupting NAs
#~ 			#print(iinext)
#~ 		  if (is.na(iinext)) stop(paste('Fatal error. Shared memory/file buffer for id',id,'is full, or invalid NAs in data.'))
#~ 		  mmappings[[id]][iinext,1] <- gridtime #.index(newbar)
#~ 		  mmappings[[id]][iinext,2] <- data[[id]][1,1] #coredata(newbar)[1,1]
#~ 		  mmappings[[id]][iinext,3] <- data[[id]][1,2] #coredata(newbar)[1,2]
#~ 		  mmappings[[id]][iinext,4] <- data[[id]][1,3] #coredata(newbar)[1,3]
#~ 		  mmappings[[id]][iinext,5] <- data[[id]][1,4] #coredata(newbar)[1,4]
#~ 		  mmappings[[id]][iinext,6] <- data[[id]][1,5] #coredata(newbar)[1,5]
#~ 		  mmappings[[id]][iinext,7] <- data[[id]][1,6] #coredata(newbar)[1,6] #NA
#~ 		  mmappings[[id]][iinext,8] <- data[[id]][1,7] #coredata(newbar)[1,7] #NA
#~ 		  mmappings[[id]][iinext,9] <- data[[id]][1,8] #coredata(newbar)[1,8] #NA      
#~ 		  mmappings[[id]][iinext,10] <- data[[id]][1,9] #coredata(newbar)[1,9] #NA      
#~ 		  mmappings[[id]][iinext,11] <- data[[id]][1,10] #coredata(newbar)[1,10] #NA      
#~ 			#cat('data: ',unlist(data[[id]]),'\n')
#~ 			cat(id,' mmappings[[id]][iinext,] ',iinext,' :',unlist(mmappings[[id]][iinext,]),'\n')			
		  
		  # BEGIN - xupdate all multisymbol streams that contain this symbol
		  xupdate.mat <- eW$get.Data("xupdate.mat")	
		  #for (xu.idx in which(xupdate.mat[,1]==id)) {
		  vapply( which(xupdate.mat[,1]==id) ,function(xu.idx) {
			target.id   <- xupdate.mat[xu.idx,2]
			offset.num  <- xupdate.mat[xu.idx,3]
			target.size <- xupdate.mat[xu.idx,4]
			#got a full multisymbol row!
			#WRITE multi* to SHMEM FILE
			if (is.na(iinext <- remembered.iinext[target.id])) {
				tstmps<-mmappings[[target.id]][,1]
				iinext<-match(NA, tstmps[[1]] )  #Only works if we don't have leading NAs or interrupting NAs
				if (is.na(iinext)) stop(paste('Fatal error. Shared memory/file buffer for id',id,'is full, or invalid NAs in data.'))
				remembered.iinext[target.id] <<- iinext  # <<- PARENT, two levels up
			} 
			mmappings[[target.id]][iinext,1] <- gridtime	#either here, with 'remembered.' or do this last for all streams. see sapply below
			ixstart <- 2+(offset.num-1)*ncol(data[[id]])
			ixend   <- ixstart + ncol(data[[id]])-1
			if (verbose>1) {
			  cat('data: ',data[[id]],'\n')
			  cat('Writing data to target stream',target.id,' at ixstart:',ixstart,' ixend:', ixend, '\n')			
			}
			#mmappings[[target.id]][iinext,  ixstart:ixend    ] <- data[[id]]  # Doesnt work like this. need to split it up.
			vapply( ixstart:ixend, function(ix) {
				mmappings[[target.id]][iinext,  ix    ] <- data[[id]][1,ix-ixstart+1]
				1
			},0)
			if (verbose>1) {	
			  cat('Stream',target.id,':', unlist( mmappings[[target.id]][iinext,]) ,'\n')
			}
								
			1	  
		  },0)
		  # END   - xupdate all multisymbol streams that contain this symbol
				
		  1		  
		},0)
		#
		if (verbose>1) cat('remembered.iinext: ',remembered.iinext,'\n')
		#
		if (verbose>0) {	
			numstreams <- eW$get.Data("numstreams")
			vapply((numcontracts+1):(numcontracts+numstreams), function(stream.id) {
				iithis <- remembered.iinext[stream.id]
				#mmappings[[stream.id]][iithis,1] 
				cat('Final Stream',stream.id,':', unlist( mmappings[[stream.id]][iithis,] ) ,'\n')
				1
			},0)
		}
				
      #END - Write the newbar to shared memory
	 }
   }


   #Define the error function
   eW$errorMessage <- function(curMsg, msg, timestamp, file, twsconn, ...)
   {
      if(msg[3] == "1100") {
        twsconn$connected <- FALSE
        ## Do not disconnect, since the TWS may reconnect, with a 1101, 1102 and
        ## we still want to be around when that happens!!!
        #twsDisconnect(twsconn)                    #Soren added
        #stop(paste("Shutting down: TWS Message:",msg[4])) #Soren added
      }
      if(msg[3] %in% c("1101","1102")) twsconn$connected <- TRUE

      cat(as.character(Sys.time()),"TWS Message:",msg,"\n")

# It looks like the 2105 (lost connection) Error and a subsequent 2106 (connection ok)
# requires us to re-issue the request for marketdata !!!
# 4-2--1-2105-HMDS data farm connection is broken:euhmds2-
# 4-2--1-2106-HMDS data farm connection is OK:euhmds2-
      if(msg[3] == "2105") {return('BREAKBREAK')}

# An error that requires re-connection is Error 420. (Happens when we request data
# from two different ip addresses, i.e. at home and at work)
# 4-2-78046390-420-Invalid Real-time Query:Trading TWS session is connected from a different IP address-
      if(msg[3] == "420") {return('BREAKBREAK')}

   }

  return(eW)

}


## our custimized twsCALLBACK. only change is that it allows us to exit from the otherwise
## infinite while(TRUE) loop.

twsCALLBACKdatafeed <- function(twsCon, eWrapper, timestamp, file, playback=1, ...)
{
  if(missing(eWrapper))
    eWrapper <- eWrapper()
  con <- twsCon[[1]]

  if(inherits(twsCon, 'twsPlayback')) {
	stop('Playback not supported')
  } else {
    while(TRUE) {
      #socketSelect(list(con), FALSE, NULL)
      if (socketSelect(list(con), FALSE, timeout=1L)) {  #timeout at 1sec
		  curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
		  res <- NULL
		  nowtime <- Sys.time() 
		  if(!is.null(timestamp)) {
		  #print('AAA')
			res <- processMsg(curMsg, con, eWrapper, format(nowtime, timestamp), file, twsCon, ...)
		  } else {
			res <- processMsg(curMsg, con, eWrapper, timestamp, file, twsCon, ...)
		  }
      } else nowtime <- Sys.time()
      #S. Added here to make sure Mktdata is always written to a new row, even if no update ocurred
      #print('BBB')
      eWrapper$makeDataRowIfTime(nowtime) 
      #print('CCC') 
      #S. Added this as a breaking mechanism
      if (!is.null(res)) { if (as.character(res)=='BREAKBREAK') break }
    }
  }
}



