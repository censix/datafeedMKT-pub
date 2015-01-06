
require(mmap) #for creating shared memory-mapped files
require(IBrokers)

source("datafeed-config.r")

#global: create the connection to IB  
ibConnection<-NULL
if (!is.twsConnection(ibConnection)) {
   ibConnection<- ibgConnect(clientId=IBclientid, host="localhost",
                                      port=5001, verbose=TRUE,      #Connecting to PAPER trading account API port
                                      timeout=15, filename=NULL)
}

#launch the datafeed request & processing
source("datafeed.r")




