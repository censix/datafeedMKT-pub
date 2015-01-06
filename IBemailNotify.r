
# IBemailNotify will send an email Notification with Contract and Order information.
# !! IMPORTANT !!
# This function MUST NOT WAIT for the sending to complete. It should spawn a child shell and the forget about it
# !! IMPORTANT !!
# This file can be removed from the sources directory, and everything else will still run

.emailsend <- function(msgBODY, msgHEAD)  { 
  #tmpCommand <- 
  #system(command=tmpCommand , ignore.stdout = TRUE, ignore.stderr = TRUE, intern = FALSE, wait = FALSE )   #wait=FALSE is very important in order to not interrupt/delay the R processing
}

IBemailMsg <- function(msgBODY='', msgHEAD='', ...) {    
  .emailsend(msgBODY, msgHEAD) 
}

IBemailNotify <- function(emType=NULL,emSymbol=NULL, emAction=NULL, emTotalQuantity=NULL, emLmtPrice=NULL, ...) {
  if (emType=='ORDER') {
    tmpNotifyBODY <- paste('Order Details:', emAction, emTotalQuantity ,emSymbol, emLmtPrice)
    tmpNotifyHEAD <- paste('Placed',emAction,'order for ',emSymbol)
  }
  if (emType=='EXECUTION') {
    tmpNotifyBODY <- paste('Execution Details:', emAction, emTotalQuantity ,emSymbol, emLmtPrice)
    tmpNotifyHEAD <- paste('Executed',emAction,'order for ',emSymbol)
  }    
  if (emType=='CANCEL') {
    tmpNotifyBODY <- paste('Cancellation Details:', emAction, emTotalQuantity ,emSymbol)
    tmpNotifyHEAD <- paste('Cancelled',emAction,'order for ',emSymbol)
  }    
  
  .emailsend(msgBODY=tmpNotifyBODY, msgHEAD=tmpNotifyHEAD)  
}
