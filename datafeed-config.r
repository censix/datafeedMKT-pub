#  * ClientId. 
IBclientid = 88


#   * List of IB contract specifications
#				symbol, 
#				type, (stock)
#				exchange, 
#				primary,
#				currency (EUR),
#				outputbarsize=5    # ignored. 
#				keepcsvhistory= FALSE   # ignored
#				keepoutputhistory=TRUE  #if TRUE the contents of the *-shared.bin file will first be appended to a history
#                                   #file (STREAM??.rdata in xts format) before is is overwritten by new data

IBcontractSpecs <- list(
# Some Xetra stocks

 "BMW"= list( symbol="BMW", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE ),
 "DB1"= list( symbol="DB1", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE ),
 "DBK"= list( symbol="DBK", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE ),
 "DPB"= list( symbol="DPB", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE ),
 "DPW"= list( symbol="DPW", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE ),
 "LHA"= list( symbol="LHA", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE ),
 "RWE"= list( symbol="RWE", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE ),
 "SAP"= list( symbol="SAP", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE ),
 "SIE"= list( symbol="SIE", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE ),
 "TUI"= list( symbol="TUI", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE ),
 "UBS"= list( symbol="UBS", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE ),
 "VOW"= list( symbol="VOW", type='stock', exchange="IBIS", primary="IBIS", currency="EUR", outputbarsizeseconds=5L, keepcsvhistory=FALSE, keepoutputhistory=TRUE )

)

 
IBstreamsymbolSpecs <- list( stream1=c( "BMW","DB1","DBK","DPB","DPW","LHA","RWE","SAP","SIE","TUI","UBS","VOW"  )  #12
							, stream2=c("DPB","DPW","LHA")  # 3
							)  


#  * Directories
IBdatafeedRawDir =     'datafeed-raw'     # contains raw output data from IB:           'SYMBOL-raw.csv'
IBdatafeedShareDir =   'datafeed-shared'  # contains file-based shared mmap structures: 'SYMBOL-EXCHANGE-shared.bin'
IBdatafeedHistoryDir = 'rdata/intraday/ibrokers/XXsec'  # contains historical SYMBOL_xts data in files named: 'SYMBOL.rdata'
## WARNING ## note that new data will only be appended to the .rdata file at the beginning of the NEXT session. Maybe change this, to happen at the END of THIS session
# make a symbolic link: ln -s ~/portfolios/rdata ./rdata
# and create the directories ~/portfolios/rdata/intraday/ibrokers/XXsec

