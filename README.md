# datafeedMKT-pub
Tickdata streams of up to 100 symbols per stream, downsampled to 5 sec intervals

The purpose is to provide a datafeed that uses reqMktData() instead of realTimeBars(), can contain more
than one symbol per stream and delivers Bid/Ask prices to the pure-R-INTRADAY trading framework. It
is currently working with stocks and has been tested with 25 symbols in a single stream.

The entry point is '01run-paper.r' where the connection to the IB TWS is established
and the different streams in the datafeed are launched. See 'datafeed-config.r' for a 
definition of the individual streams and its components. The most basic stream consists
of only one symbol, i.e. a stock such as 'SIE' on IBIS. For each symbol in a stream, the following
fields are delivered:

    BidSize, BidPrice, AskPrice, AskSize, Last, LastSize, Volume(cumulative), Open(day), High(day), Low(day)
    
These values are sampled(!) every 5 seconds. If you need pure tickdata without the 5 sec binning, 
then this datafeed tool is not for you!




