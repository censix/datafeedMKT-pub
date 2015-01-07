# datafeedMKT-pub
Tickdata streams of up to 100 symbols per stream, downsampled to 5 sec intervals

This collection of R scripts can be used to generate live market data streams from IB.
The entry point is '00-run-PAPER.r' where the connection to the IB TWS is established
and the different streams in the datafeed are launched. See 'datafeed-config.r' for a 
definition of the individual streams and its components. The most basic stream consists
of only one symbol, i.e. a stock like 'SIE' on IBIS. The stream delivers to following for
each symbol it contains:
BidSize, BidPrice, AskPrice, AskSize, LastPrice, Volume, ... etc.
These values are sampled(!) every 5 seconds. If you need the pure tickdata 
without the 5 sec binning, then this datafeed tool is not for you!

The purpose is to provide a datafeed that does not use realTimeBars() and delivers
Bid/Ask and Volume for the pure-R-INTRADAY trading framework.


