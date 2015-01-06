

# Make a mmap 'sruct()' from a list of Ctype definitions

IBmakeVarlengthStruct <- function(atom, n=1L) {
  stopifnot(is.list(atom) & is.integer(n))
  ddd <-  rep(atom,n) #length n
  # below is identical to struct() code.
  dots <- lapply(ddd, as.Ctype)
  bytes_ <- sapply(dots, attr, which = "bytes")
  offset <- cumsum(bytes_) - bytes_
  ss<- structure(dots, 
	bytes = as.integer(sum(bytes_)), 
	offset = as.integer(offset), 
        signed = NA, 
	class = c("Ctype", "struct"))
  return(ss)
}

#~ atom <- list(double())
#~ ss<-IBmakeVarlengthStruct(atom, 200L)
#~ is.struct(ss)  # TRUE :))
#~ #str(ss)
#~ 
#~ 
#~ atom <-  list(double(), uint8())
#~ ss<-IBmakeVarlengthStruct(atom, 50L)
#~ is.struct(ss) 
#~ 
#~ 
#~ atom <-  list(double(), uint8(), int32())
#~ ss<-IBmakeVarlengthStruct(atom, 1L)
#~ is.struct(ss) 
