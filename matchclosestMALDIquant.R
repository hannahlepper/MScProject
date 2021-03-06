#From: MALDIquant package; https://www.rdocumentation.org/packages/MALDIquant/versions/1.16.1/topics/match.closest
#Accessed 27/07/17 on Github https://github.com/sgibb/MALDIquant/blob/master/R/match.closest-functions.R
#The latest package cannot be loaded into this R version; will check again later to see if I can then.

match.closest <- function(x, table, tolerance=Inf, nomatch=NA_integer_) {
  lIdx <- findInterval(x, table, rightmost.closed=FALSE, all.inside=TRUE)
  rIdx <- lIdx + 1L
  
  lIdx[lIdx == 0L] <- 1L
  
  lDiff <- abs(table[lIdx] - x)
  rDiff <- abs(table[rIdx] - x)
  
  d <- which(lDiff >= rDiff)
  
  lIdx[d] <- rIdx[d]
  
  if (any(is.finite(tolerance))) {
    if (any(tolerance < 0L)) {
      warning(sQuote("tolerance"), " < 0 is meaningless. Set to zero.")
      tolerance[tolerance < 0L] <- 0L
    }
    
    if (length(nomatch) != 1L) {
      stop("Length of ", sQuote("nomatch"), " has to be one.")
    }
    
    tolerance <- rep_len(tolerance, length(table))
    
    lDiff[d] <- rDiff[d]
    lIdx[lDiff > tolerance[lIdx]] <- nomatch
  }
  
  lIdx
}
