#====Functions to generate all combinations of parameters====

#work out number of possible combinations
ncomb <- function(params) { 
  tail(cumprod(sapply(params, length)),1)
}

#how long will each sequence set be?
multigen <- function(params, paramindex) { 
  if (paramindex==1){multi <- 1}
  else {multi <- ncomb(params[1:(paramindex-1)])}
  return(multi)
}

#make sequence
paramaeterseq <- function(sequence, multi, ncomb) { 
  rep(rep(sequence, each = multi), len = ncomb)
}

#for sapply
parametercol <- function(params, paramindex, ncomb) { 
  multi <- multigen(params, paramindex)
  paramaeterseq(params[[paramindex]], multi, ncomb)
}

#main function==== 
#generate all possible combinations in a df - receives list
parametercombinations <- function(params) { 
  n <- 1:length(params)
  combinations <- as.data.frame(sapply(n, function(n) parametercol(params, n, ncomb(params))))
  names(combinations) <- names(params)
  return(combinations)
}

#testing====
parameters <- list(a = c(1,2,3,4),
                   b = c(5,6),
                   c = c(7,8,9),
                   d = c(10, 11, 12))
test <- parametercombinations(parameters)

#Used for project====
#18.07
parameters <- list(c = c(20, 40, 60),
                   cov = c(9, 34, 60),
                   periodicity = c(5, 3.5, 2),
                   sensitvity = c(79.2, 100))
scenarios <- parametercombinations(parameters)
write.csv(scenarios, file = "allscenariocombinations.csv")
