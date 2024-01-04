
treeall <- function(formula, data, location,complexity){
  
  library(GD)
  library(rpart)
  library(IDSA)

  y <- all.vars(formula)[1]
  vars <- all.vars(formula)[-1]

  data.y <- data[, y]
  data.y <- as.vector(as.matrix(data.y))
  data.x <- data[, vars]
  data.loc <- data[, location]

  nx <- length(vars)
  

  
print('start')
  # combinations of x
  cox <- list()
  for (i in 1:nx){
    coxi <- combn(1:nx, i)
    coxl <- split(coxi, rep(1:ncol(coxi), each = nrow(coxi)))
    cox <- c(cox, coxl)
  }
  names(cox) <- 1:length(cox)
  #

  var <- unlist(lapply(1:length(cox), function(u)
    paste(vars[cox[[u]]], collapse = "_")))
  n.var <- sapply(cox, length)
  result <- data.frame("var" = var, "n.var" = n.var,
                       "q.variance" = rep(NA, length(cox)))


  for (i in 1:length(cox)){
  #for (i in 1:54){
    #print(i)
    if(i%%1000==0)
    {
      out=paste('the',i,'in',length(cox))
      print(out)
    }
      
    m <- cox[[i]]
    g <- as.formula(paste(y, paste(vars[m], collapse = "+"), sep = "~"))

    git <- rpart(g, method='anova', data=data,cp=complexity) 
    

    zones <- as.character(git$where)
    
    di <- data.frame(data.y, zones)
    gdi <- gd(data.y ~ zones, di)
    result[i, 3] <- gdi$Factor$qv
    result[i, 4] <- length(unique(zones))

  }

  
  k <- which(result[, 3] == max(result[, 3]))[1]
  
  var.k <- vars[cox[[k]]]
  
  f.k <- as.formula(paste(y, paste(var.k, collapse = "+"), sep = "~"))
  # f.k=wetland~xx
  z <- list("best.vars" = var.k, "best.formula" = f.k,
            "all.q" = result)
  
  
  
  return(z)
  

}


