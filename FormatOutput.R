format.table <- function(l){
  
  if("sample" %in% names(l)){
    l$sample <- NULL
  }
  if("dist" %in% names(l)){
    l$dist <- NULL
  }
  if("post.dist.lower" %in% names(l)){
    l$post.dist.lower <- NULL
    l$post.dist.upper <- NULL
  }
  
  names.l <- names(l)
  max.length <- max(sapply(l, length))
  which.vector <- sapply(l, length) > 1
  table <- matrix("", ncol = max.length + 1, nrow = length(l) + sum(which.vector))
  for(i in 1:length(l)){
    index.i <- i + ifelse(i == 1, 0, sum(which.vector[1:(i-1)]))
    table[index.i, 1] <- names.l[i]
    if(which.vector[i]){
      if(is.null(names(l[[i]]))){
        names.li <- rep("", times = length(l[[i]]))
      }
      else{
        names.li <- names(l[[i]])
      }
      table[index.i, seq(2, along.with = l[[i]])] <- names.li
      table[index.i + 1, seq(2, along.with = l[[i]])] <- unname(l[[i]])
    }
    else{
      table[index.i, 2] <- l[[i]]
    }
  }
  colnames(table) <- c("Attribute", paste("Value", 1:max.length))
  return(table)
}