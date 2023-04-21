riskUsersCount = function(x){
  nrow(x[x$TARGET==1,])
}

