context("test for main function")

#checking probability
check_prob = function(p){
  if((0<=p) & (p<=1)) {
    return(TRUE)
  } else {
    stop("invalid prob value")
  }
}
check_prob

#checking trials
check_trials = function(n){
  if(n>0) {
    return(TRUE)
  } else {
    stop("invalid trials value")
  }
}
check_trials

#checking success
check_success = function(k, n){
  if((0<=k) & (k<n)) {
    return(TRUE)
  } else {
    stop("invalid success value")
  }
}
check_success
