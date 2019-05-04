
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


##################

#creating bin_choose function
#'@title: Bin choose
#'@description: `"bin_choose" function returns us the number of ways we can choose success out of the total number of trials.
#'@param: n as a number of trials and k as a number of successes
#'@return: product factorial of n and k
#'@export:
#'@examples: bin_choose(n = 5, k = 2) equlas to 10
bin_choose <- function(n, k){
  if (k>n) {
    stop("k cannot be greater than n")
  }
  (factorial(n))/(factorial(k)*factorial(n-k))
}
#bin_choose


#if k is greater than n than stop

#bin_choose(n = 5, k = 2)
#bin_choose(5, 0)
#bin_choose(5, 1:3)


#creating bin_probability function
#'@title: bin_probability
#'@description: The function returns the probability of successes out of the total number of trials.
#'@param: number of successes, trials and probability
#'@return: probability
#'@export:
#'@examples: bin_probability(success = 2, trials = 5, prob = 0.5) equlas to 0.3125
bin_probability = function(success, trials, prob) {
  check_prob(prob)
  check_trials(trials)
  check_success(success,trials)
  result = bin_choose(trials, success)*prob^success*((1-prob)^(trials-success))
  return(result)
}
bin_probability


#bin_probability(success = 2, trials = 5, prob = 0.5)
#bin_probability(success = 0:2, trials = 5, prob = 0.5)
#bin_probability(success = 55, trials = 100, prob = 0.45)





#creating bin_distribution  function
#'@title: bin_distribution
#'@description: The function returns probability in binomial distribution in a dataframe with two columns: success and probability.
#'@param: triasls, probability
#'@return: distibution table in a form of a dataframe with two columns: success and probability
#'@export:
#'@examples: bin_distribution(trials = 5, prob = 0.5)
bin_distribution = function(trials, prob){
  result2 =  data.frame(success = c(0:trials), probability = bin_probability(c(0:trials), trials, prob))
  class(result2) = c("bindis","data.frame")
  return(result2)
}


#bin_distribution(trials = 5, prob = 0.5)
#bin_distribution


##create a function for plot
#'@export:
plot.bindis = function(trials, prob){
  result2 =  data.frame(success = c(0:trials), probability = bin_probability(c(0:trials), trials, prob))
  plot = barplot(result2$probability, main="Probability",
                 xlab="Successes")
  return(plot)
}

#'@export:
plot.bindis(trials = 5, prob = 0.5)


### 1.6) Function bin_cumulative()


#creating bin_cumulative function
#'@title: bin_cumulative
#'@description: The function returns us cumulative probability and a dataframe with three columns: success, probability and cumulative.
#'@param: trials and probability
#'@return: distibution table in a form of a dataframe with two columns: success and probability and cumulative probability
#'@export:
#'@examples: bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative = function(trials, prob){
  result3 =  data.frame(success = c(0:trials), probability = bin_probability(c(0:trials), trials, prob))
  cumulative = c()
  for (i in 1:(trials+1)){
    cumulative[i] = sum(result3$probability[1:i])
  }
  result3$cumulative = cumulative
  class(result3) = c("bincum","data.frame")
  return(result3)
}

#bin_cumulative(trials = 5, prob = 0.5)


#'@export:
plot.bincum = function(trials, prob){
  result3 =  data.frame(success = c(0:trials), probability = bin_probability(c(0:trials), trials, prob))
  cumulative = c()
  for (i in 1:(trials+1)){
    cumulative[i] = sum(result3$probability[1:i])
  }
  result3$cumulative = cumulative
  plot = plot(result3$cumulative, type = "l", main="Probability",
              xlab="Successes")
  class(result3) = c("bincum","data.frame")
  return(plot)
}

#plot.bincum(trials = 5, prob = 0.5)



#1.7) Function bin_variable()
#creating bin_variable function
#'@title: bin_variable
#'@description: The function returns us a list of porbabilites and success followed by print.binvar function that prints the numer of trials and the probability of success.
#'@param: trials and probability
#'@return: prints the numer of trials and the probability of success.
#'@export:
#'@examples: bin1 <- bin_variable(trials = 10, p = 0.3)
bin_variable = function(trials, prob){
  check_prob(prob)
  check_trials(trials)
  list = c(trials, prob)
  class(list) = "binvar"
  return(list)
}


#'@export:
print.binvar = function(x){
  cat("Binomial variable")
  cat("Parameters")
  cat("number of trials:", x[1], "\n")
  cat("prob of success :", x[1], "\n")

}



#bin1 <- bin_variable(trials = 10, p = 0.3)
#bin1


#auxilary functions


aux_mean = function(n,p){
  check_prob(p)
  mean = n*p
  return(mean)
}

#aux_mean(7,0.2)


aux_variance = function(n,p){
  check_prob(p)
  variance = (n*p)*(1-p)
  return(variance)
}

#aux_variance(7,0.7)


aux_mode = function(n,p){
  check_prob(p)
  m = ((n*p)+p)
  if(m %% 1 == 0){
    return(c(m,m-1))
  } else{
    return(floor(m))
  }
}


#aux_mode(7,0.5)



aux_kurtosis = function(n,p){
  check_prob(p)
  kurt = (1-(6*p)*(1-p))/(n*p*(1-p))
  return(kurt)
}

#aux_kurtosis(7,0.5)




aux_skewness = function(n,p){
  check_prob(p)
  skewness = (1-2*p)/sqrt(n*p*(1-p))
  return(skewness)
}

#aux_skewness(7,0.9)
#aux_skewness(7,2)



#creating function summary.binvar
#'@title: summary.binvar
#'@description: The function returns the number of trials and probability with mean, variance, mode, skewness and kurtosis and prints the values with summary.binvar
#'@param: trials and probability
#'@return: prints the values of trials and probability with mean, variance, mode, skewness and kurtosis with summary.binvar
#'@export:
#'@examples: #x1 <- bin_variable(10, 0.5)
             #summary.binvar(x1)


bin_variable = function(trials, prob){
  check_prob(prob)
  check_trials(trials)
  list = c(trials, prob)
  class(list) = "binvar"
  return(list)
}


#'@export:
print.binvar = function(x){
  cat("Binomial variable")
  cat("Parameters")
  cat("number of trials:", x[1], "\n")
  cat("prob of success :", x[1], "\n")

}


summary.binvar = function(x){
  object = list(trials = x[1], probability = x[2], mean = aux_mean(x[1], x[2]), variance = aux_variance(x[1], x[2]), mode = aux_mode(x[1], x[2]), skewness = aux_skewness(x[1], x[2]), kurt = aux_kurtosis(x[1], x[2]))
  return(object)
  class(object) = "summary.binvar"
}
#x1 <- bin_variable(10, 0.5)
#summary.binvar(x1)


#'@export:
print.summary.binvar = function(x){
  cat("Summary Binomial")
  cat("number of trials:",  x[1], "\n")
  cat("prob of success :", x[2], "\n")
}

#bin1 <- bin_variable(trials = 10, p = 0.3)
#binsum1 <- summary(bin1)
#binsum1


#'@export:
print.binvar = function(x){
  cat("Binomial variable")
  cat("Parameters")
  cat("number of trials:" ,  x[1], "\n")
  cat("prob of success : ", x[2], "\n")

}
