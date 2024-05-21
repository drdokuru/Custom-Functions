##Function Scripts

#SSR for difference in beta and B priori

SSR <- function(n, Y_c, Y_a) {
  SSR_sun <- (n*(Y_c-Y_a)^2)
  return(SSR)
}

#MSE (s^2 or variance) with SSE n and p

MSE <- function(SSE,n,p) {
  MSE <- (SSE/(n-p))
  return(MSE)
}

#PRE with SSE_C and SSE_A
PRE <- function(SSE_C, SSE_A) {
  PRE <- (SSE_C - SSE_A) / SSE_C
  return(PRE)
}

#F* with PRE
F_PRE <- function(PRE, PA, PC, n) {
  F <- (PRE/(PA - PC)) / ((1-PRE)/(n-PA))
  return(F)
}


#F* with PRE
F_SSR <- function(SSR, SSE_A, PA, PC, n) {
  F_SSR <- (SSR/(PA - PC)) / ((SSE_A)/(n-PA))
  return(F_SSR)
}

#t* with F*
t_F <- function(F) {
  t_F <- (sqrt(F))
  return(t_F)
}

#95% CI with t_crit, RMSE, n
CI_t <- function(b0,t, RMSE, n) {
  CI_t <- (t) * ((RMSE)/(sqrt(n)))
  CI_L<- b0-CI_t
  CI_U<-b0+CI_t
  list<-c(CI_L,CI_U)
  return(list)
}


#To get Fcrit
#library(daewr)
#Fcrit(alpha, PA-PC, n)
#95% CI with F_crit, MSE (s^2)(Error line), n
CI_F <- function(b0,F, MSE, n) {
  CI_F <- (sqrt((F*MSE)/n))
  CI_L<- b0-CI_F
  CI_U<-b0+CI_F
  list<-c(CI_L,CI_U)
  return(list)

}

#Effect Size with PRE
Etasq_PRE <- function(PRE, PC, PA, n) {
  Etasq_PRE <- (1-((1-PRE)*((n-PC)/(n-PA))))
           return(Etasq_PRE)
}

#95% CI for slope with t_crit, se (of slope)
slope_CI_t <- function(b1,t, se) {
  slope_CI_t <- (t*se)
  CI_L<- b1-slope_CI_t
  CI_U<-b1+slope_CI_t
  list<-c(CI_L,CI_U)
  return(list)
}

 