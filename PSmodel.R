library(deSolve)

PSmodel <- function (t, x, pars) {

  with(as.list(c(x,pars)),{
    
    CDR_survey <- CDR_int(CDR, cov, k)
    del <- approxfun(x=survey_times(survey_interval),y=rep(c(CDR, CDR_survey, CDR),3),method="linear",rule=2)

    births <- Mu*(U + Ls + Lf + I + N + C) + Mui*I + Mun*N   
    P <- U + Ls + Lf + I + N + C 
    
    #Equations####
    #Betas
    bw <- (1-r) * b
    bc <- r * b
    
    #FOIs
    foi_basic <- bw * U * (I + (c*N))
    foi_comm <- bc * U *Ic
    foi_exo_react <- bw * a * x * (I + (c*N)) * Ls
    foi_exo_react_comm <- bc * a * x * Ic * Ls
    foi_reinf <- bw * a * x * (I + (c*N)) * C
    foi_reinf_comm <- bc * a * x * Ic * C
    
    # Derivatives
    
    dU <- births - foi_basic - foi_comm - Mu*U                    
    dLs <- ((1-a)*(foi_basic + foi_comm)) - foi_exo_react - foi_exo_react_comm - (vs + Mu)*Ls   
    dLf <- (a * (foi_basic + foi_comm)) - (vf + Mu)*Lf 
    dI <- sg*(vs*Ls + vf*Lf + foi_exo_react + foi_exo_react_comm 
                            + foi_reinf + foi_reinf_comm + p*C) + theta*N - (del(t)* tau + Mu + Mui + nc)*I 
    dN <- (1-sg)*(vs*Ls + vf*Lf + foi_exo_react + foi_exo_react_comm 
                                + foi_reinf + foi_reinf_comm + p*C) - (theta + del(t)* tau + Mu + Mun + nc)*N  
    dC <- (nc + del(t)* tau)*(I + N) - foi_reinf - foi_reinf_comm - (p + Mu)*C 

    # Outputs====
    
    Inc <- vs*Ls + vf*Lf + foi_exo_react + foi_exo_react_comm + foi_reinf + foi_reinf_comm + p*C 
    Inc_first <- vf*Lf
    Inc_react <- vs*Ls + foi_exo_react_comm + foi_exo_react
    Inc_reinf <- foi_reinf + foi_reinf_comm
    Inc_relap <- p*C
    Inc_recent <- Inc_first + foi_exo_react + foi_exo_react_comm + Inc_reinf 
    Inc_inf_out_comm <- foi_comm + foi_exo_react_comm + foi_reinf_comm
    case_notifications <- del(t) * (I + N) #Is this right?
    cases_removed <- del(t) * tau * (I + N) #Is this right?
    treatment_cov <- case_notifications / Inc
    TB_prev <- I+N                 
    Inf_prev <- Ls + Lf + I + N + C        
    TB_Deaths <- Mui*I + Mun*N
    
    dur_I <- 1/(Mu + Mui + del(t)* tau + nc)
    dur_N <- 1/(theta + Mu + Mun + del(t)* tau + nc)
    
    dur_active_TB <- (I*dur_I + N*dur_N)/(I + N)
    dur_active_inf_TB <- (I*dur_I + c*N*dur_N)/(I + c*N)
    
    list(
      c(dU,dLs,dLf,dI,dN,dC),
      Total = P,
      Inc = Inc * 100000,
      Inc_first = Inc_first * 100000,
      Inc_react = Inc_react * 100000,
      Inc_reinf = Inc_reinf *100000,
      Inc_relap = Inc_relap *100000,
      Inc_recent = Inc_recent * 100000,
      Inc_inf_out_comm = Inc_inf_out_comm * 100000,
      case_notifications = case_notifications * 100000,
      treatment_cov = treatment_cov,
      cases_removed = cases_removed * 100000,
      Prev = TB_prev * 100000,
      Inf_prev = Inf_prev * 100000,
      Mort = TB_Deaths * 100000,
      dur_active_TB = dur_active_TB,
      dur_active_inf_TB = dur_active_inf_TB
    )
  })
}

CDR_int <- function(CDR, cov, k) {CDR + ((1-CDR)*cov*k)} #Need to check this is OK

survey_times <- function(survey_interval) {
  cumsum(c(500, 0.2, 0.2, rep(c(survey_interval, .2, .2), 2)))
}

Init_inf <- 0.2 # Fraction of the pop initially infected
yinit <- c(U=1-Init_inf,Ls=0.99*Init_inf,Lf=0,I=0.01*Init_inf,N=0,C=0)

#pars====
pars_base <- c(b=22,
              p=0.01,
              a=0.11,
              vf=0.67,
              vs=0.0005,
              sg=0.45,
              x=0.65,
              nc=0.2,
              theta=0.015,
              Mu=0.06,
              Mui=0.3,
              Mun=0.21,
              CDR=0.7,
              cov = 0, 
              k = 0,
              tau=0.91,
              r=0.2,
              c=0.22,
              Ic = 0.002,
              survey_interval=5)
#Solve====
# sol_base <-ode(y=yinit,times=seq(0,100, by=0.02),func=PSmodel,parms=pars_base)
# #time step = 0.02 of a year
# sol_base_df <- as.data.frame(sol_base)
# plot(sol_base_df$time, sol_base_df$Prev, type = "l")
# lines(sol_base_df$time, sol_base_df$case_notifications)
# plot(sol_base_df$time, sol_base_df$treatment_cov, type = "l")
# plot(sol_base_df$time, sol_base_df$cum_I, type = "l")
# plot(sol_base_df$time, sol_base_df$I, type = "l")










                 
                 
                 