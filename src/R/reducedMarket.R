reducedMarket = function(database, reducedVariable, base) {
  apollo_beta = c(0, 0)
  names(apollo_beta) = c(
    paste("asc_", reducedVariable[1], sep = ""),
    paste("asc_", reducedVariable[2], sep = "")
  )
  
  apollo_fixed = c(paste("asc_", base, sep = ""))
  
  apollo_control = list(modelName = "MNL",
                        modelDescr = "SimpleMNL",
                        indivID = "X")
  
  apollo_inputs = apollo_validateInputs(apollo_beta, apollo_fixed, database, apollo_control)
  
  model = apollo_estimate(apollo_beta,
                          apollo_fixed,
                          prob_reducedMarket,
                          apollo_inputs)
  
  assign("reducedVariable", reducedVariable)
  
  return(model)
}


prob_reducedMarket = function(apollo_beta,
                        apollo_inputs,
                        functionality = "estimate") {
  apollo_attach(apollo_beta,
                apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  V = list()
  
  if ((reducedVariable == c("train","auto")) %>% all) {
    alternatives = c(train = 4, auto = 7)
    V[["train"]] = asc_train 
    V[["auto"]] = asc_auto 
  }
  if ((reducedVariable == c("el","auto")) %>% all) {
    alternatives = c(el = 2, auto = 7)
    V[["el"]] = asc_el 
    V[["auto"]] = asc_auto 
  }
  if ((reducedVariable == c("el","train")) %>% all) {
    alternatives = c(el = 2, train = 4)
    V[["el"]] = asc_el 
    V[["train"]] = asc_train 
  }
  
  
  mnl_settings = list(
    alternatives = alternatives,
    avail = list(el = 1, train = 1, auto = 1),
    choiceVar = choice,
    V = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
