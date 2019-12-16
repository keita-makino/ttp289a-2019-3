market = function(database) {
  apollo_beta = c(asc_el = 0,
                  asc_train = 0,
                  asc_auto = 0)
  
  apollo_fixed = c("asc_auto")
  
  apollo_control = list(modelName = "MNL",
                        modelDescr = "SimpleMNL",
                        indivID = "X")
  
  apollo_inputs = apollo_validateInputs(apollo_beta, apollo_fixed, database, apollo_control)
  
  model = apollo_estimate(apollo_beta,
                          apollo_fixed,
                          prob_market,
                          apollo_inputs)
  
  return(model)
}


prob_market = function(apollo_beta,
                                apollo_inputs,
                                functionality = "estimate") {
  apollo_attach(apollo_beta,
                apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  V = list()
  
  V[["el"]] = asc_el
  V[["train"]] = asc_train
  V[["auto"]] = asc_auto
  
  mnl_settings = list(
    alternatives = c(el = 2, train = 4, auto = 7),
    avail = list(el = 1, train = 1, auto = 1),
    choiceVar = choice,
    V = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}