nestedMarket = function(database, groupVariable, base) {
  apollo_beta = c(
    asc_el = 0,
    asc_train = 0,
    asc_auto = 0,
    lambda_Group = 0.5
  )
  
  apollo_fixed = c(paste("asc_", base, sep = ""))
  
  apollo_control = list(modelName = "MNL",
                        modelDescr = "SimpleMNL",
                        indivID = "X")
  
  apollo_inputs = apollo_validateInputs(apollo_beta, apollo_fixed, database, apollo_control)
  
  model = apollo_estimate(apollo_beta,
                          apollo_fixed,
                          prob_nestedMarket,
                          apollo_inputs)
  
  assign("groupVariable", groupVariable)
  
  source("./output.R")
  output(model)
  
  return(model)
}


prob_nestedMarket = function(apollo_beta,
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
  
  nlNests = list(root = 1, Group = lambda_Group)
  
  name = c("el","train","auto")
  
  nlStructure=list()
  nlStructure[["root"]]=c(name[!(name %in% groupVariable)],"Group")
  nlStructure[["Group"]]=groupVariable
  
  nl_settings = list(
    alternatives = c(el = 2, train = 4, auto = 7),
    avail = list(el = 1, train = 1, auto = 1),
    choiceVar = choice,
    V = V,
    nlNests = nlNests,
    nlStructure = nlStructure
  )
  
  P[["model"]] = apollo_nl(nl_settings, functionality)
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}