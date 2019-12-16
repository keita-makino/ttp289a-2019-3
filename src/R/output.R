output = function(model) {
  print(model$estimate %>% round(3))
  print(model$se %>% round(3))
  print((model$estimate / model$se) %>% round(3))
  print(model$LLout %>% round(3))
  print(model$LL0 %>% round(3))
  print(apollo_modelOutput(model))
}