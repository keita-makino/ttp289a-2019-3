setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library("readr")
library("stats")
library("dplyr")
library("apollo")

database = read.csv("../data/data.csv")

apollo_initialise()

source("./market.R")
m = market(database)

source("./base.R")
ba = base(database %>% .[complete.cases(.),], "auto")
bt = base(database %>% .[complete.cases(.),], "train")
bm = market(database %>% .[complete.cases(.),])

pchisq(2 * (bm$LLout - ba$LLout), 3)

source("./reduced.R")
source("./reducedMarket.R")
assign("reducedVariable", c("train", "auto"))
re = reduced(database %>% .[.$choice != 2,] %>% .[complete.cases(.),], reducedVariable, "auto")
rem = reducedMarket(database %>% .[.$choice != 2,] %>% .[complete.cases(.),], reducedVariable, "auto")

assign("reducedVariable", c("el", "auto"))
rt = reduced(database %>% .[.$choice != 4,] %>% .[complete.cases(.),], reducedVariable, "auto")
rtm = reducedMarket(database %>% .[.$choice != 4,] %>% .[complete.cases(.),], reducedVariable, "auto")

assign("reducedVariable", c("el", "train"))
ra = reduced(database %>% .[.$choice != 7,] %>% .[complete.cases(.),], reducedVariable, "train")
ram = reducedMarket(database %>% .[.$choice != 7,] %>% .[complete.cases(.),], reducedVariable, "train")

getP = function(r, u) {
  name = names(u$estimate) %in% names(r$estimate)
  rb = r$estimate %>% .[. != 0]
  ub = u$estimate[name] %>% .[. != 0]
  name = colnames(u$varcov) %in% colnames(r$varcov)
  rv = r$varcov
  uv = u$varcov[name,name]
  chi = (rb - ub) %*% solve(rv - uv) %*% (rb - ub)
  p <- pchisq(chi,df=sum(name),lower.tail=FALSE)
  return(c(chi = chi,p = p, df = sum(name)))
}

getP(re, ba)
getP(rt, ba)
getP(ra, bt)

source("./nested.R")
assign("groupVariable", c("train", "auto"))
ne = nested(database %>% .[complete.cases(.),], groupVariable, "auto")

assign("groupVariable", c("el", "auto"))
nt = nested(database %>% .[complete.cases(.),], groupVariable, "auto")

assign("groupVariable", c("el", "train"))
na = nested(database %>% .[complete.cases(.),], groupVariable, "auto")

source("./nestedmarket.R")
assign("groupVariable", c("el", "train"))
nme = nestedMarket(database %>% .[complete.cases(.),], groupVariable, "auto")
assign("groupVariable", c("el", "auto"))
nmt = nestedMarket(database %>% .[complete.cases(.),], groupVariable, "auto")
assign("groupVariable", c("train", "auto"))
nma = nestedMarket(database %>% .[complete.cases(.),], groupVariable, "auto")

