example_strategies <- c("subStrategy_1",
                        "subStrategy_2",
                        "team8"
                        )

example_params <- list("subStrategy_1"= list(series = c(1,2,3,4,5,6,7,8,9,10),
                                       cmoLookback = 25, cmoUperBound = 25,
                                       #cciLookback = 18,
                                       maFast = 4, maMedium = 6, maSlow = 12,
                                       volFast = 4, volSlow = 10),
                       
                    "subStrategy_2" = list(series = c(1,2,3,4,5,6,7,8,9,10),
                                             cmoLookback = 25, cmoLowerBound = 25,
                                             bbandsLookback = 30,bbandsSdParam = 1.5, bbandRiskCount = 5,
                                             #rsiFast = 6, rsiSlow =12,
                                             cciLookback = 18),
                    
                    "team8" = list(series = c(1,2,3,4,5,6,7,8,9,10),
                                   cmoLookback = 25, cmoUperBound = 25,cmoLowerBound = 25,
                                   maFast = 4, maMedium = 6, maSlow = 12,
                                   volFast = 4, volSlow = 10,
                                   bbandsLookback = 30,bbandsSdParam = 1.5,bbandRiskCount = 7,
                                   cciLookback = 18))

load_strategy <- function(strategy) {

    strategyFile <- file.path('strategies', paste0(strategy,'.R'))

    # load strategy
    cat("Sourcing",strategyFile,"\n")
    source(strategyFile) # load in getOrders

    # set params
    params <<- example_params[[strategy]]
    print("Parameters:")
    print(params)
}
