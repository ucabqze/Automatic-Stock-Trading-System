#team 08
params = list(series = c(1,2,3,4,5,6,7,8,9,10),
              cmoLookback = 25, cmoUperBound = 25,cmoLowerBound = 25,
              maFast = 4, maMedium = 6, maSlow = 12,
              volFast = 4, volSlow = 10,
              bbandsLookback = 30,bbandsSdParam = 1.5,bbandRiskCount = 7,
              cciLookback = 20)


maxRows <- 3100

getOrders <- function(store, newRowList, currentPos, info, params){
  
  #######Build store###########################################################
  allzero  <- rep(0,length(newRowList)) 
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStoreBefore(store, newRowList, params$series)
  
  marketOrders <- -currentPos 
  pos <- allzero 
  
  
  #######Calcualte positionSize##############################################
  if (store$iter == 50){
    
    for(i in 1:length(params$series)){
      
      store$avgAbsDiffs[i]= mean(abs(diff(store$op[1:50,i])))
      
    }
    
    store$posSizes = round(max(store$avgAbsDiffs)/store$avgAbsDiffs)
    #store$posSizesInitial = round(max(store$avgAbsDiffsavgAbsDiffs)/store$avgAbsDiffsavgAbsDiffs)
    
  }
  
  # if(store$iter > 50){
  #  for (i in 1:length(params$series)) {
  
  #   ret = getTradeReturn(store$cl[,i], store$entry[i], store$entry[i]+1, store$position[store$entry[i], i] < 0)
  
  #  if(ret > 0 && store$posSizes[i]< 10*store$posSizesInitial[i] && store$posSizes[i] > 0){
  #   store$posSizes[i] = store$posSizes[i]+2*store$posSizesInitial[i]
  #}
  #if(ret < 0 && store$posSizes[i]< 10*store$posSizesInitial[i] && store$posSizes[i] > 0){
  # store$posSizes[i] = store$posSizes[i]-2*store$posSizesInitial[i]
  #}
  
  #}
  #}
  
  #######Calculate position#######################################
  if (store$iter > 50 && store$iter > params$cmoLookback) {
    for (i in 1:length(params$series)) {
      
      cl <- newRowList[[params$series[i]]]$Close
      
      #######DMI#######
      high_low_close = cbind(store$high[,i],store$low[,i],store$cl[,i])
      dmi = ADX(high_low_close)
      
      #######MA#######
      ma_fast  = EMA(store$cl[,i], n = params$maFast)
      ma_medium = EMA(store$cl[,i], n = params$maMedium)
      ma_slow = EMA(store$cl[,i], n = params$maSlow)
      
      #######CMO#######
      cmo  = CMO(store$cl[,i], n = params$cmoLookback)
      
  
      
      
      #######Decision#######
      
      ###Trend Following strategy
      if(abs(cmo[store$iter]) > params$cmoUperBound){
        
        if(ma_fast[store$iter] > ma_medium[store$iter] && ma_medium[store$iter] > ma_slow[store$iter]
           && dmi[store$iter, "DIp"]> dmi[store$iter, "DIn"] && dmi[store$iter, "ADX"] <20
           #&& cci[store$iter] > 100
           && last(ave(store$vol[(store$iter-params$volFast):store$iter,i]))>last(ave(store$vol[(store$iter-params$volSlow):store$iter,i]))){
          
          pos[params$series[i]] <- store$posSizes[params$series[i]]; store$entry[i] = store$iter
          
        }
        
        if((ma_fast[store$iter] < ma_medium[store$iter] && ma_medium[store$iter] < ma_slow[store$iter]) 
           &&dmi[store$iter, "DIp"] < dmi[store$iter, "DIn"] && dmi[store$iter, "ADX"] <20
           #&& cci[store$iter] < -100
           && last(ave(store$vol[(store$iter-params$volFast):store$iter,i]))<last(ave(store$vol[(store$iter-params$volSlow):store$iter,i]))){
          
          pos[params$series[i]] <- -store$posSizes[params$series[i]];store$entry[i] = store$iter
          
        }
        
      }
      
      
      
      
    }
  }
  
  #######Return########################################
  store <- updateStoreAfter(store, pos, params$series)
  marketOrders <- marketOrders + pos
  
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
  
  
}

#######init and update ClStore and Store##################################################################

initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
initOpStore  <- function(newRowList,series) {
  opStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(opStore)
}
initHighStore  <- function(newRowList,series) {
  highStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(highStore)
}
initLowStore  <- function(newRowList,series) {
  lowStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(lowStore)
}
initVolStore = function(newRowList,series){
  volStore = matrix(0,nrow = maxRows, ncol = length(series))
  return(volStore)
}
initPositionStore = function(series){
  positionStore = matrix(0,nrow = maxRows, ncol = length(series))
  return(positionStore)
}

updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
updateOpStore <- function(opStore, newRowList, series, iter) {
  for (i in 1:length(series))
    opStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Open)
  return(opStore)
}
updateHighStore <- function(highStore, newRowList, series, iter) {
  for (i in 1:length(series))
    highStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(highStore)
}
updateLowStore <- function(lowStore, newRowList, series, iter) {
  for (i in 1:length(series))
    lowStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(lowStore)
}
updateVolStore = function(volStore, newRowList, series, iter){
  for (i in 1:length(series))
    volStore[iter,i] = as.numeric(newRowList[[series[i]]]$Volume)
  return(volStore)
}

updatePositionStore = function(positionStore, pos, series, iter){
  for (i in 1:length(series))
    positionStore[iter,i] = pos[series[i]] 
  return(positionStore)
}

initStore <- function(newRowList,series) {
  return(list(iter=0,avgAbsDiffs =rep(0,length(series)),posSizes = rep(0,length(series)),posSizesInitial = rep(0,length(series)),
              entry = rep(1,length(series)),exit = rep(TRUE,length(series)),
              #trendUpCount = rep(0,length(series)),trendDownCount = rep(0,length(series)),
              bbandsUpCount = rep(0,length(series)), bbandsDownCount = rep(0,length(series)),
              cl=initClStore(newRowList,series), op = initOpStore(newRowList,series), high = initHighStore(newRowList,series), low = initLowStore(newRowList,series), vol = initVolStore(newRowList,series), 
              position = initPositionStore(series)))
}

updateStoreBefore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$op <- updateOpStore(store$op,newRowList,series,store$iter)
  store$high <- updateHighStore(store$high,newRowList,series,store$iter)
  store$low <- updateLowStore(store$low,newRowList,series,store$iter)
  store$vol = updateVolStore(store$vol, newRowList, series, store$iter)
  return(store)
}

updateStoreAfter <- function(store, pos, series) {
  store$position = updatePositionStore(store$position, pos, series, store$iter)
  return(store)
}

########Other Function######################################################
getTradeReturn = function(prices, entry, exit, short = FALSE){
  prices = as.numeric(prices)
  if (short) {
    x = prices[entry]/prices[exit]-1
  }
  else{
    x = prices[exit]/prices[entry]-1
  }
  return(x)
}

