## An example partially taken from chapter 3 of my book Data Mining
## with R (Torgo,2010)

## First a trading policy function
## This function implements a strategy to trade on futures with
## long and short positions. Its main ideas are the following:
## - all decisions aretaken at the end of the day, that is, after
## knowing all daily quotes of the current session.
## - if at the end of day d our models issue a sell signal and  we
## currently  do not hold any opened position, we will open a short
## position  by issuing a sell order. When this order is carried out  by
## the market at a price  pr sometime in the  future, we will
## immediately post two other orders. The first is a buy limit order
## with  a limit price of pr - p%, where p% is a target profit margin.
## We will wait 10 days for this target to be reached. If the  order  is
## not carried out by this deadline, we will buy at the closing price
## of  the 10th day. The second order is a buy stop order with a  price
## limit  pr + l%. This order is placed with the goal of limiting our
## eventual  losses with this position. The order will be executed if
## the  market reaches the price pr + l%, thus limiting our possible
## losses  to l%.
## - if the end of the day signal is buy the strategy is more or less
## the inverse

## Not run:
library(xts)
policy.1 <- function(signals,market,opened.pos,money,bet=0.2,hold.time=10,exp.prof=0.025, max.loss= 0.05)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b' && !nOs) {
    quant <- round(bet*money/market[d,'Close'],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         market[d,'Close']*(1+exp.prof),
                                         market[d,'Close']*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's' && !nOs) {
    # this is the nr of stocks we already need to buy 
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*market[d,'Close']
    quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         market[d,'Close']*(1-exp.prof),
                                         market[d,'Close']*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  # Now lets check if we need to close positions
  # because their holding time is over
  if (nOs) 
    for(i in 1:nOs) {
      if (d - opened.pos[i,'Odate'] >= hold.time)
        orders <- rbind(orders,
                        data.frame(order=-opened.pos[i,'pos.type'],
                                   order.type=1,
                                   val = NA,
                                   action = 'close',
                                   posID = rownames(opened.pos)[i]
                        )
        )
    }
  
  orders
}

## Now let us play a bit with the SP500 quotes availabe in our package
data(GSPC)

## Let us select the last 3 months as the simulation period
market <- last(GSPC,'3 months')

## now let us generate a set of random trading signals for
## illustration purpose only
ndays <- nrow(market)
aRandomIndicator <- rnorm(sd=0.3,ndays)
theRandomSignals <- trading.signals(aRandomIndicator,b.t=0.1,s.t=-0.1)

## now lets trade!
tradeR <- trading.simulator(market,theRandomSignals,'policy.1',list(exp.prof=0.05,bet=0.2,hold.time=10))

## a few stats on the trading performance
summary(tradeR)
tradingEvaluation(tradeR)
## End(Not run)

## See the performance graphically

## Not run:
plot(tradeR,market)
## End(Not run)
