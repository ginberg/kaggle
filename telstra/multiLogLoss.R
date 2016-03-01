MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(nrow(act))      
  return(ll);
}

pred1 = c(0.8,0.2)
pred2 = c(0.6,0.4)
pred <- rbind(pred1,pred2)
pred
act1 <- c(1,0)
act2 <- c(1,0)
act <- rbind(act1,act2)

MultiLogLoss(act, pred)