setwd("~/Documents/Graduate/Fall Semester/IND ENG 222/Final Project")

tmp <- read.csv("~/Dropbox/IEOR 222/wmt 05062010 LOB.csv")

# CLEAN THE DATA

indi <- matrix(nrow=nrow(tmp), ncol=3)
indi[,1] <- tmp$TVITCH_41..WMT.BID_PRICE1..TVITCH_41__WMT
indi[,2] <- tmp$TVITCH_41..WMT.ASK_PRICE1..TVITCH_41__WMT
indi[,3] <- round(indi[,2]-indi[,1],digits = 2)
ftmp <- tmp[c(TRUE, abs(indi[2:40001,2]-indi[1:40000,2]) >= 1e-06),]
ftmp <- ftmp[ftmp$Index < 40000,]
ftmp$d <- as.integer(ftmp$d*100)

#ftmp <- read.csv("splitted_WMT_2010.csv")


# Show liquidity
x <- round(indi[,2]-indi[,1], digits = 2)
plot(x, main="Spread for WMT 05062010", 
     xlab="Index", ylab="Spread", pch=20)
table(x)

# Spread
table(round(ftmp$TVITCH_41..WMT.ASK_PRICE1..TVITCH_41__WMT-ftmp$TVITCH_41..WMT.BID_PRICE1..TVITCH_41__WMT, digits = 2))
# Movement of best ask
table(round(diff(ftmp$TVITCH_41..WMT.ASK_PRICE1..TVITCH_41__WMT),digits = 2))


tmp2 <- read.csv("wmt 05102012 LOB.csv")

indi2 <- matrix(nrow=nrow(tmp2), ncol=3)
indi2[,1] <- tmp2$TVITCH_41..WMT.BID_PRICE1..TVITCH_41__WMT
indi2[,2] <- tmp2$TVITCH_41..WMT.ASK_PRICE1..TVITCH_41__WMT
indi2[,3] <- round(indi2[,2]-indi2[,1],digits = 2)
ftmp2 <- tmp2[c(TRUE, abs(indi2[2:nrow(tmp2),2]-indi2[1:nrow(tmp2)-1,2]) >= 1e-06),]
ftmp2$d <- as.integer(ftmp2$d*100)

ftmp2 <- read.csv("splitted_WMT_2012.csv")

x2 <- round(indi2[,2]-indi2[,1], digits = 2)
plot(x2, main="Spread for WMT 05102012", 
     xlab="Index", ylab="Spread", pch=20)
table(x2)

table(round(ftmp2$TVITCH_41..WMT.ASK_PRICE1..TVITCH_41__WMT-ftmp2$TVITCH_41..WMT.BID_PRICE1..TVITCH_41__WMT, digits = 2))
table(round(diff(ftmp2$TVITCH_41..WMT.ASK_PRICE1..TVITCH_41__WMT),digits = 2))


tmp3 <- read.csv("ms 05062010 LOB.csv")

indi3 <- matrix(nrow=nrow(tmp3), ncol=3)
indi3[,1] <- tmp3$TVITCH_41..MS.BID_PRICE1
indi3[,2] <- tmp3$TVITCH_41..MS.ASK_PRICE1
indi3[,3] <- round(indi3[,2]-indi3[,1],digits = 2)
ftmp3 <- tmp3[c(TRUE, abs(indi3[2:40001,2]-indi3[1:40000,2]) >= 1e-06),]
ftmp3 <- ftmp3[ftmp3$Index < 40000,]
ftmp3$d <- as.integer(ftmp3$d*100)

ftmp3 <- read.csv("splitted_ms_2010.csv")

x3 <- round(indi3[,2]-indi3[,1], digits = 2)
plot(x3, main="Spread for MS 05062010", 
     xlab="Index", ylab="Spread", pch=20)
table(x3)

table(round(ftmp3$TVITCH_41..MS.ASK_PRICE1-ftmp3$TVITCH_41..MS.BID_PRICE1, digits = 2))
table(round(diff(ftmp3$TVITCH_41..MS.ASK_PRICE1),digits = 2))


tmp4 <- read.csv("ms 05102012 LOB.csv")

indi4 <- matrix(nrow=nrow(tmp4), ncol=3)
indi4[,1] <- tmp4$TVITCH_41..MS.BID_PRICE1
indi4[,2] <- tmp4$TVITCH_41..MS.ASK_PRICE1
indi4[,3] <- round(indi4[,2]-indi4[,1],digits = 2)
ftmp4 <- tmp4[c(TRUE, abs(indi4[2:nrow(tmp4),2]-indi4[1:nrow(tmp4)-1,2]) >= 1e-06),]
ftmp4$d <- as.integer(ftmp4$d*100)

ftmp4 <- read.csv("splitted_ms_2012.csv")

x4 <- round(indi4[,2]-indi4[,1], digits = 2)
plot(x4, main="Spread for MS 05102012", 
     xlab="Index", ylab="Spread", pch=20)
table(x4)

table(round(ftmp4$TVITCH_41..MS.ASK_PRICE1-ftmp4$TVITCH_41..MS.BID_PRICE1, digits = 2))
table(round(diff(ftmp4$TVITCH_41..MS.ASK_PRICE1),digits = 2))

write.csv(ftmp,"WMT2010.csv")
write.csv(ftmp2,"WMT2012.csv")
write.csv(ftmp3,"MS2010.csv")
write.csv(ftmp4,"MS2012.csv")

attach(mtcars)
par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(2,2,2,2))
plot(x, main="WMT 05062010", 
     xlab="Index", ylab="Spread", pch=20)
plot(x2, main="WMT 05102012", 
     xlab="Index", ylab="Spread", pch=20)
plot(x3, main="MS 05062010", 
     xlab="Index", ylab="Spread", pch=20)
plot(x4, main="MS 05102012", 
     xlab="Index", ylab="Spread", pch=20)

# PARAMETER ESTIMATION

# p

indi <- as.integer((ftmp$d[2:199][-1]+ftmp$d[2:199][-198])*100)
p <- 1-sum(indi == 0)/length(indi)

indi2 <- as.integer((ftmp2$d[2:26][-1]+ftmp2$d[2:26][-25])*100)
p2 <- 1-sum(indi2 == 0)/length(indi2)

indi3 <- as.integer((ftmp3$d[2:145][-1]+ftmp3$d[2:145][-144])*100)
p3 <- 1-sum(indi3 == 0)/length(indi3)

indi4 <- as.integer((ftmp4$d[2:9][-1]+ftmp3$d[2:9][-8])*100)
p4 <- 1-sum(indi4 == 0)/length(indi4)

# pbar
ftmp$pbar <- (ftmp$bsize)/(ftmp$bsize+ftmp$asize)
ftmp2$pbar <- (ftmp2$bsize)/(ftmp2$bsize+ftmp2$asize)
ftmp3$pbar <- (ftmp3$bsize)/(ftmp3$bsize+ftmp3$asize)
ftmp4$pbar <- (ftmp4$bsize)/(ftmp4$bsize+ftmp4$asize)

# CORRELATED RANDOM WALK GENERATION

corrrw <- function(p, pbar, n){
  y <- runif(n)
  x <- vector(mode='numeric',length=n)
  x[1] <- 2*(y[1] < pbar)-1
  for (i in 2:n){
    x[i] <- x[i-1] * ((y[i] < p)*2-1)
  }
  x <- cumsum(x)
  return (x)
}

rwgenerate <- function(p, pbar, n, N){
  x <- matrix(nrow=N, ncol=n)
  for (i in 1:N){
    x[i,] <- corrrw(p,pbar,n)
  }
  return (x)
}

k <- 2; q <- 0.2; p <- 0.4 ; n <- 100; pbar <- 0; N <- 10000

SimulationforC <- function(k, q, p, n, pbar, N){
  a <- 0; b <- 0; c <- 0; Sc <- 0; Sb <- 0; Sbnew <- 0;
  for (i in 1:N){
    x <- corrrw(p, pbar, n)
    if (min(x) <= -k){
      a <- a + 1
    }
    if (min(x) > -k+1){
      c <- c + 1
      Sc <- Sc + x[n]
    }
    if (min(x) == -k+1){
      y <- c(pbar, diff(x))
      b <- b+1
      prob <- 1
      lprob <- 0
      nk <- length(which(x==-k+1))
      qnew <- 1-(1-q)^nk
      for (j in 2:n){
        prob <- prob * ( p * (y[j]==y[j-1]) + (1-p) * (y[j]!=y[j-1]) )
        lprob <- lprob + log( p * (y[j]==y[j-1]) + (1-p) * (y[j]!=y[j-1]) )
      }
      Sb <- Sb + prob * (qnew*(-k) + (1-qnew) * x[n])
      Sbnew <- Sbnew + exp(lprob) * (q*(-k) + (1-q) * x[n])
    }
  }
  C <- Sbnew + a * (-k) / N + Sc/N
  return(C)
}

k <- 2; q <- 0.2; p <- 0.4; n <- 100; pbar <- 0; N <- 10000

# PRACTICE

strategy <- 1; subdata <- c(1, -1, 1, 1, 1, -1, 1); q <- 0.2

PracticeofStrategy <- function(strategy, subdata, q){
  if(strategy == 0){
    return (0)
  }
  subdata[1] <- 0
  subdata <- cumsum(subdata)
  if (min(subdata) <= -strategy){
    return (-strategy)
  }
  if (min(subdata) > -strategy+1){
    return (subdata[length(subdata)])
  }
  if (min(subdata) == -strategy+1){
    nk <- length(which(subdata==-strategy+1))
    qnew <- 1-(1-q)^nk
    return (qnew*(-strategy) + (1-qnew) * subdata[length(subdata)])
  }
}
PracticeofStrategy(strategy, subdata, q)

ChoiceofStrategy <- function(cost,pbar){
  temp1=-1+pbar*(cost[2]-cost[1])
  temp2=cost[3]+(cost[4]-cost[3])*pbar
  compare=c(0,temp1,temp2)
  strategy=which(compare==min(compare))-1
  TEC=min(compare)
  return(c(strategy,TEC))
}

PracticeofMStrategy <- function(t1, t2, q, subdata, n){
  MCost <- rep(0,n)
  MInd  <- rep(0,n)
  subdata[1] <- 0
  step  <- cumsum(subdata)
  k <- 0
  for (i in 2:(t1-1)){
    if (subdata[i] == -1){
      if (subdata[i+1] == -1){
        return (sum(MCost*MInd)+(1-q)^k*(step[i]-1))
      }
      if (subdata[i+1] == 1){
        MCost[k+1] <- step[i]-1
        MInd[k+1]  <- (1-q)^k*q
        k=k+1
      }
    }
  }
  for (i in t1:(t2-1)){
    if (subdata[i+1] == -1){
      return (sum(MCost*MInd)+(1-q)^k*(step[i]-1))
    }
    if (subdata[i+1] == 1){
      MCost[k+1] <- step[i]-1
      MInd[k+1]  <- (1-q)^k*q
      k=k+1
    }
  }
  for (i in t2:(n-1)){
    if (subdata[i] == 1){
      if (subdata[i+1] == -1){
        return (sum(MCost*MInd)+(1-q)^k*(step[i]-1))
      }
      if (subdata[i+1] == 1){
        MCost[k+1] <- step[i]-1
        MInd[k+1]  <- (1-q)^k*q
        k=k+1
      }
    }
    if (subdata[i] == -1){
      return (sum(MCost*MInd)+(1-q)^k*(step[i]))
      }
  }
  return (sum(MCost*MInd)+(1-q)^k*(step[i]))
}
PracticeofMStrategy(t1, t2, q, subdata, n)

SingleStrategy <- function(data,I,q,p,n) {
  temp=floor((nrow(data)-I)/n)
  result <- data.frame(matrix(ncol = 5, nrow = temp)) 
  colnames(result)<- c("SP","Strategy","TEC","SEPC","MPC")
  N=10000
  cost=c()
  cost[1]=-1
  cost[2]=SimulationforC(1,q,p,n,1,N)
  cost[3]=SimulationforC(2,q,p,n,0,N)
  cost[4]=SimulationforC(2,q,p,n,1,N)
  sp <- vector(mode='numeric',length=temp+1)
  for (i in 0:temp){
    sp[i+1] <- I+n*i+1
  }
  result$SP=sp[1:temp]
  for (j in 1:(length(sp)-1)){
    index=sp[j]
    pbar=data$pbar[index]
    result$Strategy[j]=ChoiceofStrategy(cost,pbar)[1]
    result$TEC[j]=ChoiceofStrategy(cost,pbar)[2]
    result$SEPC[j]=PracticeofStrategy(result$Strategy[j], data$d[sp[j]:(sp[j+1]-1)], q)
  }
  t1=n-ceiling(log(p-p*q)^(-1)*log(q*(1-2*p)/((2*(1-p+p*q)-1)*(1-q)*(p^2*q+1+p^2-2*p))))-1
  t2=n-ceiling(log(p-p*q)^(-1)*log(q/((1-p)*(1-q)*(2*(1-p+p*q)-1))))
  for (j in 1:(length(sp)-1)){
    result$MPC[j]=PracticeofMStrategy(t1, t2, q, data$d[sp[j]:(sp[j+1]-1)], n)
  }
  return (result)
}
SingleStrategy(ftmp, 200, 0.2, p, 25)

z <- vector(mode='numeric',length=10)
for (l in 1:10){
  z [l] <- SimulationforC(k, q, p, n, pbar, N)
}
minOfRows=apply(x, 1, function(x) min(x))


