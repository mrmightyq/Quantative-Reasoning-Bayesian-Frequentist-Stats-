table( rbinom(n=100,size=6,prob=0.5) )
table( rbinom(n=100,size=6,prob=0.5) ) * 100
table( rbinom(n=100,size=6,prob=0.5) )/100
table( rbinom(n=100,size=6,prob=0.5)/100 )
table( rbinom(n=100000, size=6, prob=0.5) )/100000

table( rbinom(n=10,size=1,prob=0.5) )

table(rep(sample(c(0,1),size=10, prob=c(0.5,0.5),replace=TRUE),100))

table( rbinom(n=100,size=6,prob=0.5) )

data <-c(30,40)

round(table( rbinom(n=9, size=1, prob=0.5) )/9,2)
round(table( rbinom(n=100000, size=1, prob=0.5) )/100000,4)

data1 <- round(table( rbinom(n=9, size=1, prob=0.5) )/9,2)
barplot(data1, main="9 Coin Flips"
        ,xlab="Probability of Heads or Tails"
        ,names.arg=c("Heads", "Tails")
        , col=c("darkblue","red")
         )

data2 <- round(table( rbinom(n=100000, size=1, prob=0.5) )/100000,4)
barplot(data2, main="100,000 Coin Flips"
        ,xlab="Probability of Heads or Tails"
        ,names.arg=c("Heads", "Tails")
        , col=c("darkblue","red")
)


barplot(table( rbinom(n=1,size=9,prob=0.5) ),main="1 trial of 9 Coin Flips")


barplot(table( rbinom(n=100000,size=9,prob=0.5) ), main="100,000 trails of 9 Coin Flips")
