
#1. globe tossing data = 4 water and 11 land
#Construct posterior distribution

## from the book
# define grid
p_grid <- seq( from=0 , to=1 , length.out=11 )
# define prior
prior <- rep( 1 , 11 )
# compute likelihood at each value in grid
likelihood <- dbinom( 4 , size=11 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)


plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )


##via beta distribution
compute_posterior = function (W, L, poss = c(0,0.25,.5,.75,1)){
  ways = sapply(poss, function(q) q^W * (1-q)^L)
  post = ways/sum(ways)
  data.frame(poss, ways, post = round(post,3))
  
}
compute_posterior(4,11,poss = seq(from =0, to=1, len =11))


#2. compute the posterior predictive distribution for the next 5 tosses


p_samples = rbeta(1e4, 4+1, 11+1)
W_sim = rbinom(1e4, size = 5, p = p_samples)
plot(table(W_sim))

#3. use 2 to caluclate prob of 3 or more water in next 5 tosses
sum(W_sim>=3)/ 1e4

                  