# Goal: generate 50 sets of data for each single proportion, single mean, 
# two proportions/means, and paired data.

set.seed(218)

data_type <- c('one-mean', 'one-proportion', 'two-means', 'two-proportions', 'paired')

for(i in 1:20){
  
  # one-mean
  while(T) {
    n1 <- sample(20:100, 1); n2 <- sample(20:100, 1)
    mu <- rbeta(1,1,1)*100
    sigma <- rbeta(1,2,2)*10
    one.mean.sample <- rnorm(n=n1, mean = mu, sd = sigma)
    if(all(one.mean.sample>0)) break
  }
  df1 <- data.frame(
    obs = 1:length(one.mean.sample),
    response = round(one.mean.sample, 2)
  )
  write.csv(df1, glue::glue('assessments/final/data/one-mean-set-{i}.csv'), row.names = F)
  
  # one-prop
  while(T) {
    n1 <- sample(20:100, 1); n2 <- sample(20:100, 1)
    p <- rbeta(1,1,1)
    one.prop.sample <- rbinom(n=n1, size=1, prob=p)
    if(all(one.prop.sample>0 | one.prop.sample<1)) break
  }
  df2 <- data.frame(
    obs = 1:length(one.prop.sample),
    response = one.prop.sample
  )
  write.csv(df2, glue::glue('assessments/final/data/one-proportion-set-{i}.csv'), row.names = F)
  
  # two-means
  while(T) {
    n1 <- sample(20:100, 1); n2 <- sample(20:100, 1)
    mu1 <- rbeta(1,1,1)*100
    mu2 <- rbeta(1,1,1)*100
    sigma1 <- rbeta(1,2,2)*10
    sigma2 <- rbeta(1,2,2)*10
    two.mean.samp1 <- rnorm(n=n1, mean=mu1, sd=sigma1)
    two.mean.samp2 <- rnorm(n=n2, mean=mu2, sd=sigma2)
    if(all(two.mean.samp1>0) & all(two.mean.samp2>0)) break
  }
  df3 <- data.frame(
    obs = 1:length(c(two.mean.samp1, two.mean.samp2)),
    group = c(rep('A', times=length(two.mean.samp1)), 
              rep('B', times=length(two.mean.samp2))),
    response = round(c(two.mean.samp1, two.mean.samp2),2)
  )
  write.csv(df3, glue::glue('assessments/final/data/two-means-set-{i}.csv'), row.names = F)

  # two-props
  while(T) {
    n1 <- sample(20:100, 1); n2 <- sample(20:100, 1)
    p1 <- rbeta(1,1,1)
    p2 <- rbeta(1,1,1)
    two.prop.samp1 <- rbinom(n=n1, size=1, prob=p1)
    two.prop.samp2 <- rbinom(n=n2, size=1, prob=p2)
    if(all((two.prop.samp1>0 | two.prop.samp1<1)) & all((two.prop.samp2>0 | two.prop.samp2<1))) break
  }
  df4 <- data.frame(
    obs = 1:length(c(two.prop.samp1, two.prop.samp2)),
    group = c(rep('A', times=length(two.prop.samp1)), 
              rep('B', times=length(two.prop.samp2))),
    response = round(c(two.prop.samp1, two.prop.samp2),2)
  )
  write.csv(df4, glue::glue('assessments/final/data/two-proportions-set-{i}.csv'), row.names = F)
  
  # paired
  while(T) {
    n1 <- sample(20:100, 1); n2 <- sample(20:100, 1)
    mu1 <- rbeta(1,1,1)*100
    mu2 <- rbeta(1,1,1)*100
    Mu <- c(mu1,mu2)
    sigma <- rbeta(1,2,2)*10
    rho <- 2*rbeta(1,1,1)-1
    Sigma <- sigma * matrix(c(1,rho,rho,1), nrow=2)
    if(all(eigen(Sigma, symmetric = T)$values > 0)) {
      paired.samp <- MASS::mvrnorm(n=n1, Mu, Sigma)
      if(all(paired.samp > 0)) break
    }
  }
  df5 <- data.frame(
    obs = 1:nrow(paired.samp),
    round(paired.samp,2)
  )
  names(df5) <- c('obs', 'first', 'second')
  write.csv(df5, glue::glue('assessments/final/data/paired-set-{i}.csv'), row.names = F)
  

}