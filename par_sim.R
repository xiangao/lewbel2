
# in this version, I try to use parallel cores.

library(ivlewbel)
require(snowfall)
set.seed(666)

# initialize parallel cores.
sfInit( parallel=TRUE, cpus=12)


gen.sim <- function(df){
    k <- df['k']
    nobs <- df['nobs']
    x<-runif(nobs, min=-1, max=1)
    u <- rnorm(nobs,0,1)
    u1 <- rnorm(nobs,0,1)
    u2 <- rnorm(nobs,0,1)
#    x1 <- rnorm(nobs,0,1)
    x1 <-runif(nobs, min=-1, max=1)
    x2 <- rnorm(nobs,0,1)
    z <- rnorm(nobs,0,1)
    ## e1 = rnorm(nobs, 0, .3*(x+1))
    ## e2 = rnorm(nobs, 0, .3*k*(x+1))
    ## e1 = u + exp(k*x)*u1
    ## e2 = u + exp(k*x)*u2
    # here is to specify the heteroskedasticity for y1
    ## e1 = u + exp(.3*k*(x+x1))*u1
    ## e2 = u + u2
    e1 = exp(.3*k*(x+x1))*u1
    e2 = u2
    ## e1 = rnorm(nobs, 0, sd=k*(x^2+11))
    ## e2 = rnorm(nobs, 0, sd=k*x^2)
    ## y1 is the endogenous variable; z is the instrument; x1 is
    ## omitted but determines heteroskedasticity of y1; e1 e2 are
    ## correlated because of common factor of u; x is the only
    ## observed exogenous variable.  The true coefficient on y1 should
    ## be 1.  lewbel model use x as both the exogenous variable and
    ## the heteroscedasticity factor.  tsls assumes we have an
    ## instrument z.  k is to adjust for degree of heteroscedasticity.
    y1 = 1 + z + x + x1 +  e1
    y2 = 1 + y1  + x + x1 +  e2
    data = data.frame(y2, y1, x1, x2, z, x)

    lewbel.model <- lewbel(formula = y2 ~ y1  | x   | x  , data = data)
    lm.model <- lm(y2 ~ y1 + x, data=data)
    tsls.model <- tsls(y2 ~ y1 + x   , ~ z + x  , data=data)

    lm.y1 <- summary(lm.model)$coefficients['y1','Estimate']-1
    tsls.y1 <- tsls.model$coefficients['y1']-1
    lewbel.y1 <- lewbel.model$coef.est['y1', 'Estimate']-1
    return(c(lm=lm.y1, lewbel=lewbel.y1,tsls=tsls.y1))
}


# set parameter space
sim.grid = seq(1,100,1)
k.grid=seq(1,10,1)
nobs.grid = ceiling(exp(seq(4, 8, 1))/100)*100
data.grid <- expand.grid(nobs.grid, sim.grid, k.grid)
names(data.grid) <- c('nobs', 'nsim', 'k')

# export functions to the slaves
# export data to the slaves if necessary
sfExport(list=list("gen.sim"))

# export function to the slaves
sfLibrary(ivlewbel)

# parallel computing
results <- data.frame(t(sfApply(data.grid, 1, gen.sim)))

# stop the cluster
sfStop()

names(results) <- c('lm','lewbel','tsls')
forshiny <- cbind(data.grid, results)
# write out for use in shiny.
write.csv(forshiny, 'results.csv')
