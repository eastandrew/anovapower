library(pwr)

pwr.anova.test(k = 5, n = 5,f = NULL,sig.level = .05,power = 0.8)   # as per AMIS
pwr.anova.test(k = 5, n = 5,f = NULL,sig.level = .05,power = 0.6)   # reducing power (acceptable) reduces effect size
plot(pwr.anova.test(k = 5, f = 0.3,sig.level = .05,power = 0.8))    # what sample size IS "optimal?" with a power of 0.3 ("large" by Cohen, 88)
plot(pwr.anova.test(k = 5, f = 0.3,sig.level = .05,n=5))            # what kind of power are we looking at with 0.3 effect size and n=5



################ pretend anova data to explore what effect size is from the opposite direction.
## Make fake data to see what effect sizes look like
set.seed(101987)                                                    # make your random numbers the same as mine for this R session (close after done!!)
resp <- c(rnorm(n=6,mean=10,sd=1),
          rnorm(n=6,mean=9,sd=1),
          rnorm(n=6,mean=10,sd=2))                                  # build response vector
treat <- c(rep("a",6),rep("b",6),rep("c",6))                        # build treatment vector
df <- data.frame(resp=resp,treat=treat)                             # combine vectors into dataframe
df                                                                  # look at dataframe

## to calculate Cohen's f
grandmean <- mean(df$resp)                                          # the mean resp of the df
a <- mean(df$resp[df$treat=="a"])                                   # group means of df
b <- mean(df$resp[df$treat=="b"])
c <- mean(df$resp[df$treat=="c"])
leng <- length(unique(df$treat))                                    # number of groups

sigma_m <- sqrt((sum((c(a,b,c)-grandmean)^2))/leng)                 # see Cohen 88, page 275, but essentially the sum of the suqared mean differences divded by number of groups and sqaure rooted (similar to sum of squares in an ANOVA)
sigma <- sd(df$resp)                                                # overall sd for response
f <- sigma_m/sigma                                                  # Cohen's f for this particular ANOVA test.
f                                                                   # the kind of small size indicates that there is little effect of the treatment

## also, to calculate Cohen's n^2
aov1 <- aov(resp~factor(treat), data=df)                            # create ANOVA object with resp explained by factorized treatment in df dataframe
aov1                                                                # look at ANOVA object
summary(aov1)                                                       # see summary, including p-value, of ANOVA object
boxplot(resp~factor(treat), data=df)                                # look at boxplot of same data
sumobj <- unlist(summary(aov1))                                     # change the structure of the anova summary object
sumobj["Sum Sq1"]/sumobj["Sum Sq2"]                                 # the treatment sum of squares / residual sum of squares
                                                                    # this is the n^2, approximately this % of the variation in the data is explained by the treatment, in this case, that value is small.

## to calculate Cohen's d

d <- (max(c(a,b,c))-min(c(a,b,c)))/sigma
d                                                                   # this d suggests small to intermediate effects

pwr.anova.test(k=3,n=6,f=0.25,sig.level=0.05)
# notice the power is very low if the effect size is small/moderate but the sample sizes are small--look at the boxplots, why is the power low?
plot(pwr.anova.test(k=3,n=6,f=0.25, sig.level=0.05))


## run this script again but skip the set.seed() line to try with different randomness, but the same shape.  
## similar to above, but change the standard deviations or means to see how different kinds of results can influence.
## use this to inform your understanding of effect size--how much does the treatment change the response in relation to the overall data?




####### back to your above power analysis

pwr.anova.test(k=5, n=5,f=NULL,sig.level=0.05,power=0.8)   # as per AMIS
# with an f=0.78, this implies you would need to have a large amount of the variation in the data explained by the treatments in order to have lots of power and significance
# Let's make more similar fake data and fudge an ANOVA to see how this would look with your kind of experiment:

## another round of fake data based effect size calculation (NOT ESTIMATION--the above pwr.anova.test is effect size estimation)
set.seed(10191987)
df2 <- data.frame(resp=c(rnorm(5,mean=10,sd=1), 
                         rnorm(5,mean=9,sd=1),
                         rnorm(5,mean=8,sd=1),
                         rnorm(5,mean=7,sd=1),
                         rnorm(5,mean=6,sd=1)),
                  treat=c(rep("a",5),
                          rep("b",5),
                          rep("c",5),
                          rep("d",5),
                          rep("e",5)))

df2

grandmean2 <- mean(df2$resp)                                         
a2 <- mean(df2$resp[df2$treat=="a"])                                  
b2 <- mean(df2$resp[df2$treat=="b"])
c2 <- mean(df2$resp[df2$treat=="c"])
leng2 <- length(unique(df2$treat))                                    

sigma_m2 <- sqrt((sum((c(a2,b2,c2)-grandmean2)^2))/leng2)                 
sigma2 <- sd(df2$resp)                                                
f2 <- sigma_m2/sigma2                                                 
f2      

aov2 <- aov(resp~factor(treat), data=df2)
boxplot(resp~factor(treat), data=df2)
summary(aov2)
sumobj2 <- unlist(summary(aov2))                                     
sumobj2["Sum Sq1"]/sumobj2["Sum Sq2"]                                 

d2 <- (max(c(a2,b2,c2))-min(c(a2,b2,c2)))/sigma2
d2

## now look at the boxplot and the f value.  There is a painfully clear monotonic downward trend in the means (and a constant SD, which I created), so the majority of the variation is caused by the treatment and not by the variation in the data
## accordingly, if, in your data you show egregiously clear trends, you can accept a high f value in the pursuit of high power because you know you'll have a strong effect size.
## HOWEVER, you do not have strong confidence about what the data will look like so this becomes sketchy af.
## SO, the idea is to have a large enough sample size that you can stil have power with a low effect size.

