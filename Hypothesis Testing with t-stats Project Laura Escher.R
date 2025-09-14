#Hypothesis Testing with t-statistics Project Laura Escher

library(stats)
library(dplyr)

costs <- read.csv("Problem 10.13 - CollegeCosts.csv")
sat <- read.csv("Problem 10.16 - SATMath.csv")

public_sd <- sd(costs$Public)
public_mean <- mean(costs$Public)

#public mean is 22.3 and public sd is 4.53

private_sd <- sd(na.omit(costs$Private))
private_mean <- mean(na.omit(costs$Private))

#private mean is 42.5 and sd is 6.98

n_priv <-length(na.omit((costs$Private)))
n_public <- length(na.omit((costs$Public)))

point_estimate_costs <- public_mean - private_mean
#-20.2 is the point estimate for the population means.
#Interpret this value in terms of the annual cost of attending private and public colleges:
#Evaluated at the population mean, private colleges cost $20,200 more per year than public colleges.

#we need a t-test, pop sd not known

se_costs <- sqrt( (public_sd^2 ) / n_public  + ( private_sd^2 ) / n_priv )

df_numerator <- ( ( (public_sd^2) / n_public ) + ( (private_sd^2) / n_priv) )^2
df_denominator <- ( (1/(n_public-1)) * ( (public_sd^2 ) / n_public )^2 ) + ( (1/(n_priv-1)) * ( ( ( private_sd^2 ) / n_priv)^2 )) 

df_final_costs <- df_numerator / df_denominator

t_crit_costs <- qt(1 - 0.05/2, df = df_final_costs)

clower_costs <- point_estimate_costs - ( t_crit_costs * se_costs ) 
cupper_costs <- point_estimate_costs + ( t_crit_costs * se_costs )
print(paste('The 95% confidence interval is [', clower_costs, ',', cupper_costs, ']' ))

t.test(costs$Private, costs$Public, alternative = 'less', var.equal = FALSE)

View(sat)
college_mean <- mean(sat$College)
college_sd <- sd(sat$College)

hs_mean <- mean(na.omit(sat$High.School))
hs_sd <- sd(na.omit(sat$High.School))

point_estimate_sat <- college_mean - hs_mean
#the point estimate of the difference between the means for the two populations is 38.

#We need a t-test again because we do not know the population standard deviation.

n_college <- length(na.omit((sat$College))) #16
n_hs <- length(na.omit(sat$High.School)) #16

se_sat <- sqrt( (college_sd^2 ) / n_college  + ( hs_sd^2 ) / n_hs )

df_numerator_sat <- ( ( (college_sd^2 ) / n_college ) + ( hs_sd^2 ) / n_hs) ^2
df_denominator_sat <- ( (1/(n_college-1)) * ( (college_sd^2 ) / n_college )^2 ) + ( (1/(n_hs-1)) * ( ( ( hs_sd^2 ) / n_hs)^2 ))

df_final_sat <- df_numerator_sat / df_denominator_sat

t_test_sat <- point_estimate_sat / se_sat
print(t_test_sat) #1.803753
pvalue_sat <- pt(1.803753, df=df_final_sat, lower.tail = FALSE)
print(pvalue_sat) #p-value is 0.04158621

t.test(sat$College, sat$High.School, alternative = "greater", var.equal = FALSE)
       