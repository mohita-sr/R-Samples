require(plyr)
require(boot)

baseball <- baseball[baseball$year > 1990, ]

## Build a function for calculating batting averge = total hits / total at bats

bat.avg <- function(data, indices=1:NROW(data), hits="h", at.bats="ab")
{
  sum(data[indices, hits], na.rm = T) / sum(data[indices, at.bats], na.rm = T)
}  

## Test it on the original data
bat.avg(baseball)

## Bootstrap it
avgBoot <- boot(data=baseball, statistic=bat.avg, R=1200, stype="i")

# print original measure, estimates of bias, & std error
avgBoot

# print the confidence intervals
boot.ci(avgBoot, conf=.95, type="norm")

#visual results - histogram
ggplot() + 
  geom_histogram(aes(x=avgBoot$t), fill="grey", color="grey") + 
  geom_vline(xintercept = avgBoot$t0 + c(-1,1)*2*sqrt(var(avgBoot$t)), linetype=2)

