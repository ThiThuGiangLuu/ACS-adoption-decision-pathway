library(binom)
library(cowplot)
library(ggplot2)
library(ggthemes)
library(gsDesign)
library(lattice)
library(plyr)
library(reshape2)
library(scales)
library(tidyverse)
library(xtable)
library(wordcloud2)

test_relationship<-function(data, intervention, impact)
{
  
  # intervention subset. List all the impact obs under intervention condition 
  # "impact" means returns all the impact value under intervention condition
  # Check the return results in the global environment for more details of the calculation
  # Remove NAs value. Otherwise it will also count NA value as 1
  intervention_subset <- na.exclude(data[data[intervention]==1, impact] )
  
  #control subset. List all the impact observation under control condition 
  # Remove NAs value. Otherwise it will also count NA value as 0
  control_subset <- na.exclude(data[data[intervention]==0, impact] )
  
  #n of intervention. This length is also equal to the count of impact observation under intervention condition
  intervention_subset_n <- length(intervention_subset)
  
  #n of control. The length is also equal to the count of impact observations under control condition 
  control_subset_n <- length(control_subset)
  
  #filtering for '1' to get success count
  intervention_subset_success <- length(which(intervention_subset==1))
  #filter for '1' to count success despite no intervention
  control_subset_success <- length(which(control_subset==1))
 
  
  #Confidence interval of likelihood of same outcome in control condition
  confinf<-ciBinomial(x1 = intervention_subset_success, #number of success in exprimental
                      x2 = control_subset_success, #number of success in control
                      n1 = intervention_subset_n, #number of obs in experimental
                      n2 = control_subset_n) #number of obs in control
  
  
  return(list(CI = confinf, #confidence intervals for rate of success in control condition
              total_n = intervention_subset_n+control_subset_n, #n under all conditions
             
              # return these results to make it clear 
              intervention_subset, 
              intervention_subset_success,
              control_subset,
              control_subset_success,
              
              #rate of success for all intervention group
              yes_rate_intervention_subset = length(which(intervention_subset==1))/(intervention_subset_n),
              #rate of success despite no intervention
              yes_rate_control_subset = length(which(control_subset==1))/(control_subset_n),
              #n for all interventions
              intervention_subset_n = intervention_subset_n,
              #n for all control
              control_subset_n = control_subset_n))
}



example <- read.csv("example.csv", header=TRUE)

understand_apply <- test_relationship(example,"understand","apply")

jpeg(file="0.understand_apply.jpeg",width = 3000, height=2000, res=350)
layout(matrix(c(1,2),1,2),width=c(1.5,1.3))
par(mar=c(3.1,4.1,2.1,0.5))
barplot(c(understand_apply$yes_rate_intervention_subset*100,#percentage of success in experimental
          understand_apply$yes_rate_control_subset*100),#percentage of success in control
        ylim=c(0,100),
        ylab="Percentage applying advice (%)",
        col=c("dark green", "orange")
)
mtext ("a", side=3, line=1.0, adj=0.5, font = 2, cex=1.2)
box() 
axis(1,at=c(0.7,1.9),labels=c("Access information","Do not access"))
par(mar = c(3.1, 0.5, 2.1, 4.1))
plot(1,ylim=c(-100,100),col="WHITE",axes=FALSE,ylab="",xlab="")
arrows(1, as.numeric(understand_apply$CI[1])*100, 1, 
       as.numeric(understand_apply$CI[2])*100, 
       length=0.05, 
       angle=45, 
       code=3,
       lwd=3)
box()
axis(4)
lines(x=c(-2,2),y=c(0,0), col="red", lwd=2)
abline(h=30,col="orange", lwd=2)
abline(h=-30,col="orange", lwd=2)
text(x=1, y=70, label="Above 30% (strong  positive relation)", cex=0.9)
text(x=1, y=25, label="Below 30% (weak  positive relation)", cex=0.9)
text(x=1, y=-70, label="Below -30% (strong negative relation)", cex=0.9)
text(x=1, y=-25, label="Above -30% (weak negative relation)", cex=0.9)
text(x=1, y=5, label="Contains 0 (not statistically significant)", cex=0.9)
mtext("95% confidence level",side=1, line=1, cex = 1.0, adj=0.5)
mtext ("b", side=3, line=1.0, adj=0.5, font = 2, cex=1.2)
mtext("Probability difference between groups (%)",adj=0.5, side=4,line=2, cex=1.2)
dev.off()

