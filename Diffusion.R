library(ggplot2)
library(gsDesign)


#Function to test difference of the two binomial rates
#Ref. Eike's function to test relationship----

test_relationship<-function(data, intervention, impact)
{
  
  # Intervention means causal event with [yes] observation
  # But for simplification, we call it "intervention".
  # intervention_subset: List all the impact observation under intervention condition 
  # Check the return results in the global environment for more details of the calculation
  # Remove NAs value. Otherwise it will also count NA value as 1
  intervention_subset <- na.exclude(data[data[intervention]==1, impact] )
  
  # control_subset: List all the impact observation under control condition 
  # control means causal variable with [no] observation. It might not really be control. 
  # However, to make it simple, we call it "control".
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
  confinf<-ciBinomial(x1 = intervention_subset_success, #number of success in attributable to intervention
                      x2 = control_subset_success, #number of success attributable to control
                      n1 = intervention_subset_n, #length of impact observation under intervention
                      n2 = control_subset_n) #length of impact observation under control
  
  
  return(list(#n observation under all conditions
              total_n = intervention_subset_n+control_subset_n, 
              # Return observations under intervention  
              Intervention_subset=intervention_subset, 
              #x1 for success under intervention
              x1_intervention_subset_success=intervention_subset_success,
              #n1 for all interventions
              n1_intervention_subset_n = intervention_subset_n,
              #rate of success for intervention group
              yes_rate_intervention_subset = length(which(intervention_subset==1))/(intervention_subset_n),
              # Return observations under control
              Control_subset=control_subset,
              #x2 for success under control
              x2_control_subset_success=control_subset_success,
              #n2 for all control
              n2_control_subset_n = control_subset_n,
              #rate of success despite no intervention
              yes_rate_control_subset = length(which(control_subset==1))/(control_subset_n),
              #Confidence intervals for rate of success in intervention condition compared with control condition
              CI = confinf ))
}

# CW NOTE ####
# remove error messages if these are no longer valid
# error checkRange: Error in checkRange(x, ..., varname = deparse(substitute(x))) : n1 not on interval [1, Inf] 
# can not return result if there is no "control"n1=0 or "experiment"n2=0, 

#Subsetting data for analysis####
diffusion <- read.csv("diffusion.csv", header=TRUE)
#VSLA dataset####
vsla<-subset(diffusion,VSLA=="1")
#write.csv(vsla, "vsla-table.csv")

#non-VSLA dataset####
nonvsla<-subset(diffusion,VSLA=="0")
#write.csv(nonvsla, "nonvsla-table.csv")
#
vsla$total_adopt
nonvsla$total_adopt
#Subset VSLA and non-VSLA who adopted ACIS: to analyze dynamics of adoption####
adopt<-subset(diffusion, total_adopt=="1")
vslaadopt<-subset(adopt,VSLA=="1")
nonvslaadopt<-subset(adopt,VSLA=="0")

#I. VSLA and non-VSLA testing: Adopting pathway####
#Subset VSLA data
vsla<-subset(diffusion,VSLA=="1")

#1.1 VSLA TESTING risk occurance and risk perception----
# CW NOTE ####
# some issues here. Maybe becuase of NA in the data. Did you apply that change here? 
vsladisaster_perception <- test_relationship(vsla,"risk","risk_impact")

#1.2 Non-VSLA TESTING risk occurance and risk perception----

nonvslarisk_perception <- test_relationship(nonvsla,"risk","risk_impact")

#2.1 VSLA TESTING risk impact and need----

vslarisk_need <- test_relationship(vsla,"risk_impact","need")

#2.2 Non VSLA TESTING risk impact and need----

nonvslarisk_need <- test_relationship(nonvsla,"risk_impact","need")

#3.1 VSLA TESTING need and access----

vslaneed_access <- test_relationship(vsla,"need","access")

#3.2 nonVSLA TESTING need and access----

nonvslaneed_access <- test_relationship(nonvsla,"need","access")
#4.1 VSLA TESTING access and total read_listen----

vslaaccess_totalreadlisten <- test_relationship(vsla,"access","total_readlisten")

#4.2.NonVSLA TESTING access and total read_listen----

nonvslaaccess_totalreadlisten <- test_relationship(nonvsla,"access","total_readlisten")


#5.1 VSLA TESTING total read_listen and total discuss----

vslatotalreadlisten_discuss <- test_relationship(vsla,"total_readlisten", "total_discuss")

#5.2 nonVSLA TESTING total read_listen and total discuss----

nonvslatotalreadlisten_discuss <- test_relationship(nonvsla,"total_readlisten", "total_discuss")


#6.1 VSLA TESTING total discuss and total understand----

vsla_total_discuss_understand <- test_relationship(vsla, "total_discuss","total_understand")


#6.2 NonVSLA TESTING total discuss and total understand----

nonvsla_total_discuss_understand <- test_relationship(nonvsla, "total_discuss","total_understand")

#7.1 VSLA TESTING total read_listen and total_understand----

vsla_total_readlisten_understand <- test_relationship(vsla, "total_readlisten","total_understand")

#7.2 nonVSLA TESTING total read_listen and total_understand----

nonvsla_total_readlisten_understand <- test_relationship(nonvsla, "total_readlisten","total_understand")


#8.1 VSLATESTING total understand and total perceive ACS positively----

vsla_total_understand_totalposper <- test_relationship(vsla, "total_understand", "total_posperception")

#8.2 nonVSLATESTING total understand and total perceive ACS positively----

nonvsla_total_understand_totalposper <- test_relationship(nonvsla, "total_understand", "total_posperception")

#9.1 VSLATESTING total perceive ACS positively and total intention to apply----

vsla_total_posper_intention <- test_relationship(vsla, "total_posperception", "total_intention")


#9.2 Non-VSLA TESTING total perceive ACS positively and total decide to apply----

nonvsla_total_posper_intention <- test_relationship(nonvsla, "total_posperception", "total_intention")


#10.1 VSLATESTING intention to adopt and total adoption----

vsla_total_intention_adopt <- test_relationship(vsla, "total_intention", "total_adopt")

#10.2 non-VSLATESTING intention to adopt and total adoption----

nonvsla_total_intention_adopt <- test_relationship(nonvsla, "total_intention", "total_adopt")

#II. Dynamics of adoption####

#1.TESTING VSLA and confirm need among adopters----

#Subset VSLA and non-VSLA who adopted ACS only
adopt<-subset(diffusion, total_adopt=="1")
vsla_confirm2 <- test_relationship(adopt,"VSLA","confirm_need2")

#2.TESTING VSLA and scale-out (recommendation)----

vsla_recommend <- test_relationship(adopt,"VSLA","recommend")

#3. What advice do farmers dis-adopt####
# Seed dis-adoption
seed_disadoption<-ciBinomial(15, 17, 38, 38, alpha = 0.05, adj = 0, scale = "Difference")
#-26-16%#
fertilizer_disadoption<-ciBinomial(9, 4, 39, 36, alpha = 0.05, adj = 0, scale = "Difference")
#-5-29%#
plant_protection_disadoption<-ciBinomial(8, 8, 39, 37, alpha = 0.05, adj = 0, scale = "Difference")
#-19-17%#
water_management_disadoption<-ciBinomial(15, 12, 39, 38, alpha = 0.05, adj = 0, scale = "Difference")
#-14-27%#








