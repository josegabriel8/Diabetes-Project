# Methods and Tools for Health Statistics - Standardization Methods and multivariable analysis

# Eduart Murati - Fabrizio Carinci

# Standardization Methods ----

# Clear environment, load functions, packages and data 

list.files()
rm(list=ls())
options(scipen=999)

source("methods_lab_functions.R")

#installLibraries(c("ggplot2","gmodels"))

library(gmodels)
require(ggplot2)


# Load data 

load(file="diadata_2017.Rda")


## Direct Standardization ----
# Adopts AHRQ formulas for the direct standardization.

# Merge categories with few observations

diadata$recl_age<-ifelse(diadata$cl_age %in% c(1,2,3),3,diadata$cl_age) # Creates a new variable assigning to class 3, the age classes 1,2,3 of cl_age variable


# Create a common structure for the denominator/nominator
n_terms<-length(levels(as.factor(diadata$recl_age)))*length(levels(as.factor(diadata$males)))
std_m<-matrix(c(rep(0,1,n_terms)),length(levels(as.factor(diadata$recl_age))))
rownames(std_m) <- levels(as.factor(diadata$recl_age))
colnames(std_m) <- levels(as.factor(diadata$males))


# Population table for the whole population
pop_ij<-table(diadata$recl_age,diadata$males)   # Contingency table for Age Class and gender
totpop<-sum(pop_ij)                                           # N - number of total population (sum of all the counts of the cells of the table)    



# CALCULATE CRUDE AND DIRECT STANDARDIZED RATES FOR HOSPITAL 19


label_target_group<-"Hospital 19"  

target_group<-subset(diadata,diadata$ny_hosp_id=="19")          # Select from amidata only patients of hosp_id 19
outcome_intarget<-subset(target_group,target_group$los8==1)     # Select from patients of hosp_id 19 only the expired ones 

label_target_group<- paste("Hospital", unique(as.character(target_group$ny_hosp_id)) )                        # Store the label name

num_data<-table(outcome_intarget[,c("recl_age")],outcome_intarget[,c("males")])   # Age - gender table of dead patients of hosp_id 19. Cells contain dij.
den_data<-table(target_group[,c("recl_age")],target_group[,c("males")])           # Age - gender table of patients of hosp_id 19. Cells contain nij.


# Note: tables have different shapes

num_data
den_data


# Make the numerator uniform with denominator. To make matrix operations we need to have matrices of the same dimensions

# Adapt num_data structure if needed

res<-merge(as.data.frame.matrix(num_data),as.data.frame.matrix(std_m),by="row.names",all.y=TRUE)
res<-res[2:3]
res[is.na(res)] <- 0
res<-res[,c(sort(names(res)))]

res <- as.matrix(res)
rownames(res)<-rownames(std_m)
colnames(res)<-colnames(std_m)
res<-rowsum(res,row.names(res))

# Note: If the numerator has 0 rows than apply directly: *res<-as.data.frame.matrix(std_m)*

# Adapt den_data structure if needed

den_data<-merge(as.data.frame.matrix(den_data),as.data.frame.matrix(std_m),by="row.names",all.y=TRUE)
den_data<-den_data[,2:3]
den_data<-den_data[,c(sort(names(den_data)))]
rownames(den_data) <- levels(as.factor(diadata$recl_age))
colnames(den_data) <- levels(as.factor(diadata$males))
den_data[is.na(den_data)] <- 0


# res and num_data have the same length now - 3x2

res
den_data

asr_ijt<-as.matrix(res/den_data)                                        # Age - gender table with dij/nij cells                        
cr_t<-((sum(num_data)/sum(den_data) ))*100                              # Crude rate=sum(dij)/Sum(nij) *100
sr_t<-sum(unclass(asr_ijt)*unclass(pop_ij))/totpop*100                  # Standardized rate=sum(dij/nij * Nij)/N *100
sr_t.se<- sqrt((sr_t/100*(1-sr_t/100))/(sum(den_data)))                 # Standard error  
sr_t.ll95  <- sr_t - 1.96 * sr_t.se                                     # Lower 95% C.I. 
sr_t.ul95  <- sr_t + 1.96 * sr_t.se                                     # Upper 95% C.I.

output<-data.frame(label_target_group,round(cr_t,2),round(sr_t,2),round(sum(den_data),2),round(sr_t.ll95,2),round(sr_t.ul95,2))
names(output)<-c("Unit","CR","SR","N","Lower95","Upper95")
output

### the only thing whe have to do is change ny_hosp_id for whatever we want to make the standardization

calculate_rates <- function( hosp_id, pop_ij, totpop) {
  # Select from data only patients of specified hosp_id
  target_group <- subset(diadata, diadata$ny_hosp_id == hosp_id) ##we have to change heeere
  
  # Select from patients of specified hosp_id only the expired ones
  outcome_intarget <- subset(target_group, target_group$los8 == 1)
  
  # Store the label name
  label_target_group <- paste("Hospital", unique(as.character(target_group$ny_hosp_id)))
  
  # Age - gender table of dead patients of hosp_id. Cells contain dij.
  num_data <- table(outcome_intarget[, c("recl_age")], outcome_intarget[, c("males")])
  
  # Age - gender table of patients of hosp_id. Cells contain nij.
  den_data <- table(target_group[, c("recl_age")], target_group[, c("males")])
  
  # Make the numerator uniform with denominator.
  res <- merge(as.data.frame.matrix(num_data), as.data.frame.matrix(pop_ij), by = "row.names", all.y = TRUE)
  res <- res[2:3]
  res[is.na(res)] <- 0
  res <- res[, c(sort(names(res)))]
  res <- as.matrix(res)
  rownames(res) <- rownames(pop_ij)
  colnames(res) <- colnames(pop_ij)
  res <- rowsum(res, row.names(res))
  
  # Adapt den_data structure if needed
  den_data <- merge(as.data.frame.matrix(den_data), as.data.frame.matrix(pop_ij), by = "row.names", all.y = TRUE)
  den_data <- den_data[, 2:3]
  den_data <- den_data[, c(sort(names(den_data)))]
  rownames(den_data) <- levels(as.factor(diadata$recl_age))
  colnames(den_data) <- levels(as.factor(diadata$males))
  den_data[is.na(den_data)] <- 0
  
  # Age - gender table with dij/nij cells
  asr_ijt <- as.matrix(res / den_data)
  
  # Crude rate
  cr_t <- ((sum(num_data) / sum(den_data))) * 100
  
  # Standardized rate
  sr_t <- sum(unclass(asr_ijt) * unclass(pop_ij)) / totpop * 100
  
  # Standard error
  sr_t.se <- sqrt((sr_t / 100 * (1 - sr_t / 100)) / (sum(den_data)))
  
  # Lower 95% C.I.
  sr_t.ll95 <- sr_t - 1.96 * sr_t.se
  
  # Upper 95% C.I.
  sr_t.ul95 <- sr_t + 1.96 * sr_t.se
  
  # Create output data frame
  output <- data.frame(
    Unit = label_target_group,
    CR = round(cr_t, 2),
    SR = round(sr_t, 2),
    N = round(sum(den_data), 2),
    Lower95 = round(sr_t.ll95, 2),
    Upper95 = round(sr_t.ul95, 2)
  )
  return(output)
}




result_df <- data.frame()

# Iterate over each value in the vector "klk"
for (value in unique(diadata$ny_hosp_id)) {   #also here we have to changeeee
  # Perform the calculation
  result <- calculate_rates(value, pop_ij=std_m, totpop)
  
  # Append the result to the data frame
  result_df <- rbind(result_df, result)
}


# Example usage:
# result <- calculate_rates(data = diadata, hosp_id = "19", pop_ij = std_m, totpop = 1000)
# print(result)



## Indirect Standardization ----
# Adopts AHRQ formulas for indirect standardization
# Regression Standardization estimate the model using quality score as an outcome of a different logistic regression model


# Fit a logistic model with death outcome and class age and males control variables 
DLogit<-glm(los8~d_severe+males+ampt+white+year2021+ampt+compl,
                 family = binomial("logit"),data=diadata)


#Show selected results: Coefficients, C.I. of the coefficients, Exp(Coefficiets)=Odd Ratios, C.I. of the OR and P-value of wald test 

round(cbind(Beta=coef(DLogit),confint(DLogit),P=coef(summary(DLogit))[,4],exp(cbind(OR=coef(DLogit),confint(DLogit)))),4)

# Linear combination of the predictors

diadata$p<-predict(DLogit,newdata=diadata,type="response")           # Create a variable p with predicted probabilities from fitted logistic model              

# Total deaths from dia patients divided by the number of observations.Average outcome in the entire sample:reference population rate

Ybar<-sum(diadata$los8,na.rm=TRUE)/dim(diadata)[1]                   # Overall death rate 


# Average predicted for ALL dataset
mean(diadata$p)                                                      # Error, with missing values.         
mean(diadata$p,na.rm=TRUE)                                           # Without missing values 
sum(diadata$p,na.rm=TRUE)/dim(diadata)[1]                            # Divided by the number of observations
sum(diadata$p,na.rm=TRUE)/sum(!is.na(diadata$p))                     # Divided by the number of observations WITHOUT missing values

# Calculate Standardized Rates for hosp_id = 1
O1<-mean(diadata[diadata$ny_hosp_id=="1",c("los8")],na.rm=TRUE)     # Oi=(1/ni )sum(Yij ) OBSERVED DEATH RATE at hosp_id 1
E1<-mean(diadata[diadata$ny_hosp_id=="1",c("p")],na.rm=TRUE)        # Ei = (1/ni )sum(Pij) EXPECTED DESTH RATE at hosp_id 1O17
N1<-dim(diadata[diadata$ny_hosp_id=="1",])[1]                       # Number of patients at hosp_id 1.
P1j<-diadata[diadata$ny_hosp_id=="1",c("p")]                        # Predicted probability for the patients of hospital 1, for every j-th patint


rar1<-Ybar*(O1/E1)                                                  # Risk adjusted rate (indirect standardized rate)
rar1.se<-sqrt( (Ybar/E1)^2 *(1/N1)^2 * sum(P1j*(1-P1j),na.rm=TRUE)) # Standard error of risk adjusted rate
rar1.ll95  <- rar1 - 1.96 * rar1.se                                 # Lowwer 95% C.I. 
rar1.ul95  <- rar1 + 1.96 * rar1.se                                 # Upper 95% C.I.

# output label, crude death rate, standardized rate, nr of observations in hosp_id 1 and C.I. 
output2<-data.frame(paste("HOSPITAL","1"),round(O1*100,2),round(rar1*100,2),N1,round(rar1.ll95*100,2),round(rar1.ul95*100,2))
names(output2)<-c("Unit","CR","SR","N","Lower95","Upper95")   # Rename the variables

# View output
output2
#########################generalizing the results for all hospitals#####
outputs <- data.frame(ny_hosp_id=numeric(),Unit = character(), CR = numeric(), SR = numeric(), N = numeric(), Lower95 = numeric(), Upper95 = numeric())

# Loop through each unique hospital ID
for (id in unique(diadata$ny_hosp_id)) {
  # Subset data for the current hospital ID
  hosp_data <- diadata[diadata$ny_hosp_id == id, ]
  
  # Calculate Oi, Ei, and N
  Oi <- mean(hosp_data$los8, na.rm = TRUE)
  Ei <- mean(hosp_data$p, na.rm = TRUE)
  Ni <- nrow(hosp_data)
  
  # Calculate rar1 and rar1.se
  rar1 <- Ybar * (Oi / Ei)
  rar1.se <- sqrt((Ybar / Ei)^2 * (1 / Ni)^2 * sum(hosp_data$p * (1 - hosp_data$p), na.rm = TRUE))
  
  # Calculate lower and upper 95% CI
  rar1.ll95 <- rar1 - 1.96 * rar1.se
  rar1.ul95 <- rar1 + 1.96 * rar1.se
  
  # Create output data frame for current hospital ID
  output <- data.frame(ny_hosp_id=id,Unit = paste("HOSPITAL", id),
                       CR = round(Oi * 100, 2),
                       SR = round(rar1 * 100, 2),
                       N = Ni,
                       Lower95 = round(rar1.ll95 * 100, 2),
                       Upper95 = round(rar1.ul95 * 100, 2))
  
  # Bind output data frame to outputs
  outputs <- rbind(outputs, output)
}


########################################



## Funnel Plots using the NY data with crude rateee----

m_dia<-aggregate(diadata$los8,by=list(diadata$ny_hosp_id),FUN="mean")             
m_dia_nrow<-aggregate(diadata$los8,by=list(diadata$ny_hosp_id),FUN=NROW)          
names(m_dia)<-c("ny_hosp_id","cr")                                                
names(m_dia_nrow)<-c("ny_hosp_id","n")                                            
m_dia<-merge(m_dia,m_dia_nrow,by=c("ny_hosp_id"))                                 
m_dia$cr<-m_dia$cr*100    
#hosp_county<-aggregate(ny_county ~ ny_hosp_id,diadata,FUN="unique") 
hosp_county <- aggregate(ny_county ~ ny_hosp_id, diadata, FUN = function(x) head(x, 1))
names(hosp_county)<-c("ny_hosp_id","ny_county")
z_dia<-merge(m_dia,hosp_county,by=c("ny_hosp_id"),all.x=TRUE)  
z_dia$county_up<-ifelse(unlist(z_dia$ny_county)>20,2,1)   #     there is a problem here                          


#funnel plot for indirect rates
funnel_plot(title="los8 crude Rates\n in the State of New York - Year 2019 and 2021", # Graph Title
            names=z_dia$ny_hosp_id,                # variable for group names
            names_outliers=1,                      # differently plot outliers by names (0=No;1=Yes)
            plot_names=1,                          # plot names instead of dots
            in_colour=1,                           # colour points within boundaries (0=No;1=Yes)
            colour_var=z_dia$ny_county,            # variable for colouring groups (numeric)
            colour_values=c("indianred2","blue"),  # list of colours for all levels of colour_var
            rate=z_dia$cr,                         # value of standardized rates
            unit=100,                              # denominator of rates
            population=z_dia$n,                    # total number of subjects for each rate
            binary=1,                              # binary variable (0=No;1=Yes)
            p_mean="weighted",                     # Unweighted, Weighted or specified value
            filename="",                           # output graph filename 
            graph="dia_mortality",                 # name of graph object
            pdf_height=3.5,                        # height pdf output
            pdf_width=6,                           # width pdf output
            ylab="Indicator per 100",              # y axis label
            xlab="Total N",                        # x axis label
            selectlab="County",                    # Label for group legend
            selectlev=c("1","2"),                  # Values of group legend
            dot_size=1.5,                          # Scaling factor for dots in the funnel plot
            dfout="dfout")                         # funnel dataset saved in global output object

print(dia_mortality)


##do the previous but with indirect rates
#hosp_county<-aggregate(ny_county ~ ny_hosp_id,diadata,FUN="unique") 
hosp_county <- aggregate(ny_county ~ ny_hosp_id, diadata, FUN = function(x) head(x, 1))

names(hosp_county)<-c("ny_hosp_id","ny_county")


z_dia2<-merge(outputs,hosp_county,by=c("ny_hosp_id"),all.x=TRUE)  
z_dia2$county_up<-ifelse(unlist(z_dia2$ny_county)>20,2,1)   #     there is a problem here                          


#funnel plot for indirect rates
funnel_plot(title="los8 indirect std Rates\n in the State of New York - Year 2019 and 2021", # Graph Title
            names=z_dia2$ny_hosp_id,                # variable for group names
            names_outliers=1,                      # differently plot outliers by names (0=No;1=Yes)
            plot_names=1,                          # plot names instead of dots
            in_colour=1,                           # colour points within boundaries (0=No;1=Yes)
            colour_var=z_dia2$county_up,            # variable for colouring groups (numeric)
            colour_values=c("indianred2","blue"),  # list of colours for all levels of colour_var
            rate=z_dia$cr,                         # value of standardized rates
            unit=100,                              # denominator of rates
            population=z_dia2$N,                    # total number of subjects for each rate
            binary=1,                              # binary variable (0=No;1=Yes)
            p_mean="weighted",                     # Unweighted, Weighted or specified value
            filename="",                           # output graph filename 
            graph="dia_mortality2",                 # name of graph object
            pdf_height=3.5,                        # height pdf output
            pdf_width=6,                           # width pdf output
            ylab="Indicator per 100",              # y axis label
            xlab="Total N",                        # x axis label
            selectlab="County",                    # Label for group legend
            selectlev=c("1","2"),                  # Values of group legend
            dot_size=1.5,                          # Scaling factor for dots in the funnel plot
            dfout="dfout")                         # funnel dataset saved in global output object

print(dia_mortality2)



# Multivariable analysis ----


list.files()
rm(list=ls())
options(scipen=999)


source("methods_lab_functions.R")

load(file="diadata.Rda")


#installLibraries(c("Hmisc","ggplot2","RCurl","devtools","gmodels","gridExtra","bootLR",
#                   "grid","plyr","geepack","caret","forestplot","Epi","MatchIt","epitools","ROCR","ROCit","pROC","cutpointr"))

library(Hmisc)
library(gmodels)
require(ggplot2)
library(gridExtra)
library(plyr)
library(grid)
library(geepack)
library(caret)
library(geepack)
library(forestplot)
library(Epi)
library(MatchIt)
library(epitools)
library(ROCR)
library(ROCit)
library(pROC)
library(cutpointr)
library(bootLR)


# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# MODEL BUILDING STRATEGIES
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


## Logistic regression model ----

# crate a dummy variable for severity  


diadata$d_severe<-0                                                       
diadata$d_severe<-ifelse(diadata$severe>=2,1,diadata$d_severe)            
diadata$d_severe<-ifelse(is.na(diadata$severe)==TRUE,NA,diadata$d_severe)  

# crate a dummy variable for  risk (similar to severity)  

diadata$d_risky<-0
diadata$d_risky<-ifelse(diadata$risky>=2,1,diadata$d_risky)
diadata$d_risky<-ifelse(is.na(diadata$risky)==TRUE,NA,diadata$d_risky)


# Plot log odds graph to explore risk patterns associated with specific levels of risk factors

emplogit(diadata$cost,diadata$los8,xlab="Cost",ylab="los8")       # Cost - logit(dead) plot

# Simple base model of diadata, Age, Sex and risk

Logit_reduced<-glm(los8~cl_age+males+d_risky,family = binomial("logit"),data=diadata)

round(cbind(Beta=coef(Logit_reduced),confint(Logit_reduced),P=coef(summary(Logit_reduced))[,4],exp(cbind(OR=coef(Logit_reduced),confint(Logit_reduced)))),4) # Results of Coefficients + 95% C.I. and exp(coefficients)=ORs + 95% 

round(cbind(BIC=BIC(Logit_reduced),AIC= AIC(Logit_reduced), "-2*Log-Likelihood"= -2 * as.numeric(logLik(Logit_reduced))))  # AIC,BIC and -2*log-likelihood

# Add other variables to the base model and check the results  

Logit_full<-glm(dead~cl_age+males+surgical+d_risky+d_severe+ampt,family = binomial("logit"),data=diadata)

round(cbind(Beta=coef(Logit_full),confint(Logit_full),P=coef(summary(Logit_full))[,4],exp(cbind(OR=coef(Logit_full),confint(Logit_full)))),4)

round(cbind(BIC=BIC(Logit_full),AIC= AIC(Logit_full), "-2*Log-Likelihood"= -2 * as.numeric(logLik(Logit_full))))


# Carry out formal test using the likelihood ratio

ndiffpars<-length(Logit_full$coefficients)-length(Logit_reduced$coefficients)       # Difference of number of variables- degrees of freedom 
LL1<-(-2*as.numeric(logLik(Logit_full)))                                            # -2 * log-likelihood of full model
LL2<-(-2*as.numeric(logLik(Logit_reduced)))                                         # -2 * log-likelihood of reduced model
LR<-pchisq(LL2-LL1,ndiffpars,lower.tail=FALSE)                                      # Chi Square test of: -2 * (log-likelihood of reduced model -log-likelihood of full model)
message(paste("-2 LogLik FULL:",LL1))                                               # Show -2 * log-likelihood of full model                          
message(paste("-2 LogLik REDUCED:",LL2))                                            # Show  -2 * log-likelihood of reduced model
message(paste("Likelihood Ratio:",LL2-LL1,"; P(chi-square)=",round(LR,4),"; df=",ndiffpars)) # Show Likelihood ratio, p value of chi square test and degrees of freedom

# Compute expected outcomes using a threshold of .5 in the probability of the outcomes and compute accuracy

diadata$p<- predict(Logit_reduced,newdata=diadata,type="response")      # Create a variable with prediction probabilities from the fitted model
predicted.value <- ifelse(diadata$p> 0.5,1,0)                           # Create vector with 1 entry if prediction probability higher than 0.5 and 0 otherwise
discordant <- mean(predicted.value != diadata$los8,na.rm=TRUE)          # Nr. of discordances between observed and expected values divided by nr of observations     
message(paste('Accuracy of the REDUCED model:', 1-discordant))           # Show accuracy = 1-discordance

# Compare to the survivors

1-mean(diadata$los8)



