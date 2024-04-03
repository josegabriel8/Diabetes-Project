########### CREATION OF DIADATA DATASET, QUICK ANALYSIS AND BIVARIATE ANALYSIS ######################

#SCRIPT PROJECT
#Group Blue: Niccolò Caselli, José Escarramàn, Cristian Perrone

setwd("C:/Users/cripe/OneDrive/Desktop/Methods and Tools - Carinci")
library(gmodels)
library(epitools)

#INITIAL DATASET
hospdata19<-read.csv("Hospital_Inpatient_Discharges__SPARCS_De-Identified___2019.csv",stringsAsFactors = F,header=T,sep=",")
hospdata21<-read.csv("Hospital_Inpatient_Discharges__SPARCS_De-Identified___2021.csv",stringsAsFactors = F,header=T,sep=",")
colnames(hospdata19)==colnames(hospdata21)
hospdata=rbind.data.frame(hospdata19,hospdata21)
write.csv(hospdata,"hospdata1921.csv")

#Creating a function for dataset reduction
create_hospdata<- function(input,output,value_labels="value_labels.txt") {
  
  hospdata <- read.csv(file=input,stringsAsFactors = F,header=T,sep=",")
  
  print(names(hospdata))
  
  #Creates a dummy variable for 2021 discharge year
  hospdata$year2021<-0
  hospdata$year2021<-ifelse(hospdata$Discharge.Year==2021 & !is.na((hospdata$Discharge.Year)),1,hospdata$year2021) 
  
  # Creates a dummy variable for gender  
  hospdata$males<-0                                                                         
  hospdata$males<-ifelse(hospdata$Gender=="U",NA,hospdata$males)                            
  hospdata$males<-ifelse(hospdata$Gender=="M" & !is.na((hospdata$Gender)),1,hospdata$males) 
  
  #Creates a dummy variable for race 
  hospdata$white<-0
  hospdata$white<-ifelse(hospdata$Race=="White" & !is.na((hospdata$Race)),1,hospdata$white) 
  
  ## Extract the levels of the variables
  
  # Assign the unique levels of the variable considering it as factor variable. 
  
  ny_counties<-levels(factor(hospdata$Hospital.County))    # Save the levels of the variable Hospital.County to ny_counties
  ny_areas<-levels(factor(hospdata$Hospital.Service.Area))
  ny_age<-levels(factor(hospdata$Age.Group))  
  ny_adm_types<-levels(factor(hospdata$Type.of.Admission))
  ny_races<-levels(factor(hospdata$Race))
  ny_ethnicities<-levels(factor(hospdata$Ethnicity))
  ny_diags<-levels(factor(hospdata$CCSR.Diagnosis.Description))
  ny_procs<-levels(factor(hospdata$CCSR.Procedure.Description))
  ny_dispositions<-levels(factor(hospdata$Patient.Disposition))
  ny_severities<-levels(factor(hospdata$APR.Severity.of.Illness.Description))
  ny_risks<-levels(factor(hospdata$APR.Risk.of.Mortality)) 
  
  # Store the levels into the working space.
  
  assign("ny_counties",ny_counties,envir=.GlobalEnv)       # assign ny_counties values to ny_counties variable and store in global envoirment
  assign("ny_areas",ny_areas,envir=.GlobalEnv)
  assign("ny_age",ny_age,envir=.GlobalEnv)
  assign("ny_adm_types",ny_adm_types,envir=.GlobalEnv)
  assign("ny_races",ny_races,envir=.GlobalEnv)
  assign("ny_ethnicities",ny_ethnicities,envir=.GlobalEnv)
  assign("ny_diags",ny_diags,envir=.GlobalEnv)
  assign("ny_procs",ny_procs,envir=.GlobalEnv)
  assign("ny_dispositions",ny_dispositions,envir=.GlobalEnv)
  assign("ny_severities",ny_severities,envir=.GlobalEnv)
  assign("ny_risks",ny_risks,envir=.GlobalEnv)
  
  
  ## Transformation into numerical variables 
  
  # Turn the levels of a variable transformed in a factor variable in numeric entries, assigning the index number.
  
  hospdata$ny_hosp_id<-as.numeric(as.factor(hospdata$Facility.Name))  # generates ny_hosp_id variable in hospdata data.frame with the transformed values of Facility_Name 
  hospdata$ny_county<-as.numeric(as.factor(hospdata$Hospital.County))
  hospdata$ny_area<-as.numeric(as.factor(hospdata$Hospital.Service.Area))
  hospdata$cl_age<-as.numeric(as.factor(hospdata$Age.Group))
  hospdata$zipcode<-as.numeric(as.factor(hospdata$Zip.Code...3.digits))
  hospdata$adm_type<-as.numeric(as.factor(hospdata$Type.of.Admission))
  hospdata$race<-as.numeric(as.factor(hospdata$Race))
  hospdata$ethnicity<-as.numeric(as.factor(hospdata$Ethnicity))
  hospdata$los<-hospdata$Length.of.Stay                                                      # generates los variable in hospdata data.frame 
  hospdata$disposition<-as.numeric(as.factor(hospdata$Patient.Disposition))
  hospdata$diagnosis<-as.numeric(as.factor(hospdata$CCSR.Diagnosis.Description))
  hospdata$procedure<-as.numeric(as.factor(hospdata$CCSR.Procedure.Description))
  hospdata$drg<-as.numeric(as.factor(hospdata$APR.DRG.Code))
  hospdata$mdc<-as.numeric(as.factor(hospdata$APR.MDC.Code))
  hospdata$severity<-as.numeric(as.factor(hospdata$APR.Severity.of.Illness.Description))
  hospdata$risk<-as.numeric(as.factor(hospdata$APR.Risk.of.Mortality))
  hospdata$payment_type<-as.numeric(as.factor(hospdata$Payment.Typology.1))
  hospdata$cost<-hospdata$Total.Charges
  
  # Dummy variable for surgical. The not available entries were recorded as Not Applicable in the original variable.
  
  hospdata$surgical<-0
  hospdata$surgical<-ifelse(hospdata$APR.Medical.Surgical.Description=="Not Applicable",NA,hospdata$surgical)
  hospdata$surgical<-ifelse(hospdata$APR.Medical.Surgical.Description=="Surgical" & !is.na((hospdata$surgical)),1,hospdata$surgical)
  
  # Diabetes- Diabetes Mellitus, 
  #grep function search for matches. 
  #Output the index number of DMC from the ny_diags stored before. 
  
  grep("Diabetes mellitus",ny_diags)
  ny_diags[195]
  ny_diags[196]
  ny_diags[197]
  ny_diags[198]
  
  # Create a dummy variable for expired patients (dead). Disposition = 6  matches with expired.   
  
  grep("Expired", ny_dispositions)
  ny_dispositions[6]
  
  hospdata$dead<-0
  hospdata$dead<-ifelse(hospdata$disposition==6,1,hospdata$dead) 
  
  # Create a dummy variable for Diabetes mellitus. 
  
  hospdata$diabetes<-0
  hospdata$diabetes<-ifelse(hospdata$diagnosis>=195 & hospdata$diagnosis<=198,1,hospdata$diabetes)
  
  #Create dummy variable for diabetes-related complications
  
  hospdata$compl<-0
  hospdata$compl<-ifelse(hospdata$diagnosis==195|hospdata$diagnosis==196,1,hospdata$compl)
  
  
  # Remove comma from cost variable and different entries exept 0-9. digits.
  
  hospdata$cost<-as.numeric(gsub("[^0-9.]","",hospdata$cost))
  hospdata$los <-as.numeric(gsub("[^0-9.]","",hospdata$los))
  
  # Create a dataframe with the selected variables. Include all the rows and the specified selected columns.   
  
  ny_hospdata<-hospdata[,c("year2021","ny_hosp_id","ny_county","ny_area","cl_age",
                           "males","white","zipcode","adm_type","race","ethnicity","los",
                           "disposition","diagnosis","procedure","drg","mdc","severity",
                           "risk","payment_type","cost","surgical",
                           "dead","diabetes","compl")]
  
  # Save the re-encoded and reduced data frame in .csv format in the working directory. Row names setted as false because first column does not contains the names of the observations.      
  
  ##write.csv(ny_hospdata,output,row.names=FALSE)
  
  write.csv(ny_hospdata,"ny_hospdata.csv",row.names=FALSE)  # Manual save
  
  ##  Save the levels in an external file 
  
  # Save the levels of factor variables in a text file (.txt).
  
  sink(value_labels)
  # sink()  # Manual run 
  cat ("\n")
  cat ("Counties\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_counties)
  cat ("\n")
  cat ("Areas\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_areas)
  cat ("\n")
  cat ("Age\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_age)
  cat ("\n")
  cat ("Adm Types\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_adm_types)
  cat ("\n")
  cat ("Races\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_races)
  cat ("\n")
  cat ("Ethnicities\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_ethnicities)
  cat ("\n")
  cat ("Diagnoses\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_diags)
  cat ("\n")
  cat ("Procedures\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_procs)
  cat ("\n")
  cat ("Dispositions\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_dispositions)
  cat ("\n")
  cat ("Risks\n")
  cat ("##################################\n")
  cat ("\n")
  print(ny_risks)
  sink()
  
  # Remove hospdata (the complete dataset) from Global environment to clear the memory.
  
  rm(hospdata)
}


# Use the function created. Parameters: input equal to the NYSDOH data and value_labels.txt and  output reduced dataset in .csv format saved 
# in the working directory and text file with the levels of the  factor variables.

create_hospdata(input="hospdata1921.csv",output="ny_hospdata.csv", value_labels="value_labels.txt")

## Load and save the reduced dataset ----

# Load the reduced dataset.

ny_hospdata<-read.csv("ny_hospdata.csv")


# Save to RDA file. The RDA format is less heavy and loads quicker. 

save(ny_hospdata,file="ny_hospdata.Rda")


# From now on we will use only the RDA files. 

rm(ny_hospdata)
load("ny_hospdata.Rda")


#INITIAL ANALYSIS
diadata<-ny_hospdata[ny_hospdata$diabetes==1,] 


#Create a variable for amputations 

diadata$ampt<-0
diadata$ampt <- ifelse(diadata$procedure==5 | diadata$procedure==26| 
                         diadata$procedure==114| 
                         diadata$procedure==300, 1, diadata$ampt)
#5:"ABOVE KNEE AND OTHER PROXIMAL LOWER EXTREMITY AMPUTATION"
#26:"BELOW KNEE AMPUTATION"
#114:"FINGER AND OTHER UPPER EXTREMITY AMPUTATION"  
#300: "TOE AND MID FOOT AMPUTATION" 


# Re-encode risk and severity of illness.

#ny_risks:  # Extreme=2, Major=3, Minor=4, Moderate=5 (the natural order: 0 -> Minor, 1 -> Moderate, 2 -> Major and 3 -> Extreme)

diadata$risky<-NA                
diadata$risky<-ifelse(diadata$risk=="4",0,diadata$risky) # <= the order is inverted
diadata$risky<-ifelse(diadata$risk=="5",1,diadata$risky)    
diadata$risky<-ifelse(diadata$risk=="3",2,diadata$risky)
diadata$risky<-ifelse(diadata$risk=="2",3,diadata$risky)

#dummy variable for risk
diadata$d_risky<-0
diadata$d_risky<-ifelse(diadata$risky>=2,1,diadata$d_risky)
diadata$d_risky<-ifelse(is.na(diadata$risky)==TRUE,NA,diadata$d_risky)


#ny_severities:  # Extreme=2, Major=3, Minor=4, Moderate=5 

diadata$severe<-NA
diadata$severe<-ifelse(diadata$severity=="4",0,diadata$severe)# <= the order is inverted
diadata$severe<-ifelse(diadata$severity=="5",1,diadata$severe)  
diadata$severe<-ifelse(diadata$severity=="3",2,diadata$severe)
diadata$severe<-ifelse(diadata$severity=="2",3,diadata$severe)

ny_severities
ny_risks
#dummy variable for severity
diadata$d_severe<-0
diadata$d_severe<-ifelse(diadata$severe>=2,1,diadata$d_severe)
diadata$d_severe<-ifelse(is.na(diadata$severe)==TRUE,NA,diadata$d_severe)

ny_age
#dummy variables for age
diadata$age70ormore <- as.numeric(diadata$cl_age == 4)
diadata$fiftyto69 <- as.numeric(diadata$cl_age == 5)
diadata$more_than49<-0
diadata$more_than49<-ifelse(diadata$cl_age>3,1,diadata$more_than49)
diadata$more_than49<-ifelse(is.na(diadata$cl_age)==TRUE,NA,diadata$more_than49)

#Levels: Elective Emergency Newborn Not Available Trauma Urgent
diadata$d_admtype<-0
diadata$d_admtype<-ifelse(diadata$adm_type==2|diadata$adm_type>4,1,diadata$d_admtype)

diadata$pci<-0
diadata$pci<-ifelse(diadata$procedure==227,1,diadata$pci)

diadata$dialysis<-0
diadata$dialysis<-ifelse(diadata$procedure==140|diadata$procedure==237,1,diadata$dialysis)

diadata19<-diadata[diadata$year2021==0,]
diadata21<-diadata[diadata$year2021==1,]
quantile(diadata19$los,probs=0.8)

#Since the threshold between the 4th and the 5th quintile of length of stay is 8 days, we create a binary variable
#called los8: 0 for those who have los <8; 1 for those who have los>=8

diadata$los8<-0
diadata$los8<-ifelse(diadata$los>=8,1,diadata$los8)



# Save the subset

save(diadata,file="diadata.Rda")

# Quick analysis

##2019
table(diadata19$diabetes,diadata19$los8) # table with absolute frequencies (rows= diabetes) .  
round(prop.table(table(diadata19$diabetes,diadata19$los8)),4)
table(diadata19$compl,diadata19$los8)
round(prop.table(table(diadata19$compl,diadata19$los8)),4)
table(diadata$year2021,diadata$los8)
round(prop.table(table(diadata$year2021,diadata$los8)),4)
##2021
table(diadata21$diabetes,diadata21$los8)  # table with absolute frequencies (rows= diabetes) .  
round(prop.table(table(diadata21$diabetes,diadata21$los8)),4)
table(diadata21$compl,diadata21$los8)
round(prop.table(table(diadata21$compl,diadata21$los8)),4)
table(diadata$year2021,diadata$los8)
round(prop.table(table(diadata$year2021,diadata$los8)),4)
# Consider each level of the severity of illness 
##2019
table(diadata19$severe,diadata19$los8)
round(prop.table(table(diadata19$severe,diadata19$los8),1),2)
##2021
table(diadata21$severe,diadata21$los8)
round(prop.table(table(diadata21$severe,diadata21$los8),1),2)
# Split the data into subsets and return the result in a convenient form. FUN parameter computes summary statistics which can be applied to all data subsets.

m_dia<-aggregate(diadata$los8,by=list(diadata$year2021,diadata$ny_hosp_id),FUN="mean")
m_dia_nrow<-aggregate(diadata$los8,by=list(diadata$year2021,diadata$ny_hosp_id),FUN="NROW")    # Nr. of rows for each hospital_id.
names(m_dia)<-c("year2021","ny_hosp_id","cr")                   
names(m_dia_nrow)<-c("year2021","ny_hosp_id","n")               
m_dia<-merge(m_dia,m_dia_nrow,by=c("ny_hosp_id","year2021"))    # Merge data matching data frames by the id of the hospitals found in both data frames
m_dia$cr<-m_dia$cr*100                               # los8 rates*100

# Aggregate the deaths by ny areas
m_dia_area<-aggregate(diadata$los8,by=list(diadata$year2021,diadata$ny_area),FUN="mean")      # los8 rates, grouped by NY area 
names(m_dia_area)<-c("year2021","ny_area","los8_rate")                                 
m_dia_area$los8_rate<-round(m_dia_area$los8_rate*1000,2)                     # los8 rates (x1000)


### Bivariate analysis ----

#### Categorical variables ---- 
##2019
chisq.test(diadata19$males,diadata19$los8, correct=FALSE)   #highly significant
chisq.test(diadata19$white,diadata19$los8, correct=FALSE)   #highly significant
chisq.test(diadata19$compl,diadata19$los8, correct=FALSE)   #highly significant
chisq.test(diadata19$ampt,diadata19$los8, correct=FALSE)    #highly significant
##2021
chisq.test(diadata21$males,diadata21$los8, correct=FALSE)   #highly significant
chisq.test(diadata21$white,diadata21$los8, correct=FALSE)   #highly significant
chisq.test(diadata21$compl,diadata21$los8, correct=FALSE)   #highly significant
chisq.test(diadata21$ampt,diadata21$los8, correct=FALSE)    #highly significant

##2019
CrossTable(diadata19$males,diadata19$los8)
CrossTable(diadata19$white,diadata19$los8)
CrossTable(diadata19$year2021,diadata19$los8)
CrossTable(diadata19$compl,diadata19$los8)
CrossTable(diadata19$ampt,diadata19$los8)
##2021
CrossTable(diadata21$males,diadata21$los8)
CrossTable(diadata21$white,diadata21$los8)
CrossTable(diadata21$year2021,diadata21$los8)
CrossTable(diadata21$compl,diadata21$los8)
CrossTable(diadata21$ampt,diadata21$los8)

##2019
oddsratio(diadata19$males,diadata19$los8)  #OR=1.27
oddsratio(diadata19$white,diadata19$los8)  #OR=1.08
oddsratio(diadata19$compl,diadata19$los8)  #OR=5.15
oddsratio(diadata19$ampt,diadata19$los8)   #OR=5.98
oddsratio(diadata19$d_severe,diadata19$los8)  #OR=5.62
oddsratio(diadata19$d_risky,diadata19$los8)  #OR=3.54
oddsratio(diadata19$dead,diadata19$los8)    #OR=5.43
oddsratio(diadata19$surgical,diadata19$los8) #OR=6.83
oddsratio(diadata19$more_than49,diadata19$los8) #OR=3.08
ny_age

##2021
oddsratio(diadata21$males,diadata21$los8)  #OR=1.16
oddsratio(diadata21$white,diadata21$los8)  #OR=1.06
oddsratio(diadata21$compl,diadata21$los8)  #OR=5.93
oddsratio(diadata21$ampt,diadata21$los8)   #OR=5.55
oddsratio(diadata21$d_severe,diadata21$los8)  #OR=5.57
oddsratio(diadata21$d_risky,diadata21$los8)  #OR=3.47
oddsratio(diadata21$dead,diadata21$los8)    #OR=5.15
oddsratio(diadata21$surgical,diadata21$los8) #OR=6.03
oddsratio(diadata21$more_than49,diadata21$los8) #OR=3.24

##############numero de ny_procs for amputations 6,26, 112,294




####################################################################################
###################### logistic GLM and GEE models #####################

##2019
diadata19$recl_age<-ifelse(diadata19$cl_age %in% c(1,2,3),3,diadata19$cl_age)  #the first 3 age categories collapsed in the same category (3)
diadata19$recl_age=as.factor(diadata19$recl_age)

#FULL MODEL

logit_model<-glm(los8~white+d_risky+d_severe+males+recl_age+ampt+d_admtype+pci+dialysis+compl,
                 family = binomial(link="logit"),data=diadata19)
summary(logit_model)


#REDUCED MODEL

library(MASS)
backward_model <- step(logit_model, direction = "backward")
summary(backward_model)

#OR
exp(backward_model$coefficients)
odds_ratio_ci <- exp(confint(backward_model))
odds_ratio_ci

 # Carry out formal test using the likelihood ratio
ndiffpars<-length(logit_model$coefficients)-length(backward_model$coefficients)       # Difference of number of variables- degrees of freedom 
LL1<-(-2*as.numeric(logLik(logit_model)))                                            # -2 * log-likelihood of full model
LL2<-(-2*as.numeric(logLik(backward_model)))                                         # -2 * log-likelihood of reduced model
LR<-pchisq(LL2-LL1,ndiffpars,lower.tail=FALSE)                                      # Chi Square test of: -2 * (log-likelihood of reduced model -log-likelihood of full model)
message(paste("-2 LogLik FULL:",LL1))                                               # Show -2 * log-likelihood of full model                          
message(paste("-2 LogLik REDUCED:",LL2))                                            # Show  -2 * log-likelihood of reduced model
message(paste("Likelihood Ratio:",LL2-LL1,"; P(chi-square)=",round(LR,4),"; df=",ndiffpars)) # Show Likelihood ratio, p value of chi square test and degrees of freedom


#ROC ANALYSIS
# Extract the predicted probabilities
diadata19$p<- predict(backward_model,newdata=diadata19,type="response")

#ACCURACY REDUCED MODEL
diadata19$p<- predict(backward_model,newdata=diadata19,type="response")
predicted.value <- ifelse(diadata19$p> 0.5,1,0)
discordant <- mean(predicted.value != diadata19$los8,na.rm=TRUE)
message(paste('Accuracy of the REDUCED model:',1-discordant))

# Compute predicted scores and graph thresholds in the ROC curve using the ROCR package
pr <- prediction(diadata19[!is.na(diadata19$p),c("p")],diadata19[!is.na(diadata19$p),c("los8")]) # Transforms the input data into "performance object" used from ROCR package
prf <- performance(pr, measure = "tpr", x.measure = "fpr")                               # An object of ROCR package that contains True positive rates and False positive rates
plot(prf)                                                                                # Receiver operating characteristic (ROC) Curve
abline(a=0, b= 1)                                                                        # Add line with intercept 0 and slope 1
title("ROC Curve Reduced Model")                                                         # Title of the plot

# Compute area under the curve (AUC). # AUC uses all possible cutpoints for prediction
auc <- performance(pr, measure = "auc")                                                  # Area under the curve - "AUC object" of ROCR package
message(paste(("AUC REDUCED:"),auc@y.values[[1]]))  

# Accuracy for  full model using a threshold of 0.5
diadata19$p2<- predict(logit_model,newdata=diadata19,type="response")
predicted.value <- ifelse(diadata19$p2> 0.5,1,0)
discordant <- mean(predicted.value != diadata19$los8,na.rm=TRUE)
message(paste('Accuracy of the FULL model:',1-discordant))


# ROC cuve and AUC for the full model

pr2 <- prediction(diadata19[!is.na(diadata19$p2),c("p2")],diadata19[!is.na(diadata19$p),c("los8")])
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2)
abline(a=0, b= 1)
title("ROC Curve Full Model")

# Compute area under the curve (AUC)

auc2<- performance(pr2, measure = "auc")
message(paste(("AUC FULL:"),auc2@y.values[[1]]))

# Compare ROC curves

plot(prf, col="red", lwd = 2)
plot(prf2, col="green", add=T)
legend(0.6,0.2,c('Reduced model','Full Model'),lty=c(1,1),lwd=c(1,1),
       col=c('red', 'green'), cex = 1.3,bty = "n")
abline(a=0, b= 1)
title("Roc curves")

#GEE MODEL

library(geepack)
modgee<-geeglm(los8~white+d_risky+d_severe+males+recl_age+ampt+d_admtype+pci+dialysis,
                 family = binomial(link="logit"),data=diadata19,id=ny_hosp_id,corstr="exchangeable")
summary(modgee)


confint.geeglm <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1+level)/2)
  citab <- with(as.data.frame(cc),
                cbind(Lower=Estimate-mult*Std.err,
                      Upper=Estimate+mult*Std.err))
  rownames(citab) <- rownames(cc)
  citab[parm,]
}

cbind(exp(coef(summary(modgee))),exp(confint.geeglm(modgee)))



#################################
#summary tables for demographics: age, sex and race
library(dplyr)

##2019
tabledemographic1_19<-diadata19 %>% 
  mutate(value = ny_age[cl_age]) %>% 
  group_by(value) %>% 
  summarise(characteristic="age", Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))

tabledemographic2_19<-diadata19 %>% 
  mutate(value = if_else(diadata19$male==1,"male","female")) %>% 
  group_by(value) %>% 
  summarise(characteristic="sex",  Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))

tabledemographic3_19<-diadata19 %>% 
  mutate(value = ny_races[diadata19$race]) %>% 
  group_by(value) %>% 
  summarise(characteristic="race",  Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))

##2021
tabledemographic1_21<-diadata21 %>% 
  mutate(value = ny_age[cl_age]) %>% 
  group_by(value) %>% 
  summarise(characteristic="age", Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))

tabledemographic2_21<-diadata21 %>% 
  mutate(value = if_else(diadata21$male==1,"male","female")) %>% 
  group_by(value) %>% 
  summarise(characteristic="sex",  Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))

tabledemographic3_21<-diadata21 %>% 
  mutate(value = ny_races[diadata21$race]) %>% 
  group_by(value) %>% 
  summarise(characteristic="race",  Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))


#summary table for Admission/Hospital characteristics severity, county, compl

##2019
tabledemographic4_19<-diadata19 %>% 
  mutate(value = ny_severities[severity]) %>% 
  group_by(value) %>% 
  summarise(characteristic="severity", Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))

tabledemographic5_19<-diadata19 %>% 
  mutate(value = ny_areas[diadata19$ny_area]) %>% 
  group_by(value) %>% 
  summarise(characteristic="area",  Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))

tabledemographic6_19<-diadata19 %>% 
  mutate(value = if_else(compl==1,"With Complications",
                         "Without Complications")) %>% 
  group_by(value) %>% 
  summarise(characteristic="Diabetes complications", Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))

##2021
tabledemographic4_21<-diadata21 %>% 
  mutate(value = ny_severities[severity]) %>% 
  group_by(value) %>% 
  summarise(characteristic="severity", Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))

tabledemographic5_21<-diadata21 %>% 
  mutate(value = ny_areas[diadata21$ny_area]) %>% 
  group_by(value) %>% 
  summarise(characteristic="area",  Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))

tabledemographic6_21<-diadata21 %>% 
  mutate(value = if_else(compl==1,"With Complications",
                         "Without Complications")) %>% 
  group_by(value) %>% 
  summarise(characteristic="Diabetes complications", Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead))


tabledemographic7<-diadata %>% 
  mutate(value = if_else(diadata$year2021==1,"2021","2019")) %>% 
  group_by(value) %>% 
  summarise(characteristic="Year",  Patients = n(), Average_lenghofstay=mean(los), min=min(los), max=max(los),deaths=sum(dead),amputations=sum(ampt),los8=sum(los8))



diadata$procesos <- ny_procs[diadata$procedure]

unique(diadata$procesos)

#summary for demographic characteristics
demog_car_19<-rbind(tabledemographic1_19,tabledemographic2_19,tabledemographic3_19)
demog_car_21<-rbind(tabledemographic1_21,tabledemographic2_21,tabledemographic3_21)

#summary for hospital characteristics
Hospital_car_19<-rbind(tabledemographic4_19,tabledemographic5_19,tabledemographic6_19)
Hospital_car_21<-rbind(tabledemographic4_21,tabledemographic5_21,tabledemographic6_21)

write.csv(demog_car,"demog_car.csv")
write.csv(demog_car,"hospital_car.csv")



####///////////////////////////////////////////////////
#STANDARDIZATION
####///////////////////////////////////////////////////


# DIRECT STANDARDIZATION: 


# list.files()
rm(list=ls())
options(scipen=999)

library(dplyr)

#preparing the countie population data

popcounty19<-cc_est2019_alldata %>% 
  filter(STNAME=="New York", YEAR==12) %>%
  mutate(Age_Category = case_when(
    AGEGRP %in% 0:10 ~ "0_49",
    AGEGRP %in% 11:14 ~ "50_69",
    AGEGRP %in% 15:18 ~ "70_"
  )) %>%
  group_by(CTYNAME,COUNTY,Age_Category) %>% 
  summarise(males=sum(TOT_MALE),females=sum(TOT_FEMALE))

popcounty21 <-cc_est2022_all %>% 
  filter(STNAME=="New York", YEAR==3) %>%
  mutate(Age_Category = case_when(
    AGEGRP %in% 0:10 ~ "0_49",
    AGEGRP %in% 11:14 ~ "50_69",
    AGEGRP %in% 15:18 ~ "70_"
  )) %>%
  group_by(CTYNAME,COUNTY,Age_Category) %>% 
  summarise(males=sum(TOT_MALE),females=sum(TOT_FEMALE))

save(popcounty19,file="popcounty19.Rda")
save(popcounty21,file="popcounty21.Rda")


library(tidyr)

# Creamos las nuevas columnas para hombres y mujeres según la categoría de edad
pop19 <- popcounty19 %>%
  pivot_longer(cols = c(males, females),
               names_to = "gender",
               values_to = "pop") %>%
  #separate(col = Genero, into = c("Genero", "Categoria"), sep = "_") %>%
  pivot_wider(names_from = c(Age_Category,gender), values_from = pop)

pop21 <- popcounty21 %>%
  pivot_longer(cols = c(males, females),
               names_to = "gender",
               values_to = "pop") %>%
  #separate(col = Genero, into = c("Genero", "Categoria"), sep = "_") %>%
  pivot_wider(names_from = c(Age_Category,gender), values_from = pop)

d<-diadata %>% 
  filter(diadata$Hospital.County=="na")

save(pop19,file="pop19.Rda")
save(pop21,file="pop21.Rda")

load(file="pop19.Rda")
load(file="pop21.Rda")

write.csv(pop19,file = "pop19.csv")




source("methods_lab_functions.R")

#installLibraries(c("Hmisc","gmodels","ggplot2","grid","plyr","gridExtra"))

library(Hmisc)
library(gmodels)
require(ggplot2)
library(gridExtra)
library(plyr)
library(grid)

# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
load(file="diadata.Rda")
load(file="ny_hospdata1921.Rda")




# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Direct Standardization for  Hospitals ----

# merge categories with few observations
diadata<-subset(diadata,diadata$year2021==0) #2019
diadata<-subset(diadata,diadata$year2021==1)#2021


diadata<-subset(diadata,diadata$ny_hosp_id!=141) #Redacted for Confidentiality
diadata<-subset(diadata,diadata$ny_hosp_id!=21) #county not found Cobleskill Regional Hospital
diadata<-subset(diadata,!is.na(diadata$males)) 


diadata$recl_age<-ifelse(diadata$cl_age %in% c(1,2,3),3,diadata$cl_age)
diadata$age_males<-paste(diadata$recl_age,"_",diadata$males)


diadata$hp_c<-if_else(diadata$ny_hosp_id==24,"St Lawrence",diadata$Hospital.County)
length(unique(diadata$hp_c))

# Create the common structure for the denominator
n_terms<-length(levels(as.factor(diadata$hp_c)))*length(levels(as.factor(diadata$age_males)))
std_m<-matrix(c(rep(0,1,n_terms)),length(levels(as.factor(diadata$hp_c))))
rownames(std_m) <- levels(as.factor(diadata$hp_c))
colnames(std_m) <- levels(as.factor(diadata$age_males))


# Population table for the whole population
pop_ij<-table(diadata[,c("hp_c")],diadata[,c("age_males")]) #we did not used this one
totpop<-sum(pop_ij) #i did not used this one
hosp_names<-levels(as.factor(as.numeric(diadata$ny_hosp_id)))

# Adapt the population table to common structure (if needed)
pop_ij<-merge(as.data.frame.matrix(pop_ij),as.data.frame.matrix(std_m),by="row.names",all.y=TRUE)
pop_ij<-as.matrix(pop_ij[,2:7])
rownames(pop_ij) <- levels(as.factor(diadata$hp_c))
colnames(pop_ij) <- levels(as.factor(diadata$age_males))
pop_ij[is.na(pop_ij)] <- 0

#we used this ones
pop_ij_c<-read.csv("pop_ij_county19.csv")# change for 2021  #https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/
pop_ij_c<-read.csv("pop_ij_county21.csv")# change for 2019  #https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/

pop_ij_c<-as.matrix(pop_ij_c[,2:7])

rownames(pop_ij_c) <- levels(as.factor(diadata$hp_c))
colnames(pop_ij_c) <- levels(as.factor(diadata$age_males))

totpop_c<-sum(pop_ij_c)
# CALCULATE CRUDE AND DIRECT STANDARDIZED RATES FOR ALL HOSPITALS




for (i in 1:length(hosp_names)) {
  
  # i<-6 # check any level here (do not run the for loop)
  
  target_group<-subset(diadata,diadata$ny_hosp_id==hosp_names[i])
  outcome_intarget<-subset(target_group,target_group$los8==1)
  
  label_target_group<-hosp_names[i]
  
  num_data<-table(outcome_intarget[,c("hp_c")],outcome_intarget[,c("age_males")])
  den_data<-table(target_group[,c("hp_c")],target_group[,c("age_males")])
  
  # adapt num_data structure if needed
  if (nrow(num_data)>0) {
    res<-merge(as.data.frame.matrix(num_data),as.data.frame.matrix(std_m),by="row.names",all.y=TRUE)
    res<-res[2:7]
  } else {
    res<-as.data.frame.matrix(std_m)
  }
  res[is.na(res)] <- 0
  res<-res[,c(sort(names(res)))]
  
  res <- as.matrix(res)
  rownames(res)<-rownames(std_m)
  res<-rowsum(res,row.names(res))
  
  # adapt den_data structure if needed
  den_data<-merge(as.data.frame.matrix(den_data),as.data.frame.matrix(std_m),by="row.names",all.y=TRUE)
  den_data<-den_data[,2:7]
  den_data<-den_data[,c(sort(names(den_data)))]
  rownames(den_data) <- levels(as.factor(diadata$hp_c))
  colnames(den_data) <- levels(as.factor(diadata$age_males))
  den_data[is.na(den_data)] <- 0
  
  asr_ijt<-as.matrix(res/den_data)
  
  asr_ijt<-as.data.frame.matrix(asr_ijt)
  asr_ijt<-merge(as.data.frame.matrix(asr_ijt),as.data.frame.matrix(std_m),by="row.names",all.y=TRUE)
  
  asr_ijt<-as.matrix(asr_ijt[2:7])
  asr_ijt[is.nan(asr_ijt)] <- 0
  asr_ijt[is.na(asr_ijt)] <- 0
  
  
  # calculate crude and standardized rates for the selected hospital 
  cr_t<-((sum(num_data)/sum(den_data) ))*100
  sr_t<-(sum(asr_ijt*pop_ij_c)/totpop_c)*100
  
  if (sum(den_data)>0) {
    sr_t.se<- sqrt((sr_t/100*(1-sr_t/100))/(sum(den_data)))
    sr_t.ll95  <- sr_t - 1.96 * sr_t.se
    sr_t.ul95  <- sr_t + 1.96 * sr_t.se
  } else {
    sr_t.se    <- NA
    sr_t.ll95  <- NA
    sr_t.ul95  <- NA
  }
  
  output<-data.frame(as.numeric(label_target_group),round(cr_t,2),round(sr_t,2),round(sum(den_data),2),round(sr_t.ll95,2),round(sr_t.ul95,2))
  names(output)<-c("Unit","CR","SR","N","Lower95","Upper95")
  
  if (i==1) {sr_output<-output} else {sr_output<-rbind(sr_output,output)}
  rm(output)
  
}
sr_output19<-sr_output
save(sr_output19, file = "sr_output19.Rda")
load("sr_output19.Rda")
load("sr_output21.Rda")

library(dplyr)

sr_output19$SR<- if_else(sr_output19$SR=="Inf",NA,sr_output19$SR)
#we used this one for the map

sr_output21$SR<- if_else(sr_output21$SR=="Inf",NA,sr_output21$SR)


mean(sr_output19$SR, na.rm  =TRUE)
mean(sr_output21$SR, na.rm  =TRUE)
median(sr_output19$SR, na.rm  =TRUE)
median(sr_output21$SR, na.rm  =TRUE)

sr_output21<-sr_output
save(sr_output21, file = "sr_output21.Rda")
load("sr_output21.Rda")
################################################################
sr_output2021<-sr_output %>% 
  mutate(year2021=1)

sr_output1921 <- rbind(sr_output2019,sr_output2021) %>% 
  mutate(Unit1=ifelse(year2021==1,paste(Unit,"-21"),paste(Unit,"-19"))) %>% 
  filter(Unit!="16") %>% 
  mutate(year2021= ifelse(year2021==1,"2021","2019"))



library(tidyr)

sr_output19$year2021 <- "0"
sr_output21$year2021 <- "1"


sr_output1921<-rbind(sr_output19,sr_output21)

# Plotting
ggplot(data = sr_output1921, aes(x = year2021, y = SR, fill = year2021)) +
  geom_violin(trim = FALSE, alpha = 0.8) +  # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.6) +  # Box plot
  labs(title = "Comparison of los8 Rates between 2019 and 2021",
       x = "Year",
       y = " Rate") +
  theme_minimal() +
  theme(legend.position = "none")



ggplot(data = sr_output1921, aes(x = year2021, y = SR)) +
  geom_point() +
  labs(title = "Difference in the rates of diabetes patients from 2019 to 2021",
       x = "year",
       y = "rate",
       color = "Year") +
  theme_minimal()



# Standardized rates for each hospital

print(sr_output)

#//////////////////////////////////////////////////////////
#indirect rates

source("methods_lab_functions.R")

#installLibraries(c("Hmisc","gmodels","ggplot2","grid","plyr","gridExtra"))

library(Hmisc)
library(gmodels)
require(ggplot2)
library(gridExtra)
library(plyr)
library(grid)

# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

load(file="diadata.Rda")
load(file="ny_hospdata1921.Rda")




length(unique(diadata$ny_county))
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# PRESENTATION. Direct Standardization for one Hospital ----
# adopts AHRQ formulas for the direct standardization
# see oecd_ahrq_standardized.pdf Page 5 (Header REFERENCE MATERIAL)
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# merge categories with few observations
diadata<-subset(diadata,diadata$year2021==0) #2019
diadata<-subset(diadata,diadata$year2021==1)#2021


diadata<-subset(diadata,diadata$ny_hosp_id!=141) #Redacted for Confidentiality
diadata<-subset(diadata,diadata$ny_hosp_id!=21) #county not found Cobleskill Regional Hospital
diadata<-subset(diadata,!is.na(diadata$males)) #county not found Cobleskill Regional Hospital
diadata$hp_c<-if_else(diadata$ny_hosp_id==24,"St Lawrence",diadata$Hospital.County)

diadata$recl_age<-ifelse(diadata$cl_age %in% c(1,2,3),3,diadata$cl_age)
diadata$age_males<-paste(diadata$recl_age,"_",diadata$males)


diadata$hp_c<-if_else(diadata$ny_hosp_id==24,"St Lawrence",diadata$Hospital.County)
length(unique(diadata$hp_c))



# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
# Regression Standardization

# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

###############################################
# Indirect Standardization using the NY Dataset
###############################################
load("diadata.RDA")

diadata<-subset(diadata,diadata$year2021==0)

outcome<-c("los8") 
covariates<-c("recl_age+males+hp_c")
# insert formula
DLogit<-glm(formula=paste(outcome,"~",covariates,sep=""),family = binomial("logit"),data=diadata)
#round(cbind(Beta=coef(DLogit),confint(DLogit),P=coef(summary(DLogit))[,4],exp(cbind(OR=coef(DLogit),confint(DLogit)))),4)
summary(DLogit)
# Linear combination of the predictors: p(outcome)
diadata$p<- predict(DLogit,newdata=diadata,type="response")
Ybar<-sum(diadata$dead,na.rm=TRUE)/dim(diadata)[1] # POPULATION AVERAGE: divided by the number of observations

# POPULATION AVERAGE YBAR predicted for ALL dataset
mean(diadata$p) # with missing values
mean(diadata$p,na.rm=TRUE) # without missing values
sum(diadata$p,na.rm=TRUE)/dim(diadata)[1] # divided by the number of observations
sum(diadata$p,na.rm=TRUE)/sum(!is.na(diadata$p))  # divided by the number of observations WITHOUT missing values

hosp_county <- aggregate(hp_c ~ ny_hosp_id, diadata, FUN = function(x) head(x, 1))
names(hosp_county)<-c("ny_hosp_id","hp_c")

# CRUDE AND INDIRECT STANDARDIZED RATES
Pij<-aggregate(diadata$p*(1-diadata$p),by=list(diadata$ny_hosp_id),FUN="sum",na.rm=TRUE)
names(Pij)<-c("ny_hosp_id","Pij")

Oi<-aggregate(diadata$los8,by=list(diadata$ny_hosp_id),FUN="mean",na.rm=TRUE)
names(Oi)<-c("ny_hosp_id","Oi")

Ni<-aggregate(diadata$los8,by=list(diadata$ny_hosp_id),FUN=NROW)
names(Ni)<-c("ny_hosp_id","Ni")

Ei<-aggregate(diadata$p,by=list(diadata$ny_hosp_id),FUN="mean",na.rm=TRUE)
names(Ei)<-c("ny_hosp_id","Ei")

SR_ami<-merge(Pij,Oi,by=c("ny_hosp_id"))
SR_ami<-merge(SR_ami,Ni,by=c("ny_hosp_id"))
SR_ami<-merge(SR_ami,Ei,by=c("ny_hosp_id"))
SR_ami$Ybar<-Ybar

SR_ami$rar     <-SR_ami$Ybar*(SR_ami$Oi/SR_ami$Ei)
SR_ami$rar.se  <-sqrt( (SR_ami$Ybar/SR_ami$Ei)^2 *((1/SR_ami$Ni)^2 * SR_ami$Pij))

SR_ami$rar.ll95<-SR_ami$rar - 1.96 * SR_ami$rar.se
SR_ami$rar.ul95<-SR_ami$rar + 1.96 * SR_ami$rar.se

SR_ami$Oi<-round(SR_ami$Oi*100,2)
SR_ami$Ei<-round(SR_ami$Ei*100,2)
SR_ami$rar<-round(SR_ami$rar*100,2)
SR_ami$rar.ll95<-round(SR_ami$rar.ll95*100,2)
SR_ami$rar.ul95<-round(SR_ami$rar.ul95*100,2)

SR_ami<-merge(SR_ami,hosp_county,by=c("ny_hosp_id"),all.x=TRUE)
SR_ami$county_new<-ifelse(SR_ami$hp_c %in% c("Kings", "Queens","Manhattan",
                                             "Suffolk", "Bronx", "Nassau"),2,1)

SR_ami$Ybar<-round(SR_ami$Ybar,2)
SR_ami$rar.se<-round(SR_ami$rar.se,2)
SR_ami$Pij<-round(SR_ami$Pij,2)

output<-SR_ami[,c("county_new","ny_hosp_id","hp_c","Ni","Oi","Ei","Ybar","Pij","rar","rar.se","rar.ll95","rar.ul95")]
names(output)<-c("county_new","Unit","County","N","CR","Ei","Ybar","Sum(p*(1-p))","SR","se(SR)","SR(Lower95)","SR(Upper95)")

print(output)
library(dplyr)



###################################################################################################
#  Funnel Plots using the NY Dataset
###################################################################################################

#los8
funnel_plot(title="lenth of stay greater than 8 dyas Rates\n in the State of New York for diabetes pacients, 2019", # Graph Title
            names=sr_output19$Unit,                     # variable for group names
            names_outliers=1,                      # differently plot outliers by names (0=No;1=Yes)
            plot_names=0,                          # plot names instead of dots
            in_colour=1,                           # colour points within boundaries (0=No;1=Yes)
            colour_var=sr_output19$county_new,           # variable for colouring groups (numeric)
            colour_values=c("indianred2","blue"),  # list of colours for all levels of colour_var
            rate=sr_output1921$SR,                        # value of standardized rates
            unit=100,                              # denominator of rates
            population=sr_output19$N,                   # total number of subjects for each rate
            binary=1,                              # binary variable (0=No;1=Yes)
            p_mean="weighted",                     # Unweighted, Weighted or specified value
            filename="",                           # output graph filename 
            graph="dia_los_col",             # name of graph object
            pdf_height=1.5,                        # height pdf output
            pdf_width=3,
            low=-2,
            high=10,
            alpha_1=0.001,
            alpha_2=0.5,                          # width pdf output
            ylab="Indicator per 100",              # y axis label
            xlab="Total N",                        # x axis label
            selectlab="Year",                    # Label for group legend
            selectlev=c("Major counties","Other counties"),                  # Values of group legend
            dot_size=1.5,                          # Scaling factor for dots in the funnel plot
            dfout="dfout")  
print(dia_los_col)


load(file="indirect19.csv")
load(file="indirect19_c.csv")
load(file="indirect21.csv")
load(file="indirect21_c.csv")



in_output1921 <-rbind(output19_c,output21_c)

sr_output1921$year<-if_else(sr_output1921$Year2021==1,"2021","2019")

boxplot(SR ~ year, data = sr_output1921, col = c("indianred2", "blue"),
        xlab = "Year", ylab = "Los8 rates", main = "Length of stay greater than 8 days Rates\n in the State of New York, 2019-21")














library(tableone)
tableOne1_1 <- CreateTableOne(vars = "SR", strata = "Year2021", data = sr_output1921, test=T)

print(tableOne1_1, nonnormal= "SR", test=T, showAllLevels=T)
#level 0                 1                 p      test 
#SR (median [IQR])       0.46 [0.05, 1.26] 0.59 [0.06, 1.41]  0.391 nonnorm

tableOne1_1<- as.data.frame(print(tableOne1_1, nonnormal= num, test=T, showAllLevels=T)) %>%
  filter(as.numeric(p)<=0.05) %>% 
  select(-level)




#to export for map data
hosp_county <- aggregate(hp_c ~ ny_hosp_id, diadata, FUN = function(x) head(x, 1))
names(hosp_county)<-c("ny_hosp_id","hp_c")


library(dplyr)
rates_c<-merge(sr_output19,hosp_county,by.x="Unit",by.y = "ny_hosp_id")

rates_c19<- rates_c %>% 
  mutate(SR=if_else(SR=="Inf",NA,SR),
         county= as.character(rates_c$hp_c)) %>% 
  group_by(county) %>% 
  summarise(m_CR=mean(CR),m_SR=mean(SR,na.rm = T),s_N=sum(N))
write.csv(rates_c19,file="rates_c19.csv")
