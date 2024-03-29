# Methods and Tools for Health Statistics 

# Eduart Murati - Fabrizio Carinci

###########################################################################################################################
# Hospital data, initial analysis and life expectancy                                                                     #
###########################################################################################################################

# New York Hospital Data ----

rm(list=ls())

# Load the dataset as a data frame. 
setwd("C:/Users/jose-/Desktop/health methods")
hospdata<-read.csv("Hospital_Inpatient_Discharges__SPARCS_De-Identified___2016_20240229.csv",stringsAsFactors = F,header=T,sep=",")

# Store the levels of a variable to a vector

gender<-levels(factor(hospdata$Gender))

# Create a dummy variable for Gender 

hospdata$males<-0                                                 # Create a variable called males with 0 entries.  

hospdata$males<-ifelse(hospdata$Gender=="U",NA,hospdata$males)    # If the variable Gender is equal to U we assign NA to the variable males.

hospdata$males<-ifelse(hospdata$Gender=="M" & !is.na((hospdata$Gender)),1,hospdata$males) #If the variable Gender is equal to M and is not NA. 

View(hospdata[c("Gender", "males")])


# Check number of unique entries of a vector

unique(gender)         # Unique entries. 

length(unique(gender)) # Length of the unique entries.


# Store the values to the environment 

assign("gen_lev",gender,envir=.GlobalEnv)

# Search for text matches

grep("M",gender)

gender[2]

grep("York", hospdata$Health.Service.Area)

hospdata$Health.Service.Area[31]

# replace of entries of all matches with gsub()

hospdata$cost<-as.numeric(gsub("[^0-9.]","",hospdata$Total.Charges))  # Remove any character except 0 to 9 digits also leave the "." for  the decimals. 

View(hospdata[1:10,c("Total.Charges","cost")])  # Compare THE FIRST 10 rows of the columns.

# Save the levels of the gender variable in a text file 

sink("levels_of_gender.txt")      # Create a .txt file named levels_of_gender.
cat ("\n")                        # Skip the row 
cat ("Gender\n")                  # Write Gender and skip a row. 
cat ("\n")
print(gender)                     # Write the values of the gender vector into the txt file. 
sink()                            # Close the diversion.

grep("Diabetes mellitus with complications",hospdata$CCS.Diagnosis.Description)

## Create a function to process the hospital data ----

create_hospdata<- function(input,output,value_labels="value_labels.txt") {
  
  hospdata <- read.csv(file=input,stringsAsFactors = F,header=T,sep=",")
  
  print(names(hospdata))
  
  # Creates a dummy variable for gender  
  hospdata$males<-0                                                                         
  hospdata$males<-ifelse(hospdata$Gender=="U",NA,hospdata$males)                            
  hospdata$males<-ifelse(hospdata$Gender=="M" & !is.na((hospdata$Gender)),1,hospdata$males) 
  
  ## Extract the levels of the variables
  
  # Assign the unique levels of the variable considering it as factor variable. 
  
  ny_counties<-levels(factor(hospdata$Hospital.County))       # Save the levels of the variable Hospital.County to ny_counties
  ny_areas<-levels(factor(hospdata$Health.Service.Area))
  ny_age<-levels(factor(hospdata$Age.Group))  
  ny_adm_types<-levels(factor(hospdata$Type.of.Admission))
  ny_races<-levels(factor(hospdata$Race))
  ny_ethnicities<-levels(factor(hospdata$Ethnicity))
  ny_diags<-levels(factor(hospdata$CCS.Diagnosis.Description))
  ny_procs<-levels(factor(hospdata$CCS.Procedure.Description))
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
  hospdata$ny_area<-as.numeric(as.factor(hospdata$Health.Service.Area))
  hospdata$cl_age<-as.numeric(as.factor(hospdata$Age.Group))
  hospdata$zipcode<-as.numeric(as.factor(hospdata$Zip.Code...3.digits))
  hospdata$adm_type<-as.numeric(as.factor(hospdata$Type.of.Admission))
  hospdata$race<-as.numeric(as.factor(hospdata$Race))
  hospdata$ethnicity<-as.numeric(as.factor(hospdata$Ethnicity))
  hospdata$los<-hospdata$Length.of.Stay                                                      # generates los variable in hospdata data.frame 
  hospdata$disposition<-as.numeric(as.factor(hospdata$Patient.Disposition))
  hospdata$diagnosis<-as.numeric(as.factor(hospdata$CCS.Diagnosis.Description))
  hospdata$procedure<-as.numeric(as.factor(hospdata$CCS.Procedure.Description))
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
  
  # Ami- Acute myocardial infarction, grep function search for matches. Output the index number of Acute myocardial infarction from the ny_diags stored before. 
  
  grep("Diabetes mellitus",ny_diags, ignore.case = FALSE)
  
  
  ny_diags[75]
  ny_diags[76]
  
  # Create a dummy variable for expired patients (dead). Disposition = 6  matches with expired.   
  
  grep("Expired", ny_dispositions)
  ny_dispositions[6]
  
  hospdata$dead<-0
  hospdata$dead<-ifelse(hospdata$disposition==6,1,hospdata$dead) 
  
  # Create a dummy variable for AMI. Note: the index for ami equal to 8 was checked before ( with grep("infarction",ny_diags)) from ny_diags variable. 
  
  hospdata$diabetes<-0
  hospdata$diabetes<-ifelse(hospdata$diagnosis==75|hospdata$diagnosis==75,1,hospdata$diabetes)
  
  # Remove comma from cost variable and different entries exept 0-9. digits.
  
  hospdata$cost<-as.numeric(gsub("[^0-9.]","",hospdata$cost))
  hospdata$los <-as.numeric(gsub("[^0-9.]","",hospdata$los))
  
  # Create a dataframe with the selected variables. Include all the rows and the specified selected columns.   
  
  ny_hospdata<-hospdata[,c("ny_hosp_id","ny_county","ny_area","cl_age",
                           "males","zipcode","adm_type","race","ethnicity","los",
                           "disposition","diagnosis","procedure","drg","mdc","severity",
                           "risk","payment_type","cost","surgical",
                           "dead","diabetes")]
  
  # Save the re-encoded and reduced data frame in .csv format in the working directory. Row names setted as false because first column does not contains the names of the observations.      
  
  ##write.csv(ny_hospdata,output,row.names=FALSE)
  
  write.csv(ny_hospdata,"ny_hospdata_2016.csv",row.names=FALSE)  # Manual save
  
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

create_hospdata(input="Hospital_Inpatient_Discharges__SPARCS_De-Identified___2016_20240229.csv",
                output="ny_hospdata_2016.csv", value_labels="value_labels.txt")


## Load and save the reduced dataset ----

# Load the reduced dataset.

ny_hospdata<-read.csv("ny_hospdata_2016.csv")


# Save to RDA file. The RDA format is less heavy and loads quicker. 

save(ny_hospdata,file="ny_hospdata_2016.Rda")


# From now on we will use only the RDA files. 

rm(ny_hospdata)
rm(hospdata)

load("ny_hospdata_2016.Rda")


## Initial analysis of hospital data ----

source("methods_lab_functions.R")

installLibraries(c("gmodels","epitools","Hmisc"))

library(gmodels)
library(epitools)
library(Hmisc)


#Subset data by selecting a subset of rows.

diadata<-ny_hospdata[ny_hospdata$diabetes==1,]         # select the rows with only  AMI cases, from the ami dummy variable
cordata<-ny_hospdata[ny_hospdata$diagnosis==69,]  # select only Coronary Atherosclerosis, from the diagnosis


# Example on how to create a meaningful clinical dummy variable for SURGICAL interventions.

diadata$ptca<-NA                                                                           # Create ptca variable assigning NA entries
diadata$ptca<-ifelse(diadata$procedure==185,1,diadata$ptca)                                # If procedure is equal to 185 assign 1
diadata$ptca<-ifelse(diadata$procedure!=185 & !is.na((diadata$procedure)),0,diadata$ptca)  # otherwise assign 0 (if procedure is not 185 and is not Na). 

diadata$procedure[185]
# Re-encode risk and severity of illness.

#ny_risks:  # Extreme=2, Major=3, Minor=4, Moderate=5 (the natural order: 0 -> Minor, 1 -> Moderate, 2 -> Major and 3 -> Extreme)

diadata$risky<-NA                
diadata$risky<-ifelse(diadata$risk=="4",0,diadata$risky) # <= the order is inverted
diadata$risky<-ifelse(diadata$risk=="5",1,diadata$risky)    
diadata$risky<-ifelse(diadata$risk=="3",2,diadata$risky)
diadata$risky<-ifelse(diadata$risk=="2",3,diadata$risky)


#ny_severities:  # Extreme=2, Major=3, Minor=4, Moderate=5 

diadata$severe<-NA
diadata$severe<-ifelse(diadata$severity=="4",0,diadata$severe)# <= the order is inverted
diadata$severe<-ifelse(diadata$severity=="5",1,diadata$severe)  
diadata$severe<-ifelse(diadata$severity=="3",2,diadata$severe)
diadata$severe<-ifelse(diadata$severity=="2",3,diadata$severe)


# Save the subsets

save(diadata,file="diadata_2017.Rda")
save(cordata,file="cordata_2017.Rda")


### Explore the Data  ----
# Explore the contents of the NY Dataset and interpret the source code below row by row. 
# Plan possible data analyses of interest based upon specific research questions check some descriptive stats

# Quick analysis

table(ny_hospdata$diabetes,ny_hospdata$dead)                # table with absolute frequencies (rows= ami) .  

prop.table(table(ny_hospdata$diabetes,ny_hospdata$dead))    # table with relative frequencies, total sum of cells = 1
prop.table(table(ny_hospdata$diabetes,ny_hospdata$dead),1)  # table with relative frequencies, total sum of rows =  1
prop.table(table(ny_hospdata$diabetes,ny_hospdata$dead),2)  # table with relative frequencies, total sum of columns = 1

# Round the entries to 2 decimal places

round(prop.table(table(ny_hospdata$diabetes,ny_hospdata$dead)),2)

# Consider each level of the severity of illness 

table(diadata$severe,diadata$dead)

round(prop.table(table(diadata$severe,diadata$dead),1),2)

# Handle missing data  

sum(ny_hospdata$males,na.rm=TRUE)             # Sum the counts of the cells, ignoring NA entries

Hmisc::describe(as.factor(ny_hospdata$males)) # Describe function from Hmisc package: turn a table with absolute and relative frequencies and including also the NA's and distinct entries.

# Split the data into subsets and return the result in a convenient form. FUN parameter computes summary statistics which can be applied to all data subsets.

m_dia<-aggregate(diadata$dead,by=list(diadata$ny_hosp_id),FUN="mean")         # Death rates (mean of dead variable) for each distinct hopital_id. Group by hospitals_id using as function the mean of death variable.
m_dia_nrow<-aggregate(diadata$dead,by=list(diadata$ny_hosp_id),FUN="NROW")    # Nr. of rows for each hospital_id.

names(m_dia)<-c("ny_hosp_id","cr")                   
names(m_dia_nrow)<-c("ny_hosp_id","n")               
m_dia<-merge(m_dia,m_dia_nrow,by=c("ny_hosp_id"))    # Merge data matching data frames by the id of the hospitals found in both data frames (similar to the keys in sql)
m_dia$cr<-m_dia$cr*100                               # Death rates*100
hosp_county<-aggregate(ny_county ~ ny_hosp_id,ny_hospdata,FUN="unique")   # For each unique hospital id returns the county number  
names(hosp_county)<-c("ny_hosp_id","ny_county")    


z_dia<-merge(m_dia,hosp_county,by=c("ny_hosp_id"),all.x=TRUE) # Merge the data by ny_hosp_id. It also includes rows from x with no matching, it assigns NA. 
z_dia$county_up<-ifelse(z_dia$ny_county>20,2,1)               

# Aggregate the deaths by ny areas

m_dia_area<-aggregate(diadata$dead,by=list(diadata$ny_area),FUN="mean")      # Death rates, grouped by NY area 
names(m_dia_area)<-c("ny_area","mortality")                                 
m_dia_area$mortality<-round(m_dia_area$mortality*1000,2)                     # Death rates (x1000)


### Bivariate analysis ----

#### Categorical variables ---- 

# Conduct a chi-square test of independence (Without the Yates' continuity correction)

chisq.test(diadata$males,diadata$dead, correct=FALSE)

options(scipen=999)                           # Prevent scientific e notation by using a large value like 999. 

# Frequencies and chi-square test of independence (gmodels package)

CrossTable(diadata$males,diadata$dead)  
CrossTable(diadata$males,diadata$dead,format="SPSS") 
CrossTable(diadata$males,diadata$dead,chisq=TRUE) 

# Odds and risk ratio and test of independence (epitools package)

oddsratio(diadata$males,diadata$dead)   
oddsratio(diadata$males,diadata$dead, method = "wald")  
riskratio(diadata$males,diadata$dead)  

#Odds ratios from a model logistic regression model 

logit_model<-glm(dead~males,family = binomial("logit"),data=diadata)

# Results from basic command of R

summary(logit_model)

#Show selected results: Coefficients, C.I. of the coefficients, Exponential(Coefficiets)=Odd Ratios, C.I. of the OR and P-value of wald test 

round(cbind(Beta=coef(logit_model),confint(logit_model),P=coef(summary(logit_model))[,4],exp(cbind(OR=coef(logit_model),confint(logit_model)))),4)

#### Continuous variables----

# Conduct an independent sample t-test 

t.test(diadata$cost[diadata$dead==1], diadata$cost[diadata$dead==0])

# Explore the relation of continuous variables

plot (diadata$los, diadata$cost, xlab="LOS", ylab="COST", main = "Length of stay  - Cost ")

# Plot a regression line

abline(lm(cost~los, data=diadata),col='red')

# A Pearson Correlation Coefficient to quantify the linear relationship

cor(diadata$cost, diadata$los)

# Simple regression

linear_model<- glm(cost~ los,family = gaussian(link = identity),data=diadata)

summary(linear_model)
