# Variability of length of stay for Diabetic patients in the year 2019 and 2021 in the NY Dataset of hospital inpatient discharges (SPARCS)

## This repository is related to the Final Project of the Methods and Tools for Health Statistics course. 2-year master's degree in Statistical Sciences 


This project was made in collaboration with Niccolò Caselli, José Escarramán, and Cristian Perrone 

# ABSTRACT
INTRODUCTION: Diabetes is a condition characterized by high blood sugar levels resulting from the body's inability to
produce or effectively use insulin. The pancreas plays a crucial role in diabetes as it produces insulin, a hormone that
helps regulate blood sugar levels. We aim to describe the main factors that can affect excessive in-hospital length of
stay of diabetic patients by focusing on differences among counties and social and clinical aspects of the patients. We
also analyzed how it changed between 2019 and 2021, before and after the outbreak of the Covid-19 pandemic.
(Miao et al. 2022), (Katipoglu et al. 2022)
# AIMS:
● What are the specific risk factors contributing to prolonged hospitalization among diabetic patients?
● Is there excessive geographical variation of high length of stay between hospitals in the State of New York?
● How did the COVID-19 pandemic affect hospitalization of diabetic patients?
#  METHODS: 
The length of stay variable (LOS) was dichotomized by using the upper quintile of 2019 distribution as
cutoff, 1 for a LOS greater than 8 days, 0 otherwise, obtaining LOS8. Using the NY hospital inpatient discharge
datasets from 2019, we conducted the analysis using Generalised Linear Models (GLMs) and Generalised Estimating
Equation Models (GEEs) to identify potential risk factors associated with excessive length of stay. The likelihood ratio
test and accuracy metrics were employed to compare models obtained through backward elimination. Direct and
indirect standardisation methods were utilized to assess the hospitals’ performances, and comparisons were made
among hospitals in various areas. A funnel plot was used to focus on outlying observations. To assess if COVID-19
affected hospitalisation, we also considered the 2021 dataset, and in the model, we adjusted for the year difference
between 2019 and 2021 as well.

# CONCLUSIONS: 
The analysis identified significant predictors for prolonged hospital stays among diabetic patients,
emphasising the importance of considering age, condition severity, and other factors in patient management. When
comparing 2021 to 2019, there also seems to be an effect hinting at a correlation between Covid-19 and the length of
stay of diabetic patients. Geographical analysis revealed disparities in hospital stays across New York counties.Future
research should explore pandemic-related disruptions, including telemedicine's role, in improving diabetic patient
care.
