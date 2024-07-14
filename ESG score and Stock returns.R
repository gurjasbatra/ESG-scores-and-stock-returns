# Loading libraries -------------------------------------------------------
library(readxl)
library(dplyr)
library(pheatmap)
library(lmtest)
library(sandwich)
library(car)
library(broom)
library(knitr)
library(kableExtra)

# Loading data ------------------------------------------------------------
ESG_3000 <- read_excel("D:/McMaster/Coursework/FINANCE 701 - Financial Econometrics I - Dr John Maheu/Project/ESG 3000.xlsx")
summary(ESG_3000)

# Correcting datatypes ----------------------------------------------------
columns_to_numeric <- c(
  "ESG_SCORE", "ENVIRONMENTAL_SCORE", "GOVERNANCE_SCORE",
  "SOCIAL_SCORE", "CUR_MKT_CAP", "PROF_MARGIN",
  "EBITDA_TO_REVENUE", "SALES_REV_TURN", "CURRENT_TRR_1YR", "PE", "DE"
)
ESG_3000 <- mutate_at(ESG_3000, vars(columns_to_numeric), as.numeric)

# Dealing with NAs --------------------------------------------------------
colSums(is.na(ESG_3000))

#Since PE values can be negative, we'll replace them with 0 where they are.
ESG_3000$PE[is.na(ESG_3000$PE)] <- 0

# Subset the dataset to include only complete rows
ESG_3000 <- ESG_3000[complete.cases(ESG_3000), ]
colSums(is.na(ESG_3000))

#Now, all NA values have been removed, and we are left with 2088 out of the original 3000-odd observations
summary(ESG_3000)

# EDA ---------------------------------------------------------------------
#Histograms of the ESG scores
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
hist(ESG_3000$ESG_SCORE, main = "ESG Score")
hist(ESG_3000$ENVIRONMENTAL_SCORE, main = "Environmental Score")
hist(ESG_3000$GOVERNANCE_SCORE, main = "Governance Score")
hist(ESG_3000$SOCIAL_SCORE, main = "Social Score")

sum(ESG_3000$ENVIRONMENTAL_SCORE == 0)
#560 observations where this is zero.
#This might be a problem

sum(ESG_3000$SOCIAL_SCORE == 0)
#Only 32 here

summary(ESG_3000$CUR_MKT_CAP)
#Between 34 mn and 3.15 tr
plot(ESG_3000$ESG_SCORE, ESG_3000$CUR_MKT_CAP)
#Clearly market cap has a much bigger scale than the other variables
#Let's scale it down. Let's also do the same thing to Sales/Revenue turnover
par(mfrow = c(1, 2))
hist(ESG_3000$CUR_MKT_CAP)
hist(ESG_3000$SALES_REV_TURN)

ESG_3000$mcap_ln <- log(ESG_3000$CUR_MKT_CAP)
ESG_3000$sales_ln <- log(ESG_3000$SALES_REV_TURN)

hist(ESG_3000$mcap_ln)
hist(ESG_3000$sales_ln)

par(mfrow = c(1, 1))
plot(ESG_3000$ESG_SCORE,ESG_3000$mcap_ln)
plot(ESG_3000$ESG_SCORE,ESG_3000$sales_ln)
#There is a somewhat positive-linear pattern between the two
cor(ESG_3000$ESG_SCORE,ESG_3000$mcap_ln)
cor(ESG_3000$ESG_SCORE,ESG_3000$sales_ln)
#Bigger the company, higher the ESG score

plot(ESG_3000$ESG_SCORE,ESG_3000$PROF_MARGIN)
summary(ESG_3000$PROF_MARGIN)
hist(ESG_3000$PROF_MARGIN)
#The minimum profit margin is -344489.5. Seems to be an outlier. Let's winsorize this.
ESG_3000$pm <- ESG_3000$PROF_MARGIN
ESG_3000$pm[ESG_3000$pm < quantile(ESG_3000$pm,0.05)] <- quantile(ESG_3000$pm,0.05)

ESG_3000$pm_ln <- custom_log(ESG_3000$pm)

hist(ESG_3000$pm_ln)     
summary(ESG_3000$pm)
#Seems fine now

plot(ESG_3000$ESG_SCORE,ESG_3000$pm)
cor(ESG_3000$ESG_SCORE,ESG_3000$pm)
#28% correlation. Only slightly positive

custom_log <- function(x) {
  ifelse(x >= 0.000000001, log(x), 0)
}

hist(ESG_3000$PE)
ESG_3000$pe_ln <- custom_log(ESG_3000$PE)
hist(ESG_3000$pe_ln)

hist(ESG_3000$DE)
ESG_3000$de_ln <- custom_log(ESG_3000$DE)
hist(ESG_3000$de_ln)

summary(ESG_3000)

#Now that all variables have been scaled, winsorized and engineered appropriately,


# Checking for multicollinearity ------------------------------------------

#we will make a correlation matrix to see if we need to engineer the features

data <- ESG_3000[,c("ESG_SCORE", "ENVIRONMENTAL_SCORE", "GOVERNANCE_SCORE",
                    "SOCIAL_SCORE", "mcap_ln", "sales_ln", 
                    "pm_ln","pe_ln", "de_ln","CURRENT_TRR_1YR")]
cor_mat <- cor(data)
pheatmap(cor(data), 
         main = "Correlation Matrix Heatmap",
         cluster_rows = FALSE,  # Do not cluster rows
         cluster_cols = FALSE)

write.csv(cor_mat, file = "cor.csv", row.names = FALSE)
#mcap and sales are highly correlated. Let's remove sales as a feature.
#ESG_SCORE is correlated to the individual category scores. So, we can't use them together.

data <- select(data, -"sales_ln")

# Model definition --------------------------------------------------------
m1 <- lm(CURRENT_TRR_1YR ~ . - ESG_SCORE, data=data)
summary(m1)
m2 <- lm(CURRENT_TRR_1YR ~ . - ESG_SCORE - pm_ln - de_ln - GOVERNANCE_SCORE, data=data)
summary(m2)

# Testing for and fixing heteroskedasticity and autocorrelation -----------
bptest(m2)
#p-value = 0.0007. Heteroskedasticity is present
bgtest(m2)
#p-value = 0.02. There is possibility of autocorrelation
#But our data is cross-sectional, so this is not that concerning

#Let us fix heteroskedasticity using robust standard errors
robust_se <- coeftest(m2, vcov = vcovHC(m2))
print(robust_se)

# Testing for significance of ESG scores ----------------------------------
#Running F-test to see whether any of the ESG scores are significant or not
linearHypothesis(m1, c("ENVIRONMENTAL_SCORE=0", "SOCIAL_SCORE=0"),
                 white.adjust = TRUE)
#p-value 9.642e-07. Can reject null that all are 0. 

linearHypothesis(m1, c("GOVERNANCE_SCORE=0"),
                 white.adjust = TRUE)
#p-value 0.08507. Can reject null that all are 0. 
