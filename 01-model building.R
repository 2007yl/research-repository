rm(list=ls())
install.packages(c("GGally", "reshape2", "lme4", "compiler", "parallel", "boot", "lattice"))
require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(lattice)
require(forcats) #set reference level
require(gtsummary)
require(broom.mixed)
require(flexplot)
require(lmtest)
require(ResourceSelection)
require(Deducer)
require(car)
library(visdat)
library(easystats)
#load data
data2 <- readRDS("data2.rds")

#visualize missing data
data1 |>
  select(bmi, bmigroup1, race, asaclass, flg_cmb_diabetes, insurance,volume, hct, hb_mis, 
         bleeding_barrier, surgery_time, uterine, cmp_anymajor) |>
       visdat::vis_miss()

###Fit a multiple logistic regression model using the variables selected###
l1=glm(cmp_anymajor1 ~ race + bmigroup1 + asaclass +  insurance +flg_cmb_diabetes+
           volume + bleeding_barrier + hct + hb_mis + surgery_time+
           uterine, data=data2, family=binomial)
###Fit a mixed effects logistic regression
l2=glmer(cmp_anymajor1 ~ race + bmigroup1 + asaclass + insurance +flg_cmb_diabetes+
           bleeding_barrier + hct + hb_mis+uterine + surgery_time +
           (1 | physician_cid), data=data2, family=binomial, control = glmerControl(optimizer = "bobyqa"),
         nAGQ = 10)
performance::model_performance(l1) #aic 3005 bic 3130
performance::model_performance(l2) #aic3004 bic 3130
performance::check_model(l1)
performance::check_model(l2)
#delta.coef hb_mis and uterine >20%, add back?
#Likelihood ratio test
lrtest(full, reduced) #p=0.745, no difference, keep reduced model.
model.comparison(full, reduced) #better fitting model: lower aic, lower bic,
# Bayes factors larger than around 10, p<0.05;all favors reduced model
#https://quantpsych.net/stats_modeling/model-comparisons.html
#remove volume (p=0.66)
reduced1=glm(cmp_anymajor ~ race + bmigroup1 + asaclass + insurance +
              bleeding_barrier + prefer + ebl + hct + hb_mis+surgery_time +uterine,
             data=data1, family=binomial)
summary(reduced1)
delta.coef=abs((coef(reduced1)-coef(reduced)[-5])/coef(reduced)[-5])
round(delta.coef, 3)
lrtest(reduced, reduced1) #p=0.85

#check for multicollinearity 
car::vif(reduced1) #value greater than 5 indicates potentially severe correlation
#interaction
inter=glm(cmp_anymajor ~ race + bmigroup1 + asaclass + insurance +
               bleeding_barrier + prefer + ebl + hct + hb_mis+uterine + hb_mis*ebl,
             data=data1, family=binomial)
summary(inter)
lrtest(inter, reduced1) #p=0.04, keep interaction?
compare.fits(cmp_anymajor ~ surgery_time | race, data = data1, inter, reduced1 )
model.comparison(inter, reduced1) #better fitting model: lower aic, lower bic,
# Bayes factors larger than around 10, p<0.05
#overall, i decided to drop interaction.
#goodness of fit
hoslem.test(reduced1$y, fitted(reduced1)) # p=0.56, no difference between observed and predicted values. Model fits.
#random effects
l1=glmer(cmp_anymajor ~ race + bmigroup1 + asaclass + insurance +
            bleeding_barrier + prefer + ebl + hct + hb_mis+uterine + 
              (1 | physician_cid),
          data=data1, family=binomial)
lrtest(reduced1, l1) #p=0.005, keep random effect
#Table
tbl_regression(l1, exponentiate=TRUE)
#Black patients had increased odds of major postoperative complications after controlling
# for bmigroup1 + asaclass + insurance +bleeding_barrier + prefer + ebl + hct + hb_mis+uterine +
#  (1 | physician_cid)
#OR 1.38, 95% CI 1.16-1.65, p<0.001
#Methods: Generalized linear mixed model fit by maximum likelihood 
#The end

#05-16 notes:
#Neil: interaction race:hb_mis, race:uterine
#Neil:how group of factors(patient level, hospital level, surgeon level) influenced the outcome
#Chris paper https://pubmed.ncbi.nlm.nih.gov/35580633/ supported the proposed hypothesis
#(racial disparity and intraoperative characteristics contributed to surgical outcomes)
#however, intrawarming, test and imaging documentation, preferred antibiotics didn't explain
#surgical outcome difference in Black and White.
#take-home message: 
#Factors related to intraoperative produced models with the strongest predictive ability. 

###Notes
#Why mixed model? data structure: multiple patients within surgeons, which violates 
#assumption of independence.
#Model: Generalized linear mixed model fit by maximum likelihood 
#outcome: cmp_anymajor
#fixed effects: race + bmigroup1 + asaclass + e_teaching + insurance  + 
#prefer + uterine + hct + hb_mis + surgery_time + bleeding_barrier
#random effect: surgeon

#base R method
m <- glmer(cmp_anymajor ~ race + bmigroup1 + asaclass + e_teaching + insurance  + 
             prefer + uterine + hct + hb_mis + surgery_time + bleeding_barrier +
             (1 | physician_cid), data = data1, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

# print the mod results without correlations among fixed effects
print(m, corr = FALSE)
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                se))
exp(tab)
#output
#Race, Black vs White, aOR 1.33, 95%CI 1.11-1.59, p=0.002, adjusting for bmigroup1 + asaclass + e_teaching + insurance  + 
# prefer + uterine + hct + hb_mis + surgery_time + bleeding_barrier + surgeon
lattice::dotplot(ranef(m, which = "physician_cid", condVar = TRUE), scales = list(y = list(alternating = 0)))                                

#random effects: surgeon + site (code only, site data unavailable)
m1 <- glmer(cmp_anymajor ~ race + bmigroup1 + asaclass + e_teaching + insurance  + 
             prefer + uterine + hct + hb_mis + surgery_time + bleeding_barrier +
             (1 | physician_cid) + (1 | site), data = data1, family = binomial,
           nAGQ = 1)


#gtsummary method
lme4::glmer(cmp_anymajor ~ race + bmigroup1 + asaclass + e_teaching + insurance  + 
        prefer + uterine + hct + hb_mis + surgery_time + bleeding_barrier +
        (1 | physician_cid), data = data1, family = binomial,control = glmerControl(optimizer = "bobyqa"),
        nAGQ = 10)|>
  tbl_regression(exponentiate = TRUE,
# set the tidying function to broom.mixed::tidy to show random effects
    tidy_fun = broom.mixed::tidy)

#step 1 -minimize effects of potential correlations between patient variables, such as 
#asaclass/hct
#-minimize effects of potential correlations between hospital characteristics and patients 
#variables: surgeon/surgery_time, uterine/hb_mis, bmi/hb_mis, insurance/hb_mis
#Goodnessof-fit was assessed using the HosmerLemeshow test and the area under the
#receiver operating characteristic (ROC) curve.The methods of Belsely et al
# were used to check for collinearity among the independent variables. These were
# used to prevent the exclusion of significant variables from the model due to collinearity
#References
#https://stats.oarc.ucla.edu/other/mult-pkg/introduction-to-generalized-linear-mixed-models/
#https://jamanetwork-com.proxy.lib.umich.edu/journals/jama/fullarticle/408728
#https://journals.sagepub.com/doi/10.1177/1745691620917333

#codes didn't work
par(mfrow=c(2,2))
scatter.smooth(data1$hct_no, log(full$pr/(1-full$pr)), cex=0.5) 
predprob=predict(inter, data1, type="response")
plot(predprob, jitter(as.numeric(data1$cmp_anymajor),0.5), cex=0.5) 
rocplot(inter) 
InformationValue::plotROC(data1$cmp_anymajor, predprob)                
icc(mymodel)



full = glmer(cmp_anymajor1 ~ surgery_time + race + bmigroup1 + asaclass  + insurance  + 
               prefer + uterine + hct + hb_mis + bleeding_barrier +
               (1 | physician_cid), data = data1, family = binomial, control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 10)
reduced = glm(cmp_anymajor ~ surgery_time + race + bmigroup1 + asaclass + e_teaching + insurance  + 
                  prefer + uterine + hct + hb_mis + bleeding_barrier, data = data1, family = binomial)
compare.fits(cmp_anymajor ~ surgery_time | race, data = data1, full, reduced )


