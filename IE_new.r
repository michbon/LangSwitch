##### NEW ANALYSIS FOR THE LANGUAGE SWITCHING TASK. SEPTEMBER 2017 ##############################

##### ## IE ## ##### 

require(lme4)
require(lmerTest)
require(ggplot2)
require(multcomp)
require(afex)
library(aod)
library(Rcpp)
library(effects)
library(lattice)



########## read the data ########## ########## ########## ########## ########## ########## 

allrt<- read.csv('LSw_data_all_var_order.csv')



########## define order of task as categorical ########## ########## ########## ########## 

allrt$ordervar <- factor(allrt$ordervar)



########## take IE  ########## ########## ########## ########## ########## ########## 

iert <- allrt[allrt$group == 'IE', ]



########## separate mix and switch cost   ########## ########## ########## ########## 

iemixrt <- iert[iert$type != 'switch', ]
ieswrt <- iert[iert$type != 'blocked', ]






########## regress confounds ########## ########## ########## ########## ########## ########## 

## mix
mix_confounds <- lm(RT ~ scale(age) + scale(corr_edu) + ordervar, data = iemixrt)
summary(mix_confounds)
resmixrt <- mix_confounds$residuals

## switch
sw_confounds <- lm(RT ~ scale(age) + scale(corr_edu) + ordervar, data = ieswrt)
summary(sw_confounds)
resswrt <- sw_confounds$residuals



########## random structure ########## ########## ########## ########## 

# (1+type|subject)+(1+language|subject)+(1+language|picture)




########## MIX  ######### ########## ########## ########## ########## ########## ########## 

ie.mix.null <- lmer(resmixrt ~ (1+type|subject)+(1+language|subject)+(1+language|picture),
                    data = iemixrt,
                    control = lmerControl(optCtrl=list(maxfun=6e4)))

ie.mix.type <- lmer(resmixrt ~ type
                     + (1+type|subject)+(1+language|subject)+(1+language|picture),
                    data = iemixrt,
                    control = lmerControl(optCtrl=list(maxfun=6e4)))

ie.mix.typeelang <- lmer(resmixrt ~ type + language
                    + (1+type|subject)+(1+language|subject)+(1+language|picture),
                    data = iemixrt,
                    control = lmerControl(optCtrl=list(maxfun=6e4)))

ie.mix.typelang <- lmer(resmixrt ~ type * language
                         + (1+type|subject)+(1+language|subject)+(1+language|picture),
                         data = iemixrt,
                         control = lmerControl(optCtrl=list(maxfun=6e4)))
ss <- getME(ie.mix.typelang,c("theta","fixef"))
ie.mix.typelang <- update(ie.mix.typelang, start=ss,control=lmerControl(optimizer = "bobyqa"))


anova(ie.mix.null, ie.mix.type, test = 'Chisq')
anova(ie.mix.type, ie.mix.typeelang, test = 'Chisq')
anova(ie.mix.typeelang, ie.mix.typelang, test = 'Chisq')
summary(ie.mix.typelang)



########## SWITCH  ######### ########## ########## ########## ########## ########## ########## 


ie.switch.null <- lmer(resswrt ~ (1+type|subject)+(1+language|subject)+(1+language|picture),
                    data = ieswrt,
                    control = lmerControl(optCtrl=list(maxfun=6e4)))

ie.switch.type <- lmer(resswrt ~ type
                       +(1+type|subject)+(1+language|subject)+(1+language|picture),
                       data = ieswrt,
                       control = lmerControl(optCtrl=list(maxfun=6e4)))
ss <- getME(ie.switch.type, c("theta","fixef"))
ie.switch.type <- update(ie.switch.type, start=ss,control=lmerControl(optimizer = "bobyqa"))


ie.switch.typeelang <- lmer(resswrt ~ type + language
                       +(1+type|subject)+(1+language|subject)+(1+language|picture),
                       data = ieswrt,
                       control = lmerControl(optCtrl=list(maxfun=6e4)))

ie.switch.typelang <- lmer(resswrt ~ type * language
                            +(1+type|subject)+(1+language|subject)+(1+language|picture),
                            data = ieswrt,
                            control = lmerControl(optCtrl=list(maxfun=6e4)))

anova(ie.switch.null, ie.switch.type, test = 'Chisq')
anova(ie.switch.type, ie.switch.typeelang, test = 'Chisq')
anova(ie.switch.typeelang, ie.switch.typelang, test = 'Chisq')
summary(ie.switch.typelang)


sink("mix cost in IE.txt") # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
print(summary(ie.mix.typelang))
sink() 

sink("switch cost in IE.txt") # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
print(summary(ie.switch.typelang))
sink() 