##### NEW ANALYSIS FOR THE LANGUAGE SWITCHING TASK. SEPTEMBER 2017 ##############################

##### ## IS ## ##### 

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

isrt <- allrt[allrt$group == 'IS', ]



########## separate mix and switch cost   ########## ########## ########## ########## 

ismixrt <- isrt[isrt$type != 'switch', ]
isswrt <- isrt[isrt$type != 'blocked', ]






########## regress confounds ########## ########## ########## ########## ########## ########## 

## mix
mix_confounds <- lm(RT ~ scale(age) + scale(corr_edu) + ordervar, data = ismixrt)
summary(mix_confounds)
resmixrt <- mix_confounds$residuals

## switch
sw_confounds <- lm(RT ~ scale(age) + scale(corr_edu) + ordervar, data = isswrt)
summary(sw_confounds)
resswrt <- sw_confounds$residuals



########## random structure ########## ########## ########## ########## 

# (1+type|subject)+(1+language|subject)+(1+language|picture)




########## MIX  ######### ########## ########## ########## ########## ########## ########## 

is.mix.null <- lmer(resmixrt ~ (1+type|subject)+(1+language|subject)+(1+language|picture),
                    data = ismixrt,
                    control = lmerControl(optCtrl=list(maxfun=6e4)))

is.mix.type <- lmer(resmixrt ~ type
                    + (1+type|subject)+(1+language|subject)+(1+language|picture),
                    data = ismixrt,
                    control = lmerControl(optCtrl=list(maxfun=6e4)))

is.mix.typeelang <- lmer(resmixrt ~ type + language
                         + (1+type|subject)+(1+language|subject)+(1+language|picture),
                         data = ismixrt,
                         control = lmerControl(optCtrl=list(maxfun=6e4)))

is.mix.typelang <- lmer(resmixrt ~ type * language
                        + (1+type|subject)+(1+language|subject)+(1+language|picture),
                        data = ismixrt,
                        control = lmerControl(optCtrl=list(maxfun=6e4)))

anova(is.mix.null, is.mix.type, test = 'Chisq')
anova(is.mix.type, is.mix.typeelang, test = 'Chisq')
anova(is.mix.typeelang, is.mix.typelang, test = 'Chisq')
summary(is.mix.typelang)



########## SWITCH  ######### ########## ########## ########## ########## ########## ########## 


is.switch.null <- lmer(resswrt ~ (1+type|subject)+(1+language|subject)+(1+language|picture),
                       data = isswrt,
                       control = lmerControl(optimizer = "bobyqa"))
ss <- getME(is.switch.null,c("theta","fixef"))
is.switch.null <- update(is.switch.null, start=ss,control=lmerControl(optimizer = "bobyqa"))
# doesnt work

is.switch.type <- lmer(resswrt ~ type
                       +(1+type|subject)+(1+language|subject)+(1+language|picture),
                       data = isswrt,
                       control = lmerControl(optCtrl=list(maxfun=6e4)))

#is.switch.lang <- lmer(resswrt ~ language
 #                      +(1+type|subject)+(1+language|subject)+(1+language|picture),
  #                     data = isswrt,
   #                    control = lmerControl(optCtrl=list(maxfun=10e4)))
#ss <- getME(is.switch.lang,c("theta","fixef"))
#is.switch.lang <- update(is.switch.lang, start=ss,control=lmerControl(optimizer = "bobyqa"))


is.switch.typeelang <- lmer(resswrt ~ type + language
                            +(1+type|subject)+(1+language|subject)+(1+language|picture),
                            data = isswrt,
                            control = lmerControl(optCtrl=list(maxfun=6e4)))

is.switch.typelang <- lmer(resswrt ~ type * language
                           +(1+type|subject)+(1+language|subject)+(1+language|picture),
                           data = isswrt,
                           control = lmerControl(optCtrl=list(maxfun=10e4)))
ss <- getME(is.switch.typelang,c("theta","fixef"))
is.switch.typelang <- update(is.switch.typelang, start=ss,control=lmerControl(optimizer = "bobyqa"))


#anova(is.switch.null, is.switch.type, test = 'Chisq')
anova(is.switch.type, is.switch.typeelang, test = 'Chisq')
anova(is.switch.typeelang, is.switch.typelang, test = 'Chisq')
summary(is.switch.typelang)


sink("mix cost in IS.txt") # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
print(summary(is.mix.typelang))
sink() 

sink("switch cost in IS.txt") # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
print(summary(is.switch.typelang))
sink() 