##### NEW ANALYSIS FOR THE LANGUAGE SWITCHING TASK. SEPTEMBER 2017 ##############################

#### --> COVARIATES ONLY, NO GROUP <--- ####


require(lme4)
require(lmerTest)
require(ggplot2)
require(multcomp)
require(afex)
library(aod)
library(Rcpp)
library(effects)
library(lattice)
library(lmerTest)
library(MASS)



########## read the data ########## ########## ########## ########## ########## ########## 

allrt<- read.csv('LSw_data_all_var_order.csv')
allcost<- read.csv('LSw_costs_all_var_order.csv')



########## define order of task as categorical ########## ########## ########## ########## 

allrt$ordervar <- factor(allrt$ordervar)
allcost$ordervar <- factor(allcost$ordervar)



########## define active and passive proficiency ########## ########## ########## ########## 


allrt$profac <- rowMeans(subset(allrt, select = c(L2_oral_compr, L2_writ_compr)))
allrt$profpas <- rowMeans(subset(allrt, select = c(L2_oral_prod, L2_writ_prod)))



########## basic model ########## ########## ########## ########## ########## 

rt_confounds <- lm(RT ~ scale(age) + scale(corr_edu) + ordervar, data = allrt)
summary(rt_confounds)
resrt <- rt_confounds$residuals

rtf <- lmer(resrt ~ type * language 
            + (1+type|subject)+(1+language|subject)+(1+language|picture),
            data = allrt,
            control = lmerControl(optCtrl=list(maxfun=6e4)))
summary(rtf)





########## a methodic approach ########## ########## ########## ########## ########## 


rt_c0 <- lmer(resrt ~ type + language 
              + type:language
              
              + (1+type|subject)+(1+language|subject)+(1+language|picture),
              data = allrt,
              control = lmerControl(optCtrl=list(maxfun=6e4)))

rt_c1 <- lmer(resrt ~ type + language 
              + type:language
              
              + scale(profac)
              
              + (1+type|subject)+(1+language|subject)+(1+language|picture),
              data = allrt,
              control = lmerControl(optCtrl=list(maxfun=6e4)))

anova(rt_c0, rt_c1, test = 'Chisq') # prof as main effect is ns

rt_c2 <- lmer(resrt ~ type + language 
              + type:language
              
              + scale(profac)
              + scale(profac):type
              
              + (1+type|subject)+(1+language|subject)+(1+language|picture),
              data = allrt,
              control = lmerControl(optCtrl=list(maxfun=6e4)))

anova(rt_c1, rt_c2, test = 'Chisq') # prof ac interacts with type
summary(rt_c2)


rt_c3 <- lmer(resrt ~ type + language 
              + type:language
              
              + scale(profac)
              + scale(profac):type
              
              + scale(profpas)
              
              + (1+type|subject)+(1+language|subject)+(1+language|picture),
              data = allrt,
              control = lmerControl(optCtrl=list(maxfun=6e4)))

anova(rt_c2, rt_c3, test = 'Chisq') # prof pass is not significant

rt_c4 <- lmer(resrt ~ type + language 
              + type:language
              
              + scale(profac)
              + scale(profac):type 
              
              + scale(profpas)
              + scale(profpas):type:language
              
              + (1+type|subject)+(1+language|subject)+(1+language|picture),
              data = allrt,
              control = lmerControl(optCtrl=list(maxfun=6e4)))

ss <- getME(rt_c4,c("theta","fixef"))
rt_c4 <- update(rt_c4, start=ss,control=lmerControl(optimizer = "bobyqa"))

anova(rt_c2, rt_c4, test = 'Chisq') # trying interactions with prof pass: 

rt_c5 <- lmer(resrt ~ type + language 
              + type:language
              
              + scale(profac)
              + scale(profac):type 
              
              + scale(L2_begin_learn)
              
              + (1+type|subject)+(1+language|subject)+(1+language|picture),
              data = allrt,
              control = lmerControl(optCtrl=list(maxfun=6e4)))

anova(rt_c2, rt_c5, test = 'Chisq') # AoA is sgnft
summary(rt_c5)


rt_c6 <- lmer(resrt ~ type + language 
              + type:language
              
              + scale(profac)
              + scale(profac):type 
              
              + scale(L2_begin_learn)
              + scale(L2_begin_learn): language
              
              + (1+type|subject)+(1+language|subject)+(1+language|picture),
              data = allrt,
              control = lmerControl(optCtrl=list(maxfun=6e4)))

anova(rt_c5, rt_c6, test = 'Chisq') # AoA interacts with language
summary(rt_c6)


rt_c7 <- lmer(resrt ~ type + language 
              + type:language
              
              + scale(profac)
              + scale(profac):type 
              
              + scale(L2_begin_learn)
              + scale(L2_begin_learn): language
              + scale(L2_begin_learn): language: type
              
              + (1+type|subject)+(1+language|subject)+(1+language|picture),
              data = allrt,
              control = lmerControl(optimizer = "bobyqa"))
anova(rt_c6, rt_c7, test = 'Chisq') # AoA interacts with language but not type of trial

rt_c8 <- lmer(resrt ~ type + language 
              + type:language
              
              + scale(profac)
              + scale(profac):type 
              
              + scale(L2_begin_learn)
              + scale(L2_begin_learn): language
              
              + scale(L2_begin_fluent)

              + (1+type|subject)+(1+language|subject)+(1+language|picture),
              data = allrt,
              control = lmerControl(optimizer = "bobyqa"))

ss <- getME(rt_c8,c("theta","fixef"))
rt_c8 <- update(rt_c8, start=ss,control=lmerControl(optimizer = "bobyqa"))

anova(rt_c6, rt_c8, test = 'Chisq') # AoA of acquired fluency doesnt do anything

rt_c9 <- lmer(resrt ~ type + language 
              + type:language
              
              + scale(profac)
              + scale(profac):type 
              
              + scale(L2_begin_learn)
              + scale(L2_begin_learn): language
              
              + scale(L2_begin_fluent)
              + scale(L2_begin_fluent) : language : type
              
              + (1+type|subject)+(1+language|subject)+(1+language|picture),
              data = allrt,
              control = lmerControl(optimizer = "bobyqa"))

anova(rt_c6, rt_c9, test = 'Chisq') # AoA fluency doesnt interact either


rt_c10 <- lmer(resrt ~ type + language 
              + type:language
              
              + scale(profac)
              + scale(profac):type 
              
              + scale(L2_begin_learn)
              + scale(L2_begin_learn): language
              
              + scale(L2_mean_exp)

              + (1+type|subject)+(1+language|subject)+(1+language|picture),
              data = allrt,
              control = lmerControl(optimizer = "bobyqa"))

anova(rt_c6, rt_c10, test = 'Chisq') # Exposure doesnt do anything
summary(rt_c10)

rt_c11m <- lmer(resrt ~ type + language 
               + type:language
               
               + scale(profac)
               + scale(profac):type 
               
               + scale(L2_begin_learn)
               + scale(L2_begin_learn):language
               
               + scale(L2_mean_exp)
               + scale(L2_mean_exp):type
               
               + (1+type|subject)+(1+language|subject)+(1+language|picture),
               data = allrt,
               control = lmerControl(optimizer = "bobyqa"))

ss <- getME(rt_c11m,c("theta","fixef"))
rt_c11m <- update(rt_c11m, start=ss,control=lmerControl(optimizer = "bobyqa"))
anova(rt_c10, rt_c11m, test = 'Chisq') # exposure marginally interacts with type



rt_c11 <- lmer(resrt ~ type + language 
               + type:language
               
               + scale(profac)
               + scale(profac):type 
               
               + scale(L2_begin_learn)
               + scale(L2_begin_learn):language
               
               + scale(L2_mean_exp)
               + scale(L2_mean_exp):type:language
               
               + (1+type|subject)+(1+language|subject)+(1+language|picture),
               data = allrt,
               control = lmerControl(optimizer = "bobyqa"))

ss <- getME(rt_c11,c("theta","fixef"))
rt_c11 <- update(rt_c11, start=ss,control=lmerControl(optimizer = "bobyqa"))

anova(rt_c11m, rt_c11, test = 'Chisq') #  exposure interacts with type and language

sink("all RT covariates final SPLIT.txt") # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
print(summary(rt_c11)) # <------ FINAL MODEL  # # # # # # # # # # # # # # # # # # # 
sink() 


rt_c12 <- lmer(resrt ~ type + language 
               + type:language
               
               + scale(profac)
               + scale(profac):type 
               
               + scale(L2_begin_learn)
               + scale(L2_begin_learn): language
               
               + scale(L2_mean_exp)
               + scale(L2_mean_exp): type:language
               
               + scale(freq_switch) # tried on its own it is ns
               + scale(freq_switch) : type : language
               
               + (1+type|subject)+(1+language|subject)+(1+language|picture),
               data = allrt,
               control = lmerControl(optimizer = "bobyqa"))

anova(rt_c11, rt_c12, test = 'Chisq') # frequency doesnt interact either




