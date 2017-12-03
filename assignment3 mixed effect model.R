##################################
#                                #
#     Repeated measures with     #
#     Mixed effect models        #
#                                #
##################################

# Loading packages                      
library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(cAIC4) # for cAIC
library(r2glmm) # for r2beta
library(influence.ME) # for influence
library(lattice) # for qqmath
library(reshape2) # for melt function
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer

# Load data                    
data_pain=read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv")

# asign ID as factor
data_pain$ID = factor(data_pain$ID)

# varriables
names(data_pain)

# designate which are the repeated varibales
repeated_variables = c("pain1",	"pain2", "pain3",	"pain4")


### explore data
# descriptives
describe(data_pain)

# histograms
hist(data_pain$pain1)
hist(data_pain$pain2)
hist(data_pain$pain3)
hist(data_pain$pain4)

# correlation of repeated variables
cor(data_pain[,repeated_variables])


### Transform wide to long format  
# Transform id.vars to be non-repeated variables
data_pain_long = melt(data_pain, measure.vars=repeated_variables, variable.name = "time", value.name = "pain")
# order data frame by participant ID
data_pain_long = data_pain_long[order(data_pain_long[,"ID"]),]
# change the time variable to a numerical vector
data_pain_long$time = as.numeric(data_pain_long$time)

# view the data looks like in long format
data_pain_long
summary(data_pain_long)

###analysis
#random intercept model
mod_rep_int = lmer(pain ~ time + sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness 
                   + (1|ID), data = data_pain_long)
#random slope model
mod_rep_slope = lmer(pain ~ time + sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness 
                     + (time|ID), data = data_pain_long)
summary(mod_rep_int)
summary(mod_rep_slope)


### model comparison to see whether to use random slope or random intercept models
## plot the regression line (prediction)
# save the predictions of bot models to variables
data_pain_long$pred_int = predict(mod_rep_int)
data_pain_long$pred_slope = predict(mod_rep_slope)
# random intercept model
ggplot(data_pain_long, aes(y = pain, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# random slope and intercept model
ggplot(data_pain_long, aes(y = pain, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope, x=time))+
  facet_wrap( ~ ID, ncol = 5)

# compare models with cAIC
cAIC(mod_rep_int)$caic
cAIC(mod_rep_slope)$caic

# compare models with anova
anova(mod_rep_int, mod_rep_slope)


### slope one is better 

### adding a quadratic term of time to the slope model
# to account for curved relationship between time and pain
mod_rep_slope_quad = lmer(pain ~ time + I(time^2) + sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness 
                        + (time|ID), data = data_pain_long)
summary(mod_rep_slope_quad)
confint(mod_rep_slope_quad)
# marginal R2 of quadratic model
?r2beta
r2beta (mod_rep_slope_quad)


## plot the results
# save prediction of the model to new variable
data_pain_long$pred_slope_quad = predict(mod_rep_slope_quad)

# random intercept model
ggplot(data_pain_long, aes(y = pain, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope_quad, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# this looks like a better fit than the others

# compare models with cAIC
cAIC(mod_rep_slope)$caic
cAIC(mod_rep_slope_quad)$caic

# compare models with anova
anova(mod_rep_slope, mod_rep_slope_quad)

# based on thee results it seems that the random intercept model
# including the quadratic term of time would be the best model to choose

### model diagnostics
# normality assumption
# QQ plot
qqmath(mod_rep_slope_quad, id=0.05) 

# linearity assumption
plot(mod_rep_slope_quad)
# linearity of each predictor and the standardized residual
predictors <- c("age", "sex", "weight", "STAI_trait", "pain_cat", "cortisol_serum", "day")

for(i in 1:length(predictors)){
  predictor_to_test = data_pain_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test, pearson = residuals(mod_rep_slope_quad, type = "pearson")),
           aes(x = x, y = pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw()
  )
}


# homoscedasticty assumption
# look for funnel shape on this graph
plot(mod_rep_slope_quad)
summary (lm(residuals(mod_rep_slope_quad)^2 ~ data_pain_long[,"time"])) ## look at the overall model F and p, if it is significant, there may be heteroscedasticity
           
# multicollinearity
pairs.panels(data_pain_long, col = "red", lm = T)







