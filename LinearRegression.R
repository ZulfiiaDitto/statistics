# Document created by ZD
# linear regression analysis of FEV data 

# library imports 
library(tidyverse)

fev <- read_tsv("https://raw.githubusercontent.com/GTPB/PSLS20/master/data/fev.txt")

names(fev)
dim(fev) # (606,5)
head(fev)

summary(fev)

# checking the classes 
class(fev$smoking) # numeric class
class(fev$gender) # character class 

# changing the variables 
fev$gender = as.factor(fev$gender)
fev$smoking = as.factor(fev$smoking)

### Simple Linear regression 
plot(fev$height, fev$fev,main = 'scaterplot height vs fev',
     xlab = 'Height (in)' , ylab = 'fev(l)', las = 1)


# fitting linear regresion 
mod1 = lm(fev$fev ~ fev$height)

# summary of the model 
summary(mod1)

# fev = -5.74 + 0.136*height

# meaning that with one inch of increase in height -> 
#fev will increase 0.136 liters 

# when the height is zero inches  fev = -5.43 
# (if height = 0 , fev = should be also zero, cuz no human is height 0 inch)
# meaning we probably have to centered the data around some height number 
# Multiple R-squired = 0.72 -> meaning 72% of the variability are explained by the model 

# residual standard error = 0.43 -> you can think of this as average error

# adding the abline
abline(mod1, col = 'green', lwd = 3)

# asking for the coefficients /parameters (b's)
coef(mod1)
mod1$coef

# asking for the 95% coenfidence interval for them 
confint(mod1)
# lets presented as a table 
cbind(ModelCoef = coef(mod1), confint(mod1))


# plotting the model to check for assumptions
plot(mod1) # press enter to follow for the next plot 


############# TRANSFORMATION FOR LINEAR REGRESSION MODELS######
############# TRANSFORM THE X = HEIGHT TO LOG(HEIGHT)
# we use the same dataset as above

plot(log(fev$height), fev$fev, main = 'plot Height on log scale')
# the log function did not help -> more not linear 
# lets see what model will tell us
mod2 = lm(fev$fev ~ log(fev$height))
summary(mod2)
# when the log(height) increases by one the fev increase by 8.29


################ TRANSFORM THE Y = FEV TO LOG(FEV)
plot(fev$height, log(fev$fev)) # actually not that bad -> relationship is more linear 
# conceptually as you grow -> your body also grow -> so we have exponentially relationship here 
# that is explain why it is more linear 

mod3 = lm(log(fev$fev) ~ fev$height)
# as height is increased by one inch -> log(fev) aslso increased by 0.05
# model explains 76 % of the variability 
# residual standard error are pretty small 0.147

abline(mod3, col = 2, lwd = 3, lty = 1)
summary(mod3)

plot(mod3) # analysis the plots are also confirm 
# that the relationship is more linear and variance is more constant 
###### THIS WORKS PRETTY WELL FOR THE PREDICTIVE MODEL, 
##### HOWEVER THIS WILL NOT WORK FOR THE EFFECT SIZE MODEL, CUZ
####### WE USED THE LOG TRANSFORMATION IN ABOVE CASE 


# LETS USE THE POLIMONIAL TRANSFORMATION FOR THE HEIGHT 

fev['height_sq'] = fev$height^2 

head(fev)
# fitting the model 

mod4 = lm(fev$fev ~ fev$height + fev$height_sq) 
summary(mod4)
plot( fev$height, fev$fev)
lines(smooth.spline(fev$height, predict(mod4)), col = 'magenta', lwd = 3)

plot(mod4)

####### CENTIRNG THE HEIGHT VARIABLE 

fev['center_height'] = (fev$height - mean(fev$height))^2 
head(fev)
# fitting model 
mod5 = lm(fev$fev ~ fev$height + fev$center_height)
summary(mod5)


######### CATEGORICAL TRANSFORMATION OF X VARIEBLE = HEIGHT

fev['cat_height'] = cut(fev$height, breaks = c(0,50,55,60,65,70,100),
                        labels =c('A', 'B', 'C', 'D', 'E', 'F'), right = 'F')

# fitting the model 
mod6 = lm(fev$fev ~ fev$cat_height)
summary(mod6)

############ Multiple linear regressions - num, num / num, cat 
fev <- read_tsv("https://raw.githubusercontent.com/GTPB/PSLS20/master/data/fev.txt")

# transform the gender variable 

fev$gender <- as.factor(ifelse(fev$gender== 'f', 0, 1 ))
fev$smoking <- as.factor(fev$smoking)

# two variable age and height 
# lets build 3D-plot of those variables 
library(plotly)
plot_ly(fev, x = ~height, y = ~age, z= ~fev, type = 'scatter3d', 
        mode = 'markers')

# fitting the model 
model1 = lm(fev$fev ~ fev$age + fev$height)
summary(model1)

####### MODEL WITH ONE OF THE NUMERIC AND ONE OF CATEGORICAL values 
# lets plot the values 
plot_ly(fev,  x = ~age, y = ~gender, z= ~fev, type = 'scatter3d', 
        mode = 'markers', color = ~gender)

# fitting the model with no interaction (parralel lines)
model2 = lm(fev$fev ~ fev$age + fev$gender)
summary(model2)

# effect modification (no paralel lines, we consider the effect modification)

model3 = lm(fev$fev ~ fev$age + fev$gender + fev$age * fev$gender)

summary(model3)

######### T-test and linear regression 

mean(fev$smoking =='0')
mean(fev$smoking == '1')

mean(fev$smoking =='0')-mean(fev$smoking == '1')

t.test(fev$fev ~ fev$smoking, var.eq= T)
slr = lm(fev$fev ~ fev$smoking)
summary(slr)
confint(slr)
cbind(coef(slr), confint(slr))

############ Checking for confoundings variables 
boxplot(fev$age ~fev$smoking, ylab= 'age',
        xlab = 'smoke 0=no, 1=yes', las = 1)
# you can see that smokers are older 
# lets look into association: does smoking affect age
# or age affects smoking -> age affects smoking 
plot(fev$age, fev$fev) # you can see strong correlation as age increase fev increase 
# which confirmed by correlation 
cor(fev$age, fev$fev) #0.7283901
# conceptually this does make sense 

# lets fit model which adjust for the age and smoking 
model.age.adj = lm(fev$fev ~fev$age+ fev$smoking)
summary(model.age.adj)

#### Confounding variables 

model_unj = lm(fev$fev ~ fev$smoking)
summary(model_unj)

# age is conceptually confounder -> lets check it 

plot(fev$age, fev$fev)
cor(fev$age, fev$fev)

# above we already plotted the age of smokers and non-smokers and 
# found that age of non-smokers are hieghter, 
# conceptually it is make sense 

model_adj = lm(fev$fev ~ fev$age + fev$smoking)
summary(model_adj)


# COLLINEARITY 

library(data.table)     # Load data.table package
url <- "http://jse.amstat.org/datasets/fat.dat.txt"  
bodyfat <- fread(url, col.names = c("case", "brozek", "siri", 
                                    "density", "age", 
                                    "weight_lbs", 
                                    "height_in", "bmi", 
                                    "fat_free_weight", "neck_cm", 
                                    "chest_cm", "abdomen_cm", 
                                    "hip_cm", "thigh_cm", 
                                    "knee_cm", "ankle_cm", 
                                    "biceps_cm", "forearm_cm",
                                    "wrist_cm"))


# simple LR fat ~ bmi and wrist measure 

summary(lm(bodyfat$weight_lbs ~ bodyfat$bmi))
# st.error =  0.2349 

summary(lm(bodyfat$weight_lbs ~ bodyfat$wrist_cm))
# SD.error = 1.361

# now lets put in to model those two collinear variables and see the standard error 

summary(lm(bodyfat$weight_lbs ~ bodyfat$bmi + bodyfat$wrist_cm))

cor(bodyfat$bmi, bodyfat$wrist_cm)

########### BUILDING MODEL FOR FEV data #############

head(fev)
# age is confounded, it is linear 
# height -> if included non-linearity should be fixed by ^2 
# smokers per age distribution is different in both groups 
table(fev$age, fev$smoking)

mod_fin = lm(fev$fev ~ fev$age + fev$smoking)
summary(mod_fin)


mod_zero = lm(fev$fev ~fev$smoking)
summary(mod_zero)

# the b1 for smoking = 0.60541 in mod_zero is positive and sd. error = 0.10935 
# while when we adjust for the age b1 for smoking is negative -0.235833, 
# which make sense conceptually and the sd.error = 0.083327 
# we can see huge change in b1 -> from  positive to negative, 
# which is what we should see in confounder, and changes St error -> not inflated. Not big changes. 



# lets include the height now 
mod_h = lm(fev$fev ~ fev$age + fev$smoking + fev$height + I(fev$height^2))
plot(mod_h)

# if we include only one height -> linearity did not met 
mod_one_h = lm(fev$fev ~ fev$age + fev$smoking + fev$height)
plot(mod_one_h) #-> residual plot did not smooth around zero. 

# lets use partiall F test to confirm that model with height^2 is a better fit 
# H0 - no significant diff in models 
# H1 - model with H^2 fits significantly better 
anova(mod_one_h, mod_h)
# RSS=105.497 for model with one height 
# and RSS dropped significantly to 98.166 in model where we ^2 height 
# and it is stat significant 



# lets add the gender variable as well 

mod4 = lm(fev$fev ~ fev$age + fev$smoking + fev$height + I(fev$height^2) + fev$gender)
plot(mod4)
summary(mod4)

# gender is face-validity variable -> need ot include 
# plus there is association b/n gender and smoking 
mosaicplot(table(fev$gender, fev$smoking))

# there are less female smokers in a data than male smokers 

# lets explore the effect size of smoking and age 

mod5 = lm(fev$fev ~ fev$age + fev$smoking + fev$height + I(fev$height^2) +
            fev$gender + fev$smoking*fev$age)

summary(mod5)

# checking is mod5 is better fit 
anova(mod4, mod5)
#RSS dropped from the 97.276 to 96.750 from mod4 to mod5, 
# however the P value is not stat significant => mod5 is NOT better fit than mod4

# plus the conceptually the age should not effect smoking 

# lets explore the gender*smoking effect size 

mod6 = lm(fev$fev ~ fev$age + fev$smoking + fev$height + I(fev$height^2) +
            fev$gender + fev$smoking*fev$gender)

anova(mod4, mod6)
# not significantly -> mod6 is not better than mod4 -> 
# adding this gender*smoking effect modification is not making the model better 


#### FINAL MODEL IS MOD4 
# checking the diagnostics 
plot(mod4)
summary(mod4)

coef(mod4)
confint(mod4)

round(cbind("effect (b)" = coef(mod4), confint(mod4)), 3)
