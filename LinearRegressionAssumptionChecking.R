# Linear regression Assumptions checking 
# created on 9/9/24 

df = read.table(file="http://tiny.cc/econ226/data/LungCapData.txt", header = T, sep = "\t")

# correlation between two of the variables 
cor(df$Age, df$LungCap)

plot(df$Age, df$LungCap)

mod1 = lm(df$LungCap ~df$Age)
summary(mod1)

abline(mod1)



