library("multcomp")
data("recovery", package = "multcomp")
summary(recovery)
recovery.aov <- aov(minutes ~ blanket, data = recovery)
summary(recovery.aov)
summary(recovery.mc)

recovery.mc <- glht(recovery.aov,linfct = mcp(blanket = "Dunnett"), alternative = "less")

summary(recovery.mc, test = adjusted(type = "bonferroni")) #es menos exigente que dunnet

recovery.ci <- confint(recovery.mc, level = 0.95)
recovery.ci
plot(recovery.ci, main = "", ylim = c(0.5, 3.5),xlab = "Minutes")


data("immer", package = "MASS")
immer.aov <- aov((Y1 + Y2)/2 ~ Var + Loc, data = immer)
Lme.mod <- lme((Y1 + Y2)/2 ~ Var+Loc, data = immer)
model.tables(immer.aov, type = "means")$tables$Var
immer.mc <- glht(immer.aov, linfct = mcp(Var = "Tukey"))
summary(immer.mc)

immer.mc2 <-TukeyHSD(immer.aov, which = "Var")
immer.mc2$Var

immer.ci <- confint(immer.mc, level = 0.95)
immer.ci

plot(immer.ci, main = "", xlab = "Yield")