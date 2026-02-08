#test the hypothesis that soil carbon stock (soil.carbon) depends on species richness, altitude, sand cover, annual mean temperature, and precipitation.


library(tidyverse) 


data <- read.csv("../data/FunDivEUROPE.csv")


head(data)
str(data)
summary(data)


# check the data tyoe of each predictor

for(var in names(data)){
  cat("Variable:", var, "\n")
  cat("  Class:", class(data[[var]]), "\n")
  cat("  Unique values:", length(unique(data[[var]])), "\n")
  cat("  First 5 values:", head(data[[var]],5), "\n\n")
}

#with linear regression and check

#transformation to factor of target species richness
data$target.species.richness <- as.factor(data$target.species.richness)
#transformation to factor of sand
data$sand <- as.factor(data$sand)

#try single linear regression and check statistic 
lm.soilcarbon.tsr <- lm(soil.carbon ~ target.species.richness, data = data)
lm.soilcarbon.sd  <- lm(soil.carbon ~ sand, data = data)
lm.soilcarbon.altitude <- lm(soil.carbon ~ altitude, data = data)
lm.soilcarbon.temp <- lm(soil.carbon ~ annual.mean.temperature, data = data)
lm.soilcarbon.preci <- lm(soil.carbon ~ annual.mean.precipitation, data = data)



#check model of target species richness
par(mfrow = c(2,2))
plot(lm.soilcarbon.tsr)
par(mfrow = c(1,1))


# check sd
par(mfrow = c(2,2))
plot(lm.soilcarbon.sd)
par(mfrow = c(1,1))

#check altitude
par(mfrow = c(2,2))
plot(lm.soilcarbon.altitude) 
par(mfrow = c(1,1))

#check temp
par(mfrow = c(2,2))
plot(lm.soilcarbon.temp)
par(mfrow = c(1,1))

#check prici
par(mfrow = c(2,2))
plot(lm.soilcarbon.preci)
par(mfrow = c(1,1))


#log transformation and try  again lm
min(data$soil.carbon, na.rm = TRUE)
data$log.soil.carbon <- log(data$soil.carbon)

lm.sc.log.tsr <- lm(log.soil.carbon ~ target.species.richness, data = data)
lm.sc.log.sd  <- lm(log.soil.carbon ~ sand, data = data)
lm.sc.log.altitude <- lm(log.soil.carbon ~ altitude, data = data)
lm.sc.log.temp <- lm(log.soil.carbon ~ annual.mean.temperature, data = data)
lm.sc.log.preci <- lm(log.soil.carbon ~ annual.mean.precipitation, data = data)
```


#check model againl with log

#check model of target species richness
par(mfrow = c(2,2))
plot(lm.sc.log.tsr)
par(mfrow = c(1,1))


# check sd
par(mfrow = c(2,2))
plot(lm.sc.log.sd)
par(mfrow = c(1,1))

#check altitude
par(mfrow = c(2,2))
plot(lm.sc.log.altitude) 
par(mfrow = c(1,1))

#check temp
par(mfrow = c(2,2))
plot(lm.sc.log.temp)
par(mfrow = c(1,1))

#check prici
par(mfrow = c(2,2))
plot(lm.sc.log.preci)
par(mfrow = c(1,1))

#summary

summary(lm.sc.log.tsr)

sink("output/tables/summary_soilcarbon_tree_species_richness.txt")

summary(lm.sc.log.tsr)

sink()

#The target species richness did not have a significant effect on soil carbon stock (F = 0.425, p = 0.79). The differences in predicted values among the richness levels were also not statistically significant. The overall explanatory power of the model was very low (R² = 0.008), suggesting that most of the variation in soil carbon is dependent on other factors.



summary(lm.sc.log.sd)

sink("output/tables/summary_soilcarbon_sand.txt")

summary(lm.sc.log.sd)

sink()

#The type of sand had a significant effect on soil carbon stock (F = 20.28, p < 0.001). Both sand2 and sand3 had significantly lower soil carbon compared to the reference sand type (sand1). The overall explanatory power of the model was 16%, indicating that sand type accounts for a portion of the variation in soil carbon.


summary(lm.sc.log.altitude)
sink("output/tables/summary_soilcarbon_altitude.txt")

summary(lm.sc.log.tsr)

sink()

#Altitude had a significant positive effect on soil carbon stock (F = 92.21, p < 0.001), with soil carbon increasing as altitude rises. The overall explanatory power of the model was approximately 31%, indicating that altitude accounts for a portion of the variation in soil carbon.



summary(lm.sc.log.temp)

sink("output/tables/summary_soilcarbon_tempreture.txt")

summary(lm.sc.log.tsr)

sink()

#Annual mean temperature did not have a significant effect on soil carbon stock on its own (F = 1.176, p = 0.28). The overall explanatory power of the model was minimal (R² ≈ 0.006), indicating that variation in soil carbon cannot be explained by temperature alone.


summary(lm.sc.log.preci)

sink("output/tables/summary_soilcarbon_precipitation.txt")

summary(lm.sc.log.tsr)

sink()

#Annual mean precipitation had a small but significant negative effect on soil carbon stock (β = -0.00076, p = 0.011). The overall explanatory power of the model was low (R² ≈ 0.03), indicating that precipitation alone cannot account for most of the variation in soil carbon.

#Based on the results so far, species richness, temperature, and precipitation cannot be considered as primary explanatory variables for soil carbon on their own. In contrast, altitude and sand type appear to have the potential to act as major single factors influencing soil carbon. Therefore, a base model using altitude and sand as fixed effects should first be constructed, and the distribution of the residuals should be re-examined. After that, models incorporating interaction effects and random effects can be created, visualized, and tested separately.



# sand, atitude model

lm.sd.at <- lm(log.soil.carbon ~ sand + altitude, data = data)

par(mfrow = c(2, 2))
plot(lm.sd.at)
summary(lm.sd.at)

sink("output/tables/summary_soilcarbon_sand_altitude.txt")


lm.sd.at.int <- lm(log.soil.carbon ~ sand * altitude, data = data)
anova(lm.sd.at, lm.sd.at.int)


#No interaction was detected between sand and altitude. Adding the sand × altitude interaction to the model did not significantly improve it (F = 0.192, p = 0.826). Therefore, the effects of sand and altitude on soil carbon stock can be adequately explained by their main effects alone.

#about precipitation
lm.base <- lm(log.soil.carbon ~ sand + altitude, data = data)
lm.p <- lm(log.soil.carbon ~ sand + altitude + annual.mean.precipitation, data = data)
anova(lm.base, lm.p)
sink("output/tables/anova_soilcarbon_sand_altitude_vs_precipitation.txt")
anova(lm.base, lm.p)
sink()


#The effect of annual mean precipitation on soil carbon is only marginally significant, but considering potential influences of random effects, it will be retained as a fixed effect.

#Step 1: Visualization of the Base Model with Fixed Effects

#The fixed effects to be included are:
  
#Sand (categorical variable)

#Altitude (continuous variable)

#Annual mean precipitation (continuous variable)

#This base model will allow us to visualize the relationships of these key predictors with soil carbon before adding interaction or random effects.



library(effects)


m_base <- lm(log.soil.carbon ~ sand + altitude + annual.mean.precipitation, data = data)


eff_sand <- effect("sand", m_base, partial.residuals = TRUE)


plot(eff_sand,
     main = "",
     ylab = "log(Soil Carbon)",
     xlab = "Sand Type")


plot(eff_sand,
     main = "",
     ylab = "Soil Carbon",
     xlab = "Sand Type",
     transformation = list(link = log, inverse = exp))



ggplot(data, aes(x = altitude, y = log.soil.carbon)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  ylab("log(Soil Carbon)") +
  xlab("Altitude (m)") +
  theme_classic()



ggplot(data, aes(x = annual.mean.precipitation, y = log.soil.carbon)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "darkgreen") +
  ylab("log(Soil Carbon)") +
  xlab("Annual Mean Precipitation (mm)") +
  theme_classic()



#use effect

eff_alt <- effect("altitude", m_base, partial.residuals = TRUE)
plot(eff_alt,
     main = "",
     ylab = "log(Soil Carbon)",
     xlab = "Altitude")



eff_precip <- effect("annual.mean.precipitation", m_base, partial.residuals = TRUE)
plot(eff_precip,
     main = "",
     ylab = "log(Soil Carbon)",
     xlab = "Annual Precipitation")


eff_all <- allEffects(m_base, partial.residuals = TRUE)
plot(eff_all)


library(emmeans)
anova_sand <- lm(log.soil.carbon ~ sand, data = data)
emmeans(anova_sand, pairwise ~ sand, adjust = "Tukey")

emm_res <- emmeans(anova_sand, pairwise ~ sand, adjust = "Tukey")

capture.output(
  print(emm_res),
  file = "output/tables/emmeans_sand_tukey.txt"
)


#Based on the model-adjusted estimated means (emmeans), sand type had a clear effect on soil carbon stock. Soil carbon in sand3 was significantly lower than in sand1 and sand2 (sand1–sand3: p < 0.0001, sand2–sand3: p = 0.016), while the difference between sand1 and sand2 was marginally significant (p = 0.056). Overall, this suggests that sand type has a notable influence on soil carbon.

#So far, the factors that had significant effects on soil carbon as single predictors were sand, altitude, and annual mean precipitation. Other factors did not show significant effects on soil carbon on their own.

#For interaction effects, the following combinations will be examined:
  
#sand × species richness

#sand × precipitation

#altitude × temperature

#altitude × precipitation

#precipitation × target species richness



#intaraction between sand and target.species.richness
m_sand_rich <- lm(log.soil.carbon ~ sand*target.species.richness, data = data)
summary(m_sand_rich)
sum_sand_rich <- summary(m_sand_rich)

capture.output(
  print(sum_sand_rich),
  file = "output/tables/summary_sand_x_species_richness.txt"
)
```
#There was no interaction between sand and target species richness.



#sand and annual.mean.precipitation
m_sand_precip <- lm(log.soil.carbon ~ sand*annual.mean.precipitation, data = data)
summary(m_sand_precip)
sum_sand_precip <- summary(m_sand_precip)

capture.output(
  print(sum_sand_precip),
  file = "output/tables/summary_sand_x_precipitation.txt"
)

#The effect of precipitation reduces the differences in soil carbon among the sand types.

#altitude and temperature
m_alt_temp <- lm(log.soil.carbon ~ altitude*annual.mean.temperature, data = data)
summary(m_alt_temp)
sum_alt_temp <- summary(m_alt_temp)

capture.output(
  print(sum_alt_temp),
  file = "output/tables/summary_altitude_x_tempreture.txt"
)
#Altitude mitigates the decrease in soil carbon caused by higher temperatures.

#altitude and annual.mean,precipitation
m_alt_precip <- lm(log.soil.carbon ~ altitude*annual.mean.precipitation, data = data)
summary(m_alt_precip)

sum_alt_precip <- summary(m_alt_precip)

capture.output(
  print(sum_alt_precip),
  file = "output/tables/summary_altitude_x_precipitation.txt"
)

#Altitude has a negative effect on the impact of precipitation on soil carbon.


m_precip_rich <- lm(log.soil.carbon ~ annual.mean.precipitation*target.species.richness, data = data)
summary(m_precip_rich)

sum_precip_rich <- summary(m_precip_rich)

capture.output(
  print(sum_precip_rich),
  file = "output/tables/summary_precipitation_x_speciesrichnessn.txt"
)
#There is no interaction between precipitation and target species richness.

#In summary, the observed interactions among factors are as follows:
  
#The effect of precipitation reduces the differences in soil carbon among sand types.

#Altitude mitigates the decrease in soil carbon caused by higher temperatures.

#Altitude has a negative effect on the impact of precipitation on soil carbon.


library(ggplot2)


data$pred <- predict(m_sand_precip, newdata = data)


ggplot(data, aes(x = annual.mean.precipitation, y = log.soil.carbon, color = sand)) +
  geom_point(alpha = 0.5) +          
  geom_line(aes(y = pred), size = 1) + 
  labs(x = "Annual Mean Precipitation", y = "Log Soil Carbon", color = "Sand Type") +
  theme_minimal()


#The annual mean precipitation shows an interesting effect in its interaction with sand type on soil carbon. 
#Sand type had a significant impact on soil carbon (sand2: -1.76, p = 0.0045; sand3: -2.71, p = 0.0020). Additionally, precipitation reduced soil carbon in the reference sand, 
#but this negative effect was mitigated in sand2 and sand3 due to the interaction with sand type. The overall model was significant (F = 17.72, p < 0.001) and explained approximately 30% of the variation in soil carbon (R² = 0.305).
#However, soil carbon may also be influenced by country, litter layer, root biomass, and microbial biomass. 
#Therefore, these factors should be incorporated as random effect(country) or interaction terms, and the changes in fixed effects, as well as factors that were not significant on their own, should be re-evaluated.


#random effect; country
library(lme4)

m_mixed_country <- lmer(
  log.soil.carbon ~ sand + altitude + annual.mean.precipitation +
    (1 | country),
  data = data
)

#sand/ randome effect country plot and result

library(emmeans)

emm_sand <- emmeans(m_mixed_country, ~ sand)

plot(emm_sand)

eff_alt <- effect("altitude", m_mixed_country)
plot(eff_alt,
     xlab = "Altitude",
     ylab = "log(Soil Carbon)")

eff_precip <- effect("annual.mean.precipitation", m_mixed_country)
plot(eff_precip,
     xlab = "Annual Mean Precipitation",
     ylab = "log(Soil Carbon)")

newdat <- expand.grid(
  sand = levels(data$sand),
  altitude = seq(min(data$altitude), max(data$altitude), length = 100),
  annual.mean.precipitation = mean(data$annual.mean.precipitation),
  country = NA   # ← ここが重要
)

newdat$pred <- predict(m_mixed_country, newdata = newdat, re.form = NA)

newdat <- expand.grid(
  sand = levels(data$sand),
  altitude = seq(min(data$altitude), max(data$altitude), length = 100),
  annual.mean.precipitation = mean(data$annual.mean.precipitation),
  country = NA
)

newdat$pred <- predict(m_mixed_country, newdata = newdat, re.form = NA)

ggplot(data, aes(x = altitude, y = log.soil.carbon, color = sand)) +
  geom_point(alpha = 0.3) +
  geom_line(data = newdat,
            aes(x = altitude, y = pred, color = sand),
            size = 1) +
  labs(y = "log(Soil Carbon)",
       x = "Altitude") +
  theme_classic()

summary(m_mixed_country)
sum_mixed_country <- summary(m_mixed_country)

capture.output(
  print(sum_mixed_country),
  file = "output/tables/summary_mixied_country.txt"
)
#After accounting for between-country variability, sand type and altitude remained significant predictors of soil carbon, 
#whereas the effect of annual mean precipitation was no longer significant.

#intaraction of sand and precipitation with counrty effect

m_mixed_country_sd.pre <- lmer(log.soil.carbon ~ sand * annual.mean.precipitation + altitude +
       (1 | country), data = data)

newdat <- expand.grid(
  sand = levels(data$sand),
  annual.mean.precipitation =
    seq(min(data$annual.mean.precipitation),
        max(data$annual.mean.precipitation),
        length = 100),
  altitude = mean(data$altitude),
  country = NA
)

newdat$pred <- predict(
  m_mixed_country_sd.pre,
  newdata = newdat,
  re.form = NA
)

ggplot(data,
       aes(x = annual.mean.precipitation,
           y = log.soil.carbon,
           color = sand)) +
  geom_point(alpha = 0.3) +
  geom_line(data = newdat,
            aes(y = pred),
            size = 1) +
  labs(
    x = "Annual Mean Precipitation",
    y = "Predicted log(Soil Carbon)",
    color = "Sand type"
  ) +
  theme_classic()

summary(m_mixed_country_sd.pre)
sum_mixed_country_sd.pre <- summary(m_mixed_country_sd.pre)

capture.output(
  print(sum_mixed_country_sd.pre),
  file = "output/tables/summary_mixied_country_sd.pre.txt"
)
#The effect of precipitation on soil carbon tended to differ among sand types, 
#but this interaction became weaker after accounting for between-country variability.