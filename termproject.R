setwd("/Users/rowan/Downloads")
project_data <- read_csv("argentina.csv")
library(pacman)
p_load(AER, stargazer, tidyverse, broom, car)
head(project_data)

# prepare data for analysis - log transform pop and gdp
total_gdp <- sum(project_data$gdp)
project_data <- project_data %>% 
    mutate(loggdp = (log(gdp)))
project_data <- project_data %>% 
    mutate(health_proportion = ifelse(
          no_healthcare > 50, 1, 0))
project_data <- project_data %>% 
    mutate(buenos_control = ifelse(
          province == "Buenos Aires", 0, 1))
project_data <- project_data %>% 
    mutate(prop_gdp = ifelse(
        I(gdp/total_gdp) > 0.4, 0, 1))
project_data <- project_data %>% 
    mutate(logpop = log(pop))
ggplot(
  data = project_data, mapping = aes(x = log(doctors_per_cap), y = loggdp)) + geom_point() + stat_smooth()
project_data <- project_data %>% 
    mutate(logdoc = log(doctors_per_cap))
  
# justification for variables of interest
ggplot(data = project_data, mapping = aes(
  x = school_dropout, y = illiteracy)) + geom_point()
summary(lm(illiteracy ~ school_dropout, data = project_data))

    # ovb test on school_dropout
just_reg1 <- lm(
  loggdp ~ illiteracy, data = project_data
  )
just_reg2 <- lm(
  loggdp ~ illiteracy + school_dropout, data = project_data
  )
stargazer(just_reg1, just_reg2, type = "text")
var1 <- var(project_data$illiteracy)
cov1 <- cov(project_data$school_dropout, project_data$illiteracy)
OVB1 <- -0.013 * (cov1/var1)
      # negligible OVB !!! 
      # therefore will not include school_dropout


      # OVB test on illiteracy with poverty
ggplot(data = project_data, mapping = aes(x = poverty, y = illiteracy)) + geom_point()
summary(lm(poverty ~ illiteracy, data = project_data))
just_reg3 <- lm(
  loggdp ~ poverty, data = project_data
  )
just_reg4 <- lm(
  loggdp ~ poverty + illiteracy, data = project_data
  )
stargazer(just_reg3, just_reg4, type = "text")
var2 <- var(project_data$poverty)
cov2 <- cov(project_data$poverty, project_data$illiteracy)
OVB2 <- -0.059 * (cov1/var1)
ggplot(data = project_data, mapping = aes(
  x = health_proportion, y = birth_mortal)) + geom_point()
    # shouldn't include school_dropout or illiteracy
      

reg_pop <- lm(loggdp ~ logpop, data = project_data)
stargazer(reg1, type = "text")
reg_control_buenos <- lm(loggdp ~ logpop + buenos_control, data = project_data)
stargazer(reg_control_buenos, type = "text")
ggplot(data = project_data, mapping = aes(x = logpop, y = loggdp)) + geom_point()
  # proves that magnitude of B1 increases when buenos controlled for

# so variables of interest are: 
    # poverty, doctors_per_cap, deficient_infra, pop, no_healthcare
    # plus buenos_control later


# begin regressing
firstreg <- lm(
  loggdp ~ logpop + logdoc + deficient_infra + no_healthcare + poverty, 
  data = project_data
  )
stargazer(firstreg, type = "text")
secondreg <- lm(
  loggdp ~ logpop + logdoc + deficient_infra + no_healthcare + poverty + buenos_control, 
  data = project_data
  )
stargazer(secondreg, type = "text")


# F-tests for model significance 
linearHypothesis(secondreg, c(
  "deficient_infra = 0", "no_healthcare = 0", "logdoc = 0"))
      # significant at 0.1

# begin heteroskedasticity tests
residuals <- resid(secondreg)
ggplot(data = project_data, mapping = aes(x = logdoc, y = residuals)) + geom_point()

    # could be heteroskedasticity, so start with goldfeld-quandt test
hetcheck <- project_data %>% arrange(deficient_infra)
n_GQ <- as.integer(nrow(project_data)*3/8)
gf_lm1 <- lm(
  loggdp ~ logpop + logdoc + deficient_infra + no_healthcare + poverty + buenos_control, 
  data = head(project_data, n_GQ)
)
gf_lm2 <- lm(
  loggdp ~ logpop + logdoc + deficient_infra + no_healthcare + poverty + buenos_control, 
  data = tail(project_data, n_GQ)
)
e_gf1 <- resid(gf_lm1)
sse1 <- sum(e_gf1^2)
e_gf2 <- resid(gf_lm2)
sse2 <- sum(e_gf2^2)
gstat <- sse2/sse1

pf(q = gstat, df1 = n_GQ - 7, df2 = n_GQ - 7, lower.tail = F)
# no evidence of heteroskedasticity - fail to reject H0

# white test
r_white <- resid(secondreg)
resid_reg <- lm(
  I(r_white^2) ~ logpop + logdoc + deficient_infra + no_healthcare + poverty + buenos_control
  + I(logpop^2) + I(logdoc^2) + I(deficient_infra^2) + 
    I(no_healthcare^2) + I(poverty^2) + I(buenos_control^2) + 
    logpop:logdoc + logpop:deficient_infra + 
    logpop:no_healthcare + logpop:poverty + logpop:buenos_control 
  + logdoc:deficient_infra 
  + logdoc:no_healthcare + logdoc:poverty + logdoc:buenos_control +
    deficient_infra:no_healthcare + deficient_infra:poverty + 
    deficient_infra:buenos_control + 
    no_healthcare:poverty + no_healthcare:buenos_control + 
    poverty:buenos_control, data = project_data
   )

r2_white <- summary(resid_reg)$r.squared
lagrange_multiplier <- nrow(project_data)*r2_white
pchisq(q = lagrange_multiplier, df = 28, lower.tail = F)
    # huge p-value, no evidence of heteroskedasticity


