library(dplyr)
library(haven)
library(lmtest)


stdata <- read_dta("finaldata_stata.dta") %>% 
  filter(week <= 28 & !(country_char %in% c("CZ", "CL", "IL", "NG", "SG", "VN"))) %>% 
  mutate(across(starts_with("dev"), as.numeric)) %>% 
  mutate(closedschools = ifelse(covid_school_closing <= 1, 0, 1),
         curfews = ifelse(covid_stay_home_restrictions <= 1, 0, 1),
         workplace = ifelse(covid_workplace_closing <= 1, 0, 1)) %>% 
  mutate(across(c(country_code, week, closedschools, workplace, curfews), factor))


m00 <- lm(diff ~ country_code + week, data = stdata[stdata$female == 0,])
coeftest(m00, vcov = vcovCL, cluster = ~country_code)

m01 <- lm(diff ~ country_code + week, data = stdata[stdata$female == 1,])
coeftest(m01, vcov = vcovCL, cluster = ~country_code)


m10 <- lm(diff ~ covid_stringency + country_code + week, 
          data = stdata[stdata$female == 0,])
coeftest(m10, vcov = vcovCL, cluster = ~country_code)

m11 <- lm(diff ~ covid_stringency + country_code + week,
          data = stdata[stdata$female == 1,])
coeftest(m11, vcov = vcovCL, cluster = ~country_code)


m20 <- lm(diff ~ closedschools + workplace + curfews + country_code + week, 
          data = stdata[stdata$female == 0,])
coeftest(m20, vcov = vcovCL, cluster = ~country_code)

m21 <- lm(diff ~ closedschools + workplace + curfews + country_code + week, 
          data = stdata[stdata$female == 1,])
coeftest(m21, vcov = vcovCL, cluster = ~country_code)


m30 <- lm(diff ~ closedschools*workplace + curfews + country_code + week, 
          data = stdata[stdata$female == 0,])
coeftest(m30, vcov = vcovCL, cluster = ~country_code)

m31 <- lm(diff ~ closedschools*workplace + curfews + country_code + week, 
          data = stdata[stdata$female == 1,])
coeftest(m31, vcov = vcovCL, cluster = ~country_code)
