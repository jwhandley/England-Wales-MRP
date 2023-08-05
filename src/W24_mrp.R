library(tidyverse)
library(haven)
library(Hmisc)
library(brms)

## Constituency-level predictors
# Just using 2019 vote shares to keep things simple
# but could be worth adding some 2011 or 2021 census outputs
context <- read_stata("data/BES-2019-General-Election-results-file-v1.1.dta") %>%
  rename(pcon_code = ONSConstID) %>%
  select(pcon_code,Lab19,LD19,Con19,Turnout19) %>%
  mutate(across(c(Lab19,LD19,Con19,Turnout19), ~.x/100)) 

## Post-stratification frame
# Made using the ONS's custom table builder
# Using age, sex, ethnicity, and qualifications as the groups
# Might replace ethnicity or qualifications with NS-SEC in a future version
read_csv("data/pstrat-noeth.csv")  %>%
  mutate(sex = factor(sex_name, levels = c("Male", "Female")),
         age = factor(age_name, levels = c("Aged 16 to 24 years",
                                           "Aged 25 to 34 years",
                                           "Aged 35 to 49 years",
                                           "Aged 50 to 64 years",
                                           "Aged 65 years and over")),
         degree = ifelse(qual_code == 4, "Degree", "No degree"),
         degree = factor(degree, levels = c("Degree", "No degree"))) %>%
  select(pcon_name,pcon_code,age,sex,degree,pop) %>%
  group_by(pcon_code,age,sex,degree) %>%
  summarise(pcon_name = first(pcon_name),
            pop = sum(pop)) %>%
  filter(pop != 0,
         age != "Aged 15 years and under") %>%
  left_join(context) %>%
  mutate(LD19 = ifelse(is.na(LD19), 0, LD19),
         Con19 = ifelse(is.na(Con19), 0, Con19)) -> pstrat

## Survey data
# Wave 24 of the BES Panel for the survey data
# Education level was difficult to harmonize the Census NVQ levels and BES codes so I just went with a Degree/No degree binary
# Re-coded vote choice to include four levels (Con, Lab, Lib Dem, Other)
survey_cols <- c("wt_new_W24",
                 "ageW24",
                 "generalElectionVoteW24",
                 "p_edlevelW24",
                 "gender",
                 "pcon_codeW24",
                 "wave24")

survey <- read_stata("data/BES2019_W24_Panel_v24.0.dta", col_select = all_of(survey_cols)) %>%
  filter(wave24 == 1) %>%
  mutate(sex = as_factor(gender),
         degree = ifelse(p_edlevelW24 >= 4,"Degree","No degree"),
         degree = factor(degree, levels = c("Degree", "No degree")),
         age = case_match(ageW24,
                          16:24 ~ "Aged 16 to 24 years",
                          25:34 ~ "Aged 25 to 34 years",
                          35:49 ~ "Aged 35 to 49 years",
                          50:64 ~ "Aged 50 to 64 years",
                          65:999 ~ "Aged 65 years and over"),
         age = factor(age, levels = c("Aged 16 to 24 years",
                                      "Aged 25 to 34 years",
                                      "Aged 35 to 49 years",
                                      "Aged 50 to 64 years",
                                      "Aged 65 years and over")),
         vote = case_match(generalElectionVoteW24,
                           1 ~ "Conservative",
                           2 ~ "Labour",
                           3 ~ "Liberal Democrat",
                           0 ~ "Wouldn't vote",
                           9999 ~ "Wouldn't vote",
                           NA ~ "Wouldn't vote",
                           .default = "Other"),
         vote = factor(vote, levels = c("Conservative","Labour","Liberal Democrat","Other","Wouldn't vote")),
         wt = wt_new_W24,
         pcon_code = pcon_codeW24) %>%
  select(wt,vote,age,sex,degree,pcon_code) %>%
  left_join(context) %>%
  filter(!if_any(c(vote,age,sex,degree,pcon_code), ~is.na(.x))) %>%
  mutate(LD19 = ifelse(is.na(LD19), 0, LD19),
         Con19 = ifelse(is.na(Con19), 0, Con19))

## Multi-level regression
# brms allows for categorical response variables which makes modelling multi-party elections a lot easier
# For a more complicated model, I would include interaction terms but for now I just want to see if I can get it working reasonably well
fit <- brm(vote ~ Turnout19 + Con19 + Lab19 + LD19 + degree + sex + (1|age) + (1|pcon_code),
           data = sample_n(survey, size = 5000),
           prior = c(
             prior(normal(0, 5), class = Intercept),
             prior(normal(0, 1), class = b),
             # Exponential priors on the standard deviation because it is constrained to be non-zero
             prior(exponential(0.5), class = sd, dpar = muLabour),
             prior(exponential(0.5), class = sd, dpar = muLiberalDemocrat),
             prior(exponential(0.5), class = sd, dpar = muOther),
             prior(exponential(0.5), class = sd, dpar = muDidntvote)
           ),
           family = categorical(refcat="Conservative"),
           iter = 1000,
           chains = 4,
           cores = 4,
           threads = threading(3),
           backend = "cmdstanr",
           control = list(adapt_delta = 0.95, max_treedepth = 10))

## Save fitted model
saveRDS(fit,"output/W24_mrp_fit.RDS")

## Generate predictions
preds <- predict(fit, pstrat, allow_new_levels = TRUE, cores = 6, summary = TRUE)
pstrat[,c("Con","Lab","LD","Oth","DNV")] <- preds

# Plot scatter plot of predictions vs. election result
pstrat %>%
  mutate(Turnout = 1 - DNV) %>%
  mutate(across(c(Con,Lab,LD,Oth), ~.x/Turnout)) %>%
  group_by(pcon_name) %>%
  summarise(across(c(Con,Lab,LD,Oth,Turnout,Con19,Lab19,LD19,Turnout19),
                   ~wtd.mean(.x, Turnout*pop, na.rm = TRUE))) %>%
  mutate(Oth19 = 1 - Con19 - Lab19 - LD19) %>%
  pivot_longer(-pcon_name) %>%
  mutate(vote = str_extract(name,"[a-z|A-Z]+"),
         vote = factor(vote, levels = c("Con","Lab","LD","Oth","Turnout")),
         type = ifelse(grepl("[0-9]+",name),"Actual","Pred")) %>%
  select(-name) %>%
  pivot_wider(id_cols = c(pcon_name, vote), names_from = type, values_from = value) %>%
  ggplot(aes(x = Actual, y = Pred, color = vote)) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  facet_wrap(~vote, labeller = as_labeller(c("Con" = "Conservative",
                                             "Lab" = "Labour",
                                             "LD" = "Liberal Democrat",
                                             "Oth" = "Other",
                                             "Turnout" = "Turnout"))) +
  scale_color_manual(values = c("#0087DC","#DC241f","#FAA61A","grey","black"),
                     labels = c("Conservative","Labour","Liberal Democrat","Other","Turnout"),
                     name = NULL) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(x = "2019 general election result",
       y = "December 2022 MRP prediction",
       title = "Predicted vote share in December 2022 vs. 2019 general election result",
       subtitle = "BES Internet Panel Wave 24; 2021 England and Wales census",
       caption = "@jwhandley17")

ggsave("output/W24_mrp_scatter.png", width = 8, height = 6)

## Compare top-line results
pstrat %>%
  mutate(Turnout = 1 - DNV) %>%
  mutate(across(c(Con,Lab,LD,Oth), ~.x/Turnout)) %>%
  ungroup() %>%
  summarise(across(c(Con,Lab,LD,Oth,Turnout,Con19,Lab19,LD19,Turnout19),
                   ~wtd.mean(.x, Turnout*pop, na.rm = TRUE))) %>%
  mutate(Oth19 = 1 - Con19 - Lab19 - LD19) 
