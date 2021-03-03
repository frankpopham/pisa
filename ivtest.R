test34 <- scotpisadf2018 %>%
  filltar() %>%
  filter(dob >= ymd("2003_02_01") & dob <=ymd("2003_03_01")) %>%
  mutate(month=if_else(dob==ymd("2003_02_01"), "Feb", "March")) %>%
  mutate(grade11=if_else(grade=="Grade 11", 1, 0)) %>%
  filter(!is.na(ESCS)) 



test1 <- svyglm(ESCS ~ month, design=test34)

test2 <- svyglm(grade11 ~ month, design=test34)

##test3 manually
test1$coefficients[2] / test2$coefficients[2]

test3 <- svyivreg(ESCS ~ grade11 | month, design=test34)
vcov(test3)
confint(test3)



test34 <- test34 %>%
  mutate(pgrade11=test2$fitted.values) %>%
  mutate(ipwgrade11=if_else(grade11==1, 1/pgrade11, 1/ (1-pgrade11)))

test4 <- svyglm(ESCS ~  pgrade11, design=test34)


test34 %>% group_by(grade11) %>%
  survey_tally(wt=ipwgrade11)


