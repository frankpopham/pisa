test34 <- scotpisadf2018 %>%
  filltar() %>%
  filter(dob >= ymd("2003_02_01") & dob <=ymd("2003_03_01")) %>%
  mutate(month=if_else(dob==ymd("2003_02_01"), "Feb", "March")) %>%
  mutate(grade11=if_else(grade=="Grade 11", 1, 0)) %>%
  filter(!is.na(AGE)) 



test1 <- svyglm(AGE ~ month, design=test34)

test2 <- svyglm(grade11 ~ month, design=test34)

##test3 manually
test1$coefficients[2] / test2$coefficients[2]



test3 <- svyivreg(AGE ~ grade11 | month, design=test34)
vcov(test3)
confint(test3)



test34 <- test34 %>%
  mutate(pgrade11=test2$fitted.values) %>%
  mutate(ipwgrade11=if_else(grade11==1, 1/pgrade11, 1/ (1-pgrade11)))

test4 <- svyglm(AGE ~  pgrade11, design=test34)


test34 %>% group_by(grade11) %>%
  survey_tally(wt=ipwgrade11)


out_list  <- list("AGE", "ESCS", "HOMEPOS", "HISEI", "HISCED=='ISCED 5A, 6'")
out <-  map(out_list, ~scotpisadf2018 %>%
  filltar() %>%
  filter(dob >= ymd("2003_02_01") & dob <=ymd("2003_03_01")) %>%
  mutate(month=if_else(dob==ymd("2003_02_01"), "Feb", "March")) %>%
  mutate(grade11=if_else(grade=="Grade 11", 1, 0)) %>%
  filter(!is.na(.x)) %>%
  svyivreg(as.formula(paste(.x, "~ grade11 | month")) , design=.) 
) 
names(out) <- out_list

outdf <- map_dfr(out, ~as.tibble(.x[["grade11"]]), .id="outcome") %>%
  mutate(lower=map(out, ~confint(.x)[2])) %>%
  mutate(upper=map(out, ~confint(.x)[4]))



