test34 <- scotpisadf2018 %>%
  filltar() %>%
  filter(dob >= ymd("2003_02_01") & dob <=ymd("2003_03_01")) %>%
  mutate(month=if_else(dob==ymd("2003_02_01"), "Feb", "March")) %>%
  mutate(grade11=if_else(grade=="Grade 11", 1, 0)) %>%
  filter(!is.na(cog)) 



test1 <- svyglm(cog ~ month, design=test34)

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

scotpisadf2018 <- scotpisadf2018 %>%
  mutate(cog=5-as.numeric(ST184Q01HA)) %>%
  mutate(cog=scale(cog)) %>%
  mutate(Lifesat=scale(Lifesat))
out_list  <- list("cog", "MASTGOAL", "WORKMAST", "RESILIENCE", "GFOFAIL", 
                  "EUDMO", "SWBP", "Lifesat", "COMPETE", "BEINGBULLIED", 
                  "PERCOMP", "PERCOOP", "EMOSUPS", "BELONG")
out_scot <-  map(out_list, ~scotpisadf2018 %>%
  filltar() %>%
  filter(dob >= ymd("2003_02_01") & dob <=ymd("2003_03_01")) %>%
  mutate(month=if_else(dob==ymd("2003_02_01"), "Feb", "March")) %>%
  mutate(grade11=if_else(grade=="Grade 11", 1, 0)) %>%
  filter(!is.na(.x)) %>%
  svyivreg(as.formula(paste(.x, "~ grade11 | month")) , design=.) 
) 

outlist2 <- list("Cognitive wellbeing" ,
     "Learning goals", 
     "Motivation to master tasks", 
     "Resilience", 
     "Fear of failure", 
     "Meaning in life", 
     "Positive feelings", 
     "Life satisfaction", 
     "Attitudes to competition", 
     "Exposure to bullying", 
     "Student competition", 
     "Student co-operation", 
     "Parents' emotional support", 
     "Sense of belonging")

names(out_scot) <- outlist2

outdf_scot <- map_dfr(out_scot, ~as.tibble(.x[["grade11"]]), .id="outcome") %>%
  mutate(lower=map_dbl(out_scot, ~confint(.x)[2])) %>%
  mutate(upper=map_dbl(out_scot, ~confint(.x)[4])) 
  

###eng
engpisadf2018 <- engpisadf2018 %>%
mutate(cog=5-as.numeric(ST184Q01HA)) %>%
  mutate(cog=scale(cog)) %>%
  mutate(Lifesat=scale(Lifesat))

out_eng <-  map(out_list, ~engpisadf2018 %>%
                   filltar() %>%
                   filter(dob >= ymd("2003_02_01") & dob <=ymd("2003_03_01")) %>%
                   mutate(month=if_else(dob==ymd("2003_02_01"), "Feb", "March")) %>%
                   filter(!is.na(.x)) %>%
                   svyglm(as.formula(paste(.x, "~ month")) , design=.)  
)                
names(out_eng) <- outlist2

outdf_eng <- map_dfr(out_eng, ~tidy(.x, conf.int=TRUE), .id="outcome") %>%
  filter(term=="monthMarch") %>%
  select(outcome, value=estimate, lower=conf.low, upper=conf.high)

outdf <- bind_rows("A) Scotland" =outdf_scot, 
                   "B) England" =outdf_eng,
                   .id="Country")







outpvdf_scot <- function(df, subj) {
  v1 <- df %>%
    select(ends_with(subj)) 
  m1 <- names(v1$variables) %>%
    map(~df %>%
          filltar() %>%
          filter(dob >= ymd("2003_02_01") & dob <=ymd("2003_03_01")) %>%
          mutate(month=if_else(dob==ymd("2003_02_01"), "Feb", "March")) %>%
          mutate(grade11=if_else(grade=="Grade 11", 1, 0)) %>%
          rename(res=.data[[.x]])) %>%
    map(~svyivreg(res ~ grade11 | month, design=.x)) %>%
    MIcombine()
    r1 <- tibble(.id=subj, value=m1$coefficients[2]) %>%
  mutate(lower=confint(m1)[2]) %>%
  mutate(upper=confint(m1)[4]) 
}


read_scot <- outpvdf_scot(scotpisadf2018, "READ")
math_scot <- outpvdf_scot(scotpisadf2018, "MATH")
scie_scot <- outpvdf_scot(scotpisadf2018, "SCIE")

outpvdf_eng <- function(df, subj) {
  v1 <- df %>%
    select(ends_with(subj)) 
  m1 <- names(v1$variables) %>%
    map(~df %>%
          filltar() %>%
          filter(dob >= ymd("2003_02_01") & dob <=ymd("2003_03_01")) %>%
          mutate(month=if_else(dob==ymd("2003_02_01"), "Feb", "March")) %>%
          rename(res=.data[[.x]])) %>%
    map(~svyglm(res ~ month, design=.x)) %>%
    MIcombine()
r1 <- tibble(.id=subj, value=m1$coefficients[2]) %>%
    mutate(lower=confint(m1)[2]) %>%
    mutate(upper=confint(m1)[4]) 
  }


read_eng <- outpvdf_eng(engpisadf2018, "READ")
math_eng <- outpvdf_eng(engpisadf2018, "MATH")
scie_eng <- outpvdf_eng(engpisadf2018, "SCIE")

tests_scot <- bind_rows(Reading=read_scot,
                     Maths=math_scot,
                     Science=scie_scot, .id="outcome")
tests_eng <- bind_rows(Reading=read_eng,
                       Maths=math_eng,
                       Science=scie_eng, .id="outcome")

tests <- bind_rows("A) Scotland"=tests_scot,
                   "B) England"=tests_eng, .id="Country")
tests %>%
ggplot(aes(y=outcome, x=value, xmax=lower, xmin=upper)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  geom_vline(xintercept = 0, colour="red") +
  scale_x_continuous(limits = c(-80, 80),
                     breaks = c(-80, -50, -20, 0, 20, 50, 80)) +
  facet_wrap(vars(Country), ncol=2) +
  labs(x="Scotland S4 vs S5 in Feb & March, England March vs Feb", y=" ",
       caption="Scotland from instrumental variable analysis")
  
 outdf %>% 
   ggplot(aes(y=outcome, x=value, xmax=lower, xmin=upper)) +
   geom_point() +
   geom_errorbarh(height=.1) +
   geom_vline(xintercept = 0, colour="red") +
   scale_x_continuous(limits = c(-.8, .8), 
                      breaks = c(-.8, -.5, -.2, 0, .2, .5, .8)) +
   facet_wrap(vars(Country), ncol=2) +
   labs(x="Scotland S4 vs S5 in Feb & March, England March vs Feb", y=" ",
        caption="Scotland from instrumental variable analysis")
