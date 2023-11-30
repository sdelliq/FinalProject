#I take the info from collection that's usefull to check with agreement.summary

temp.vars$agreement.collection <- df0$collection %>% filter(class=="extrajudicial" & type=="agreement") %>%
  left_join(df0$agreement.summary, by=c("id.bor", "id.group"), relationship = "many-to-many") %>%
  group_by(id.bor) %>% 
  mutate(multiple.status= ifelse(n_distinct(status) > 1, TRUE, FALSE)) %>%  ungroup() %>%
  group_by(id.agreement, id.bor) %>%
  filter(date>date.agreement & date<date.end) %>%
  summarise(
    across(c(amount.agreement, status, residual, paid), first),
    paid.in.collections= ifelse(multiple.status==TRUE ,
                                sum(amount[date<=date.last.payment]),
                                sum(amount)),
    date.last.paid=max(date),
    .groups = "drop"
  ) %>%
  distinct() %>%
  filter(paid!=paid.in.collections)   

temp.vars$agreement.collection.exeptions <- find_duplicates_couple(temp.vars$agreement.collection, id.bor, paid.in.collections) 
temp.vars$agreement.collection <- temp.vars$agreement.collection %>% filter(!temp.vars$agreement.collection$id.bor %in% temp.vars$agreement.collection.exeptions$id.bor)


#sum(temp.vars$agreement.collection$paid.in.collections) 
  #854887.5 when filter(date>date.agreement & date<date.end) 
  #1250128 without the filter -> it-s a big difference considering I-m not sure if i should take it, so i wont
temp.vars$agreement.collection  %>% View()


#There's a difference of 475k between the info in collections and the info in agreement. It's 20% of the total (2.645.861)
#since the difference is positive, the amount paid according to agreement is bigger than the one in collections
#Being this the case, I'll update the info of agreement from collections, since this is usually the most "correct" one
  #sum(temp.vars$agreement.collection$paid) - sum(temp.vars$agreement.collection$paid.in.collections)
  #sum(df0$agreement.summary$paid)

temp.vars$agreement.summary <- df0$agreement.summary %>% left_join(temp.vars$agreement.collection %>% 
                                                                     select(id.agreement, paid.in.collections,date.last.paid), 
                                    by=c("id.agreement") ) %>% 
  mutate(
    gbv.agreement =ifelse(gbv.agreement < amount.agreement, amount.agreement, gbv.agreement),
    paid = ifelse(!is.na(paid.in.collections), paid.in.collections, paid),
    residual= ifelse(residual<0, 0, amount.agreement - paid),
    amount.agreement = ifelse(residual<0, paid.in.collections, amount.agreement),
    residual= ifelse(residual<0, amount.agreement - paid, residual),
    date.last.payment = ifelse(date.last.paid>date.last.payment, date.last.paid, date.last.payment),
    date.last.payment = as.Date(date.last.payment),
    
    discount = ifelse(gbv.agreement != 0 & gbv.agreement>amount.agreement, 1- amount.agreement/gbv.agreement,0) %>% round(2),
    range.discount = cut(
      discount,
      breaks = c(0.00, 0.25, 0.5, 0.6, 0.7, Inf),  # Adjusted breaks
      labels = c("0-25%", "25-50%", "50-60%", "60-70%", "70%+"),
      include.lowest = TRUE
    ),
    range.amount = cut(
      amount.agreement,
      breaks = c(0, 2500, 5000, 10000, 20000, Inf),  # Adjusted breaks
      labels = c("0-2.5k", "2.5k-5k", "5k-10k", "10k-20k", "20k+"),
      include.lowest = TRUE
    )
    ) 
#temp.vars$agreement.summary %>% View()  
  
  