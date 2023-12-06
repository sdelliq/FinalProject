#This is the red flag that first caught my eye:
#df0$agreement.summary %>% filter(residual<0) %>% summarise(negative.residuals = sum(residual)*(-1))
# € 752.856 were paid extra according to agreement.summary -> so I'll go check in collections what makes sense

#n_distinct(df0$agreement.summary$id.bor) == df0$agreement.summary %>% filter(id.bor %in% df0$loan$id.bor) %>%  summarise(n.borrowers = n_distinct(id.bor))
#all my borrowers in agreement.summary exist on loans

#I take the info from collection that's usefull to check with agreement.summary
#####
#Here I'm trying to analyse which dates I want to take from collections
#sum(temp.vars$agreement.collection$paid) - sum(temp.vars$agreement.collection$paid.in.collections) 
  #NO FILTER → 767  → closer to the weird residuals
  #FILTER date>date.agreement → 772k
  #filter(date>date.agreement & date<date.end) → 497k 
# So I decide not to filter at the beggining, mostly since I'm filtering the amount when a borrower has more than one agreement
#####
temp.vars$agreement.collection <- df0$collection %>% filter(class=="extrajudicial" & type=="agreement") %>%
  left_join(df0$agreement.summary, by=c("id.bor", "id.group"), relationship = "many-to-many") %>%
  group_by(id.bor) %>% 
  mutate(multiple.agreements= ifelse(n_distinct(id.agreement) > 1, TRUE, FALSE)) %>%  ungroup() %>%
  group_by(id.agreement, id.bor) %>%
  #filter(date>date.agreement) %>% # & date<date.end) %>%
  summarise(
    across(c(amount.agreement, status, residual, paid), first),
    paid.in.collections= ifelse(multiple.agreements==TRUE ,
                                sum(amount[date>date.agreement & date<=date.last.payment]), #Usually then there are two status, one failed or closedand I can take the date of the last.payment 
                                sum(amount)),
    date.last.paid=max(date),
    .groups = "drop"
  ) %>%
  distinct() %>%
  filter(paid!=paid.in.collections)   

#I see the cases in which the previous logic didn't work well → In this case only one weirdo with two active agreements at the same time
temp.vars$agreement.collection.exeptions <- find_duplicates_couple(temp.vars$agreement.collection, id.bor, paid.in.collections) 
#temp.vars$agreement.collection.exeptions %>% View()
#I decide to ignore the cases in which the amount paid in collections is duplicated (it's not clear to which agreement was the paid to) 
temp.vars$agreement.collection <- temp.vars$agreement.collection %>% filter(!temp.vars$agreement.collection$id.bor %in% temp.vars$agreement.collection.exeptions$id.bor)

#sum(temp.vars$agreement.collection$paid) - sum(temp.vars$agreement.collection$paid.in.collections) # = 775k → 18% of the total (2.645.861) #sum(df0$agreement.summary$paid) 
  #The amount paid in agreement is 775k bigger than the one in collections. Considering the 752k of negative residuals, I decide to keep the info from collections


#Here I create my agreement.summary to then create reports based on it :)
qt.months <- 6 #I say an agreement failed when they didn't pay for 6 months
temp.vars$agreement.summary <- df0$agreement.summary %>% left_join(temp.vars$agreement.collection %>% 
                                                                     select(id.agreement, paid.in.collections,date.last.paid), 
                                    by=c("id.agreement") ) %>% 
  mutate(
    gbv.agreement =ifelse(gbv.agreement < amount.agreement, amount.agreement, gbv.agreement),
    paid = ifelse(!is.na(paid.in.collections), paid.in.collections, paid),
    residual= ifelse(residual<0, 0, amount.agreement - paid),
    amount.agreement = ifelse(residual<0, paid.in.collections, amount.agreement),
    residual= ifelse(residual<0, amount.agreement - paid, residual),
    date.last.payment = ifelse(is.na(date.last.payment) | date.last.paid>date.last.payment, date.last.paid, date.last.payment),
    date.last.payment = as.Date(date.last.payment),
    date.end = ifelse(is.na(date.end), date.start %m+% months(length), date.end), #if it's NA I calculate it
    date.end=as.Date(date.end),
      
    discount = ifelse(gbv.agreement != 0 & gbv.agreement>amount.agreement, 1- amount.agreement/gbv.agreement,0) %>% round(2),
    #quantile(created.tables$agreement.summary$discount, probs = c(0.25, 0.5, 0.75),na.rm = T)
    range.discount = cut(
      discount,
      breaks = c(0.00, 0.05, 0.2, 0.5, Inf),  # Adjusted breaks
      labels = c("0-5%", "5-20%", "20-50%", "50%+"),
      include.lowest = TRUE
    ),
    range.amount = cut(
      amount.agreement,
      breaks = c(0, 2500, 5000, 10000, 20000, Inf),  # Adjusted breaks
      labels = c("0-2.5k", "2.5k-5k", "5k-10k", "10k-20k", "20k+"),
      include.lowest = TRUE
    ),
    status = case_when( 
      date.start > date.cutoff ~ 'proposal',
      (paid == 0) &  as.numeric(difftime(date.cutoff, date.start, units = "days")) / 30.44 > qt.months ~ 'failed',
      (paid == 0) &  as.numeric(difftime(date.cutoff, date.start, units = "days")) / 30.44 <  qt.months ~ 'active',
      is.na(date.last.payment) ~ status,
      (paid != amount.agreement) &  as.numeric(difftime(date.cutoff, date.last.payment, units = "days")) / 30.44 > qt.months ~ 'failed',
      (paid != amount.agreement) &  as.numeric(difftime(date.cutoff, date.last.payment, units = "days")) / 30.44 < qt.months ~ 'active',
      paid == amount.agreement ~ 'closed',
      TRUE ~ status
    )
    #With my calculations I have 92 active instead of 85, 249 instead of 252, and 334 instead of 338 → Only 14 changes → I'll keep them
    
  )

#I take the cluster from loans (I know each borrower only exists in one ptf)
temp.vars$agreement.loan <- temp.vars$agreement.summary %>% select(id.agreement, id.bor, id.group) %>%
  left_join(df0$loan %>% select(id.bor, id.group, ptf, gbv.original), by=c("id.bor", "id.group"), relationship = "many-to-many") %>% 
    group_by(id.agreement) %>% summarise(
      ptf=first(ptf),
      gbv.original=max(gbv.original)
    )
temp.vars$agreement.summary <- temp.vars$agreement.summary %>% 
  left_join(temp.vars$agreement.loan, by="id.agreement") %>%
  mutate(gbv.original= ifelse(gbv.original==0, pmax(amount.agreement, gbv.agreement, na.rm=T)*1.5, gbv.original)) 
#If I don't have the information for the gbv.original I assume it's at least 5% bigger than the gbv.agreement

#temp.vars$agreement.summary %>% View()  

temp.vars$dates_to_calculate <- temp.vars$agreement.summary %>%
  filter(paid > 0 & is.na(date.last.payment)) %>%
  mutate(payments_made = as.integer(paid / (amount.agreement / n.instalment))) %>%
  mutate(new_date =  date.start %m+% months(payments_made),
         new_date = ifelse(new_date>date.cutoff, date.cutoff, new_date)) %>%
  select(id.agreement, new_date) 

temp.vars$agreement.summary <- temp.vars$agreement.summary  %>%
  left_join(temp.vars$dates_to_calculate , by = "id.agreement") %>%
  mutate(date.last.payment = ifelse(is.na(date.last.payment) & !is.na(new_date), new_date, date.last.payment),
         date.last.payment = as.Date(date.last.payment)) %>%
  select(-new_date)




created.tables$agreement.summary <- temp.vars$agreement.summary

