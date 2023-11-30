#No sub/cluster has relevant information by year
df0$loan %>% mutate(year= as.numeric(format(date.status, "%Y"))) %>% group_by(ptf, cluster.ptf, year) %>% 
  summarise(n= n()) %>% View()

#orchestra - winds are all bad loans 
df0$loan %>% group_by(ptf, cluster.ptf, status) %>% 
  summarise(n= n()) %>% View()

#type noup
df0$loan %>% group_by(ptf, cluster.ptf, type) %>% 
  summarise(n= n()) %>% View()

#Vienna in both clusters has all individuals
borrowers %>% group_by(ptf, cluster.ptf, type.bor) %>% 
  summarise(n= n()) %>% View()

#area noup
borrowers %>% group_by(ptf, cluster.ptf, area) %>% 
  summarise(n= n()) %>% View()

#vienna - amadeus had all it's loans over 30k 
borrowers %>% group_by(ptf, cluster.ptf, range.gbv.original) %>% 
  summarise(n= n()) %>% View()

#range.gbv.residual noup
borrowers %>% group_by(ptf, cluster.ptf, range.gbv.residual) %>% 
  summarise(n= n()) %>% View()

#vienna salieri NA 2182 (it only has 30 guarantors) 
borrowers %>% left_join(df0$guarantees, by="id.bor") %>% group_by(ptf, cluster.ptf, id.guarantee) %>% 
  summarise(n= n(), .groups="drop")  %>% View()

#orchestra winds has mostly lien guarantees 
borrowers %>% left_join(df0$guarantees, by="id.bor") %>% group_by(ptf, cluster.ptf, type) %>% 
  summarise(n= n(), .groups="drop")  %>% View()
