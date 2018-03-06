gss %>% 
  filter(year > 2007) %>% 
  filter(race ==2) %>% 
  count(denom) %>% 
  na.omit() %>% 
  mutate(pct = prop.table(n)) %>% 
  arrange(-pct) %>% filter(pct > .1) %>% 
  select(-n) %>% 
  mutate(denom = recode(denom, "18 = 'Baptist, Unsure Which'; 60 = 'Other Protestant'; 70 = 'Nondenominational'; 14 = 'Southern Baptist'")) %>% 
  add_row(denom = "All Other Denominations", pct = .228)

bar <- cces16 %>% 
  filter(race ==2) %>% 
  count(religpew_protestant) %>% 
  filter(religpew_protestant != 98)  %>% 
  mutate(pct = prop.table(n)) %>% 
  arrange(-pct) %>% head(5) %>% 
  add_row(religpew_protestant = "All Other Denominations", n = 111, pct = .125) %>% 
  mutate(religpew_protestant = recode(religpew_protestant, "1 = 'Baptist'; 3 = 'Nondenominational'; 2= 'Methodist'; 6 = 'Pentecostal'; 8 = 'Church of Christ'")) %>% 
  select(-n)


bar %>% 
  mutate(pct = round(pct, 3)) %>% 
  ggplot(., aes(x=reorder(religpew_protestant, -pct), y = pct)) + geom_col(color = "black", fill = "mediumorchid4") + bar_rb() +
  scale_y_continuous(labels = scales::percent) + 
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  labs(x= "Protestant Tradition", y = "Percent of African American Protestants", title = "African American Protestants by Denomination", caption = "Data: CCES 2016") +
  theme(plot.title = element_text(size=64)) 

ggsave(file="D://ct_stuff/denom_aa.png", type = "cairo-png", width = 21, height = 15)
