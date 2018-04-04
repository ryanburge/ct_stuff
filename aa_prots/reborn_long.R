reborn <- gss %>% 
  mutate(bagain = recode(reborn, "1=1; else=0")) %>% 
  group_by(reltrad, year) %>% 
  count(bagain) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(bagain ==1) %>% 
  filter(reltrad <5) %>% 
  filter(year > 2007) %>% 
  ungroup(reltrad)


reborn <- reborn %>% mutate(reltrad = recode(reltrad, "1 = 'Evangelical Protesant'; 2 = 'Mainline Protestant'; 3 = 'Black Protestant'; 4 = 'Catholic'; 5 = 'Jewish'; 6 = 'Other Faith'; 7 = 'No Faith'")) %>% 
  select(-bagain, -n)


reborn %>% 
  ggplot(., aes(x=year, y=pct, group=reltrad, label=reltrad, color =reltrad)) + geom_line(size =2)+ long_rb() + scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  geom_point(colour = "black", size =2, shape =21, stroke =1.5, show.legend = F) +
  labs(x = "Year", y = "Percent That Identify as Born-Again", title = "Which Traditions Embrace the Born-Again Label?", caption = "Data: GSS (2008-2016)") +
  annotate("text", x = 2013.75, y = .70, label = "Black Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 2013.5, y = .80, label = "Evangelical Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 2014, y = .38, label = "Mainline Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 2014, y = .245, label = "Catholic", size = 10, family = "Product Sans") + theme(legend.position="none")


ggsave(file="D://ct_stuff/long_reborn_aa.png", type = "cairo-png", width = 21, height = 15)
