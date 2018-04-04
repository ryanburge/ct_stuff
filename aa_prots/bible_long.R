bible <- gss %>% 
  group_by(year, reltrad) %>% 
  count(bible) %>% 
  mutate(pct = prop.table(n)) %>% 
  filter(bible ==1) %>% 
  ungroup(reltrad) %>% 
  filter(reltrad < 5) %>% 
  mutate(reltrad = recode(reltrad, "1 = 'Evangelical Protestant';
                                    2 = 'Mainline Protestant';
                                    3 = 'Black Protestant';
                                    4 = 'Catholic'" )) %>% 
  na.omit()


bible %>% 
  ggplot(., aes(x=year, y=pct, group=reltrad, label=reltrad, color = reltrad))  + geom_line() +
  geom_line(size =2)+ long_rb() +  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  geom_point(colour = "black", size =2, shape =21, stroke =1.5, show.legend = F) +
  labs(x= "Year", y = "Percent", title = "Rates of Biblical Literalism Among Protestants", caption = "Data: GSS (1984-2016)") +
  annotate("text", x = 2005, y = .60, label = "Black Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 2014, y = .55, label = "Evangelical Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 2003.25, y = .20, label = "Mainline Protestant", size = 10, family = "Product Sans")  +
  annotate("text", x = 2008, y = .16, label = "Catholic", size = 10, family = "Product Sans") +
  theme(legend.text=element_text(size=32)) + theme(legend.position="none")

ggsave(file="D://ct_stuff/long_aa_bible.png", type = "cairo-png", width = 21, height = 15)
