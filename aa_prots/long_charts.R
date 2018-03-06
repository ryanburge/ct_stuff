aa <- gss %>% 
  filter(relig <  97) %>% 
  mutate(rel = recode(relig, "1='Protestant'; 2='Catholic'; 4='None'; else='Other'")) %>% 
  filter(race ==2) %>% 
  group_by(year) %>% 
  count(rel) %>% 
  mutate(pct = prop.table(n))

aa %>% 
  ggplot(., aes(x=year, y=pct, group=rel, label=rel, color = rel))  + geom_line() +
  geom_line(size =2)+ long_rb() +
  scale_y_continuous(labels = scales::percent) +
  geom_point(colour = "black", size =2, shape =21, stroke =1.5, show.legend = F) +
  labs(x= "Year", y = "Percent of the Population", title = "Religious Shifting Among African Americans", caption = "Data: GSS (1972-2016)") +
  annotate("text", x = 2016, y = .70, label = "67%", size = 10, family = "Product Sans") +
  annotate("text", x = 2016, y = .21, label = "18%", size = 10, family = "Product Sans") +
  annotate("text", x = 2016, y = .03, label = "6%", size = 10, family = "Product Sans")  +
  annotate("text", x = 2016, y = .11, label = "9%", size = 10, family = "Product Sans") +
  annotate("text", x = 1970.5, y = .87, label = "87%", size = 10, family = "Product Sans") +
  annotate("text", x = 1971, y = .04, label = "4%", size = 10, family = "Product Sans") +
  annotate("text", x = 1971, y = .02, label = "2%", size = 10, family = "Product Sans")  +
  annotate("text", x = 1971, y = .08, label = "8%", size = 10, family = "Product Sans") + theme(legend.text=element_text(size=32)) + theme(legend.position="none")


ggsave(file="D://ct_stuff/long_aa.png", type = "cairo-png", width = 21, height = 15)


att <- gss %>% 
  group_by(reltrad, year) %>% 
  summarise(at = mean(attend, na.rm = TRUE)) %>% 
  ungroup(reltrad)

att <- att %>% mutate(reltrad = as.factor(reltrad)) %>% na.omit()

att <- att %>% mutate(reltrad = recode(reltrad, "1 = 'Evangelical Protesant'; 2 = 'Mainline Protestant'; 3 = 'Black Protestant'; 4 = 'Catholic'; 5 = 'Jewish'; 6 = 'Other Faith'; 7 = 'No Faith'"))


att %>% 
  filter(reltrad != "No Faith") %>% 
  filter(reltrad != "Other Faith") %>%
  filter(reltrad != "Jewish") %>%
  ggplot(., aes(x=year, y=at, group=reltrad, label=reltrad, color =reltrad)) + geom_smooth(se = FALSE, size =2) + long_rb() + scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(3,6), breaks = c(3,4,5,6), labels = c("Several Times a Year", "Once a Month", "2-3x Month", "Nearly Every Week")) +
  annotate("text", x = 2005, y = 3.65, label = "Mainline Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 2005, y = 4.0, label = "Catholic", size = 10, family = "Product Sans") +
  annotate("text", x = 2010, y = 4.7, label = "Black Protestant", size = 10, family = "Product Sans") +
  annotate("text", x = 2005.8, y = 5.1, label = "Evangelical Protestant", size = 10, family = "Product Sans") + theme(legend.text=element_text(size=32)) + theme(legend.position="none") +
  labs(x = "Year", y = "Average Church Attendance", title = "Church Attendance in Christian Traditions", caption = "Data: GSS (1972-2016)")


ggsave(file="D://ct_stuff/long_aa_attend.png", type = "cairo-png", width = 21, height = 15)





