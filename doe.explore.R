# exploratory analysis for doe homeless high school data

# doe homelessness years of attendance
doe.full %>%
  group_by(id) %>%
  ggplot(data = ., aes(x = comp.grades, fill = as.factor(any.repeats))) +
  geom_bar(stat = "count") + 
  facet_grid(. ~ as.factor(any.repeats)) + 
  labs(title ="Final Grade-Level for Homeless High School Students",
       subtitle = "Registered for Freshmen Year at a NYC Public School Between 2012-2015",
       x = "Final Grade-Level Which Individual Enrolled", 
       y = "Number of Students", 
       fill = "Did Student Repeat any Grade?") +
  theme_minimal() +
  theme(text = element_text(family = "serif"), 
        axis.text = element_text(size=11), 
        axis.title = element_text(size=11, face="bold"), 
        legend.title = element_text(size=8, face="bold"), 
        plot.title = element_text(size=12, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size=9, face = "italic", hjust = 0.5),
        panel.grid.minor = element_line(colour = "gray", linetype = 'solid', size = .2))
