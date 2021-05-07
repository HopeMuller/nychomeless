# load libraries
library(viridis)
library(wesanderson)
library(ggplot2)
library(scales)

# summary statistics' plots for doe homeless high school data

# which schools have a lot of homeless students and how do their overall graduate rates compare
# to the graduation rates for the homeless students in their schools?

# organize data to create such a plot
sch <- doe.sch %>%
  select(id, mod.sch, sch.pop, final.status, sch.grad) %>%
  filter(complete.cases(sch.pop)) %>%
  group_by(mod.sch) %>%
  add_count(mod.sch, name = "tot.hmls") %>%
  mutate(hmls.per = tot.hmls/sch.pop) %>%
  mutate(grad = ifelse(final.status == 2, 1, 0)) %>%
  mutate(tot.grad = sum(grad),
         hmls.grad.per = tot.grad/tot.hmls) %>%
  select(-final.status, - grad, -id) %>%
  distinct() %>%
  filter(tot.hmls > 30) 

# create scatter plot comparing overall graduation rates and homeless graduation rates by school
# note number of homeless students at that school
ggplot(data = sch, aes(y = as.numeric(sch.grad), x = as.numeric(hmls.grad.per), 
                       color = as.numeric(tot.hmls), size = as.numeric(tot.hmls))) +
  geom_point() +
  labs(title ="NYC Public School Homeless Student Gradation Rates, by School",
       subtitle = "Includes Schools with 30 or More Students who Experienced Homelessness During High School",
       y = "Overall High School Graduation Rates\nfor Each School (2019)", 
       x = "Graduation Rates for the Homeless Students in Each School,\n(those who started 9th Grade between 2013-2015)",
       color = "Number of Homeless\nStudents in that School") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous")) +
  # scale_color_viridis(direction = -1) +
  scale_size_continuous(range = c(.25, 6), guide = FALSE) +
  theme_minimal() +
  theme(text = element_text(family = "serif"), 
        axis.text = element_text(size=11), 
        axis.title = element_text(size=11, face="bold"), 
        legend.title = element_text(size=8, face="bold"),
        plot.title = element_text(size=12, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size=8, face = "italic", hjust = 0.5),
        panel.grid.minor = element_line(colour = "gray", linetype = 'solid', size = .2))

# save plot
ggsave("gradrates.png", width = 6, height = 3)

# how to graduation rates compare for homeless students of various races?

# create bar plot to represent the answer to the question
doe.full %>%
  group_by(id) %>%
  filter(eth > 1 & eth < 6) %>%
  mutate(eth = case_when(eth == 5 ~ "White",
                         eth == 2 ~ "Asian or Pacific Islander",
                         eth == 4 ~ "Hispanic",
                         eth == 3 ~ "Black"),
         graduate = case_when(graduate == 1 ~ "Graduated",
                              graduate == 0 ~ "Didn't Finish")) %>%
  mutate(eth = factor(eth, levels = c("White", "Asian or Pacific Islander", "Hispanic", "Black")),
         graduate = factor(graduate, levels = c("Graduated", "Didn't Finish"))) %>%
  ggplot(data = ., aes(x = graduate, fill = graduate)) +
  geom_bar(stat = "count", position = "dodge") + 
  facet_wrap(~ eth, nrow = 1, strip.position = "bottom") +
  labs(title ="NYC Homeless Students' Graduation Outcomes, by Race ",
       subtitle = "includes those who started 9th grade in a public school between 2013-2015",
       x = "Graduation Outcomes as of June 2019, by Race", 
       y = "Number of Students") +
  scale_fill_manual(values = wes_palette("FantasticFox1", 2)) +
  scale_y_continuous(breaks = seq(0, 14000 , 2000), labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "serif"), 
        axis.text = element_text(size=10), 
        axis.title = element_text(size=11, face="bold"), 
        legend.position = "none",
        strip.placement = "outside",
        strip.text = element_text(size = 11, face = "bold"),
        plot.title = element_text(size=12, face="bold", hjust = 0.5), 
        plot.subtitle = element_text(size=9, face = "italic", hjust = 0.5),
        panel.grid.minor = element_line(colour = "gray", linetype = 'solid', size = .2))

# save plot
ggsave("race.grad.png", width = 8, height = 3)