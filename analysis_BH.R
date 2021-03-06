library(tidyverse)
library(openintro)

# Problem 2a
dog_30 <- seattlepets %>%
  filter(species == 'Dog') %>%
  count(animal_name) %>%
  arrange(desc(n)) %>%
  slice(1:30)

cat_30_na <- seattlepets %>%
  filter(species == 'Cat', ) %>%
  count(animal_name) %>%
  arrange(desc(n)) %>%
  slice(1:31)

num_cat_nas <- as.numeric(cat_30_na %>%
  filter(is.na(animal_name)) %>%
  select(n))

cat_30 <- na.omit(cat_30_na)


cleveland_dog <- ggplot(dog_30) +
  geom_point(aes(x=n, y=reorder(animal_name, n)), color='blue') +
  ggtitle("Popular Dog Names") +
  ylab('Dog Name') +
  xlab('Count') +
  theme_linedraw()

cleveland_cat <- ggplot(cat_30) +
  geom_point(aes(x=n, y=reorder(animal_name, n)), color = 'red') +
  ggtitle('Popular Cat Names') +
  ylab('Cat Name') +
  xlab('Count') +
  theme_linedraw()

# Problem 2b

gather_name_prop <- seattlepets %>%
  filter(species != 'Goat' | species!= 'Pig') %>%
  count(animal_name) %>%
  mutate(name_prop = n / nrow(seattlepets %>% filter(species != 'Goat' | species!= 'Pig')))

gather_dog_prop <- seattlepets %>%
  filter(species == 'Dog') %>%
  count(animal_name, name = 'n_dog') %>%
  mutate(dog_prop = n_dog / nrow(seattlepets %>% filter(species == 'Dog')))


# make this (n_dog / total_dogs) / (n_total/ total_animals)
dog_proportions <- left_join(gather_dog_prop, gather_name_prop, by = 'animal_name') %>%
  arrange(desc(dog_prop)) %>%
  slice(1:30) %>%
  mutate(prop = n_dog / n) %>%
  arrange(desc(prop))

dog_prop_plot <- ggplot(dog_proportions) +
  geom_point(aes(x=prop, y=reorder(animal_name, prop)), color='green') +
  xlim(0.5, 1.0) +
  ggtitle("") +
  theme_linedraw()

# problem 2c
both <- seattlepets %>%
  filter(species == 'Dog' | species == 'Cat') %>%
  count(animal_name, name = 'n') %>%
  arrange(desc(n))

totals <- data.frame(both)

totals$species = 'Both'

cats_and_dogs <- seattlepets %>%
  filter(species == 'Dog' | species == 'Cat') %>%
  select(species, animal_name) %>%
  count(animal_name, species)

long_data <- rbind(totals, cats_and_dogs)

full_df <- left_join(both, long_data, by='animal_name') %>%
  arrange(desc(n.x)) %>%
  slice(4:93)

cleveland_totals <- ggplot(full_df) +
  geom_point(aes(x=n.y, y=reorder(animal_name, n.x), color=species)) +
  ylab('Animal Name') +
  xlab('Count') +
  ggtitle('Most popular dog and cat names') +
  theme_linedraw()

# problem 2d

popular_names <- na.omit(cats_and_dogs %>%
  filter(n >= 2) %>%
  pivot_wider(names_from = species, values_from = n)) %>%
  mutate(prop = Cat / Dog)

# best method for deciding dog or cat name is a linear fit
scatter <- ggplot(popular_names, aes(x = Dog, y = Cat)) +
  geom_point(alpha = 0.1,
             stroke = 0,
             color = 'blue') 

fit <- broom::tidy(lm(Cat ~ 0 + Dog, data = popular_names))
slope <- as.numeric(fit$estimate)
slope_high <- slope * 1.1
slope_low <- slope * 0.9

popular_names2 <- popular_names %>%
  mutate(bias = case_when(prop < slope_low ~ 'Dog',
                          prop > slope_high ~ 'Cat',
                          TRUE  ~ 'Neutral'))

scattergories <- ggplot(popular_names2, aes(x = Dog, y = Cat, color = bias)) +
  geom_point(alpha = 0.3,
             stroke = 0) +
  geom_text(data = subset(popular_names2, Dog > 150 | Cat > 50),
            aes(label = animal_name),
            nudge_x = 10,
            size =2.5,
            check_overlap = TRUE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'))
        




