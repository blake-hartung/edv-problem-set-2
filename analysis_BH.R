library(tidyverse)
library(openintro)

# Problem 2a
name_counts <- seattlepets %>%
  filter(species == 'Dog' | species == 'Cat') %>%
  select(species, animal_name) %>%
  count(animal_name, species) %>%
  arrange(-n)

dog_30 <- name_counts %>%
  filter(species == 'Dog') %>%
  slice(1:30)

cat_30_na <- name_counts %>%
  filter(species == 'Cat', ) %>%
  slice(1:30)

num_cat_nas <- as.numeric(cat_30_na %>%
  filter(is.na(animal_name)) %>%
  select(n))

cat_30 <- na.omit(cat_30_na)

top_30s <- rbind(dog_30, cat_30)

cleveland_facet <- ggplot(top_30s) +
  geom_point(aes(x=n, y=reorder(animal_name, species, -n))) +
  facet_wrap(~species, scales='free_y', nrow=2)

cleveland_dog <- ggplot(dog_30) +
  geom_point(aes(x=n, y=reorder(animal_name, n))) +
  ggtitle("Popular Dog Names")

cleveland_cat <- ggplot(cat_30) +
  geom_point(aes(x=n, y=reorder(animal_name, n))) +
  ggtitle('Popular Cat Names')

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
  mutate(prop = n_dog / n, new_prop = dog_prop / name_prop) %>%
  arrange(desc(prop))

dog_prop_plot <- ggplot(dog_proportions) +
  geom_point(aes(x=prop, y=reorder(animal_name, prop))) +
  xlim(0.5, 1.0)

