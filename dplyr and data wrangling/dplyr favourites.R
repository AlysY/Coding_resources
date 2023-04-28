## Dplyr favourites

# Group_by

# difference between summarise and mutate

# rename

# mutate and replace
  # change the internal values of the object
dat %>%
  mutate(new_var = replace(old_var, old_var != "Target answer", "All non-target answers become this"))
# Change just one of the options
dat %>%
  mutate(new_var = replace(old_var, old_var == "Target answer", "New target"))
