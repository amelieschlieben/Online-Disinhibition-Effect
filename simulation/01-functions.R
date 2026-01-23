##### Dissociative Anonymity

### Anonymity auf Identity Compartmentalization 
IC_function <- function(AN) {
  comp <- (AN^2)/(AN^2+(1-AN)^2)
  return(comp)
}

# Überprüfung mit Vektor
an <- c(0.2, 0.5, 0.8)
IC_function(an)


## Hier noch Modellparameter einfügen irgendwie
m <- 0.8


### Identity Compartmentalization auf Felt Responsibility 
FR_function <- function(comp, m) {
  feltresp <- m *(1 - 0.8*comp)^3
  return(feltresp)
}


# Überprüfung mit Vektor
FR_function(an)





###### Invisibility
CAI_function <- function(cues) {
  concern <- cues
  return(concern)
}

# Überprüfung mit Vektor
CAI_function(an)


CE_function <- function(concern) {
  courage <- concern * (-1) +1
  return(courage)
}


# Überprüfung mit Vektor 
CE_function(an)




#### Kernfunktion 
SD_function <- function(feltresp, courage, MOD) {
  state_dis <- 0.2 * MOD + (-0.3) * feltresp + 0.2 * courage + (-0.1) * feltresp * courage
  return(state_dis)
}


SD_function(1, 0, 1)



library(ggplot2)

ggplot(df, aes(x = feltresp, y = state_dis, color = factor(courage), group = courage)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ MOD, labeller = label_both) +
  labs(
    x = "feltresp",
    y = "state_dis",
    color = "courage",
    title = "Einfluss von feltresp auf state dis",
    subtitle = "Parallele Linien für courage, getrennt nach MOD"
  ) +
  theme_minimal(base_size = 13)




