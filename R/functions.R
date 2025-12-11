create_scatterplot <- function(data, selected_species, color) {
  # Filter the data for the specified species and island
  filtered_data <- data %>%
    na.omit() %>%
    filter(species == selected_species)
  
  # Create the scatterplot
  plot <- ggplot(
    filtered_data,
    aes(x = bill_length_mm, y = bill_depth_mm)
  ) +
    geom_point(color=color) +
    labs(
      x = "Bill Length (mm)",
      y = "Bill Depth (mm)",
      title = selected_species
    ) +
    theme(legend.position = "none")
  
  return(plot)
}

# ============================================
# THÈME JOKKOO CONSEIL
# Thème ggplot2 pour analyses d'influence et plaidoyer
# ============================================

jokkoo_theme <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      # Titre principal
      plot.title = element_text(
        color = "#1a3a52",        # Bleu profond professionnel
        size = 16,
        face = "bold",
        hjust = 0,
        margin = margin(b = 10)
      ),
      
      # Sous-titre
      plot.subtitle = element_text(
        color = "#546e7a",
        size = 12,
        hjust = 0,
        margin = margin(b = 15)
      ),
      
      # Légende
      plot.caption = element_text(
        color = "#7f8c8d",
        size = 8,
        hjust = 1,
        face = "italic",
        margin = margin(t = 10)
      ),
      
      # Axes
      axis.title = element_text(
        color = "#2c3e50",
        size = 10,
        face = "bold"
      ),
      
      axis.text.x = element_text(
        color = "#546e7a",
        size = 9,
        margin = margin(t = 5)
      ),
      
      axis.text.y = element_text(
        color = "#546e7a",
        size = 9,
        margin = margin(r = 5)
      ),
      
      # Grille
      panel.grid.major = element_line(
        color = "#ecf0f1",
        size = 0.4
      ),
      
      panel.grid.minor = element_blank(),
      
      # Fond
      panel.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      # Légende
      legend.title = element_text(
        color = "#2c3e50",
        size = 10,
        face = "bold"
      ),
      
      legend.text = element_text(
        color = "#546e7a",
        size = 9
      ),
      
      legend.position = "bottom",
      
      legend.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      # Marges
      plot.margin = margin(15, 15, 15, 15)
    )
}

# ============================================
# PALETTE DE COULEURS JOKKOO CONSEIL
# ============================================

# Palette principale (influence & plaidoyer)
jokkoo_colors <- c(
  primary   = "#1a3a52",  # Bleu profond - Confiance, expertise
  secondary = "#c97a3a",  # Terracotta - Afrique, chaleur
  accent    = "#2e7d6e",  # Vert sapin - Durabilité, commerce équitable
  neutral   = "#546e7a",  # Gris ardoise - Neutralité, analyse
  light     = "#ecf0f1",  # Gris très clair - Arrière-plans
  highlight = "#e8a75d"   # Orange doux - Points d'attention
)

# Fonction pour accéder aux couleurs
jokkoo_pal <- function(name = NULL) {
  if (is.null(name)) {
    return(jokkoo_colors)
  }
  jokkoo_colors[name]
}

# Palette discrète pour catégories (maximum 6)
scale_color_jokkoo <- function(...) {
  discrete_scale(
    "colour", "jokkoo",
    manual_pal(unname(jokkoo_colors[c("primary", "accent", "secondary", 
                                      "highlight", "neutral", "light")])),
    ...
  )
}

scale_fill_jokkoo <- function(...) {
  discrete_scale(
    "fill", "jokkoo",
    manual_pal(unname(jokkoo_colors[c("primary", "accent", "secondary", 
                                      "highlight", "neutral", "light")])),
    ...
  )
}

# Palette continue (pour valeurs numériques)
scale_color_jokkoo_c <- function(...) {
  scale_color_gradient(
    low = jokkoo_colors["light"],
    high = jokkoo_colors["primary"],
    ...
  )
}

scale_fill_jokkoo_c <- function(...) {
  scale_fill_gradient(
    low = jokkoo_colors["light"],
    high = jokkoo_colors["primary"],
    ...
  )
}

