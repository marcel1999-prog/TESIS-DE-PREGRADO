# ANÁLISIS DE DENSIDAD DE ESPORAS DE HMA
# ============================================================

# 1. Cargar paquetes
library(readxl)
library(dplyr)
library(ggplot2)


# 2. Importar base de datos
datos <- read_excel(
  file.choose(),
  sheet = "Total_por_accesion")

# Revisar estructura
View(datos)
str(datos)


# 3. Calcular densidad de esporas
# El total corresponde a 3 submuestras de 2 g = 6 g de suelo

datos <- datos %>%
  mutate(
    Densidad_esporas_g = `Total de esporas` / 6
  )

View(datos)


# ============================================================
# 4. ESTADÍSTICA DESCRIPTIVA
# ============================================================

# Descriptivos generales
descriptivos_general <- datos %>%
  summarise(
    n = sum(!is.na(Densidad_esporas_g)),
    Media = mean(Densidad_esporas_g, na.rm = TRUE),
    DE = sd(Densidad_esporas_g, na.rm = TRUE),
    Mediana = median(Densidad_esporas_g, na.rm = TRUE),
    Minimo = min(Densidad_esporas_g, na.rm = TRUE),
    Maximo = max(Densidad_esporas_g, na.rm = TRUE)
  )

descriptivos_general


# Descriptivos por grupo
descriptivos_grupo <- datos %>%
  group_by(Grupo) %>%
  summarise(
    n = sum(!is.na(Densidad_esporas_g)),
    Media = mean(Densidad_esporas_g, na.rm = TRUE),
    DE = sd(Densidad_esporas_g, na.rm = TRUE),
    Mediana = median(Densidad_esporas_g, na.rm = TRUE),
    Minimo = min(Densidad_esporas_g, na.rm = TRUE),
    Maximo = max(Densidad_esporas_g, na.rm = TRUE)
  )

descriptivos_grupo


# ============================================================
# 5. PRUEBA DE NORMALIDAD
# ============================================================

# Grupo CA
shapiro_CA <- shapiro.test(
  datos$Densidad_esporas_g[datos$Grupo == "CA"]
)

shapiro_CA


# Grupo CT
shapiro_CT <- shapiro.test(
  datos$Densidad_esporas_g[datos$Grupo == "CT"]
)

shapiro_CT


# ============================================================
# 6. COMPARACIÓN ENTRE CA Y CT
# Prueba de Mann-Whitney
# ============================================================

mann_whitney <- wilcox.test(
  Densidad_esporas_g ~ Grupo,
  data = datos,
  exact = FALSE
)

mann_whitney


# ============================================================
# ============================================================
# 7. BOXPLOT CA vs CT
# ============================================================

p_valor <- mann_whitney$p.value

fig_boxplot <- ggplot(
  datos,
  aes(
    x = Grupo,
    y = Densidad_esporas_g,
    fill = Grupo
  )
) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    alpha = 0.7
  ) +
  geom_jitter(
    width = 0.10,
    size = 2
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(datos$Densidad_esporas_g, na.rm = TRUE) * 1.08,
    label = paste0(
      "Mann-Whitney: p = ",
      format.pval(p_valor, digits = 3)
    ),
    size = 4
  ) +
  expand_limits(
    y = max(datos$Densidad_esporas_g, na.rm = TRUE) * 1.15
  ) +
  labs(
    x = "Grupo",
    y = expression(
      "Densidad de esporas (esporas g"^{-1}*" de suelo)"
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  )

fig_boxplot
# ============================================================
# ============================================================
# 8. DENSIDAD DE ESPORAS POR ACCESIÓN - CA
# ============================================================

fig_CA <- ggplot(
  filter(datos, Grupo == "CA"),
  aes(
    x = reorder(Accesión, Densidad_esporas_g),
    y = Densidad_esporas_g
  )
) +
  geom_col(
    width = 0.75,
    fill = "#1F78B4",
    color = "black",
    linewidth = 0.3
  ) +
  labs(
    x = "Accesión",
    y = expression(
      "Densidad de esporas (esporas g"^{-1}*" de suelo)"
    ),
    title = "Densidad de esporas en accesiones CA"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1
    ),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

fig_CA

# ============================================================
## ============================================================
# 9. DENSIDAD DE ESPORAS POR ACCESIÓN - CT
# ============================================================

fig_CT <- ggplot(
  filter(datos, Grupo == "CT"),
  aes(
    x = reorder(Accesión, Densidad_esporas_g),
    y = Densidad_esporas_g
  )
) +
  geom_col(
    width = 0.75,
    fill = "#E66101",
    color = "black",
    linewidth = 0.3
  ) +
  labs(
    x = "Accesión",
    y = expression(
      "Densidad de esporas (esporas g"^{-1}*" de suelo)"
    ),
    title = "Densidad de esporas en accesiones CT"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1
    ),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

fig_CT

# ============================================================
# 10. EXPORTAR FIGURAS EN ALTA CALIDAD
# ============================================================

# Crear carpeta para guardar figuras
dir.create(
  "Figuras_Densidad_HMA",
  showWarnings = FALSE
)


# ------------------------------------------------------------
# BOXPLOT
# ------------------------------------------------------------

ggsave(
  filename = "Figuras_Densidad_HMA/Figura_1_Boxplot_CA_vs_CT.tiff",
  plot = fig_boxplot,
  width = 16,
  height = 14,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  filename = "Figuras_Densidad_HMA/Figura_1_Boxplot_CA_vs_CT.pdf",
  plot = fig_boxplot,
  width = 16,
  height = 14,
  units = "cm",
  device = cairo_pdf
)


# ------------------------------------------------------------
# DENSIDAD CA
# ------------------------------------------------------------

ggsave(
  filename = "Figuras_Densidad_HMA/Figura_2_Densidad_CA.tiff",
  plot = fig_CA,
  width = 22,
  height = 15,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  filename = "Figuras_Densidad_HMA/Figura_2_Densidad_CA.pdf",
  plot = fig_CA,
  width = 22,
  height = 15,
  units = "cm",
  device = cairo_pdf
)


# ------------------------------------------------------------
# DENSIDAD CT
# ------------------------------------------------------------

ggsave(
  filename = "Figuras_Densidad_HMA/Figura_3_Densidad_CT.tiff",
  plot = fig_CT,
  width = 22,
  height = 15,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

ggsave(
  filename = "Figuras_Densidad_HMA/Figura_3_Densidad_CT.pdf",
  plot = fig_CT,
  width = 22,
  height = 15,
  units = "cm",
  device = cairo_pdf
)

