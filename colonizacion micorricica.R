# ============================================================
# ANÁLISIS DE COLONIZACIÓN MICORRÍZICA
# ============================================================

library(readxl)
library(dplyr)
library(ggplot2)


# ============================================================
# 1. IMPORTAR BASE DE DENSIDAD
# Seleccionar: "densidad de esporas procesar.xlsx"
# ============================================================

densidad <- read_excel(
  file.choose(),
  sheet = "Total_por_accesion"
)


# ============================================================
# 2. IMPORTAR BASE DE COLONIZACIÓN
# Seleccionar: "base_datos_tincion.xlsx"
# ============================================================

colonizacion <- read_excel("~/TESIS_PREGRADO/base_datos_tincion.xlsx",
                           sheet = "fb")


# ============================================================
# 3. PREPARAR LOS DATOS DE COLONIZACIÓN
# ============================================================

colonizacion <- colonizacion %>%
  mutate(
    Accesión = gsub("_", "", muestra),
    Grupo = substr(Accesión, 1, 2)
  )


# ============================================================
# 4. PROMEDIO DE R1, R2 Y R3 POR ACCESIÓN
# ============================================================

colonizacion_promedio <- colonizacion %>%
  group_by(Accesión, Grupo) %>%
  summarise(
    Colonizacion = mean(porcentaje, na.rm = TRUE),
    .groups = "drop"
  )


# ============================================================
# 5. CONSERVAR SOLO LAS 35 ACCESIONES USADAS EN DENSIDAD
# ============================================================

colonizacion_analisis <- colonizacion_promedio %>%
  semi_join(
    densidad %>% select(Accesión),
    by = "Accesión"
  ) %>%
  arrange(Grupo, Accesión)

View(colonizacion_analisis)


# Verificar: deben quedar 13 CA + 22 CT = 35
colonizacion_analisis %>%
  count(Grupo)

nrow(colonizacion_analisis)


# ============================================================
# 6. ESTADÍSTICA DESCRIPTIVA GENERAL
# ============================================================

descriptivos_general <- colonizacion_analisis %>%
  summarise(
    n = n(),
    Media = mean(Colonizacion),
    DE = sd(Colonizacion),
    Mediana = median(Colonizacion),
    Minimo = min(Colonizacion),
    Maximo = max(Colonizacion)
  )

descriptivos_general


# ============================================================
# 7. ESTADÍSTICA DESCRIPTIVA POR GRUPO
# ============================================================

descriptivos_grupo <- colonizacion_analisis %>%
  group_by(Grupo) %>%
  summarise(
    n = n(),
    Media = mean(Colonizacion),
    DE = sd(Colonizacion),
    Mediana = median(Colonizacion),
    Minimo = min(Colonizacion),
    Maximo = max(Colonizacion)
  )

descriptivos_grupo


# ============================================================
# 8. NORMALIDAD: SHAPIRO-WILK
# ============================================================

shapiro_CA <- shapiro.test(
  colonizacion_analisis$Colonizacion[
    colonizacion_analisis$Grupo == "CA"
  ]
)

shapiro_CT <- shapiro.test(
  colonizacion_analisis$Colonizacion[
    colonizacion_analisis$Grupo == "CT"
  ]
)

shapiro_CA
shapiro_CT


# ============================================================
# 9. COMPARACIÓN CA vs CT
# t DE WELCH
# ============================================================

comparacion <- t.test(
  Colonizacion ~ Grupo,
  data = colonizacion_analisis,
  var.equal = FALSE
)

comparacion


# ============================================================
# 10. COLORES
# ============================================================

colores_grupo <- c(
  "CA" = "#1F78B4",
  "CT" = "#E66101"
)


# ============================================================
# ============================================================
# 11. BOXPLOT CA vs CT
# ============================================================

p_valor <- comparacion$p.value

fig_boxplot_col <- ggplot(
  colonizacion_analisis,
  aes(
    x = Grupo,
    y = Colonizacion,
    fill = Grupo
  )
) +
  geom_boxplot(
    width = 0.55,
    alpha = 0.85,
    color = "black",
    linewidth = 0.6,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = Grupo),
    width = 0.10,
    size = 2.4,
    alpha = 0.9,
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 85,
    label = paste0(
      "t de Welch: p = ",
      format.pval(p_valor, digits = 3)
    ),
    size = 4
  ) +
  scale_fill_manual(values = colores_grupo) +
  scale_color_manual(values = colores_grupo) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    x = "Grupo",
    y = "Colonización micorrízica (%)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  )

fig_boxplot_col

# ============================================================
# ============================================================
# 12. COLONIZACIÓN POR ACCESIÓN - CA
# ============================================================

fig_CA_col <- ggplot(
  filter(colonizacion_analisis, Grupo == "CA"),
  aes(
    x = reorder(Accesión, Colonizacion),
    y = Colonizacion
  )
) +
  geom_col(
    width = 0.75,
    fill = "#1F78B4",
    color = "black",
    linewidth = 0.3
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    x = "Accesión",
    y = "Colonización micorrízica (%)",
    title = "Colonización micorrízica en accesiones CA"
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

fig_CA_col

# ============================================================
# ============================================================
# 13. COLONIZACIÓN POR ACCESIÓN - CT
# ============================================================

fig_CT_col <- ggplot(
  filter(colonizacion_analisis, Grupo == "CT"),
  aes(
    x = reorder(Accesión, Colonizacion),
    y = Colonizacion
  )
) +
  geom_col(
    width = 0.75,
    fill = "#E66101",
    color = "black",
    linewidth = 0.3
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    x = "Accesión",
    y = "Colonización micorrízica (%)",
    title = "Colonización micorrízica en accesiones CT"
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

fig_CT_col

# ============================================================
# 14. EXPORTAR FIGURAS EN ALTA CALIDAD
# ============================================================

# Crear carpeta de salida
dir.create(
  "Figuras_Colonizacion_HMA",
  showWarnings = FALSE
)

# ------------------------------------------------------------
# FIGURA 1. BOXPLOT CA vs CT
# ------------------------------------------------------------

ggsave(
  filename = "Figuras_Colonizacion_HMA/Figura_1_Boxplot_Colonizacion_CA_vs_CT.tiff",
  plot = fig_boxplot_col,
  width = 16,
  height = 14,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = "Figuras_Colonizacion_HMA/Figura_1_Boxplot_Colonizacion_CA_vs_CT.pdf",
  plot = fig_boxplot_col,
  width = 16,
  height = 14,
  units = "cm",
  device = cairo_pdf,
  bg = "white"
)

# ------------------------------------------------------------
# FIGURA 2. COLONIZACIÓN EN ACCESIONES CA
# ------------------------------------------------------------

ggsave(
  filename = "Figuras_Colonizacion_HMA/Figura_2_Colonizacion_CA.tiff",
  plot = fig_CA_col,
  width = 22,
  height = 15,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = "Figuras_Colonizacion_HMA/Figura_2_Colonizacion_CA.pdf",
  plot = fig_CA_col,
  width = 22,
  height = 15,
  units = "cm",
  device = cairo_pdf,
  bg = "white"
)

# ------------------------------------------------------------
# FIGURA 3. COLONIZACIÓN EN ACCESIONES CT
# ------------------------------------------------------------

ggsave(
  filename = "Figuras_Colonizacion_HMA/Figura_3_Colonizacion_CT.tiff",
  plot = fig_CT_col,
  width = 22,
  height = 15,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = "Figuras_Colonizacion_HMA/Figura_3_Colonizacion_CT.pdf",
  plot = fig_CT_col,
  width = 22,
  height = 15,
  units = "cm",
  device = cairo_pdf,
  bg = "white"
)



ggsave(
  "Figuras_Colonizacion_HMA/Figura_1_Boxplot_Colonizacion_CA_vs_CT.png",
  plot = fig_boxplot_col,
  width = 16,
  height = 14,
  units = "cm",
  dpi = 600,
  bg = "white"
)

ggsave(
  "Figuras_Colonizacion_HMA/Figura_2_Colonizacion_CA.png",
  plot = fig_CA_col,
  width = 22,
  height = 15,
  units = "cm",
  dpi = 600,
  bg = "white"
)

ggsave(
  "Figuras_Colonizacion_HMA/Figura_3_Colonizacion_CT.png",
  plot = fig_CT_col,
  width = 22,
  height = 15,
  units = "cm",
  dpi = 600,
  bg = "white"
)
