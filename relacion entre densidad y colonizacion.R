# ============================================================
# ============================================================
# GRÁFICO FINAL: DENSIDAD DE ESPORAS vs COLONIZACIÓN
# ============================================================

getwd()

# Posición automática del texto
x_texto <- max(datos_cor$Densidad_esporas_g, na.rm = TRUE) * 0.98
y_texto <- max(datos_cor$Colonizacion, na.rm = TRUE) * 0.98

# Etiqueta con resultados de Spearman
etiqueta_cor <- paste0(
  "General: \u03c1 = ", sprintf("%.3f", rho_general),
  "; p = ", sprintf("%.3f", p_general),
  "\nCA: \u03c1 = ", sprintf("%.3f", rho_CA),
  "; p = ", sprintf("%.3f", p_CA),
  "\nCT: \u03c1 = ", sprintf("%.3f", rho_CT),
  "; p = ", sprintf("%.3f", p_CT)
)

# Guardar gráfico en un objeto
fig_correlacion <- ggplot(
  datos_cor,
  aes(
    x = Densidad_esporas_g,
    y = Colonizacion,
    color = Grupo
  )
) +
  geom_point(
    size = 3,
    alpha = 0.85
  ) +
  annotate(
    "text",
    x = x_texto,
    y = y_texto,
    label = etiqueta_cor,
    hjust = 1,
    vjust = 1,
    size = 4
  ) +
  scale_color_manual(
    values = c(
      "CA" = "#1F78B4",
      "CT" = "#E66101"
    )
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  labs(
    x = expression(
      "Densidad de esporas (esporas g"^{-1}*" de suelo)"
    ),
    y = "Colonización micorrízica (%)",
    color = "Grupo"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

fig_correlacion

# Crear carpeta donde se guardarán las figuras
dir.create("Figuras_Correlacion_HMA", showWarnings = FALSE)

# Exportar como PNG en alta calidad
ggsave(
  filename = "Figuras_Correlacion_HMA/Figura_Correlacion_Densidad_vs_Colonizacion.png",
  plot = fig_correlacion,
  width = 18,
  height = 14,
  units = "cm",
  dpi = 600,
  bg = "white"
)

# Exportar como TIFF
ggsave(
  filename = "Figuras_Correlacion_HMA/Figura_Correlacion_Densidad_vs_Colonizacion.tiff",
  plot = fig_correlacion,
  width = 18,
  height = 14,
  units = "cm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

# Exportar como PDF vectorial
ggsave(
  filename = "Figuras_Correlacion_HMA/Figura_Correlacion_Densidad_vs_Colonizacion.pdf",
  plot = fig_correlacion,
  width = 18,
  height = 14,
  units = "cm",
  device = cairo_pdf,
  bg = "white"
)
