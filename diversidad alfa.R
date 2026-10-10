
# DIVERSIDAD ALFA DE HMA POR ACCESIÓN
# RIQUEZA - SHANNON - SIMPSON
# =========================================================


# 1. CARGAR LIBRERÍAS

library(tidyverse)
library(readxl)
library(vegan)


# 2. CARGAR BASE DE DATOS

datos <- read_excel(
  "C:/Users/fvygh/Documents/TESIS_PREGRADO/base_de_datos_HMA.xlsx",
  sheet = "fb"
)


# 3. CREAR IDENTIFICADOR DEL TAXÓN

datos <- datos %>%
  mutate(
    taxon = paste(genero, especie)
  )


# 4. SUMAR ABUNDANCIA POR ACCESIÓN Y TAXÓN

tabla_taxon <- datos %>%
  group_by(accesion, taxon) %>%
  summarise(
    abundancia = sum(cantidad),
    .groups = "drop"
  )


# 5. CREAR MATRIZ DE ABUNDANCIAS
# Filas = accesiones
# Columnas = taxones

matriz_taxon <- tabla_taxon %>%
  pivot_wider(
    names_from = taxon,
    values_from = abundancia,
    values_fill = 0
  ) %>%
  column_to_rownames("accesion") %>%
  as.matrix()


# 6. CREAR METADATOS DE LAS ACCESIONES

metadata <- datos %>%
  distinct(accesion, grupo)

metadata <- metadata[
  match(rownames(matriz_taxon), metadata$accesion),
]


# 7. CALCULAR ABUNDANCIA TOTAL

abundancia_total <- rowSums(matriz_taxon)


# 8. CALCULAR RIQUEZA DE TAXONES

riqueza <- specnumber(matriz_taxon)


# 9. CALCULAR ÍNDICE DE SHANNON

shannon <- diversity(
  matriz_taxon,
  index = "shannon"
)


# 10. CALCULAR ÍNDICE DE SIMPSON
# vegan devuelve 1 - D

simpson <- diversity(
  matriz_taxon,
  index = "simpson"
)


# 11. CREAR TABLA FINAL DE DIVERSIDAD ALFA

diversidad_alfa <- data.frame(
  accesion = rownames(matriz_taxon),
  grupo = metadata$grupo,
  abundancia_total = abundancia_total,
  riqueza = riqueza,
  shannon = shannon,
  simpson = simpson
)


# 12. VISUALIZAR RESULTADOS

View(diversidad_alfa)

print(diversidad_alfa)

# =========================================================
# 13. RESUMEN DE DIVERSIDAD POR GRUPO
# =========================================================

resumen_diversidad <- diversidad_alfa %>%
  group_by(grupo) %>%
  summarise(
    n = n(),
    
    riqueza_media = mean(riqueza),
    riqueza_sd = sd(riqueza),
    
    shannon_media = mean(shannon),
    shannon_sd = sd(shannon),
    
    simpson_media = mean(simpson),
    simpson_sd = sd(simpson),
    
    abundancia_media = mean(abundancia_total),
    abundancia_sd = sd(abundancia_total)
  )

print(resumen_diversidad)

# =========================================================
# 14. BOXPLOT DE RIQUEZA
# =========================================================

ggplot(
  diversidad_alfa,
  aes(x = grupo, y = riqueza, fill = grupo)
) +
  geom_boxplot(
    width = 0.55,
    alpha = 0.7
  ) +
  geom_jitter(
    width = 0.10,
    size = 2
  ) +
  labs(
    x = "Grupo",
    y = "Riqueza de taxones",
    title = "Riqueza de HMA entre grupos CA y CT"
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )

# =========================================================
# 15. BOXPLOT DE SHANNON
# =========================================================

ggplot(
  diversidad_alfa,
  aes(x = grupo, y = shannon, fill = grupo)
) +
  geom_boxplot(
    width = 0.55,
    alpha = 0.7
  ) +
  geom_jitter(
    width = 0.10,
    size = 2
  ) +
  labs(
    x = "Grupo",
    y = "Índice de Shannon (H')",
    title = "Diversidad de Shannon de HMA entre grupos CA y CT"
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )

# =========================================================
# 16. BOXPLOT DE SIMPSON
# =========================================================

ggplot(
  diversidad_alfa,
  aes(x = grupo, y = simpson, fill = grupo)
) +
  geom_boxplot(
    width = 0.55,
    alpha = 0.7
  ) +
  geom_jitter(
    width = 0.10,
    size = 2
  ) +
  labs(
    x = "Grupo",
    y = "Índice de Simpson (1 - D)",
    title = "Diversidad de Simpson de HMA entre grupos CA y CT"
  ) +
  theme_classic() +
  theme(
    legend.position = "none"
  )

# =========================================================
# 17. NORMALIDAD POR GRUPO
# =========================================================

diversidad_alfa %>%
  group_by(grupo) %>%
  summarise(
    p_riqueza = shapiro.test(riqueza)$p.value,
    p_shannon = shapiro.test(shannon)$p.value,
    p_simpson = shapiro.test(simpson)$p.value
  )

# =========================================================
# 18. COMPARACIÓN CA vs CT
# =========================================================

wilcox.test(
  riqueza ~ grupo,
  data = diversidad_alfa,
  exact = FALSE
)

wilcox.test(
  shannon ~ grupo,
  data = diversidad_alfa,
  exact = FALSE
)

wilcox.test(
  simpson ~ grupo,
  data = diversidad_alfa,
  exact = FALSE
)


diversidad_alfa %>%
  group_by(grupo) %>%
  summarise(
    n = n(),
    riqueza_mediana = median(riqueza),
    shannon_mediana = median(shannon),
    simpson_mediana = median(simpson),
    riqueza_media = mean(riqueza),
    shannon_media = mean(shannon),
    simpson_media = mean(simpson)
  )

diversidad_alfa %>%
  group_by(grupo) %>%
  summarise(
    n = n(),
    riqueza_mediana = median(riqueza),
    shannon_mediana = median(shannon),
    simpson_mediana = median(simpson),
    riqueza_media = mean(riqueza),
    shannon_media = mean(shannon),
    simpson_media = mean(simpson)
  ) %>%
  print(width = Inf)
