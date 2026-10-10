# =========================================================
# PCoA DE COMUNIDADES DE HMA - BRAY-CURTIS
# =========================================================


# =========================================================
# PCoA DE LA COMPOSICIÓN DE HMA ENTRE ACCESIONES
# DISTANCIA BRAY-CURTIS
# =========================================================


# 1. CARGAR LIBRERÍAS

library(tidyverse)
library(readxl)
library(vegan)
library(ggplot2)


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


# 5. CREAR MATRIZ ACCESIÓN × TAXÓN

matriz_taxon <- tabla_taxon %>%
  pivot_wider(
    names_from = taxon,
    values_from = abundancia,
    values_fill = 0
  ) %>%
  column_to_rownames("accesion") %>%
  as.matrix()


# 6. CREAR METADATOS

metadata <- datos %>%
  distinct(accesion, grupo)

metadata <- metadata[
  match(rownames(matriz_taxon), metadata$accesion),
]


# 7. CALCULAR ABUNDANCIA RELATIVA

matriz_rel <- matriz_taxon / rowSums(matriz_taxon)


# 8. CALCULAR DISTANCIA BRAY-CURTIS

dist_bray <- vegdist(
  matriz_rel,
  method = "bray"
)


# 9. REALIZAR PCoA

pcoa <- cmdscale(
  dist_bray,
  eig = TRUE,
  k = 2,
  add = TRUE
)


# 10. CALCULAR VARIACIÓN EXPLICADA

var_exp <- pcoa$eig[pcoa$eig > 0] /
  sum(pcoa$eig[pcoa$eig > 0]) * 100


# 11. CREAR TABLA PARA GRAFICAR

pcoa_df <- data.frame(
  accesion = rownames(pcoa$points),
  PCoA1 = pcoa$points[, 1],
  PCoA2 = pcoa$points[, 2]
) %>%
  left_join(
    metadata,
    by = "accesion"
  )


# 12. MOSTRAR VARIACIÓN EXPLICADA

print(var_exp[1:2])


# 13. GRAFICAR PCoA

ggplot(
  pcoa_df,
  aes(
    x = PCoA1,
    y = PCoA2,
    color = grupo
  )
) +
  geom_point(
    size = 3,
    alpha = 0.85
  ) +
  labs(
    x = paste0(
      "PCoA1 (",
      round(var_exp[1], 1),
      "%)"
    ),
    y = paste0(
      "PCoA2 (",
      round(var_exp[2], 1),
      "%)"
    ),
    color = "Grupo",
    title = "PCoA de la composición de HMA entre accesiones"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )


# =========================================================
# PERMANOVA - COMPOSICIÓN DE HMA ENTRE CA Y CT
# =========================================================

permanova <- adonis2(
  dist_bray ~ grupo,
  data = metadata,
  permutations = 999
)

print(permanova)

# =========================================================
# PERMDISP - HOMOGENEIDAD DE DISPERSIONES
# =========================================================

dispersion <- betadisper(
  dist_bray,
  group = metadata$grupo
)

anova(dispersion)

permutest(
  dispersion,
  permutations = 999
)


# =========================================================
# SIMPER
# TAXONES QUE MÁS CONTRIBUYEN A LA DIFERENCIA ENTRE CA Y CT
# =========================================================

simper_result <- simper(
  matriz_rel,
  group = metadata$grupo,
  permutations = 999
)

# Ver resultado general
summary(simper_result)


# =========================================================
# SIMPER - TAXONES QUE EXPLICAN LA DISIMILITUD CA vs CT
# =========================================================

simper_result <- simper(
  matriz_rel,
  group = metadata$grupo,
  permutations = 999
)

# Resumen de la comparación CA vs CT
simper_CA_CT <- summary(simper_result)$CA_CT

# Convertir a tabla
tabla_simper <- as.data.frame(simper_CA_CT)

# Añadir nombre del taxón
tabla_simper$taxon <- rownames(tabla_simper)

# Reordenar columnas
tabla_simper <- tabla_simper %>%
  select(
    taxon,
    average,
    sd,
    ratio,
    ava,
    avb,
    cumsum,
    p
  )

# Ver tabla completa
View(tabla_simper)

# Taxones que explican hasta aproximadamente 70 %
n_70 <- which(tabla_simper$cumsum >= 0.70)[1]

simper_70 <- tabla_simper[1:n_70, ]

print(simper_70)
