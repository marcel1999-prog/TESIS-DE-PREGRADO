# 1. CARGAR LIBRERÍAS

library(tidyverse)
library(readxl)
library(pheatmap)
library(vegan)


# 2. CARGAR BASE DE DATOS

datos <- read_excel(
  "C:/Users/fvygh/Documents/TESIS_PREGRADO/base_de_datos_HMA.xlsx",
  sheet = "fb"
)


# 3. CREAR NOMBRE COMPLETO DEL TAXÓN

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


# 6. CALCULAR ABUNDANCIA RELATIVA POR ACCESIÓN

matriz_rel <- matriz_taxon / rowSums(matriz_taxon)


# 7. CREAR ANOTACIÓN DE GRUPO CA / CT

anotacion <- datos %>%
  distinct(accesion, grupo) %>%
  column_to_rownames("accesion")

anotacion <- anotacion[
  rownames(matriz_rel),
  ,
  drop = FALSE
]


# 8. CLUSTERING DE ACCESIONES CON BRAY-CURTIS

dist_accesiones <- vegdist(
  matriz_rel,
  method = "bray"
)

cluster_accesiones <- hclust(
  dist_accesiones,
  method = "average"
)


# 9. CLUSTERING DE TAXONES CON BRAY-CURTIS

dist_taxones <- vegdist(
  t(matriz_rel),
  method = "bray"
)

cluster_taxones <- hclust(
  dist_taxones,
  method = "average"
)


# 10. FORMATEAR NOMBRES CIENTÍFICOS EN CURSIVA

formato_taxon <- function(x) {
  
  partes <- strsplit(x, " ")[[1]]
  
  genero <- partes[1]
  especie <- paste(partes[-1], collapse = " ")
  
  if (grepl("^sp", especie, ignore.case = TRUE)) {
    
    especie <- gsub(
      "^Sp",
      "sp",
      especie,
      ignore.case = TRUE
    )
    
    bquote(
      italic(.(genero)) ~ .(especie)
    )
    
  } else {
    
    bquote(
      italic(.(x))
    )
  }
}


labels_taxon <- as.expression(
  lapply(
    colnames(matriz_rel),
    formato_taxon
  )
)


# 11. GENERAR HEATMAP

pheatmap(
  t(matriz_rel),
  
  cluster_rows = cluster_taxones,
  cluster_cols = cluster_accesiones,
  
  scale = "none",
  
  annotation_col = anotacion,
  
  labels_row = labels_taxon,
  
  color = colorRampPalette(
    c("white", "orange", "red")
  )(100),
  
  border_color = NA,
  
  fontsize_row = 7,
  fontsize_col = 8,
  angle_col = 45,
  
  main = "Patrones de abundancia relativa de HMA entre accesiones (Bray–Curtis)"
)

png(
  "Heatmap_HMA_Bray_Curtis.png",
  width = 4200,
  height = 2800,
  res = 300
)

pheatmap(
  t(matriz_rel),
  cluster_rows = cluster_taxones,
  cluster_cols = cluster_accesiones,
  scale = "none",
  annotation_col = anotacion,
  labels_row = labels_taxon,
  color = colorRampPalette(
    c("white", "orange", "red")
  )(100),
  border_color = NA,
  fontsize_row = 8,
  fontsize_col = 9,
  angle_col = 45,
  main = "Patrones de abundancia relativa de HMA entre accesiones (Bray–Curtis)"
)

dev.off()

# 12. EXPORTAR HEATMAP EN ALTA RESOLUCIÓN

pheatmap(
  t(matriz_rel),
  
  cluster_rows = cluster_taxones,
  cluster_cols = cluster_accesiones,
  
  scale = "none",
  
  annotation_col = anotacion,
  
  labels_row = labels_taxon,
  
  color = colorRampPalette(
    c("white", "orange", "red")
  )(100),
  
  border_color = NA,
  
  fontsize_row = 8,
  fontsize_col = 9,
  angle_col = 45,
  
  main = "Patrones de abundancia relativa de HMA entre accesiones (Bray-Curtis)",
  
  filename = "Heatmap_HMA_Bray_Curtis.png",
  
  width = 14,
  height = 9
)
