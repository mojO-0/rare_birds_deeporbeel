# ============================================================================
# THREE CHORD DIAGRAMS — Deepor Beel Bird Master Sheet
# Origin (always): Family
# Destinations:    1) Migratory Status   2) Breeding Status   3) Habitat Use
#
# Input file: mastersheet_dbwls.csv
# ============================================================================

# ---------------------------------------------------------------------------
# 0. SETUP — Packages
# ---------------------------------------------------------------------------
required_pkgs <- c("dplyr", "tidyr", "stringr", "readr",
                   "circlize", "RColorBrewer", "scales")

new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(circlize)
library(RColorBrewer)
library(scales)

# ---------------------------------------------------------------------------
# 1. LOAD & CLEAN DATA
# ---------------------------------------------------------------------------
# NOTE: This CSV export contains many fully-blank trailing rows (an artifact
# of how the source spreadsheet was saved). We drop any row with no Family.

raw <- read_csv("mastersheet_dbwls.csv", show_col_types = FALSE)

df <- raw %>%
  select(Family, `Scientific Name`, `Common Name`,
         `Migratory status`, `Breeding status`, `Habitat Use`) %>%
  rename(
    Species   = `Scientific Name`,
    Common    = `Common Name`,
    Migratory = `Migratory status`,
    Breeding  = `Breeding status`,
    Habitat   = `Habitat Use`
  ) %>%
  filter(!is.na(Family), !is.na(Species)) %>%        # drop blank trailing rows
  mutate(
    # Shorten family names to the taxonomic name only, dropping the
    # parenthetical common-group description, e.g.
    # "Anatidae (Ducks, Geese, and Waterfowl)" -> "Anatidae"
    Family_Short = str_trim(str_extract(Family, "^[^(]+"))
  )

cat("Loaded", nrow(df), "species across", n_distinct(df$Family_Short), "families.\n")

# ---------------------------------------------------------------------------
# 2. LABEL DICTIONARIES (expand coded abbreviations to readable text)
# ---------------------------------------------------------------------------
migratory_labels <- c(
  "W"    = "Winter Migrant",
  "R"    = "Resident",
  "S"    = "Summer Migrant",
  "P"    = "Passage Migrant",
  "P/S"  = "Passage/Summer",
  "W/R"  = "Winter/Resident",
  "W/RR" = "Winter/Rare Resident",
  "RR"   = "Rare Resident",
  "V"    = "Vagrant"
)

breeding_labels <- c(
  "B"  = "Breeder",
  "RB" = "Resident Breeder",
  "SB" = "Summer Breeder",
  "V"  = "Non-Breeding Visitor"
)

habitat_labels <- c(
  "SW" = "Shallow Water",
  "OW" = "Open Water",
  "OL" = "Open Woodland",
  "GL" = "Grassland",
  "SC" = "Scrubland",
  "LM" = "Lake Margin"
)

# ---------------------------------------------------------------------------
# 3. HELPER FUNCTION — Build an edge list (Family -> Destination)
# ---------------------------------------------------------------------------
# For Habitat Use, a species can have multiple codes in one cell
# (e.g. "GL, SC, SW"), so we split and count each combination separately.
# For Migratory/Breeding status, each species has a single code.

build_edges <- function(data, dest_col, label_map, split_multi = FALSE) {
  d <- data %>% select(Family_Short, value = all_of(dest_col))
  
  if (split_multi) {
    d <- d %>%
      mutate(value = str_split(value, ",\\s*")) %>%
      unnest(value)
  }
  
  d %>%
    filter(!is.na(value), value != "") %>%
    mutate(value_label = recode(value, !!!label_map, .default = value)) %>%
    count(Family_Short, value_label, name = "weight") %>%
    rename(from = Family_Short, to = value_label)
}

edges_migratory <- build_edges(df, "Migratory", migratory_labels, split_multi = FALSE)
edges_breeding  <- build_edges(df, "Breeding",  breeding_labels,  split_multi = FALSE)
edges_habitat   <- build_edges(df, "Habitat",   habitat_labels,   split_multi = TRUE)

# ---------------------------------------------------------------------------
# 4. COLOR PALETTE — Distinct color per family, reused across all 3 plots
#    so that the same family always appears in the same color.
# ---------------------------------------------------------------------------
families <- sort(unique(df$Family_Short))
n_fam    <- length(families)

set.seed(42)
fam_colors <- colorRampPalette(
  c("#1B4F72", "#117A65", "#7D6608", "#922B21", "#76448A",
    "#1F618D", "#28B463", "#D4AC0D", "#CA6F1E", "#CB4335",
    "#2980B9", "#1E8449", "#6E2F1A", "#7D3C98", "#17A589")
)(n_fam)
names(fam_colors) <- families

# ---------------------------------------------------------------------------
# 5. GENERIC CHORD-PLOT FUNCTION
# ---------------------------------------------------------------------------
# Builds one chord diagram: Family sectors on one side, the destination
# category's sectors on the other, with family-colored ribbons.

plot_chord <- function(edges, dest_base_colors, dest_colors_name,
                       title, subtitle, out_file,
                       dest_gap = 8, fam_gap = 1.5) {
  
  edges <- edges %>% filter(weight > 0)
  
  fam_nodes  <- sort(unique(edges$from))
  dest_nodes <- sort(unique(edges$to))
  all_nodes  <- c(fam_nodes, dest_nodes)
  
  # Safely generate exactly length(dest_nodes) colors regardless of count
  dest_colors <- setNames(
    colorRampPalette(dest_base_colors)(length(dest_nodes)),
    dest_nodes
  )
  grid_colors <- c(fam_colors[fam_nodes], dest_colors)
  
  # Ribbon color = color of the originating family (origin -> destination)
  link_colors <- adjustcolor(fam_colors[edges$from], alpha.f = 0.55)
  
  pdf(out_file, width = 14, height = 14)
  par(bg = "white", mar = c(1, 1, 4, 1))
  circos.clear()
  
  circos.par(
    gap.after = c(rep(fam_gap, length(fam_nodes) - 1), dest_gap,
                  rep(fam_gap, length(dest_nodes) - 1), dest_gap),
    start.degree = 90,
    track.height = 0.08
  )
  
  chordDiagram(
    edges,
    order               = all_nodes,
    grid.col            = grid_colors,
    col                 = link_colors,
    transparency        = 0,
    annotationTrack     = c("grid"),
    preAllocateTracks   = list(track.height = 0.32),
    link.lwd            = 0.3,
    link.border         = NA,
    link.sort           = TRUE,
    link.decreasing     = TRUE
  )
  
  # Sector labels — rotated to follow the circle, placed outside the grid
  circos.trackPlotRegion(
    track.index = 1,
    panel.fun = function(x, y) {
      sector <- get.cell.meta.data("sector.index")
      xlim   <- get.cell.meta.data("xlim")
      circos.text(
        mean(xlim), 0.5,
        sector,
        facing      = "clockwise",
        niceFacing  = TRUE,
        adj         = c(0, 0.5),
        cex         = ifelse(sector %in% fam_nodes, 0.55, 0.75),
        font        = ifelse(sector %in% fam_nodes, 1, 2),
        col         = ifelse(sector %in% fam_nodes, "grey20", "black")
      )
    },
    bg.border = NA
  )
  
  title(main = title, cex.main = 1.3, font.main = 2, line = -1)
  mtext(subtitle, side = 3, line = -2.3, cex = 0.85, col = "grey30")
  
  legend(
    "bottomright",
    legend  = names(dest_colors),
    fill    = dest_colors,
    border  = NA,
    bty     = "n",
    cex     = 0.7,
    title   = dest_colors_name,
    title.col = "grey20"
  )
  
  circos.clear()
  dev.off()
  message("Saved: ", out_file)
}

# ---------------------------------------------------------------------------
# 6. PLOT 1 — Family -> Migratory Status
# ---------------------------------------------------------------------------
plot_chord(
  edges            = edges_migratory,
  dest_base_colors = brewer.pal(9, "Blues")[3:9],
  dest_colors_name = "Migratory Status",
  title            = "Bird Families -> Migratory Status",
  subtitle         = "Deepor Beel Wetland | n = 257 species across 63 families",
  out_file         = "Chord_Family_Migratory.pdf",
  dest_gap         = 10
)

# ---------------------------------------------------------------------------
# 7. PLOT 2 — Family -> Breeding Status
# ---------------------------------------------------------------------------
plot_chord(
  edges            = edges_breeding,
  dest_base_colors = brewer.pal(9, "Greens")[3:9],
  dest_colors_name = "Breeding Status",
  title            = "Bird Families -> Breeding Status",
  subtitle         = "Deepor Beel Wetland | n = 257 species across 63 families",
  out_file         = "Chord_Family_Breeding.pdf",
  dest_gap         = 14
)

# ---------------------------------------------------------------------------
# 8. PLOT 3 — Family -> Habitat Use
# ---------------------------------------------------------------------------
plot_chord(
  edges            = edges_habitat,
  dest_base_colors = brewer.pal(9, "Oranges")[3:9],
  dest_colors_name = "Habitat Use",
  title            = "Bird Families -> Habitat Use",
  subtitle         = "Deepor Beel Wetland | n = 257 species across 63 families (multi-habitat species split across categories)",
  out_file         = "Chord_Family_Habitat.pdf",
  dest_gap         = 10
)

cat("\nAll three chord diagrams generated successfully:\n",
    "  1. Chord_Family_Migratory.pdf\n",
    "  2. Chord_Family_Breeding.pdf\n",
    "  3. Chord_Family_Habitat.pdf\n")