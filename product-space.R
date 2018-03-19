library(dplyr)
library(tidyr)

load("fantasy_world_long.rdata")

rca_long <- fantasy_world_long %>% 
  rename(c = country, 
         p = product, 
         xcp = export_val) %>% 
  group_by(c) %>% 
  mutate(sum_c_xcp = sum(xcp)) %>% 
  group_by(p) %>% 
  mutate(sum_p_xcp = sum(xcp)) %>% 
  ungroup() %>% 
  mutate(sum_c_p_xcp = sum(xcp)) %>% 
  mutate(rca = (xcp / sum_c_xcp) / 
           (sum_p_xcp / sum_c_p_xcp))

rca_long %>% print(n = 5)

rca_matrix <- rca_long %>%
  select(c, p, rca) %>%
  mutate(rca = ifelse(rca > 1, 1, 0)) %>% 
  spread(p, rca)

diversity <- rca_long %>% select(c) %>% distinct()
ubiquity <- tibble(p = colnames(rca_matrix)) %>% 
  filter(row_number() > 1)

rca_matrix <- rca_matrix %>% 
  select(-c) %>% 
  as.matrix()

library(Matrix)

rca_matrix <- Matrix(rca_matrix, sparse = T)

diversity <- diversity %>%
  mutate(val = rowSums(rca_matrix)) %>%
  filter(val > 0)

ubiquity <- ubiquity %>%
  mutate(val = colSums(rca_matrix)) %>%
  filter(val > 0)

rownames(rca_matrix) <- diversity$c

D <- as.matrix(diversity$val, ncol = 1)
U <- as.matrix(ubiquity$val, ncol = 1)

Mcp <- rca_matrix[
  which(rownames(rca_matrix) %in% unlist(diversity$c)) , 
  which(colnames(rca_matrix) %in% unlist(ubiquity$p))]
rm(rca_matrix)

Rcpp::sourceCpp("proximity_products_denominator.cpp")
n_cores <- 4

Phi_pp <- (t(Mcp) %*% Mcp) / proximity_products_denominator(Mcp, U, cores = n_cores)

Phi_pp_l <- Phi_pp
Phi_pp_l[upper.tri(Phi_pp_l, diag = T)] <- NA

Phi_pp_long <- as_tibble(as.matrix(Phi_pp_l)) %>% 
  mutate(id = rownames(Phi_pp)) %>% 
  gather(id2, value, -id) %>% 
  filter(!is.na(value))

library(ape)

Phi_pp_2 <- (t(Mcp) %*% Mcp) / 
  proximity_products_denominator(Mcp, U, cores = n_cores)
Phi_pp_3 <- -1 * Phi_pp_2

mst_pp <- ape::mst(Phi_pp_3)
mst_pp[upper.tri(mst_pp, diag = T)] <- NA
class(mst_pp) <- "matrix"

mst_pp_long <- as_tibble(mst_pp) %>% 
  mutate(id = rownames(mst_pp)) %>% 
  gather(id2, value, -id) %>% 
  filter(!is.na(value),
         value > 0)

Phi_pp_2[upper.tri(Phi_pp_2, diag = T)] <- NA

additions_pp_long <- as_tibble(as.matrix(Phi_pp_2)) %>% 
  mutate(id = rownames(mst_pp)) %>% 
  gather(id2, value, -id) %>% 
  filter(!is.na(value),
         value >= 0.3) %>% 
  anti_join(mst_pp_long, by = c("id", "id2"))

graph_long <- mst_pp_long %>% 
  bind_rows(additions_pp_long)

set.seed(1717)

graph_long %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = value, edge_width = value), edge_colour = "#a8a8a8") +
  geom_node_point(color = "darkslategray4", size = 8) +
  geom_node_text(aes(label = name), vjust = 2.2) +
  ggtitle("Sketch of Fantasy World's Country Space") +
  theme_void(base_size = 15)
