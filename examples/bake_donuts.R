library(odf)
library(donutmaps)
library(tmap)

data("NL_commuting")
data("NL_muni_point")

# Create odf object
x = od(NL_commuting, NL_muni_point, col_orig = "muni_from", col_dest = "muni_to", col_id = "id")

# Define color palette
CBS_pal = c("#d9328a", "#7d4791", "#da5914", "#53a31d", "#0581a2", "#B3B3B3")


# Bake tasty donuts (all commuting traffic)
# Edges are incoming by default
tm = bake_donuts(x,
    var = "value",
    groupname = "Netherlands",
    highlight = c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht"),
    pal = CBS_pal,
    donut_size_min = 30000, donut_size_max = 400000,
    flow_th = 500, flow_max = 20000, flow_buffer = 500, flow_scale = 10,
    donut_scale = 1.75)

# The result is a tmap object, which can best be shown in "view mode"
tmap_mode("view")
tm

# Focus on outgoing edges
tm_out = bake_donuts(x,
    var = "value",
    groupname = "Netherlands",
    highlight = c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht"),
    pal = CBS_pal,
    donut_size_min = 30000, donut_size_max = 400000,
    edge_incoming = FALSE,
    flow_th = 500, flow_max = 20000, flow_buffer = 500, flow_scale = 10,
    donut_scale = 1.75)
tm_out

# Only commute by train
x_train = x
x_train$E = x_train$E[x_train$E$mode == "train", ]

tm_train = bake_donuts(x_train,
    var = "value",
    groupname = "Netherlands",
    highlight = c("Amsterdam", "Rotterdam", "Den Haag", "Utrecht"),
    pal = CBS_pal,
    donut_size_min = 1000, donut_size_max = 20000,
    flow_th = 100, flow_max = 2000, flow_buffer = 100, flow_scale = 15,
    donut_scale = 1.75)
tm_train
