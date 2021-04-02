# https://stackoverflow.com/questions/12688717/round-up-from-5
#rnd <- function(x) trunc(x + sign(x) * 0.5)
round2 = function(x, n = 0) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}
# a = seq(0.5, 10.5, by = 0.5)
# names(a) = a
# round(a)
# round2(a, 0)
# rnd(a)

round_num = function(x, round_to) round2(x / round_to) * round_to




#' Bake donuts
#' 
#' Bake donuts
#' 
#' @param x \code{\link[odf:od]{od}} object
#' @param var name of the variable in x$E that contain the flow values
#' @param totals total numbers of units per origin. If \code{NULL} (default) the totals are derived from \code{x}. This parameter is useful, because in some applications, the total may deviate because of rounding errors.
#' @param text list of text labels in the visualization. See default function call for required list items.
#' @param groupname Name of the group what will appear as layer in the interactive map.
#' @param highlight Name(s) of the regions  (origins or destinations) that will have a distinct color
#' @param pal color palette. It should have the length of \code{length(highlight) + 2}. The first color indicate the flows to elsewhere (except to hightlights), the second color indicates "stay", i.e. flows from A to A. The other colors are used for the regions in \code{highlight}.
#' @param donut_size_min,donut_size_max,donut_scale The values that determine the donut size (totals of \code{var} per origin) are truncated at \code{donut_size_min} and \code{donut_size_max}. Next, the value range \code{c(0, donut_size_max)} is mapped to the size range \code{c(0, donot_scale)}. 
#' @param flow_th, Only flows of at least \code{flow_th} are shown.
#' @param flow_max,flow_buffer,flow_scale The values that determine the flows (specified with \code{var}) are truncated at \code{flow_max}. Next, \code{flow_buffer} is added to those values (to compensate for very thin lines). Next, the value range \code{c(0, flow_max)} is mapped to the size range \code{c(0, flow_scale)}. It is recommende to keep \code{flow_buffer} low, e.g. similar to \code{flow_th}.
#' @param round_to Rounding of the numbers of \code{x$E[[var]]} (useful in case these values are privacy sensitive). Use 1 to round to integers, 100 to round to hundreds, etc.
#' @param edge_incoming Should the edges to drawn from midpoint to destination (so incoming), by default \code{TRUE}, or from origin to midpoint (so outgoing)?
#' @param popup_perc_totals Should percentages be shown for totals in the popups?
#' @param popup_perc_items Should percentages be shown for the highlighted regions in the popups?
#' @param groups List of groups that specify grouping of nodes. Each item is a list that contains information for one group. The items should be: \code{codes}, a vector of codes of nodes to be grouped; \code{longlat}, the long/lat coordinates of the grouped node; \code{name}, the name of the grouped node; \code{code}, an arbitrary but unique code for the grouped node; \code{donut} logical indicating whether the donut for the grouped node should be shown; \code{inflow}, logical indicating whether the inflows for the grouped node should be shown; \code{outflow}, logical indicating whether the outflows for the grouped node should be shown.
#' @param mute List that specifies which codes are not shown. The items should be \code{codes}, a vector of codes of nodes to be muted; \code{donut} logical indicating whether the donut for the muted nodes should be shown; \code{inflow}, logical indicating whether the inflows for the muted nodes should be shown; \code{outflow}, logical indicating whether the outflows for the muted nodes should be shown.
#' @param legend Should the legend be shown?
#' @param group_label_show Should the group labels be printed in the map?
#' @param group_label_cex Text size of the group labels (in case \code{group_label_show})
#' @param group_trunc_m not used
#' @param border \code{sf} object with polygons which show the region borders
#' @param tm \code{tmap} element that can be used to stack donut maps
#' @param title \code{Title}
#' @param basemaps \code{Basemaps} See \code{\link[tmap:tm_basemap]{tm_basemap}}.
#' @import odf
#' @import tmap
#' @import tidyr
#' @example examples/bake_donuts.R
bake_donuts = function(x,
                       var,
                       totals = NULL,

  text = list(  legend_title = "Region",
                legend_other = "Other region",
                legend_stay = "Home region",
                
                popup_residents = "Residents",
                popup_stay = "Staying",
                popup_other = "&nbsp;&nbsp;&nbsp;&nbsp;to other regions",
                popup_inflow = "From other regions",
                popup_outflow = "Leaving",
                popup_to = "&nbsp;&nbsp;&nbsp;&nbsp;",

                edge_to = "to",
                edge_flow = "Flow"),
  
  groupname = "Data",
  highlight = NULL,
  pal = c("blue", "grey70"),
  
  donut_size_min = NA,
  donut_size_max = NA,
  
  donut_scale = 1.5,

  flow_th = NA,
  flow_max = NA,
  flow_buffer = NA,
  
  flow_scale = 10,
  
  round_to = 1,
  
  edge_incoming = TRUE,

  popup_perc_totals = TRUE,
  popup_perc_items = FALSE,
  
  groups = list(),
  mute = NULL,
  legend = TRUE,
  group_label_show = FALSE,
  group_label_cex = 1, 
  group_trunc_m = 2000,
  border = NULL,
  tm = NULL,
  title = NULL,
  basemaps = c("Esri.WorldGrayCanvas", "OpenStreetMap")
) {
  
  stopifnot(odf::od_is_valid(x))
  
  name_e = c(attr(x, "od_orig"), attr(x, "od_dest"), var)
  name_u = attr(x, "od_id")
  
  names(name_e) = c("code_from", "code_to", "flow")
  names(name_u) = "code"
  
  x$U = x$U %>% rename(!!! name_u)
  x$E = x$E %>% rename(!!! name_e)
  
  attr(x, "od_id") = "code"
  attr(x, "od_orig") = "code_from"
  attr(x, "od_dest") = "code_to"
  
  stopifnot(odf::od_is_valid(x))
  
  if (length(pal) != (length(highlight) + 2L)) stop("Length of pal should be ", (length(highlight) + 2L))
  names(pal) <- c(highlight, "other_", "stay_")
  
  if (edge_incoming) {
    edge_range = c(.5, 1)
    edge_trunc = units::set_units(c(500, 0), "m")
  } else {
    edge_range = c(0, .5)
    edge_trunc = units::set_units(c(0, 500), "m")
  }
  
  
  
  
  if (is.null(mute) || mute$donut) {
    x$U$show = TRUE
    muteU = NULL
  } else {
    x$U$show = !(x$U$code %in% mute$codes)
    muteU = x$U[x$U$code %in% mute$codes, ] %>% select(name)
  }
  
  x$E$show = TRUE

  if (!is.null(mute)) {
    if (!mute$inflow) x$E$show[x$E$code_to %in% mute$codes] = FALSE
    if (!mute$outflow) x$E$show[x$E$code_from %in% mute$codes] = FALSE
  } 
  
  
  
  
  crs = sf::st_crs(x$U)

  code_full_lines = character(0)
  
  for (g in groups) {
    pnt = sf::st_transform(sf::st_sfc(list(sf::st_point(g$longlat)), crs = 4326), crs = crs)
    
    x$U = x$U %>% 
      filter(!(code %in% g$codes)) %>% 
      rbind(sf::st_sf(code = g$code, name = g$name, geometry = pnt, show = g$donut))
    x$E = x$E %>% 
      mutate(code_from = case_when(code_from %in% g$codes ~ g$code,
                                   TRUE ~ code_from),
             code_to = case_when(code_to %in% g$codes ~ g$code,
                                   TRUE ~ code_to)) %>% 
      group_by(code_from, code_to) %>% 
      summarize(flow = sum(flow), show = show[1]) %>%
      ungroup() %>%
      mutate(show = ifelse((!g$outflow & code_from == g$code) | (!g$inflow & code_to == g$code), FALSE, show))
    if (!g$donut) code_full_lines = c(code_full_lines, g$code)
  }

  add_city_class <- function(col, col2) {
    x <- match(col, highlight)
    x[col == col2] = length(highlight) + 2L
    x[is.na(x)] <- length(highlight) + 1L
    factor(x, levels = 1:(length(highlight) + 2L), labels = c(highlight, "other_", "stay_"))  
  }
  create_grobs <- function(U, pal, scale = 1) {
    if (inherits(U, "sf")) U <- sf::st_drop_geometry(U)
    Ulong <- U %>%
      select(!!(c("name", names(pal)))) %>%
      pivot_longer(-name, names_to = "class", values_to = "value") %>%
      replace_na(list(value = 0))
    
    grobs <- lapply(U$name, function(nm) {
      df <- Ulong %>%
        filter(name == nm)
      
      k = nrow(df)
      
      if (any(df$name == df$class)) {
        lvls = c(nm, setdiff(df$class, nm))
      } else {
        lvls = c(df$class[k], df$class[1:(k-1)])
      }
      df = df %>% 
        mutate(class = factor(class, levels = lvls))
      singleCat <- sum(df$value != 0) <= 1L
      ggplotGrob(ggplot(df, aes(x=2, y=value, fill = class)) +
                   geom_bar(stat="identity", width=1, size = ifelse(singleCat, 0, 2 * scale), color = "white", show.legend = FALSE) +
                   geom_vline(xintercept = 2.5, color = "white", size = 5 * scale) +
                   geom_rect(xmin = 0, xmax = .75, ymin = 0, ymax = sum(df$value), size = 0, color = "white", fill = "grey90") +
                   geom_vline(xintercept = 1.5, color = "white", size = 5 * scale ) +
                   scale_fill_manual(values = pal) +
                   coord_polar("y", start=0) +
                   xlim(.75, 2.5) +
                   theme_void())
    })
    names(grobs) <- U$name
    grobs
  }
  
  
  
  # transform to mercator (needed later to draw straight edges in interactive mode) and put name colunm first
  x$U <- x$U %>%
    sf::st_transform(3857) %>%
    select(name, everything())
  
  # create labels ("a to b") and add class_to variable (needed later to color edges)
  x$E <- x$E %>%
    #filter(muni_from != muni_to) %>%
    group_by(code_from, code_to) %>%
    summarize(flow = sum(flow), show = show[1]) %>%
    ungroup() %>%
    mutate(name_from = x$U$name[match(code_from, x$U$code)],
           name_to = x$U$name[match(code_to, x$U$code)],
           label = paste(name_from, text$edge_to, name_to),
           class_from = add_city_class(name_from, name_to),
           class_to = add_city_class(name_to, name_from)) %>%
    select(label, everything())
  
  #browser()
  
  
  # calculate text$inflow and text$outflow
  x <- odf:::od_sum_out(x, "flow")
  x <- odf:::od_sum_in(x, "flow")
  x <- odf:::od_sum_stay(x, "flow")
  
  x$U$flow_res = round_num(x$U$flow_stay + x$U$flow_out, round_to)
  
  # replace totals by totals
  if (!is.null(totals)) {
    if (!all(c("code", "popRes") %in% names(totals))) stop("totals should have 'code' and 'popRes'")
    
    for (g in groups) {
      totals = rbind(totals, tibble(code = g$code, popRes = sum(totals$popRes[totals$code %in% g$codes])))
    }
    
    totals = totals %>% 
      filter(code %in% x$U$code)
    
    tids = match(totals$code, x$U$code)
    x$U$flow_res[tids] = totals$popRes 
  }
  
  
  x$U$flow_out = round_num(x$U$flow_out, round_to)
  x$U$flow_in = round_num(x$U$flow_in, round_to)
  x$U$flow_stay = round_num(x$U$flow_stay, round_to)
  
  
  
  if (is.na(donut_size_max)) {
    donut_size_max = max(x$U$flow_res[x$U$show])
    message("donut_size_max set to ", donut_size_max)
  }
  if (is.na(donut_size_min)) {
    donut_size_min = donut_size_max / 20
    message("donut_size_min set to ", donut_size_min)
  }
  
  
  x$U <- x$U %>%
    #filter(!(name %in% c("Vlieland", "Terschelling",  "Ameland", "Schiermonnikoog"))) %>%
    mutate(residents_ = flow_res,
           size = pmin(donut_size_max, pmax(donut_size_min, residents_)))

# Residents  40,200
# Staying    30,343  (75%)
# Outflow     9,900  (25%)
#   To ...        0   (0%)
#   To ...        0   (0%)
#   To ...        0   (0%)
#   To ...        0   (0%)
#   To other      0  (25%)
# Inflow     12,434

  
  uAbs = x$E %>% 
    group_by(name_from, class_to) %>% 
    summarize(flow = sum(flow)) %>% #round_num(sum(flow), round_to)) %>% 
    ungroup() %>% 
    complete(name_from, class_to, fill = list(flow = 0)) %>% 
    pivot_wider(names_from = class_to, values_from = flow) 
  uAbs$total_out_ = rowSums(uAbs[, c(highlight, "other_")])
    
  uRes = uAbs
  uRes[,-1] = (uRes[,-1] / (uRes$stay_ + uRes$total_out_)) * 100
  
  uAbs[,-1] = round_num(uAbs[,-1], round_to)
  
  uFor = uAbs
  
  uFor[,-1] = mapply(function(d1, d2, is_item) {
    p = ifelse(d2 > 0 & d2 < 0.5, "(<1%)", ifelse(d2 > 99.5 & d2 < 100, "(>99%)", paste0("(", round(d2), "%)")))
    
    #p = stringr::str_pad(p, 7, pad = " ")
    
    a = format(d1, big.mark = ",")

    show_perc = (popup_perc_totals && !is_item) || (popup_perc_items && is_item)
      
    if (!show_perc) p = ""
    paste0(p, '</nobr></td><td align="right"><nobr>', a)
    
  }, uAbs[,-1], uRes[,-1], c(rep(TRUE, length(highlight)+1), FALSE, FALSE), SIMPLIFY = FALSE)
  
  
  # process uRes for grobs: move "stay" to highlight category
  for (h in highlight) {
    id = which(uRes$name_from == h)
    stopifnot(length(id) == 1)
    uRes[[h]][id] = uRes$stay_[id]
    uRes$stay_[id] = 0
  }
  grobU = x$U %>% left_join(uRes, by = c("name" = "name_from"))


  
  ###########################################################################################
  #### create lines and doughnuts
  ###########################################################################################
  grobs <- create_grobs(grobU %>% filter(show), pal, scale = .25)
  
  x$E <- x$E %>%
    mutate(flow = round_num(flow, round_to))
  
  if (is.na(flow_max)) {
    flow_max = max(x$E$flow[x$E$code_from != x$E$code_to & x$E$show])
    message("flow_max set to ", flow_max)
  }
  
  if (is.na(flow_th)) {
    flow_th = flow_max / 40
    message("flow_th set to ", flow_th)
  }
  
  if (is.na(flow_buffer)) {
    flow_buffer = flow_th
    message("flow_buffer set to ", flow_buffer)
  }
    
  
  x$E <- x$E %>%
    filter(flow >= flow_th)

  # create straight lines from midpoint to endpoints
  x_half <- od_add_lines(x, angle = 0, range = edge_range, trunc = edge_trunc, min_trunc_dist = units::set_units(1000, "m"))
  x_full <- od_add_lines(x, angle = 0, trunc = units::set_units(c(group_trunc_m, 0), "m"))
  
  # combine x_half and x_full (use x_full for )
  x = x_half
  x$E$geometry[x$E$code_from %in% code_full_lines | x$E$code_to %in% code_full_lines] = x_full$E$geometry[x$E$code_from %in% code_full_lines | x$E$code_to %in% code_full_lines]
  
  
  
  
  
  ###########################################################################################
  #### process data for popups
  ###########################################################################################
  lns <- x$E %>%
    filter(show, name_from != name_to) %>%
    mutate(width = flow_buffer + pmin(flow, flow_max)) %>%
    select(label, flow, width, class_to, class_from) %>%
    arrange(desc(flow))
  
  levels(lns$class_to)[levels(lns$class_to) == "other_"] = text$legend_other
  levels(lns$class_to)[levels(lns$class_to) == "stay_"] = text$legend_stay

  levels(lns$class_from)[levels(lns$class_from) == "other_"] = text$legend_other
  levels(lns$class_from)[levels(lns$class_from) == "stay_"] = text$legend_stay
  
  names(pal)[names(pal) == "other_"] = text$legend_other
  names(pal)[names(pal) == "stay_"] = text$legend_stay
  

  x$U <- x$U %>%
    left_join(uFor, by = c("name" = "name_from"))

  x$U$residents_ = paste0('</nobr></td><td align="right"><nobr>', format(x$U$residents_, big.mark = ","))
  x$U$flow_in = paste0('</nobr></td><td align="right"><nobr>', format(x$U$flow_in, big.mark = ","))
  
  vars = c("residents_", "stay_", "total_out_", highlight, "other_", "flow_in")
  names(vars) = c(text$popup_residents, text$popup_stay, text$popup_outflow, paste0(text$popup_to, highlight), text$popup_other, text$popup_inflow)
  
  
  
  
  pnts <- x$U[,c("name", "show", "size", vars)] %>%
    filter(show) %>% 
    mutate(show = NULL)
  names(pnts) = c("name", "size", names(vars), "geometry")
  
  names(lns)[names(lns) == "flow"] = text$edge_flow

  set_precision <- function(x, precision = 4) {
    crs = sf::st_crs(x)
    sf::st_geometry(x) <- sf::st_as_sfc(lapply(sf::st_geometry(x), function(y) {
      y[] <- round(y[], precision)
      y
    }))
    st_set_crs(x, crs)
  }
  
  
  lns <- lns %>%
    sf::st_transform(4326) %>%
    set_precision(4)
  
  
  pnts <- pnts %>%
    sf::st_transform(4326) %>%
    set_precision(4)
  
  
  
  sel_h = pnts$name %in% highlight
  pnts_oth = pnts[!sel_h, ]
  grobs_oth = grobs[match(pnts_oth$name, names(grobs))]
  
  if (length(highlight)) {
    pg = lapply(which(sel_h), function(i) {
      p = pnts[i, ]
      nm = p$name
      g = grobs[which(names(grobs) == nm)[1]]
      pid = grep(nm, names(p), fixed = TRUE)
      
      v = setdiff(names(vars), names(p)[pid])
      list(p=p, g=g, v=v)
    })
  }
  
  if (is.null(tm)) {
    tm <- tmap::tm_basemap(basemaps)  
  }

  if (!is.null(border)) {
    tm = tm + tmap::tm_shape(border) + tmap::tm_borders(col = "black", lwd = 2, group = groupname)
  }
  
  
  cls = ifelse(edge_incoming, "class_to", "class_from")
  
  tm = tm +
    tmap::tm_shape(lns) +
    tmap::tm_lines(lwd = "width", scale = flow_scale, col = cls, id = "label", popup.vars = text$edge_flow, palette = pal, title.col = text$legend_title, group = groupname, legend.col.show = legend)
  
  
  if (!is.null(muteU) && mute$dots) {
    tm = tm + tmap::tm_shape(muteU) + tmap::tm_dots("black")
  }
  
  if (TRUE) {
    if (length(highlight)) {
      for (i in 1:length(pg)) {
        tm = tm + tmap::tm_shape(pg[[i]]$p) +
          tmap::tm_symbols(size = "size", scale = donut_scale, size.max =  donut_size_max, id = "name", popup.vars = pg[[i]]$v,
                     shape = "name", shapes = pg[[i]]$g, legend.shape.show = FALSE, grob.dim = c(width = 48, height = 48, render.width = 96, render.height = 96), group = groupname, popup.format = list(html.escape = FALSE))  
      }
    }
  }
  
  tm = tm + tmap::tm_shape(pnts_oth) +
    tmap::tm_symbols(size = "size", scale = donut_scale, size.max =  donut_size_max, id = "name", popup.vars = names(vars),
               shape = "name", shapes = grobs_oth, legend.shape.show = FALSE, grob.dim = c(width = 48, height = 48, render.width = 96, render.height = 96), group = groupname, popup.format = list(html.escape = FALSE))
  
  if (length(groups) && group_label_show) {
    group_names = paste(text$from, sapply(groups, function(g) g$name))
    group_points = sf::st_sfc(lapply(groups, function(g) sf::st_point(g$longlat)), crs = 4326)
    
    labels = sf::st_sf(labels = group_names, geometry = group_points)
    tm = tm + tmap::tm_shape(labels) + tmap::tm_text("labels", size = group_label_cex)
  }
  
  if (!is.null(title)) tm = tm + tmap::tm_layout(title = title)
  
  #if (!is.null(view_args)) tm <- tm + do.call(tm_view, view_args)
  
  tm
  
}