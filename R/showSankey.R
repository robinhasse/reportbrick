#' Plot Sankey diagram
#'
#' Visualise transitions from one state to another. This plot helps to follow
#' the exact renovation flows, though it quickly gets very complex.
#'
#' @param path character, path to the run
#' @param fill character, dimension mapped to fill, either \code{"bs"} (building
#'   shell) or \code{"hs"} (heating system).
#' @param filterData named list to filter the data before plotting
#' @param maxPeriodsInRow maximum number of time steps to plot in one row. If
#'   there is more data, it is broken across multiple lines. If \code{NULL}, all
#'   data is shown in one line.
#' @param save control how the plot should be saved:
#' \itmize{
#'   \item `TRUE`: plot is saved in plots folder of run path
#'   \item `FALSE`: plot is not saved (but always returned invisibly)
#'   \item character: plot is saved in given directory
#' }
#'
#' @author Robin Hasse
#'
#' @importFrom mip plotstyle
#' @importFrom yaml read_yaml
#' @importFrom utils stack head
#' @importFrom gamstransfer Container
#' @importFrom tidyr replace_na separate_wider_delim
#' @importFrom ggpubr ggarrange annotate_figure text_grob
#' @importFrom dplyr %>% .data lead filter mutate left_join select group_by pull
#'   across all_of summarise arrange ungroup reframe everything rename
#' @importFrom ggplot2 ggplot aes geom_col position_stack geom_segment theme
#'   theme_classic scale_x_continuous scale_y_continuous scale_fill_manual
#'   scale_color_manual element_line element_blank ggsave
#' @export

showSankey <- function(path,
                       fill = c("bs", "hs"),
                       filterData = NULL,
                       maxPeriodsInRow = NULL,
                       save = TRUE) {

  if (isFALSE(requireNamespace("ggsankey", quietly = TRUE))) {
    warning("Can't plot sankey. Install 'ggsankey' from GitHub.")
    return(invisible(NULL))
  }



  fill <- match.arg(fill)

  config <- read_yaml(file.path(path, "config", "config_COMPILED.yaml"))

  eps <- 1E-3

  xPos <- list(
    con = 0.125,
    ren_start = 0.3,
    ren_end = 0.7,
    dem = 0.875
  )




  # FUNCTIONS ------------------------------------------------------------------

  .shiftXbyDt <- function(df, ...) {
    shift <- list(...)
    for (col in names(shift)) {
      df[[col]] <- df[["ttot"]]
      if (shift[[col]] != 0) {
        dtShift <- if (shift[[col]] < 0) "dt" else "dtNext"
        df[[col]] <- df[[col]] + shift[[col]] * df[[dtShift]]
      }
    }
    return(df)
  }



  .setNode <- function(df, ...) {
    nodes  <- list(...)
    for (col in names(nodes)) {
      if (nodes[[col]] %in% c("bs", "bsr", "hs", "hsr")) {
        df[[col]] <- df[[nodes[[col]]]]
      } else {
        df[[col]] <- nodes[[col]]
      }
    }
    return(df)
  }



  .defineFlow <- function(flow,
                          node = fill,
                          next_node = fill,
                          shift_x = 0,
                          shift_next_x = 0,
                          drop = "none") {

    var <- sub("_.*$", "", flow)

    dataFlow <- data[[var]] %>%
      filter(drop == "none" |
               (drop == "start" & .data$ttot > min(.data$ttot)) |
               (drop == "end"   & .data$ttot < max(.data$ttot))) %>%
      mutate(flow = flow) %>%
      left_join(dt, by = "ttot") %>%
      .shiftXbyDt(x = shift_x, next_x = shift_next_x) %>%
      .setNode(node = node, next_node = next_node)

    if (var != "Stock") {
      dataFlow[["value"]] <- dataFlow[["value"]] * dataFlow[["dt"]]
    }

    dataFlow  %>%
      select("flow", "ttot", "x", "next_x", "node", "next_node", "value")
  }



  .addSuffixCol <- function(df) {
    df[df[["flow"]] %in% c("Stock_in", "Stock_out"), "suffix"] <- "untouch"
    df[df[["next_node"]] == "0", "suffix"] <- "untouch"
    df[df[["flow"]] == "Renovation" & df[["node"]] == df[["next_node"]], "suffix"] <- "ident"
    return(df)
  }



  .pasteSuffix <- function(df, to) {
    for (col in to) {
      df[[col]] <- ifelse(is.na(df[["suffix"]]),
                          df[[col]],
                          ifelse(is.na(df[[col]]),
                                 NA,
                                 paste(df[[col]], df[["suffix"]], sep = "_")))
    }
    return(df)
  }



  .finaliseNodes <- function(df) {
    df %>%
      mutate(next_node = ifelse(.data$next_node == "0",
                                .data$node,
                                .data$next_node)) %>%
      .pasteSuffix(c("node", "next_node")) %>%
      select(-"suffix")
  }



  .addVirtualOutFlows <- function(df) {
    df %>%
      group_by(across(all_of(c(x = "next_x", node = "next_node", "ttot")))) %>%
      summarise(flow = if (all(.data$flow == "Demolition")) "Demolition_end" else "Flow_end",
                next_x = NA,
                next_node = NA,
                value = sum(.data$value),
                .groups = "drop") %>%
      rbind(df)
  }



  .addShiftCol <- function(df) {
    relGap <- 0

    totStock <- df %>%
      filter(.data$flow == "Stock_in") %>%
      group_by(.data$x) %>%
      summarise(value = sum(.data$value), .groups = "drop")
    maxStock <- totStock %>%
      getElement("value") %>%
      max()

    gap <- relGap * maxStock

    shiftDemolition <- df %>%
      filter(.data$flow == "Demolition_end") %>%
      group_by(across(all_of(c("x", "flow")))) %>%
      summarise(shift = -sum(.data$value) - gap,
                .groups = "drop")

    df <- df %>%
      left_join(shiftDemolition, by = c("x", "flow")) %>%
      mutate(shift = replace_na(.data$shift, 0))
    df[df[["flow"]] == "Construction", "shift"] <- maxStock + 2 * gap

    return(df)
  }



  .shiftXbyEps <- function(df) {
    df %>%
      mutate(x = .data$x + ifelse(grepl("_end$", .data$flow), 0, eps))
  }



  .levelsAsDf <- function(nodeLevels) {
    data.frame(level = nodeLevels) %>%
      separate_wider_delim("level", "_",
                           names = c("fillDim", "flowType"),
                           too_few = "align_start",
                           cols_remove = FALSE) %>%
      mutate(flowType = replace_na(.data$flowType, "effective"))
  }



  .orderNodeLevels <- function(nodeLevels, mapping) {

    nodeLevels %>%
      .levelsAsDf() %>%
      mutate(flowType = factor(.data$flowType,
                               c("untouch", "indet", "effective")),
             fillDim = factor(.data$fillDim, rev(mapping[[fill]]))) %>%
      group_by(.data$fillDim) %>%
      arrange(.data$flowType) %>%
      ungroup() %>%
      arrange(.data$fillDim) %>%
      getElement("level")
  }



  .flowNodesAsFactor <- function(df, mapping) {

    nodeLevels <- union(df[["node"]],
                        df[["next_node"]]) %>%
      .orderNodeLevels(mapping)

    df %>%
      mutate(node = factor(.data$node, nodeLevels),
             next_node = factor(.data$next_node, nodeLevels))
  }



  .barNodesAsFactor <- function(df, mapping) {
    fillLevels <- rev(mapping[[fill]])
    nodeLevels <- unique(df[["node"]])
    nodeLevels <- c(nodeLevels[nodeLevels == "Shift"],
                    nodeLevels[!nodeLevels %in% c("Shift", fillLevels)],
                    fillLevels)
    df %>%
      mutate(node = factor(.data$node, nodeLevels))
  }



  .getFillColors <- function(nodeLevels, mapping) {
    nodeLevels %>%
      .levelsAsDf() %>%
      left_join(mapping[, c(fill, "label")], by = c(fillDim = fill)) %>%
      mutate(color = plotstyle(.data$label),
             color = ifelse(.data$flowType == "ident",
                            paste0(.data$color, "80"),
                            ifelse(.data$flowType == "untouch",
                                   "#f1daab80",
                                   .data$color))) %>%
      pull("color", "level") %>%
      c(Construction = "#7CAEAF", Demolition_end = "#D26868")
  }



  .getStock <- function() {
    data[["Stock"]] %>%
      select(x = "ttot", node = !!fill, "value") %>%
      mutate(bar = "Stock")
  }



  .defineRenStockBars <- function(df) {
    renovation <- df %>%
      filter(.data$flow == "Renovation")
    rbind(select(renovation, "x", "node", "value"),
          select(renovation, x = "next_x", node = "next_node", "value")) %>%
      mutate(node = sub("_.*$", "", .data$node)) %>%
      group_by(across(all_of(c("x", "node")))) %>%
      summarise(value = sum(.data$value), .groups = "drop") %>%
      mutate(bar = "Renovation")
  }



  .defineFlowBars <- function(df) {
    df %>%
      filter(.data$flow %in% c("Construction", "Demolition_end")) %>%
      group_by(across(all_of(c("x", bar = "flow", node = "flow")))) %>%
      reframe(node = c(unique(.data$flow), "Shift"),
              value = c(sum(.data$value), mean(.data$shift))) %>%
      mutate(value = .data$value *
               ifelse(.data$node == "Demolition_end", -1, 1)) %>%
      group_by(.data$x) %>%
      filter(!(.data$node == "Shift" &
                 "Demolition_end" %in% .data$node)) %>%
      ungroup()
  }



  .ifThen <- function(node, bar, stock, ren, con, dem) {
    ifelse("Construction" %in% node,
           con,
           ifelse("Demolition_end" %in% node,
                  dem,
                  ifelse(bar == "Renovation",
                         ren,
                         stock)))
  }



  .setBarAesthetics <- function(df) {
    stockBarWidth <- 0.1 * min(dt[["dt"]])
    df %>%
      group_by(.data$x) %>%
      mutate(width = stockBarWidth * .ifThen(.data$node, .data$bar, 1, 0.2, 0.5, 0.5),
             just = .ifThen(.data$node, .data$bar, 0, 0, 1, -1),
             outline = .data$bar %in% c("Stock", "Construction", "Demolition_end") &
               .data$node != "Shift")
  }



  .getIneffSuffix <- function(x) {
    ifelse(grepl("_", x), "ineff", NA)
  }



  .markEffFlows <- function(df) {
    nodeLevels <- levels(df[["node"]])
    nodeLevels <- paste(nodeLevels, .getIneffSuffix(nodeLevels), sep = "_")
    nodeLevels <- sub("_NA$", "", nodeLevels)

    df %>%
      mutate(node = as.character(.data$node),
             next_node = as.character(.data$next_node),
             suffix = .getIneffSuffix(.data$node)) %>%
      .pasteSuffix(to = c("node", "next_node")) %>%
      mutate(node = factor(.data$node, nodeLevels),
             next_node = factor(.data$next_node, nodeLevels)) %>%
      select(-"suffix")
  }



  .geomFlow <- function(data) {
    ggsankey::geom_alluvial(aes(next_x = .data$next_x,
                                node = .data$node,
                                next_node = .data$next_node,
                                value = .data$value,
                                shift = .data$shift),
                            data) %>%
      suppressWarnings()
  }



  .geomBar <- function(data, bar, just = 0.5) {
    geom_col(aes(y = .data$value,
                 colour = .data$outline,
                 width = .data$width),
             filter(data, .data$bar %in% !!bar),
             just = just,
             linewidth = 0.25,
             position = position_stack(reverse = TRUE)) %>%
      suppressWarnings()
  }



  .getMapping <- function() {
    tmplFile <- file.path(path, "config", "brickSets_COMPILED.yaml")
    if (!file.exists(tmplFile)) {
      stop("Cannot find this reporting template: ", tmplFile)
    }
    tmpl <- read_yaml(tmplFile)
    mapping <- tmpl[[fill]][["elements"]] %>%
      stack() %>%
      mutate(across(everything(), as.character))
    colnames(mapping) <- c("label", fill)
    return(mapping)
  }



  .maxStock <- function(df) {
    df %>%
      filter(.data$bar == "Stock") %>%
      group_by(.data$x) %>%
      summarise(value = sum(.data$value), .groups = "drop") %>%
      getElement("value") %>%
      max()
  }



  .yBreaks <- function(maxStock) {
    yBreaks <- pretty(c(0, maxStock), 5)
    yBreaks[yBreaks <= maxStock]
  }



  .extendYAxis <- function(bars) {
    maxStock <- .maxStock(bars)
    minX <- min(bars[["x"]]) - max(bars[["width"]])
    geom_segment(aes(x = minX, xend = minX, y = 0, yend = maxStock),
                 inherit.aes = FALSE)
  }



  .yMinorBreaks <- function(x, maxStock) {
    dx <- mean(diff(x))
    y <- sort(c(x, x + dx / 2))
    y[y <= maxStock]
  }


  .getLimits <- function(bars, flows) {
    periods <- sort(unique(flows[["ttot"]]))
    width <- max(bars[["width"]])

    if (length(periods) <= maxPeriodsInRow) {
      return(data.frame(lower = NA, upper = NA))
    }

    dt <- periods[maxPeriodsInRow] - periods[1]

    limits <- data.frame()

    while (length(periods) > 0) {
      barsInRow <- max(which(periods <= periods[1] + dt))
      if (barsInRow < 2) {
        stop("Please increase 'maxPeriodsInRow'. Can't plot this.")
      }
      limits <- rbind(limits, data.frame(lower = periods[1] - width / 2,
                                         # upper = periods[barsInRow]))
                                         upper = periods[1] + dt + width / 2))
      if (barsInRow == length(periods)) {
        break
      }
      periods <- tail(periods, -(barsInRow - 1))
    }
    return(limits)
  }


  .save <- function(p) {
    savePath <- if (isTRUE(save)) {
      file.path(path, "plots")
    } else if (is.character(save)) {
      if (!dir.exists(save)) {
        stop("Can't save the plot. This directory doesn't exist: ", save)
      }
      save
    } else {
      return()
    }
    ggsave(file.path(savePath, paste0("sankey_", fill, ".pdf")), p,
           height = 21, width = 29.7, units = "cm")
  }


  .limitData <- function(df, limits) {
    df[df$x > limits[["lower"]] & df$x < limits[["upper"]], ]
  }

  .plot <- function(bars, flows, flowsEff,
                    mapping, maxStock,
                    yName,
                    xLimits = c(NA, NA)) {

    bars <- .limitData(bars, xLimits)
    flows <- .limitData(flows, xLimits)
    flowsEff <- .limitData(flowsEff, xLimits)

    fillColors <- .getFillColors(levels(flows[["node"]]), mapping)
    fillLabels <- pull(mapping, "label", fill)
    legendTitle <- switch(fill, bs = "Building shell", hs = "Heating system")

    yBreaks <- .yBreaks(maxStock)

    ggplot(mapping = aes(x = .data$x, fill = .data$node)) +
      .geomFlow(flows) +
      .geomFlow(flowsEff) +
      .geomBar(bars, c("Stock", "Renovation")) +
      .geomBar(bars, "Construction", just = 1) +
      .geomBar(bars, "Demolition_end", just = 0) +
      .extendYAxis(bars) +
      scale_y_continuous(yName,
                         breaks = yBreaks,
                         minor_breaks = .yMinorBreaks(yBreaks, maxStock),
                         expand = c(0.01, 0.01)) +
      scale_x_continuous(NULL,
                         limits = xLimits,
                         expand = c(0, 0.01)) +
      scale_fill_manual(values = fillColors, labels = fillLabels,
                        breaks = names(fillLabels), name = legendTitle,
                        na.value = NA) +
      scale_color_manual(values = c(`TRUE` = "black", `FALSE` = NA),
                         na.value = NA, guide = "none") +
      theme_classic() +
      theme(panel.grid.major.y = element_line(color = "lightgrey"),
            panel.grid.minor.y = element_line(color = "lightgrey"),
            axis.line.y = element_blank())
  }




  # CHECK INPUT ----------------------------------------------------------------

  # find gdx file in given path
  gdxNames <- c("output.gdx",
                "abort.gdx")
  gdxFiles <- file.path(path, gdxNames)
  gdx <- head(gdxFiles[which(file.exists(gdxFiles))], 1)
  if (length(gdx) == 0) {
    warning("No suitable gdx file found to plot in ", path)
    return(NULL)
  }





  # READ DATA ------------------------------------------------------------------

  dt <- readGdxSymbol(gdx, "p_dt", asMagpie = FALSE) %>%
    select("ttot", dt = "value") %>%
    mutate(dtNext = lead(.data$dt))

  vars <- c(
    Stock        = "v_stock",
    Construction = "v_construction",
    Demolition   = "v_demolition",
    Renovation   = "v_renovation"
  )

  data <- lapply(vars, function(v) {
    var <- readGdxSymbol(gdx, v, asMagpie = FALSE, stringAsFactor = FALSE) %>%
      filter(.data$qty == "area") %>%
      select(-"qty")
    if (!is.null(filterData)) {
      for (dim in names(filterData)) {
        var <- filter(var, .data[[dim]] %in% filterData[[dim]])
      }
    }

    return(var)
  })

  mapping <- .getMapping()





  # AGGREGATE DATA -------------------------------------------------------------

  # sum over vintages and stock subsets
  aggDim <- setdiff(
    c("bs", "hs", "bsr", "hsr", "vin", "reg", "loc", "typ", "inc", "value"),
    c(fill, paste0(fill, "r"))
  )
  data <- lapply(data, function(var) {
    var %>%
      group_by(across(-any_of(aggDim))) %>%
      summarise(value = sum(.data$value), .groups = "drop") %>%
      mutate(value = .data$value / 1000) # million m2 -> billion m2
  })





  # PREPARE DATA ---------------------------------------------------------------

  flows <- rbind(
    .defineFlow("Stock_in", shift_x = xPos$ren_end - 1, drop = "start"),
    .defineFlow("Stock_out", shift_next_x = xPos$ren_start, drop = "end"),
    .defineFlow("Construction", shift_x = xPos$con - 1, shift_next_x = xPos$ren_start - 1, drop = "start"),
    .defineFlow("Renovation", shift_x = xPos$ren_start - 1, shift_next_x = xPos$ren_end - 1, next_node = switch(fill, bs = "bsr", hs = "hsr"), drop = "start"),
    .defineFlow("Demolition", shift_x = xPos$ren_end - 1, shift_next_x = xPos$dem - 1, drop = "start")
  )

  flows <- flows %>%
    .addSuffixCol() %>%
    .finaliseNodes() %>%
    .addVirtualOutFlows() %>%
    .addShiftCol() %>%
    .shiftXbyEps() %>%
    .flowNodesAsFactor(mapping)

  flowsEff <- .markEffFlows(flows)

  bars <- rbind(.defineRenStockBars(flows),
                .defineFlowBars(flows),
                .getStock()) %>%
    .barNodesAsFactor(mapping) %>%
    .setBarAesthetics()

  maxStock <- .maxStock(bars)





  # PLOT -----------------------------------------------------------------------

  yName <- "Floor space in billion m2"

  if (is.null(maxPeriodsInRow)) {

    ## primary plot ====

    p <- .plot(bars, flows, flowsEff, mapping, maxStock, yName)

  } else {

    ## multi-row plot ====

    limits <- .getLimits(bars, flows)

    plotlist <- apply(limits, 1, function(row) {
      .plot(bars, flows, flowsEff, mapping, maxStock,
            yName = NULL,
            xLimits = row[c("lower", "upper")])
    })
browser()
    p <- ggarrange(plotlist = plotlist,
                   ncol = 1,
                   # labels = c(NA, yName),
                   common.legend = TRUE,
                   legend = "right") %>%
      annotate_figure(left = text_grob(yName, rot = 90, size = 11))
  }





  # OUTPUT ---------------------------------------------------------------------

  .save(p)

  return(invisible(p))
}
