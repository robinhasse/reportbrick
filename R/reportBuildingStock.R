#' Report building Stock
#'
#' Report quantities describing the stock of buildings
#'
#' @param gdx gams transfer container of the BRICK GDX
#' @param brickSets character, BRICK reporting template
#' @param silent boolean, suppress warnings and printing of dimension mapping
#'
#' @author Robin Hasse
#'
#' @importFrom magclass mbind setNames dimSums mselect collapseDim

reportBuildingStock <- function(gdx, brickSets = NULL, silent = TRUE) {

  # READ -----------------------------------------------------------------------

  # stock variable
  v_stock <- readGdxSymbol(gdx, "v_stock")

  # unit conversion: million m2 -> billion m2
  v_stock <- .prepVar(v_stock)




  # REPORT ---------------------------------------------------------------------

  out <- mbind(

    ## Total ====
    reportAgg(v_stock,
              "Stock|Buildings (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "resCom", inc = "all"),
              silent = silent),
    reportAgg(v_stock,
              "Stock|Residential (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              silent = silent),
    reportAgg(v_stock,
              "Stock|Commercial (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", typ = "com", inc = "all"),
              silent = silent),


    ## by building type ====
    reportAgg(v_stock,
              "Stock|Residential|{typ} (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(typ = "res"),
              silent = silent),


    ## by location ====
    reportAgg(v_stock,
              "Stock|Residential|{loc} (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", vin = "all", typ = "res", inc = "all"),
              rprt = c(loc = "all"),
              silent = silent),


    ## by vintage ====
    reportAgg(v_stock,
              "Stock|Residential|{vin} (bn m2)", brickSets,
              agg = c(bs = "all", hs = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(vin = "all"),
              silent = silent),


    ## by building shell ====
    reportAgg(v_stock,
              "Stock|Residential|{bs} (bn m2)", brickSets,
              agg = c(hs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(bs = "all"),
              silent = silent),


    ## by heating system ====
    reportAgg(v_stock,
              "Stock|Residential|{hs} (bn m2)", brickSets,
              agg = c(bs = "all", vin = "all", loc = "all", typ = "res", inc = "all"),
              rprt = c(hs = "all"),
              silent = silent),


    ## by building type + heating system ====
    reportAgg(v_stock,
              "Stock|Residential|{typ}|{hs} (bn m2)", brickSets,
              agg = c(bs = "all", vin = "all", loc = "all", inc = "all"),
              rprt = c(hs = "all", typ = "res"),
              silent = silent)

  )

  return(out)
}
