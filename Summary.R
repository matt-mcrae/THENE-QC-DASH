R <- '<img src="R.png" height="60" width="60"></img>'
Y <- '<img src="Y.png" height="60" width="60"></img>'
G <- '<img src="G.png" height="60" width="60"></img>'

sumtab <- tibble(
  "Property" = c("Density", "Swell Ratio", "Ash", "Cut", "Granules/g"),
  "RV2" = c(G, G, NA, Y, G),
  "RV3" = c(G, Y, R, R, Y),
  "RV4" = c(R, G, G, Y, G)
)

rgb(0,153,255, maxColorValue = 255)


tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}")