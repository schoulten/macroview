# create theme
theme_fortietwo <- highcharter::hc_theme_merge(
  highcharter::hc_theme_elementary(),
  highcharter::hc_theme(
    chart = list(style = list(fontFamily = "Open Sans", color = "#333")),
    title = list(
      style = list(fontFamily = "Open Sans", color = "black", fontWeight = "bold"),
      align = "center"
    ),
    subtitle = list(
      style = list(fontFamily = "Open Sans", fontWeight = "bold"),
      align = "center"
    ),
    legend = list(align = "center", verticalAlign = "bottom"),
    xAxis = list(
      gridLineWidth      = 1,
      gridLineColor      = "#F3F3F3",
      lineColor          = "#F3F3F3",
      minorGridLineColor = "#F3F3F3",
      tickColor          = "#F3F3F3",
      tickWidth          = 1
    ),
    yAxis = list(
      gridLineColor      = "#F3F3F3",
      lineColor          = "#F3F3F3",
      minorGridLineColor = "#F3F3F3",
      tickColor          = "#F3F3F3",
      tickWidth          = 1
    )
  )
)
