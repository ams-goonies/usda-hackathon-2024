contactTab <- div(
  class = "contact",
  img(
    src = "images/logo-appsilon.svg",
    class = "logo-img",
    alt = "Appsilon Logo"
  ),
  tags$h4(
    tags$span("Visit us on "),
    tags$a(
      href = "https://appsilon.com/",
      target = "_blank",
      rel = "nofollow noreferrer",
      "appsilon.com"
    )
  )
)
