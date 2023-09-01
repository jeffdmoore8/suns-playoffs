library(shinytest2)

test_that("{shinytest2} recording: suns-playoffs2", {
  app <- AppDriver$new(name = "suns-playoffs2", height = 630, width = 899, load_timeout = 120000)
  app$set_inputs(player = "Deandre Ayton")
  app$expect_values(output = "plot")
})
