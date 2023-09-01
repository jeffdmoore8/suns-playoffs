library(shinytest2)

test_that("{shinytest2} recording: suns-playoffs", {
  app <- AppDriver$new(name = "suns-playoffs", height = 675, width = 899, load_timeout = 60000)
  app$set_inputs(player = "Deandre Ayton")
  app$expect_values(output = "plot")
})
