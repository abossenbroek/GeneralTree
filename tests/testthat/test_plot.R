context('Plotting tree with grViz')
test_that('plotting a tree works', {

   tree <- GeneralTree$new(1, 'parent1')
   tree$addNode(1, 2, 'child.1.2')
   tree$addNode(2, 3, 'child.2.3')
   tree$addNode(3, 4, 'child.3.4')
   tree$addNode(3, 5, 'child.3.5')
   tree$addNode(1, 7, 'child.1.7')
   tree$addNode(1, 8, 'child.1.8')
   tree$addNode(8, 9, 'child.8.9')
   tree$addNode(9, 10, 'child.9.10')
   tree$addNode(9, 11, 'child.9.11')
   tree$addNode(9, 12, 'child.9.12')
   tree$addNode(12, 13, 'child.12.13')
   tree$addNode(8, 14, 'child.8.14')
   tree$addNode(2, 6, 'child.2.6')


  if (all(c('DiagrammeRsvg', 'htmltools') %in%
        rownames(installed.packages(c(.Library.site, .Library))))) {
    svg_diagram <-
      DiagrammeRsvg::export_svg(DiagrammeR::grViz(plot(tree)$dot_code))
    html_file <- htmltools::html_print(htmltools::HTML(svg_diagram))

    expect_equal(tools::md5sum(html_file)[[1]], "658fcfdf8333339399460fb9394b302b")
  }

  nodes_df <- structure(list(nodes = c("1", "2", "3", "4", "5", "6", "7", "8",
                                       "9", "10", "11", "12", "13", "14"),
                             type = c("lower", "lower",  "lower", "lower",
                                      "lower", "lower", "lower", "lower",
                                      "lower",  "lower", "lower", "lower",
                                      "lower", "lower"),
                             label = c("", "", "", "", "", "", "", "", "", "", "", "", "", ""),
                             style = c("filled", "filled", "filled", "filled",
                                       "filled", "filled", "filled", "filled",
                                       "filled", "filled", "filled", "filled",
                                       "filled", "filled"),
                             color = c("aqua", "aqua", "aqua", "aqua", "aqua",
                                       "aqua",      "aqua", "aqua", "aqua",
                                       "aqua", "aqua", "aqua", "aqua", "aqua"
                                       ),
                             shape = c("circle", "circle", "circle", "circle",
                                       "circle",      "circle", "circle",
                                       "circle", "circle", "circle", "circle",
                                       "circle", "circle", "circle"),
                             data = c("1|parent1", "2|child.1.2",
                                      "3|child.2.3", "4|child.3.4",
                                      "5|child.3.5", "6|child.2.6",
                                      "7|child.1.7", "8|child.1.8",
                                      "9|child.8.9", "10|child.9.10",
                                      "11|child.9.11", "12|child.9.12",
                                      "13|child.12.13", "14|child.8.14")),
                        .Names = c("nodes", "type", "label", "style", "color",
                                   "shape", "data"),
                        row.names = c(NA, -14L), class = "data.frame")

  expect_identical(plot(tree)$nodes_df, nodes_df)
})

