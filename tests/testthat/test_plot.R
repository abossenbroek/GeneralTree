context("Plotting tree with grViz")
test_that("plotting a tree works", {

   tree <- GeneralTree$new(1, "parent1")
   tree$addNode(1, 2, "child.1.2")
   tree$addNode(2, 3, "child.2.3")
   tree$addNode(3, 4, "child.3.4")
   tree$addNode(3, 5, "child.3.5")
   tree$addNode(1, 7, "child.1.7")
   tree$addNode(1, 8, "child.1.8")
   tree$addNode(8, 9, "child.8.9")
   tree$addNode(9, 10, "child.9.10")
   tree$addNode(9, 11, "child.9.11")
   tree$addNode(9, 12, "child.9.12")
   tree$addNode(12, 13, "child.12.13")
   tree$addNode(8, 14, "child.8.14")
   tree$addNode(2, 6, "child.2.6")


  if (all(c("DiagrammeRsvg", "htmltools") %in%
        rownames(installed.packages(c(.Library.site, .Library))))) {
    diagrammer_tree <- GeneralTree:::generate_grViz(tree)

    svg_diagram <-
      DiagrammeRsvg::export_svg(DiagrammeR::grViz(diagrammer_tree$dot_code))
    html_file <- htmltools::html_print(htmltools::HTML(svg_diagram))

    expect_equal(tools::md5sum(html_file)[[1]],
                 "658fcfdf8333339399460fb9394b302b")
  }

  diagram <- "digraph {\n
  \"1\" [label = \"1\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"2\" [label = \"2\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"3\" [label = \"3\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"4\" [label = \"4\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"5\" [label = \"5\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"6\" [label = \"6\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"7\" [label = \"7\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"8\" [label = \"8\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"9\" [label = \"9\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"10\" [label = \"10\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"11\" [label = \"11\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"12\" [label = \"12\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"13\" [label = \"13\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"14\" [label = \"14\", style = \"filled\", color = \"gray\", shape = \"rectangle\"] 
  \"1\"->\"2\" 
  \"1\"->\"7\" 
  \"1\"->\"8\" 
  \"2\"->\"3\" 
  \"2\"->\"6\" 
  \"3\"->\"4\" 
  \"3\"->\"5\" 
  \"8\"->\"9\" 
  \"8\"->\"14\" 
  \"9\"->\"10\" 
  \"9\"->\"11\" 
  \"9\"->\"12\" 
  \"12\"->\"13\" 
}"

  expect_identical(plot(tree)$x$diagram, diagram)

  diagram_data_cyan_circle <- "digraph {\n
  \"parent1\" [label = \"parent1\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.1.2\" [label = \"child.1.2\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.2.3\" [label = \"child.2.3\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.3.4\" [label = \"child.3.4\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.3.5\" [label = \"child.3.5\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.2.6\" [label = \"child.2.6\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.1.7\" [label = \"child.1.7\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.1.8\" [label = \"child.1.8\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.8.9\" [label = \"child.8.9\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.9.10\" [label = \"child.9.10\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.9.11\" [label = \"child.9.11\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.9.12\" [label = \"child.9.12\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.12.13\" [label = \"child.12.13\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"child.8.14\" [label = \"child.8.14\", color = \"cyan\", shape = \"circle\", style = \"filled\"] 
  \"parent1\"->\"child.1.2\" 
  \"parent1\"->\"child.1.7\" 
  \"parent1\"->\"child.1.8\" 
  \"child.1.2\"->\"child.2.3\" 
  \"child.1.2\"->\"child.2.6\" 
  \"child.2.3\"->\"child.3.4\" 
  \"child.2.3\"->\"child.3.5\" 
  \"child.1.8\"->\"child.8.9\" 
  \"child.1.8\"->\"child.8.14\" 
  \"child.8.9\"->\"child.9.10\" 
  \"child.8.9\"->\"child.9.11\" 
  \"child.8.9\"->\"child.9.12\" 
  \"child.9.12\"->\"child.12.13\" 
}"
  
  expect_identical(plot(tree, what = "data", color = "cyan", shape =
                        "circle")$x$diagram, diagram_data_cyan_circle)

})


