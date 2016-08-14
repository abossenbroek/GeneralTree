context('Plotting tree with grViz')
test_that('plotting a tree works', {

  if (all(c('DiagrammeRsvg', 'htmltools') %in%
        rownames(installed.packages(c(.Library.site, .Library))))) {
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

    svg_diagram <-
      DiagrammeRsvg::export_svg(DiagrammeR::grViz(plot(tree)$dot_code))
    html_file <- htmltools::html_print(htmltools::HTML(svg_diagram))

    expect_equal(tools::md5sum(html_file)[[1]], "658fcfdf8333339399460fb9394b302b")
  }
})

