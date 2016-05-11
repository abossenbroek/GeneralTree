  tree <- GeneralTree$new(0, 'parent1')
  tree$addNode(0, 'a', 'child.a')
  tree$addNode(0, 'b', 'child.b')
  tree$addNode('b', 'c', 'child.b.c')
  tree$addNode('b', 'd', 'child.b.d')
  tree$addNode('c', 'e', 'child.c.e')
  tree$addNode('c', 'f', 'child.c.f')
  tree$addNode('c', 'g', 'child.c.g')

  i <- tree$nextElem()
  i$data
  i <- i$nextElem()
  i$data
  i <- i$nextElem()
  i$data
  i <- i$nextElem()
  i$data
  i <- i$nextElem()
  i$data
  i <- i$nextElem()
  i$data
  i <- i$nextElem()
  i$data
  i <- i$nextElem()
  i$data

  i <- i$nextElem()
  i$data
