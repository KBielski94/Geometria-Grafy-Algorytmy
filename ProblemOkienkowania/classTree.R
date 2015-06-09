
setClass("Tree")
setClass("Empty", contains="Tree")
setClass("Node", contains="Tree",
         representation=representation(elem="ANY", left="Tree", right="Tree",stow = "Tree"),
         prototype=prototype(left=new("Empty"), right=new("Empty"), stow = new("Empty")))

setClass("NodeStow", contains="Tree",
         representation=representation(elem="ANY", left="Tree", right="Tree"),
         prototype=prototype(left=new("Empty"), right=new("Empty")))



