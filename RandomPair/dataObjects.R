setClass("Indexes",representation=representation(index = "ANY",listPoints = "ANY"))

setClass("Tree")
setClass("Empty", contains="Tree")
setClass("Node", contains="Tree",
         representation=representation(elem="ANY", left="Tree", right="Tree"),
         prototype=prototype(left=new("Empty"), right=new("Empty")))