######################################################
# Helper functions:
pic.and <- function(a, b){
  return(a & b)
}

pic.or <- function(a, b){
  return(a | b)
}

pic.nand <- function(a, b){
  return(!(a & b))
}

pic.nor <- function(a, b){
  return(!(a | b))
}

pic.imp <- function(a, b){
  return((!a) | b)
}

pic.cimp <- function(a, b){
  return((!b) | a)
}

pic.inh <- function(a, b){
  return(!((!a) | b))
}

pic.cinh <- function(a, b){
  return(!((!b) | a))
}

pic.xor <- function(a, b){
  return(xor(a, b))
}

pic.xnor <- function(a, b){
  return(!(xor(a, b)))
}
################################################################################

RunLogicOperation <- function(channel.a, channel.b, op.sel = "and"){
  log.op <- switch(op.sel,
                   and = "pic.and",
                   or = "pic.or",
                   nand = "pic.nand",
                   nor = "pic.nor",
                   imp = "pic.imp",
                   inh = "pic.inh",
                   cimp = "pic.cimp",
                   cinh = "pic.cinh",
                   xor = "pic.xor",
                   xnor = "pic.xnor" )
  pic.operated <- do.call(log.op, args = list(a = channel.a, b = channel.b))
  return(pic.operated)
}
################################################################################