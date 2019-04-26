package dev.habla.optica

package object `triplet` {

  type VarTree = ITree[OpticType, (String, Boolean)]

  type Triplet = (List[TExpr], VarTree, Set[TExpr])

  type TripletFun = Triplet => Triplet
}

