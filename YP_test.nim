type
  ValType = (int, Variable)
  Variable = ref object
    isBound: bool
    value: ValType

iterator unify(vv: var Variable, val: ValType): bool =
  if not vv.isBound:
    vv.isBound = true
    vv.value = val
    yield false
    vv.isBound = false
  elif vv.value == val:
    yield false

iterator rangeList(M, N: int; List: var Variable): bool =
  if M >= N:
    for _ in List.unify((N, nil)): yield false
  else:
    var Tail = new Variable
    for _ in rangeList(M+1, N, Tail):
      for _ in List.unify((M, Tail)): yield false

proc main() =
  var List = new Variable
  for _ in rangeList(1, 11, List):
    var l = List
    while true:
      let h: int
      (h, l) = l.value
      echo h, " "
      if l.isNil: break
    echo

main()
