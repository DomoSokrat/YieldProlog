import macros

macro toItr*(x: ForLoopStmt): untyped =
  let expr = x[0]
  let call = x[1][1] # Get foo out of toItr(foo)
  let body = x[2]
  result = quote do:
    block:
      let itr = `call`
      for `expr` in itr():
        `body`

func wrapRecCalls(node: var NimNode; name: NimNode) =
  for i in 0..<node.len:
    var child = node[i]
    wrapRecCalls(child, name)
  if node.kind in {nnkCall, nnkCommand} and node[0] == name:
    if node.len > 1:
      node[1] = node.copyNimTree
      node.del(2, node.len-2)
    else:
      node.add(node.copyNimTree)
    node[0] = ident"toItr"

macro makeRecIter(name: untyped, args: varargs[untyped]): untyped =
  let retType = args[^2]
  var params: seq[NimNode]
  let procRetType = nnkIteratorTy.newTree(
    nnkFormalParams.newTree(retType), newEmptyNode())
  params.add procRetType
  for i in 0..args.len-3:
    params.add nnkIdentDefs.newTree(args[i][0], args[i][1], newEmptyNode())
  var body = args[^1][0]
  wrapRecCalls(body, name)
  let procBody = quote do:
    result = iterator(): `retType` = `body`
  result = newProc(name, params, procBody)

makeRecIter(cd, n: int, int):
  if n > 0:
    yield n
    for e in cd(n-1):
      yield e

var i = cd(6)
for e in i():
  echo e
  if e == 4:
    discard i()
echo()
for e in toItr(cd(6)):
  echo e

macro recIter(arg: untyped) =
  echo arg.name.strVal
  echo treeRepr(arg)
  arg

iterator cd1(n: int): int {.recIter.} =
  if n > 0:
    yield n
    for e in cd1(n-1):
      yield e
