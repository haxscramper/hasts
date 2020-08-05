import strutils, strformat, sequtils, ropes
import hmisc/helpers

type
  LAstNodeKind* = enum
    lnkMacro
    lnkEnv
    lnkTable
    lnkPlaintext

  LAstGroup* = object
    delims*: (string, string)
    body*: LAstNode

  LACellAlign* = enum
    lalLeft
    lalRight
    lalCenter

  LACellSpec* = object
    align*: LACellAlign
    border*: seq[bool] ## Specification for border of the right cell side
    case fixedsise*: bool
      of false:
        nil
      of true:
        sizeCm*: float

  LAstNode* = object
    case kind*: LAstNodeKind
      of lnkMacro:
        macroName*: string
        macroArgs*: seq[LAstGroup]
      of lnkEnv:
        envBody*: seq[LAstNode]
        envArgs*: seq[LastGroup]
      of lnkPlaintext:
        plaintextStr*: string
      of lnkTable:
        colSpecs*: seq[LACellSpec]
        leftBorder*: seq[bool] ## Specification for leftmost border of
                               ## the grid
        tableRows*: seq[seq[LAstNode]]

func makeBraceGroup*(body: LAstNode): LAstGroup =
  LAstGroup(delims: ("{", "}"), body: body)

func makePlaintext*(text: string): LAstNode =
  LAstNode(kind: lnkPlaintext, plaintextStr: text)

func makeMacroCall*(name: string, args: seq[LAstNode]): LAstNode =
  LastNode(
    kind: lnkMacro,
    macroName: name,
    macroArgs: args.mapIt(it.makeBraceGroup())
  )

func `&!`(ropes: seq[Rope]): Rope = `&`(ropes)
func concat(ropes: seq[Rope]): Rope = `&`(ropes)
func `&!`(a: Rope; b: Rope): Rope = {.noSideEffect.}: a & b
func `&!`(a: Rope; b: string): Rope = {.noSideEffect.}: a & b
func `&!`(a: string; b: Rope): Rope = {.noSideEffect.}: a & b
func rope(s: string): Rope = {.noSideEffect.}: ropes.rope(s)


func toRope*(node: LAstNode): Rope
func toRope*(group: LAstGroup): Rope =
  group.delims[0] &! group.body.toRope() &! group.delims[1]

func toRope*(node: LAstNode): Rope =
  case node.kind:
    of lnkMacro:
      return rope(&"\\{node.macroName}") &!
        node.macroArgs.mapIt(it.toRope()).concat()
    of lnkPlaintext:
      return rope(node.plaintextStr)
    else:
      discard

when isMainModule:
  echo makeMacroCall("intl", @[makePlaintext("12")]).toRope()
