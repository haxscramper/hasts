##[

Statically typed ast for subset of HTML

Originally implemented to generate graphviz html-like labels.

]##

import colors, xmltree, strformat, strutils, strtabs, sequtils, terminal,
       macros
import hmisc/helpers
import hmisc/types/[hprimitives, colorstring]

type
  HtmlElemKind* = enum
    hekTable
    hekRow
    hekCell

    hekElemList
    hekImage
    hekOther
    hekText

  HtmlTextProp* = enum
    htpNone
    htpBold
    htpUnderline
    htpItalic


  HtmlElem* = object
    elements*: seq[HtmlElem]
    attrs*: StringTableRef
    class*: seq[string]
    case kind*: HtmlElemKind
      of hekTable:
        border*: int
        bgcolor*: Color
        height*: int
        width*: int
      of hekText:
        textProps*: set[HtmlTextProp]
        textColor* {.requiresinit.}: Color
        textColorBg* {.requiresinit.}: Color
        textStr*: string
      of hekCell:
        cellColor*: Color
        cellBgColor*: Color
        cellSize*: ArrSize
        dotPort*: int
      of hekOther:
        tagname*: string
      else:
        discard

func `[]`*(html: HtmlElem, idx: int): HtmlElem =
  html.elements[idx]

func `[]`*(html: var HtmlElem, idx: int): var HtmlElem =
  html.elements[idx]

func `[]=`*(html: var HtmlElem, idx: int, other: HtmlElem): void =
  html.elements[idx] = other

func `[]=`*(html: var HtmlElem, attrname: string, attrval: string): void =
  if html.attrs == nil:
    html.attrs = newStringTable()

  # debugecho "set ", attrname, " as ", attrval
  html.attrs[attrname] = attrval

func add*(html: var HtmlElem, other: HtmlElem): void =
  assert html.kind notin {hekText}
  html.elements.add other

func len*(html: HtmlElem): int = html.elements.len

func toHtmlText*(text: string,
                 color: Color = colNoColor,
                 props: set[HtmlTextProp] = {},
                 bgColor: Color = colNoColor
                ): HtmlElem =
  HtmlElem(kind: hekText,
           textStr: text,
           textColor: color,
           textProps: props,
           textColorBg: bgColor)

func newElementHtml*(tag: string): HtmlElem =
  HtmlElem(kind: hekOther, tagname: tag)

func newHtmlText*(text: string): HtmlElem =
  HtmlElem(kind: hekText, textStr: text,
           textColor: colNoColor,
           textColorBg: colNoColor)

func newTree*(tag: string,
             subitems: openarray[HtmlElem],
             attrs: openarray[(string, string)] = @[]): HtmlElem =

  result = newElementHtml(tag)
  for it in subitems:
    result.add it

  # for attr in attrs:
  result.attrs = attrs.toXmlAttributes()

func newTree(subitems: seq[HtmlElem]): HtmlElem =
  HtmlElem(kind: hekElemList, elements: subitems)

func newTree*(subitems: seq[HtmlElem], tag: string): HtmlElem =
  newTree(tag, subitems)

func newElemList*(): HtmlElem = HtmlElem(kind: hekElemList, elements: @[])

func toHtmlCell*(content: HtmlElem): HtmlElem =
  if content.kind == hekCell:
    content
  else:
    HtmlElem(kind: hekCell, elements: @[content])


func toHtmlCell*(strbl: string): HtmlElem =
  HtmlElem(kind: hekCell, elements: @[strbl.toHtmlText()])


func toHtmlCell*(strbl: StrBlock): HtmlElem =
  toHtmlCell(strbl.join("\n"))

func toHtmlRow*(cell: HtmlElem): HtmlElem =
  HtmlElem(kind: hekRow, elements: @[toHtmlCell(cell)])

func toHtmlRow*(cells: seq[HtmlElem]): HtmlElem =
  HtmlElem(kind: hekRow, elements: cells)

func toHtmlTable*(cells: seq[seq[HtmlElem]]): HtmlElem =
  HtmlElem(kind: hekTable, elements: cells.map(toHtmlRow))

func toHtmlTableHoriz*(cells: seq[HtmlElem]): HtmlElem =
  HtmlElem(kind: hekTable,
           elements: @[toHtmlRow(cells.map(toHtmlCell))])

func toHtmlTableVert*(cells: seq[HtmlElem]): HtmlElem =
  HtmlElem(kind: hekTable,
           elements: cells.mapIt(it.toHtmlCell().toHtmlRow()))

func setOrAddCell*(table: var HtmlElem, pos: ArrPos, cell: HtmlElem): void =
  assert table.kind == hekTable
  if table.elements.len <= pos.row:
    for _ in table.elements.len .. pos.row:
      table.elements.add HtmlElem(kind: hekRow)

  if table.elements[pos.row].len <= pos.col:
    for _ in table.elements[pos.row].len .. pos.col:
      table.elements[pos.row].add HtmlElem(kind: hekCell)

  table[pos.row][pos.col] = cell


func wrap*(xml: HtmlElem, tag: string): HtmlElem =
  result = newElementHtml(tag)
  result.add xml

func wrap*(xml: XmlNode, tag: string): XmlNode =
  result = newElement(tag)
  result.add xml


func wrap*(xml: seq[XmlNode], tag: string): XmlNode =
  result = newElement(tag)
  for node in xml:
    result.add node


func wrap*(xml: seq[HtmlElem], tag: string): HtmlElem =
  result = newElementHtml(tag)
  for node in xml:
    result.add node

func add*(xml: var XmlNode, sub: seq[XmlNode]): void =
  for it in sub:
    xml.add it

func `[]=`*(xml: var XmlNode, attrname: string, attrval: string): void =
  var attrs = xml.attrs()
  if attrs != nil:
    attrs[attrname] = attrval
    xml.attrs = attrs
  else:
    xml.attrs = {attrname : attrval}.toXmlAttributes()


macro orCond*(body: untyped): untyped =
  var conditions: seq[(int, NimNode)]

  result = newStmtList()
  for idx, element in body:
    case element.kind:
      of nnkIfStmt:
        let
          condId = ident &"cond{idx}"
          body = element[0][1]
          cond = element[0][0]

        conditions.add (idx, cond)

        result.add quote do:
          if `condId`:
            `body`
      else:
        result.add element

  var
    tmp = newStmtList()
    condVars: seq[NimNode]
  for (idx, cond) in conditions:
    let condId = ident &"cond{idx}"
    condVars.add condId
    tmp.add quote do:
      let `condId` = `cond`


  let topCond = condVars.foldl(nnkInfix.newTree(ident "or", a, b))

  result = quote:
    `tmp`
    if `topCond`:
      `result`


func toXml*(html: HtmlElem): seq[XmlNode] =
  var tmpres: XmlNode
  case html.kind:
    of hekTable:
      tmpres = newElement("table")
      tmpres["border"] = $html.border
    of hekRow:
      tmpres = newElement("tr")
    of hekCell:
      tmpres = newElement("td")
      if html.dotPort != 0:
        tmpres["port"] = "t" & $html.dotPort

    of hekText:
      tmpres = newText(html.textStr)
      orCond:
        tmpres = tmpres.wrap("font")
        if html.textColor != colNoColor:
          tmpres["color"] = $html.textColor

        if html.textColorBg != colNoColor:
          tmpres["background-color"] = $html.textColorBg

      for prop in html.textProps:
        tmpres =
          case prop:
            of htpBold: tmpres.wrap("b")
            of htpUnderline: tmpres.wrap("u")
            of htpItalic: tmpres.wrap("i")
            of htpNone: tmpres
    of hekOther:
      tmpres = newElement(html.tagname)
    of hekElemList:
      return html.elements.mapIt(it.toXML()).concat()
      # tmpres = newText("")
      # for c in html.elements:
      #   tmpres.add c.toXml()
    else:
      discard

  if html.class.len > 0:
    tmpres["class"] = html.class.join(" ")

  if html.attrs != nil:
    var tmp = tmpres.attrs
    for key, val in pairs(html.attrs):
      tmp[key] = val

    tmpres.attrs = tmp

  for row in html.elements:
    tmpres.add row.toXml()

  return @[ tmpres ]


proc toPrettyStr*(n: XmlNode): string = add(result, n, 0, 2, true)
proc toFlatStr*(n: XmlNode): string = add(result, n, 0, 0, false)
proc toPrettyStr*(n: HtmlElem): string =
  n.toXml().mapIt(toPrettyStr(it)).join("\n")

proc toFlatStr*(n: HtmlElem): string =
  n.toXml().mapIt(toFlatStr(it)).join("\n")

func `$`*(html: HtmlElem): string =
  html.toXml().mapIt($it).join("\n")

func toHtmlDoc*(html: HtmlElem): string =
  $html.toXml().wrap("body").wrap("html")


func toDocument*(html: HtmlElem, styleSection: string = ""): string =
  var buf = newElemList()
  if styleSection.len > 0:
    buf.add newHtmlText("\n" & styleSection ).wrap("style")

  buf.add html.wrap("body")
  buf.wrap("html").toPrettyStr()

func toDocument*(html: seq[HtmlElem], styleSection: string = ""): string =
  html.newTree().toDocument(styleSection)

#=======================  Colored string to html  ========================#

func toHTML*(str: ColoredString, selector: bool = true): HtmlElem =
  if selector:
    result = newElementHtml("span")
    result.class.add case str.fg:
      of fgDefault:
        @[]
      else:
        @["term-" & ($str.fg).toLowerAscii()]

    result.class.add case str.bg:
      of bgDefault:
        @[]
      else:
        @["term-" & ($str.bg).toLowerAscii()]

    result.add newHtmlText(str.str)
  else:
    result = newHtmlText(str.str)
    result.textColor = case str.fg:
      of fgDefault: colNoColor
      of fgRed: colRed
      of fgGreen: colGreen
      else: colBlue


    result.textColorBg = case str.bg:
      of bgDefault: colNoColor
      of bgRed: colRed
      of bgGreen: colGreen
      else: colBlue

    # result.add newHtmlText(str.str)

func toHtml*(strs: openarray[ColoredString],
             selector: bool = true): HtmlElem =
  strs.mapIt(it.toHTML(selector)).newTree()
