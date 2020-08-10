# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest, terminal

import ../src/hasts/html_ast
import hmisc/types/colorstring
import hmisc/algo/halgorithm

func cs(str: string, fg: ForegroundColor): ColoredString =
  initColoredString(str, fg = fg)


func cs(str: ColoredString, bg: BackgroundColor): ColoredString =
  # result = str
  # result.bg = bg
  str.withIt:
    it.styling.bg = bg

suite "HTML ast":
  test "Convert from colored string chunks":
    let strs = @[
      "Hello world".cs(fgRed),
      "Hello world 2".cs(fgGreen),
      "Hello world 2".cs(fgGreen).cs(bgBlue),
    ]

    let doc = @[
      strs.toHTML(),
      newHtmlText("--- === ---").wrap("p"),
      strs.toHtml(false)
    ].toDocument(
      """
span.term-fgred { color: red; }
span.term-fggreen { color: green; }
span.term-bgblue { background-color: blue; }
"""
    )

    echo doc
    "/tmp/page.html".writeFile(doc)
