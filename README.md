# Fuddle

Fuddle is a continuation of the Elm language approach to web applications, with a more open mind on Javascript cooperation, extension and experimentation.

It includes:

* the ordinary Elm-derived core syntax,
* the new Fuddle declarations and effect syntax,
* the `do` / `catch` extension,
* the `@anchor` HTML sugar,
* the declaration kinds needed for regions, anchors, cells, natives, and threads.

This is a **surface-language specification**, not yet a formal static semantics document. Where needed, I note desugarings and namespace rules so the compiler work can proceed.

---

# 1. Scope of this definition

This definition covers the **Fuddle surface language** used in this conversation.

It includes:

* modules and imports,
* type declarations,
* value declarations,
* patterns,
* expressions,
* records, lists, tuples,
* effect annotations,
* `do` blocks,
* `catch` on `do`,
* `region` for client/server specialisation,
* `anchor`, `cell` for interaction binding,
* `native` invocation for EW dynamic library access,
* `thread` for distributed execution (in-page, in-browser, remote),
* HTML `@anchor` syntax.

It will include:
* embedded Javascript code blocks,
* Fuddle export to Javascript,
* specialized template structure.


It does **not** include:

* legacy Elm ports,
* kernel/native JavaScript constructs,
* user-defined infix declarations,

Those may be added later, but they are out of scope for the first compiler implementation of the new Fuddle features.

---

# 2. Lexical structure

## 2.1 Character set

Source files are Unicode text. Identifiers are restricted as below; string and comment contents may contain general Unicode.

## 2.2 Whitespace

Whitespace includes spaces, tabs, and line terminators.

Like Elm, Fuddle is **layout-sensitive**. Blocks after constructs such as:

* `let`
* `case ... of`
* `do`
* top-level declarations

use an offside rule. The grammar below is written using abstract `Block(...)` forms.

## 2.3 Comments

### Line comments

```text
-- comment until end of line
```

### Block comments

```text
{- block comment -}
```

Block comments are **nestable**.

## 2.4 Literals

```ebnf
intLit          ::= digit { digit | "_" }
floatLit        ::= digit { digit | "_" } "." digit { digit | "_" } [ exponent ]
exponent        ::= ("e" | "E") [ "+" | "-" ] digit { digit | "_" }

charLit         ::= "'" charContent "'"
stringLit       ::= "\"" stringChar* "\""
multiStringLit  ::= "\"\"\"" multiStringChar* "\"\"\""

literal         ::= intLit
                  | floatLit
                  | charLit
                  | stringLit
                  | multiStringLit
```

---

# 3. Identifier classes and namespaces

## 3.1 Identifier classes

```ebnf
lowerIdent   ::= ( "a".."z" | "_" ) { "a".."z" | "A".."Z" | "0".."9" | "_" | "'" }
upperIdent   ::= ( "A".."Z" )       { "a".."z" | "A".."Z" | "0".."9" | "_" | "'" }

moduleName   ::= upperIdent { "." upperIdent }

lowerQName   ::= [ moduleName "." ] lowerIdent
upperQName   ::= [ moduleName "." ] upperIdent
```

## 3.2 Reserved keywords

```text
module exposing import as
type alias
if then else
case of
let in
do catch
region anchor cell native thread
```

These keywords may not be used as identifiers.

## 3.3 Namespaces

Fuddle uses distinct semantic namespaces:

### Module namespace

* module names

### Uppercase declaration namespace

* type constructors
* data constructors
* region identifiers
* authority identifiers such as `ServerAuth`, `BrowserAuth`

### Lowercase declaration namespace

* values/functions
* anchors
* cells
* natives
* threads
* local bindings
* field names

### Field namespace

Field labels in records are lowercase identifiers.

## 3.4 Region naming rule

Per the decision taken in this conversation:

* **only region identifiers are uppercase among the new Fuddle declarations**
* all other new declarative symbols remain lowercase

Examples:

```fuddle
region ItemsWindow : ServerAuth
region ItemsOverlay : BrowserAuth

anchor windowRoot : ItemsWindow { offset : Int, limit : Int, total : Int }

cell busy : Bool = False

thread loadWindow : WindowRef -> ()
native itemsListWindowN : WindowRef -> RawWindowData
```

---

# 4. Source file grammar

## 4.1 Module

```ebnf
sourceFile      ::= moduleDecl importDecl* topDecl*

moduleDecl      ::= "module" moduleName "exposing" "(" exposingList ")"

exposingList    ::= ".."
                  | exposingItem { "," exposingItem }

exposingItem    ::= lowerIdent
                  | upperIdent
                  | upperIdent "(" ".." ")"
```

## 4.2 Imports

```ebnf
importDecl      ::= "import" moduleName [ "as" upperIdent ] [ "exposing" "(" exposingList ")" ]
```

---

# 5. Top-level declarations

```ebnf
topDecl         ::= typeAliasDecl
                  | typeDecl
                  | valueSigDecl
                  | valueDefDecl
                  | regionDecl
                  | anchorDecl
                  | cellDecl
                  | nativeDecl
                  | threadSigDecl
                  | threadDefDecl
```

## 5.1 Type aliases

```ebnf
typeAliasDecl   ::= "type" "alias" upperIdent typeVar* "=" typeExpr
typeVar         ::= lowerIdent
```

Example:

```fuddle
type alias WindowRef =
  { offset : Int
  , limit : Int
  }
```

## 5.2 Union types

```ebnf
typeDecl        ::= "type" upperIdent typeVar* "=" ctorDecl { "|" ctorDecl }
ctorDecl        ::= upperIdent ctorArg*
ctorArg         ::= typeAtom
```

Example:

```fuddle
type Status
  = New
  | Active
  | Archived
```

## 5.3 Ordinary value signatures and definitions

```ebnf
valueSigDecl    ::= lowerIdent ":" typeExpr

valueDefDecl    ::= lowerIdent patternAtom* "=" expr
```

Examples:

```fuddle
statusLabel : Status -> String

statusLabel st =
  case st of
    New -> "new"
    Active -> "active"
    Archived -> "archived"
```

## 5.4 Regions

```ebnf
regionDecl      ::= "region" upperIdent ":" authorityRef
authorityRef    ::= upperQName
```

Examples:

```fuddle
region ItemsWindow : ServerAuth
region ItemsOverlay : BrowserAuth
```

## 5.5 Anchors

```ebnf
anchorDecl      ::= "anchor" lowerIdent ":" anchorType

anchorType      ::= anchorIndex* regionRef anchorMetaType
anchorIndex     ::= typeAtom "->"
regionRef       ::= upperQName
anchorMetaType  ::= recordType
```

Examples:

```fuddle
anchor windowRoot :
  ItemsWindow
    { offset : Int
    , limit : Int
    , total : Int
    }

anchor rowLine :
  RowUid ->
    ItemsWindow
      { uid : RowUid }
```

Notes:

* the final record type is required,
* anchor metadata must be a record type,
* an anchor family is represented by one or more leading index arrows.

## 5.6 Cells

```ebnf
cellDecl        ::= "cell" lowerIdent ":" typeExpr "=" expr
```

Examples:

```fuddle
cell busy : Bool = False
cell editUid : Maybe RowUid = Nothing
cell editName : String = ""
```

A cell initializer is an expression. It may be a constant or a pure initializer expression such as `init (\ctx -> ...)`. The parser does not distinguish these; purity is a static-semantic rule.

## 5.7 Natives

```ebnf
nativeDecl      ::= "native" lowerIdent ":" typeExpr
```

Examples:

```fuddle
native itemsListWindowN : WindowRef -> RawWindowData ! <fail DbErr>
native itemsDeleteRowN : { uid : RowUid } -> () ! <fail DbErr>
```

## 5.8 Threads

```ebnf
threadSigDecl   ::= "thread" lowerIdent ":" typeExpr
threadDefDecl   ::= "thread" lowerIdent patternAtom* "=" expr
```

Examples:

```fuddle
thread loadWindow : WindowRef -> ()
thread loadWindow =
  renderInto ItemsWindow ReplaceInner windowFragment loadWindowData

thread openEdit : Row -> ()
thread openEdit row =
  do
    set editUid (Just row.uid)
    set editName row.name
```

A thread definition with parameters is equivalent to a lambda-bound thread body.

---

# 6. Type grammar

## 6.1 Overview

Types are Elm-like, extended with **effect suffixes**.

An effect suffix applies to the type immediately to its left and binds **more tightly** than `->`.

So:

```fuddle
A -> B ! <impure>
```

parses as:

```text
A -> (B ! <impure>)
```

not as:

```text
(A -> B) ! <impure>
```

## 6.2 Grammar

```ebnf
typeExpr        ::= funcType

funcType        ::= effType [ "->" funcType ]

effType         ::= appType [ "!" effectRow ]

appType         ::= typeAtom { typeAtom }

typeAtom        ::= lowerIdent
                  | upperQName
                  | "(" typeExpr ")"
                  | tupleType
                  | recordType

tupleType       ::= "(" typeExpr "," typeExpr { "," typeExpr } ")"

recordType      ::= "{" [ fieldTypeList ] [ "|" lowerIdent ] "}"
fieldTypeList   ::= fieldType { "," fieldType }
fieldType       ::= lowerIdent ":" typeExpr
```

Examples:

```fuddle
WindowRef
Maybe RowUid
List Row
RowUid -> ItemsWindow { uid : RowUid }    -- only in anchor declarations
WindowRef -> WindowData ! <session, native, impure, fail LoadErr>
```

## 6.3 Effect rows

```ebnf
effectRow       ::= "<" [ effectItem { "," effectItem } ] ">"

effectItem      ::= lowerIdent
                  | "fail" typeExpr
```

Examples:

```fuddle
<>
<impure>
<cell, impure>
<session, native, impure, fail LoadErr>
<dom, thread>
```

### Initial built-in effect labels

The current design uses these labels:

* `impure`
* `cell`
* `dom`
* `thread`
* `session`
* `native`
* `foreign`
* `fail E`

The parser only needs the grammar above. The compiler will validate supported labels.

### Row normalization

Order is semantically irrelevant:

```fuddle
<dom, cell>
```

and

```fuddle
<cell, dom>
```

are the same effect row.

Duplicates are invalid after normalization.

---

# 7. Pattern grammar

```ebnf
pattern         ::= consPattern

consPattern     ::= appPattern [ "::" consPattern ]

appPattern      ::= patternAtom { patternAtom }

patternAtom     ::= "_"
                  | lowerIdent
                  | upperQName
                  | literal
                  | "(" pattern ")"
                  | tuplePattern
                  | listPattern
                  | recordPattern

tuplePattern    ::= "(" pattern "," pattern { "," pattern } ")"

listPattern     ::= "[" [ pattern { "," pattern } ] "]"

recordPattern   ::= "{" [ lowerIdent { "," lowerIdent } ] "}"
```

Examples:

```fuddle
_
uid
Just uid
( x, y )
[ a, b, c ]
head :: tail
{ offset, limit }
```

---

# 8. Expression grammar

## 8.1 Overview

Fuddle expressions are Elm-like, extended with:

* `do`
* `catch`
* `@anchor` HTML sugar

Most UI/runtime constructs such as:

* `within`
* `watch`
* `run`
* `call`
* `bind`
* `local`
* `renderInto`
* `onFail`
* `onMount`
* `onClick`
* `get`
* `set`
* `metaOf`
* `patch`

are **ordinary identifiers in the standard library / compiler prelude**, not special syntax.

## 8.2 Grammar

```ebnf
expr            ::= ifExpr
                  | caseExpr
                  | letExpr
                  | lambdaExpr
                  | doExpr
                  | opExpr
```

### If

```ebnf
ifExpr          ::= "if" expr "then" expr "else" expr
```

### Case

```ebnf
caseExpr        ::= "case" expr "of" Block(caseArm)
caseArm         ::= pattern "->" expr
```

### Let

```ebnf
letExpr         ::= "let" Block(letDecl) "in" expr
letDecl         ::= valueSigDecl | valueDefDecl
```

### Lambda

```ebnf
lambdaExpr      ::= "\" pattern+ "->" expr
```

### Do / catch

```ebnf
doExpr          ::= "do" Block(doStmt) [ catchClause ]

doStmt          ::= pattern "<-" expr
                  | "let" Block(letDecl)
                  | expr

catchClause     ::= "catch" ( catchArmInline | Block(catchArm) )

catchArmInline  ::= pattern "->" expr
catchArm        ::= pattern "->" expr
```

Examples:

```fuddle
do
  raw <- call itemsListWindowN ref
  decorateWindowWithUuids raw
catch err ->
  pure emptyWindow
```

or:

```fuddle
do
  raw <- call itemsListWindowN ref
  decorateWindowWithUuids raw
catch
  DbUnavailable info ->
    pure emptyWindow

  DbPermissionDenied ->
    raise LoadPermissionDenied
```

### Operator/application expression

The parser should implement Elm-style operator precedence, plus the Fuddle anchored-element sugar described below.

---

# 9. Atomic and compound expression forms

## 9.1 Primaries

```ebnf
primaryExpr     ::= literal
                  | lowerQName
                  | upperQName
                  | accessorExpr
                  | listExpr
                  | recordExpr
                  | recordUpdateExpr
                  | "(" expr ")"
                  | tupleExpr
                  | anchoredExpr
```

## 9.2 Lists

```ebnf
listExpr        ::= "[" [ expr { "," expr } ] "]"
```

## 9.3 Tuples

```ebnf
tupleExpr       ::= "(" expr "," expr { "," expr } ")"
```

## 9.4 Records

```ebnf
recordExpr      ::= "{" [ fieldExprList ] "}"
fieldExprList   ::= fieldExpr { "," fieldExpr }
fieldExpr       ::= lowerIdent "=" expr
```

### Record update

```ebnf
recordUpdateExpr ::= "{" lowerQName "|" fieldExprList "}"
```

## 9.5 Record accessor function

```ebnf
accessorExpr    ::= "." lowerIdent
```

This is the same surface as Elm’s field accessor shorthand.

## 9.6 Field selection

Field selection is parsed as a postfix operator on expressions.

Example:

```fuddle
row.uid
m.offset
```

---

# 10. Anchored HTML sugar

This is one of the new Fuddle surface constructs.

## 10.1 Surface syntax

```ebnf
anchoredExpr    ::= htmlHead "@" anchorArg metadataArg attrsArg childrenArg

htmlHead        ::= lowerQName
                  | "(" expr ")"

anchorArg       ::= lowerQName
                  | "(" expr ")"

metadataArg     ::= recordExpr
attrsArg        ::= listExpr
childrenArg     ::= listExpr
```

Examples:

```fuddle
div @windowRoot
  { offset = win.offset
  , limit = win.limit
  , total = win.total
  }
  [ class "rounded-xl border" ]
  [ ... ]

tr @(rowLine row.uid)
  { uid = row.uid }
  [ class "border-b" ]
  [ ... ]
```

## 10.2 Desugaring

The parser or desugaring pass should transform:

```fuddle
E @A M AS CS
```

into:

```fuddle
node A M (E AS CS)
```

where:

* `E` is the HTML constructor head,
* `A` is the anchor expression,
* `M` is the metadata record literal,
* `AS` is the attribute list,
* `CS` is the children list.

## 10.3 Restriction

The metadata argument must be a **record expression**. This keeps metadata structurally typed and compiler-visible.

---

# 11. Operator precedence and associativity

For the first compiler implementation, use Elm-compatible precedence for the built-in operators.

A practical precedence ladder is:

1. field selection `e.f`
2. anchored form `html @anchor meta attrs children`
3. function application
4. unary `-`
5. `* / // %`
6. `+ -`
7. `::`
8. `++`
9. comparisons: `== /= < > <= >=`
10. `&&`
11. `||`
12. right pipe `<|`
13. left pipe `|>`

### Notes

* `@` is **not** a general infix operator. It only occurs in the anchored-element form above.
* User-defined infix declarations are out of scope for vNext syntax.

---

# 12. Predeclared ordinary identifiers

The following are **not parser keywords**. They are ordinary lower-case identifiers expected to be available in the standard environment or compiler-recognized prelude.

## 12.1 UI/runtime constructors and combinators

```text
within
watch
run
call
bind
textControl
choiceControl
local
renderInto
onFail
onMount
onClick
```

## 12.2 Browser/cell/DOM operations

```text
get
set
metaOf
patch
classAdd
classDrop
show
hide
```

## 12.3 Effect/failure operations

```text
pure
raise
attempt
recover
mapFail
```

These do not need parser support beyond ordinary expression parsing.

---

# 13. Region/anchor/cell/native/thread resolution rules

These are **semantic** rules, but the compiler side needs them early.

## 13.1 Regions

A `region` declaration introduces a binding in the **region namespace** (uppercase).

Valid region references occur in:

* `within RegionName ...`
* anchor declarations: `anchor a : RegionName { ... }`

## 13.2 Anchors

An `anchor` declaration introduces a binding in the lower-case runtime namespace.

Anchors may be:

* singleton anchors,
* anchor families (functions returning anchors).

Valid anchor uses occur in:

* anchored HTML sugar: `div @windowRoot ...`
* DOM helpers such as `metaOf windowRoot`
* patch constructors such as `classAdd windowRoot ...`

## 13.3 Cells

A `cell` declaration introduces a lower-case runtime binding.

Valid cell uses occur in:

* `get cellName`
* `set cellName value`
* `bind control cellName attrs`
* `watch { field = cellName, ... }`

## 13.4 Natives

A `native` declaration introduces a lower-case runtime binding.

Valid use:

* `call nativeName arg`

## 13.5 Threads

A `thread` declaration introduces a lower-case runtime binding.

Valid use:

* `run threadName arg`

The parser does not enforce these usage constraints; the compiler’s name-resolution and kind-checking phases do.

---

# 14. `do` / `catch` desugaring

`catch` is surface syntax only.

## 14.1 Desugaring

```fuddle
do
  s1
  s2
  ...
  x
catch p ->
  h
```

desugars to:

```fuddle
recover
  (do
    s1
    s2
    ...
    x
  )
  (\p -> h)
```

For multiple catch arms:

```fuddle
do
  ...
catch
  P1 -> H1
  P2 -> H2
```

desugars to:

```fuddle
recover
  (do
    ...
  )
  (\err ->
    case err of
      P1 -> H1
      P2 -> H2
  )
```

## 14.2 Restriction

A `catch` clause is valid only when the protected block may fail with a declared recoverable error type (`<fail E>`). It does not catch runtime `Fault`s.

---

# 15. Effect syntax and examples

## 15.1 Pure function

```fuddle
statusLabel : Status -> String
```

## 15.2 Simple impure function

```fuddle
stampLocalUpdate : () ! <cell, impure>
```

## 15.3 Server-side function with native failure

```fuddle
loadWindowData : WindowRef -> WindowData ! <session, native, impure, fail LoadErr>
```

## 15.4 Remote thread

```fuddle
thread loadWindow : WindowRef -> ()
thread loadWindow =
  renderInto ItemsWindow ReplaceInner windowFragment loadWindowData
```

---

# 16. Example file using the grammar

This small fragment uses all new top-level declaration forms:

```fuddle
module Demo.Items exposing (page)

import Fuddle.Html exposing (Html, div, text)
import Fuddle.Ui exposing (within, run, renderInto, call)

type alias WindowRef =
  { offset : Int
  , limit : Int
  }

type alias WindowData =
  { offset : Int
  , limit : Int
  , total : Int
  }

region ItemsWindow : ServerAuth

anchor windowRoot :
  ItemsWindow
    { offset : Int
    , limit : Int
    , total : Int
    }

cell busy : Bool = False

native itemsListWindowN : WindowRef -> WindowData ! <fail DbErr>

loadWindowData : WindowRef -> WindowData ! <session, native, fail DbErr>
loadWindowData ref =
  call itemsListWindowN ref

thread loadWindow : WindowRef -> ()
thread loadWindow =
  renderInto ItemsWindow ReplaceInner windowFragment loadWindowData

page : Html msg
page =
  within ItemsWindow
    []
    [ div @windowRoot
        { offset = 0, limit = 20, total = 0 }
        []
        [ text "Hello" ]
    ]
```

---

# 17. Summary of the complete new syntax surface

For compiler implementation, the **new syntax additions** beyond Elm are:

## Top-level declarations

* `region UpperIdent : AuthorityRef`
* `anchor lowerIdent : AnchorType`
* `cell lowerIdent : Type = Expr`
* `native lowerIdent : Type`
* `thread lowerIdent : Type`
* `thread lowerIdent pat* = Expr`

## Types

* effect suffix: `Type ! <effects>`
* effect item: `fail Type`

## Expressions

* `do ... [catch ...]`
* anchored element: `htmlHead @anchorExpr { meta } [attrs] [children]`

Everything else remains ordinary Elm-style surface syntax.

---

# 18. Recommended next compiler passes after parsing

Once the parser implements the syntax above, the next front-end phases should be:

1. **name resolution**

   * distinguish regions, anchors, cells, natives, threads, values, constructors

2. **anchor/region kind checking**

   * verify anchor declarations target declared regions
   * verify metadata is a record type

3. **effect parsing and normalization**

   * canonicalize effect rows
   * validate built-in effect labels

4. **desugaring**

   * `@anchor` → `node ...`
   * `do/catch` → `recover (...) (...)`
   * parameterized `thread` defs → lambda-style bodies if desired

5. **admissibility/effect analysis**

   * browser/server placement
   * native boundary and failure tracking


