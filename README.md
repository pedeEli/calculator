# Team5

Ich wollte unbedingt einen typechecker implementieren, da ich aber kein Plan davon hatte, hab ich mich ein bischen am code von ghc orientiert.
Deswegen auch die Namen von meinen Modulen. GCI steht für Gerster Calculator Interpreter.

## Structure

### Language.Calc.Syntax

Hier sind alle Module die meine Syntax exact beschreiben. Unterstützer Syntax:

- [x] Variable declaration: `inc x = x + 1`
- [x] Operator declaration: `x ++ y = x + y + inc x`
- [ ] Partial application: `inc = (+1)`

#### Wichtig: Variablen können nicht den selben namen haben wie einheiten
`hakm = 4` ist nicht möglich da das als hecktar * kilometer interpretiert wird.
`test = 4` is möglich da `e` keine Einheit is und somit `t`(Tonne) und `s`(Sekunde) ignoriert werden. 

Es gibt drei Durchgänge:

- Parser: Baut den Ast
- Renamer: Bennent alle localen und globalen variablen um, damit der typechecker sich keine gedanken darum machen muss
- Typechecker: Checkt alle types bervor eine expression ausgeführt wird. Funktioniert aber noch nich 100% wie ich wollte.

Alle drei Stationen benutzen den selben ADT (CalcExpr p).
Ich hab 'Trees that grow' implementiert und so in jeder Station Zusatzinfos an den ADT gehängt.

### GCI.Types

Hier befinden sich wichtige Types, wie der Value type welche ein Rational und units combiniert.
Hier sind auch alle operator definiert.

Units werden hier auch erstellt mit Hilfe von template haskell.

### GCI.Core.Expr

Hier is die abgespeckte version vom syntax.

### GCI.Calculator

Hier wird alles zusammen geführt.