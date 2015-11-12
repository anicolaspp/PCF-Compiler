https://travis-ci.org/anicolaspp/PCF-Compiler.svg?branch=master

# PCF-Compiler
A compiler for PCF (Programming Language for Computable Functions) ported from F# to Scala.

PCF is functional and Turing Complete!

- More details about the language can be found [here](http://www.springer.com/cda/content/document/cda_downloaddocument/9780857290755-c2.pdf?SGWID=0-0-45-1052237-p174031776)
and [here](https://medium.com/@anicolaspp/building-a-compiler-in-scala-8d51b467baec).

- Be aware that this is a project to show the power of functional programming so avoiding imperative style is priority. 
- TDD is required to commit. 

## The grammar

```
Exp ::=  x | n                               
| true                            
| false                           
| succ                            
| pred                            
| iszero                          
| if Exps then Exps else Exps     
| fun x -> Exps                   
| rec x -> Exps                   
| (Exps)                         
| let x = Exps in Exps                   
Exps ::= Exps Exp | Exp
```
