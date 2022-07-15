# SQL statement Parser (only for Select statement) by Scala

## Purpose
The aim in this project is to build a SQL parser and map the element into the expected AST data structure.

## Structure
- scala  #parser component
  - ir   #data structure to store parse rresult, you can find more details in Overview
- test   #test component
  

## Developed Scala Parser Combinator
- [] frame-spec
- [] window-defn
- [x] ordering-term
- [x] result-column
- [] join-operator
- [] join-constraint
- [] join-clause
- [] type-name
- [] raise-function
- [] over-clause
- [x] literal-value
- [] filter-clause
- [] compound-operator
- [] common-table-expression
- [] table-or-subquery
- [x] expr
- [x] select-stmt


## How to run
SQLParserTest :
   click run on SQLParserTest, it parses the test cases in the TetsCase.
   if you want to test more cases, add new case in the TetsCase, and wirte test code as template.
