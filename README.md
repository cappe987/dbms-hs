# DBMS
A small DBMS written in Haskell as a hobby project. 
There are no plans to use this in any other real-world project.

# To-do

## Short term
- [ ] Reorganize insert data to match schema
- [ ] Implement schema checking and filling in NULL/default values.
- [X] Implement hash table
- [ ] Implement main db file for storing schemas. 
- [X] Check if PK exists when inserting row.
- [X] Handle NULL values
- [ ] Maybe use a shared type (schema vs row) for the name/type of columns.

## Long term
- [ ] Add rehashing to hashtables
- [ ] Escape characters 
- [ ] Remove the limit of `INT32_MAX` on block amount
- [ ] Handle schemas with size larger than one block
- [ ] Rewrite DBMS.Storage modules to directly manipulate bytestrings. https://wiki.haskell.org/Performance/Strings
- [ ] Handle UTF-8. Use `Data.ByteString.Conversion`. Right now it only handles ASCII strings.
- [ ] Make hashtable schema independent of the schema encoder/decoders.
- [ ] Make EDSL for query language. Use a monad to accumulate the query 
requirements and then execute it.
- [ ] Replace Maybe's with Either for displaying error messages.
