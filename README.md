# DBMS
A small DBMS written in Haskell as a hobby project. 
There are no plans to use this in any other real-world project.

# To-do

## Short term
- [ ] Reorganize insert data to match schema
- [ ] Implement schema checking and filling in NULL/default values.
- [X] Implement hash table
- [ ] Implement main db file for storing schemas. 
- [ ] Check if PK exists when inserting row.

## Long term
- [ ] Add rehashing to hashtables
- [ ] Escape characters 
- [ ] Remove the limit of `INT32_MAX` on block amount
- [ ] Handle schemas with size larger than one block
- [ ] Rewrite DBMS.Storage modules to directly manipulate bytestrings. https://wiki.haskell.org/Performance/Strings
