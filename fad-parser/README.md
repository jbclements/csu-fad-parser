You might want to look at update-db.rkt as a good entry point.

ooh, unless you're wondering how the whole thing is designed.

- update-db.rkt : cal poly specific: reads a qtr's fad, produces TSVs for updating my databases
- one-quarter-data.rkt : cal poly specific : takes a qtr, finds the fad file, parses it.
  This file knows about where I store FAD files, and what they're named.
- 
