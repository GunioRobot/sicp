(ns sicp.ex2_60)

;;; a set for which duplicates are allowed

(comment
  "element-of-set? will be same program and union, intersection and
   adjoin-set will differ. element-of-set? has to traverse a bigger
   list in the worst case of element not in the set. But union is easy,
   just concat the two sets. adjoin-set also does not require traversing
   the set thru element-of-set?. Instead it is a simple cons of the new
   element with the list representing the set."
  "If the application involves"
  )