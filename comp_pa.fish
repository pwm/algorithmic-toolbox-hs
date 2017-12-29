#!/usr/local/bin/fish

function comp_pa
  set pa $argv[1]
  stack ghc -- -O $pa.hs -o $pa.hsc
  rm $pa.hi ;and rm $pa.o
end
