#!/usr/local/bin/fish

function run_pa
  set pa $argv[1]
  for file in tests/$pa/*.in
    echo "expected: "; cat tests/$pa/(basename $file .in).out;
    echo "actual: "; eval ./$pa.hsc < $file
    echo
  end
end
