x←⍎¨⊃⎕NGET 'inputs/day02.txt' 1
⎕←+/(⌈/-⌊/)¨x
⎕←+/∊∘.{(⍺>⍵)∧0=⍵|⍺:⍺÷⍵⋄0}⍨¨x
