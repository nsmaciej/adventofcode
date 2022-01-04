x←⍎¨⊃⊃⎕NGET 'inputs/day01.txt' 1
s←{+/⍵×⍵=⍵⌽⍨⍺⍺≢⍵}
⎕←1 s x
⎕←÷∘2 s x
