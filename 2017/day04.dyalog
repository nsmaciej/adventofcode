x←⊃⎕NGET 'inputs/day04.txt' 1
s←{+/(∪≡⊢)¨⍺⍺¨¨' '(≠⊆⊢)¨⍵}
⎕←⊢s x
⎕←{⍵[⍋⍵]}s x
