x←⍎¨⊃⊃⎕NGET 'inputs/day01.txt' 1
⎕←+/x×x=1⌽x
⎕←+/x×x=x⌽⍨2÷⍨≢x
