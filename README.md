# noted
A DSL for high-level music composition written in Haskell. It is built on Euterpea, the main Haskell library for music composition.  


# Example
$chrd1= 1,3,5
$chrd2= 5,7,1
$chrd3= 6,1,3
$chrd4= 4,6,1
$con = {bars=4;octave_pos=5;key=c}
//$m1 = [$chrd1 $chrd1. $chrd1.. $chrd1... $chrd1.... ] $con
$m1 = [$chrd1. $chrd1.. $chrd1 $chrd1...] $con

//$m2 = [2 1313 1515 1414] $con
$m2 = [12 122 111] $con
$m1,$m2
