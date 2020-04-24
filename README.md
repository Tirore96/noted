# noted
A DSL for high-level music composition written in Haskell. It is built on Euterpea, the main Haskell library for music composition.  


# Example
$chrd1= 1,3,5 <br>
$chrd2= 5,7,1<br>
$chrd3= 6,1,3<br>
$chrd4= 4,6,1<br>
$con = {bars=4;octave_pos=5;key=c}<br>
$m1 = [$chrd1. $chrd1.. $chrd1 $chrd1...] $con<br>
$m2 = [12 122 111] $con
$m1,$m2 <br>
