# noted
A DSL for high-level music composition written in Haskell. It is built on Euterpea, the main Haskell library for music composition.<br>
The building blocks are notes, music and contexts. A note must have a duration and a position. A set of notes is created with toNote($duration,$position), with a set of durations and positions.<br> Combining a set of notes with a context yields music. This is done by toMusic($notes,$context). Durations, positions, notes and music can all be combined serially (-) or in parallel (|). 

# Example
$c1 = 1|3|5 <br>
$c2 = 2|4|6<br>
$c3 = 3|5|7<br>
$dur = qn  - qn   - hn<br>
$pos = $c1 - $c2  - $c3<br>
$notes = toNotes($dur,$pos)<br>
$con = {octave=4;key=c}<br>
$con2 = {octave=4;key=e}<br>
$m1 = toMusic($notes,$con)<br>
$m2 = toMusic($notes,$con2)<br>
$dur2 = hn  - hn<br>
$pos2 = $c3 - $c2<br>
$notes2 = toNotes($dur2,$pos2)<br>
$m3 = toMusic($notes2,$con2)<br>
<br>
main = $m1 - $m2 - $m3<br>


# Getting started
You need to have stack installed. You also need some kind of MIDI instrument. The interpreter returns the computed value as a sequence of MIDI messages, that are sent to a specified output (with the Euterpea backend). For Linux I recommend FluidSynth. If you have all the dependencies, you can start up FluidSynth with the script ./start_synth.
