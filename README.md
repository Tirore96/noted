# noted
A DSL for high-level music composition written in Haskell. It is built on Euterpea, the main Haskell library for music composition.<br>
The building blocks are compositions and contexts. A composition is either an r-composition (relative-composiiton), n-composition (noted-composition) or np-composition (noted-positioned-composition). compositions of the same type can be combined serially (as a chord) by using ',' as a delimiter. To combine sequentially, use no delimiter (see examples). Space between compositions and delimiters are allowed if the composition is enclosed in square brackets.
A context is a struct containing a subset of the labels "bars","octave\_pos","key". r-compositions can only be applied to a context containing all the labels. n-compositions "bars" and "octave\_pos". np-compositions only require "bars. <br>
Applying a context to a composition yields music. Music can be combined sequentially or serially as compositions, but a context cannot be applied to Music.

# Example
$serial_r= 1,3,5 <br>
$serial_n= a,b,c <br>
$serial_np =a4,b4,c4 <br>
$sequential_r = 1234 <br>
$squarebracket_sequential_r = \[1 2 3 4\]
$relative_con = {bars=4;octave_pos=5;key=c}<br>
$noted_con = {bars=4;octave_pos=5}<br>
$noted_positioned_con = {bars=4}<br>
$combined_serial = [$serial_r $sequential $squarebracket_sequential_r]<br>
$music_1 = $combined_serial $relative_con  //Applying a context to a composition yields music<br>
$music_2 = $serial_n $noted_con<br>
$music_3 = $serial_np $noted_positioned_con  //<br>
$music_4 = $serial_np $relative_con   //a context can contain more labels than needed<br>
$new_music = [$music_1 $music_2, $music_3]  //Music can be combined the same way compositions can. <br>
$new_music // The last line of the program must have type Music. This will be the output <br>


# Getting started
You need to have stack installed. You also need some kind of MIDI instrument. The interpreter returns the computed value as a sequence of MIDI messages, that are sent to a specified output (with the Euterpea backend). For Linux I recommend FluidSynth. If you have all the dependencies, you can start up FluidSynth with the script ./start_synth.
