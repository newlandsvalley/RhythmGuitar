# Rhythm Guitar

### work in progress

Experimental - notate basic chord shapes that would be played at the first position on a rhythm guitar.  These are represented as patterns of MIDI notes that are suitable for use with a suitable [soundfonts](https://github.com/newlandsvalley/purescript-soundfonts) player.

The idea is eventually to provide a version of [abc-melody](https://github.com/newlandsvalley/purescript-abc-melody) that supports a player which plays the tune on channel 0 and the guitar accompaniment on channel 1 and that doesn't sound absolutely terrible!

## Chord Symbol Syntax

[This](http://abcnotation.com/wiki/abc:standard:v2.2#chord_symbols) is what the ABC Notation 2.2 specification has to say on the subject.  It makes sense to follow these guidelines, although of course it omits to say that a straightforward major chord is usually notated simply with the pitch - e.g. ```A``` and not ```Amaj```.  It doesn't clearly describe major sevenths - I've seen both ```Amaj7``` and ```AM7``` in the wild.

There is also a useful description of chord symbols in [wikipedia](https://en.wikipedia.org/wiki/Chord_names_and_symbols_(popular_music)).

My approach will be:

   * To accommodate as many chord symbols as possible that are commonly encountered (in, say, [folkwiki](http://www.folkwiki.se/) ).  
   * To normalise the chord symbols that we encounter to bias the representation towards that indicated in the ABC Notation specification. 
   * Only natural and sharp pitched chords to be held in the chord map.  The normalisation will translate any flat pitched symbol to the corresponding sharp enharmonic.
   * Bass note indicators (slash chords) to be ignored.  These won't necessarily automatically translate to the guitar first position shape.
