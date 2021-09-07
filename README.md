# Rhythm Guitar

This project notates a set of basic chord shapes that could be played at the first position on a rhythm guitar.  These are represented as patterns of MIDI notes that are suitable for use with a suitable [soundfonts](https://github.com/newlandsvalley/purescript-soundfonts) player.

The idea is to support a version of [abc-melody](https://github.com/newlandsvalley/purescript-abc-melody) that builds a player which plays the tune on channel 0 and the guitar accompaniment on channel 1 and that doesn't sound absolutely terrible!

## Chord Symbol Syntax

Chord symbols can be written in many different ways.  The approach here is largely to follow the syntax indications given in the [ABC Notation](http://abcnotation.com/wiki/abc:standard:v2.2#chord_symbols) specification. It makes sense to follow these guidelines, although, for example, the spec omits to say that a straightforward major chord is usually notated simply with the pitch - e.g. ```A``` and not ```Amaj```.  It doesn't clearly describe major sevenths - I've seen both ```Amaj7``` and ```AM7``` in the wild.

There is also a useful description of chord symbols in [wikipedia](https://en.wikipedia.org/wiki/Chord_names_and_symbols_(popular_music)).

### Notated so far

   * Major chords - for example: ```A, Bb, C#```
   * Minor chords - for example: ```Dm, Ebm, F#m```
   * Seventh chords - for example: ```A7, Bb7, C#7```
   * Minor seventh chords - for example: ```Dm7, Ebm7, F#m7```
   * Diminished seventh chords - for example: ```Ebdim7, Gdim7, C#dim7```
   * Major seventh chords - for example: ```Amaj7, Bbmaj7, C#maj7```

### To come

   * Ninth chords - for example: ```A9, Bb9, C#9```
   * Suspended chords - for example: ```Ebsus, Gsus, C#sus```

### Other chords and voicings

My approach is primarily to accommodate as many chord symbols as possible that are commonly encountered in traditional music collections (e.g. [folkwiki](http://www.folkwiki.se/) ). Also to provide only a single voicing in or around the first position on the guitar. 

However, the underlying chord descriptions are provided as a JSON file.  If you require, for example, jazz inversions or different voicings, it should be a relatively simple matter to provide your own file with these included.

## Normalisation

Chord symbols are parsed and then normalised as far as possible to a canonical form.  These normalisations include:

   * Enharmonic translation. Only natural and sharp pitched chords are held in the chord map. Any flat pitched symbol is translated to the corresponding sharp enharmonic.
   * Major chords and variants.  Both forms of the kind ```Amaj``` or ```A``` are accepted with the former translated to the latter.
   * Minor chords and variants.  Both forms of the kind ```Bmin``` or ```Bm``` are accepted with the former translated to the latter.
   * Diminished sevenths.  Both forms of the kind ```Cdim``` or ```Cdim7``` are accepted with the former translated to the latter.
   * Major sevenths.  Both forms of the kind ```Cmaj7``` or ```CM7``` are accepted with the former translated to the latter.
   * Slash chords of the type ```F/C``` where the bass note is not the root of the chord are accepted but translated to the simple chord (e.g. ```F```).

## To build

   ```npm run build```

## To build the example

   ```npm run example```

and then load ```example\dist\index.html```

## To test

   ```npm run test```

