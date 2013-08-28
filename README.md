# MIDIScript

`midiscript` is a program to translate MIDI files to/from a readable, editable
plain text format. Goals include:

- An easy way to make precise edits to existing MIDI files. For example, taking
  a chunk of events and modifying their positions by the same amount, or editing
  the time signatures of a file without changing the position of the events.

- A way to verify how quantized MIDI events are. Every position in the file is
  written as a precise fraction, not as a rounded decimal like many sequencers
  do, so events that are one tick off from a beat line become very obvious.

- Making it much easier to programatically generate MIDI files; such a program
  can generate text, without having to understand the MIDI binary format.

- The ability to use standard text-processing tools on MIDI files. For example,
  `diff` can show the differences between MIDI files, and this in turn can
  be used by version control systems like Git.

## Usage

    midiscript [options] input.mid output.txt
    midiscript [options] input.txt output.mid

The function is chosen automatically based on the input file. To use stdin or
stdout, supply `-` for the filename, or just elide arguments.

Options:

    -b     : for mid -> txt, positions are written as beats
    -m     : for mid -> txt, positions are written as measures|beats (default)
    -r INT : for txt -> mid, minimum resolution of output file (default 480)

Note that both measures and beats are 0-based, not 1-based. So the first beat of
the song is `0|0`.

The `-m` option is much more readable for humans, but the `-b` option is useful
when you want to edit time signatures without moving events.

`midiscript` uniquely identifies tracks based on their names. Every track (other
than the first track, containing tempo and time signature events) must
have a name, and if two tracks have the same name they get merged into one.

When interpreting positions that use a measure number, any time signature events
that aren't on a measure boundary are ignored. If there is no time signature
event at position 0, a default of `4/4` is used.

## Planned features

- Specify positions in seconds, by using tempo change events in the tempo track.

- Use `measure|beats` notation in place of any number. This doesn't make sense
  in all contexts, but you could for example say `(5|0) - (4|0)` to get the
  length of a measure. This would probably not be permitted inside the tempo
  track, lest it cause circular dependencies.
