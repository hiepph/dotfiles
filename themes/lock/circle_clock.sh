#!/bin/sh

B='#00000000'  # blank
C='#ffffff22'  # clear ish
D='#d79921bb'  # default
T='#d79921bb'  # text
W='#880000bb'  # wrong
V='#b16286bb'  # verifying

i3lock \
--insidevercolor=$C   \
--ringvercolor=$V     \
\
--insidewrongcolor=$C \
--ringwrongcolor=$W   \
\
--insidecolor=$B      \
--ringcolor=$D        \
--linecolor=$B        \
--separatorcolor=$D   \
\
--verifcolor=$T        \
--wrongcolor=$T        \
--timecolor=$T        \
--datecolor=$T        \
--layoutcolor=$T      \
--keyhlcolor=$W       \
--bshlcolor=$W        \
\
--screen 1            \
--blur 5              \
--clock               \
--indicator           \
--timestr="%H:%M"  \
--datestr="%a, %b %d" \
--keylayout 2         \
--veriftext="Alohomora..." \
--wrongtext="You shall not pass" \
--radius 150


# --textsize=20
# --modsize=10
# --timefont=comic-sans
# --datefont=monofur
# etc
