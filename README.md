# rxem
CF card to RXV11 interface

This project provides a Disk drive for a pdp-11 computer. The drive uses modern storage (CF card)

The interface is the same as a RX01 drive, but its not software compatable. 

32768 blocks of 512 bytes (16 meg)

No interleave, no track skew

Included is a modified DX driver called SD (for Solidstate Disk) That has been tested with RT-11 V5.03.

The Idea for this project came from RX11emu By Peter McCollum. His project used a custom board in the PDP-11.
and ran on a PC.
I use a standard RXV11 interface. (or RX11 but I can't test that) and runs on a small PC board using a ATmega32 AVR processor.

The standard RX01 toggle in boot (or ROM) works.
