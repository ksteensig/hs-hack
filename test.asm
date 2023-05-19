// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

    @1
    D=A
    @R0
    M=D // stride, i.e. memory blocks per row

    @8192
    D=A
    @R1
    M=D // memory blocks in total

    @R3
    M=0 // color

    @R4
    M=0 // tmp address holder for screen

(START)

    @R2
    M=0 // idx

(LOOP)

// determine if we are at end of loop and start over
    @R2
    D=M
    @R1
    D=M-D

    @START
    D;JEQ


// Draw color to screen
    @R2 // get idx
    D=M
    @SCREEN // compute offset
    D=A+D
    @R4 // save address temporarily
    M=D
    @R3 // fetch color, i.e. b or w
    D=M
    @R4 // write color
    A=M
    M=D

    @R0 // increment idx
    D=M
    @R2
    M=M+D

    @KBD
    D=M
    @BLACK
    D;JGT

(WHITE)
    @R3
    M=0

    @LOOP
    0;JMP

(BLACK)
    @R3
    M=-1

    @LOOP
    0;JMP