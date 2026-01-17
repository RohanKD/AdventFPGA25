# Advent of FPGA 2025 Submission
 I implemented problem 1 https://adventofcode.com/2025/day/1 parts 1 and 2
 This was my first time using Ocaml, Hardcaml, and FPGAs, so my goal was to get a basic implementation working in a few days. 
 I saw asinghani's solution which ticked one position a time, and I wanted to implement a speedup with modular arithmetic to be faster.
 My code functions by simulating the full rotation at once and then normalized and increments the number of zero crossings as I add or subtract 100 to bring the position back into the range [0, 99]. 
 It also does part1 by simply checking the position after each rotation.

 I am hoping to learn more about parallelism and pipelining to see if I can improve my current attempt. I will also be a SWE intern at JS next summer so I hope to learn more about hardcaml then!!

Commands: dune build && runtest
