# Multi-Domain Guessing Game with Solver
UCSD CSE 230 (Fall 2023) Final Project
## Members
- Xujie Chen
- Xiaochuan Yu
- Runlong Su
- Po-Chun Wu
## Proposal (11/17)
We will create a Multi-Domain Guessing Game with Solver, expanding on the popular Wordle game. The project aims to create an immersive gaming experience by allowing users to guess words from diverse datasets like names and countries. A key feature is the implementation of an intelligent solver that strategically suggests the best next guess based on the current game situation. Users could turn on the solver to enhance their guessing strategies.
## Work Division (12/1 Update)
- Xujie Chen: Solver
- Xiaochuan Yu: UI (`Brick` Library)
- Runlong Su: Testing
- Po-Chun Wu: Main Program
## Key Components (12/1 Update)
```
.
├── app
│   └── Main.hs
├── data
│   ├── animals.txt
│   ├── citiesUS.txt
│   ├── names.txt
│   └── wordle5.txt
├── package.yaml
├── src
│   ├── Choose.hs
│   ├── Guess.hs
│   └── Solver.hs
├── stack.yaml
└── test
    └── Test.hs
```
## Let's Guess!
```
stack run
```
or
```
cabal run
```
1. Choose Mode:
<p align="center">
<img src="https://github.com/pochunwu/GuessingGame/assets/118617531/f02e95f2-5387-4e33-a869-421d3adde733" />
</p>

2. Choose Difficulty:

3. Game:
    - Make a guess:
    - Get a hint:
    
4. Result:
