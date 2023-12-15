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
### User Interface
1. Choose Mode: \
<p align="center">
<img src="https://github.com/pochunwu/GuessingGame/assets/118617531/f02e95f2-5387-4e33-a869-421d3adde733" />
</p>

2. Choose Difficulty: \
![difficulty](https://github.com/pochunwu/GuessingGame/assets/118617531/f4d90f94-82a3-43c8-8b1c-b6c46b085b51) 

3. Game: \
    - Make a guess: \
      ![guess](https://github.com/pochunwu/GuessingGame/assets/118617531/294c46f1-b265-4dc9-a086-d9cf1da31c36)

    - Get a hint: \
      ![hint](https://github.com/pochunwu/GuessingGame/assets/118617531/0ecba279-591c-403c-aafe-5e45e78d0fe8)
    
4. Result: \
![success](https://github.com/pochunwu/GuessingGame/assets/118617531/7413f5b4-f125-4d55-8c8a-c72070023355)
or \
![fail](https://github.com/pochunwu/GuessingGame/assets/118617531/bfc3b8c3-27dc-4889-b69e-065e165ef037)


