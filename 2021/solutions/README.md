# Advent of Code 2021 in TypeScript

## Running in browser from the web

You can run the calculation in your browser by specifying d=NN like:
[https://aoc2021.acsaba.eu/?d=01](https://aoc2021.acsaba.eu/?d=01)

## Running locally in nodeJS

First insall npm, typescript, node.


Run like this:
```
$ cd 2021/solutions
$ node dist/main.js 01
...
```

Build like this:
```
$ tsc
```

### Installation
Insall npm, typescript, node. This end result is good:

```
$ npm --version
8.1.0
$ tsc --version
Version 4.5.2
$ node --version
v16.13.0
```

First [install nvm](https://github.com/nvm-sh/nvm#install--update-script)
Then:

```
$ npm install -g typescript
```

## Running in browser from local local web server


```
$ cd 2021/solutions
python3 -m http.server 8800
```

Then go to: [http://localhost:8800/?d=03](http://localhost:8800/?d=03)
