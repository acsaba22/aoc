# Advent of Code 2021 in TypeScript

## Running in browser from the web

You can run the calculation in your browser here:

* day 01: [https://aoc2021.acsaba.eu/src/d01/](https://aoc2021.acsaba.eu/src/d01/)
* day 02: [https://aoc2021.acsaba.eu/src/d02/](https://aoc2021.acsaba.eu/src/d02/)
* ...

## Running locally in nodeJS

First insall npm, typescript, node.


Run like this:
```
$ cd 2021/solutions
$ node dist/d01/main.js
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

Then go to: [http://localhost:8800/src/d02](http://localhost:8800/src/d02)
