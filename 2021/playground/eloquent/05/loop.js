function loop(v, test, next, f) {
    for (;test(v);v=next(v)) f(v);
}

loop(3, n => n > 0, n => n - 1, console.log);
// → 3
// → 2
// → 1
