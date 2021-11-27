function isEven(n) {
    switch (n) {
        case 0:
            return true;
        case 1:
            return false;
        default:
            return isEven((n<0)?n+2:n-2);
    }
}

console.log(isEven(50));
console.log(isEven(75));
console.log(isEven(-1));
