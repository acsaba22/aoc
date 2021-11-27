function countBs(s, c = "B") {
    let ret = 0
    for (let ic of s) {
        if (ic == c) {
            ret++
        }
    }
    return ret
}

console.log(countBs("BBC"));
// → 2
console.log(countBs("kakkerlak", "k"));
// → 4