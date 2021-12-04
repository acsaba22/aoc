class R {
  constructor() {
    console.log('c')
    this.v = 1
  }
  f() {
    console.log('f')
  }
}

let r = new R()
console.log(r)
console.log(Object.getPrototypeOf(r))
console.log(Object.getPrototypeOf(Object.getPrototypeOf(r)))
console.log(Object.getPrototypeOf(Object.getPrototypeOf(Object.getPrototypeOf(r))))
console.log(Object.getPrototypeOf(1))
console.log(Object.getPrototypeOf(Array(0)))

