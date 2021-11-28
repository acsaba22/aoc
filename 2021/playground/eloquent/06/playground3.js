let a = {}
console.log(a)
console.log(a.toString)
console.log(a.toString())

console.log()
console.log(Object.getPrototypeOf(a))
console.log(Object.getPrototypeOf(a) == Object.prototype)
console.log(Object.getPrototypeOf(Object.prototype) == null)

console.log()
console.log(Object.getPrototypeOf(Math.max))
console.log(Object.getPrototypeOf(Math.max) == Function.prototype)
console.log(Object.getPrototypeOf(Function.prototype) == Object.prototype)

console.log()
console.log(Object.getPrototypeOf([]))
console.log(Object.getPrototypeOf([]) == Array.prototype)
console.log(Object.getPrototypeOf(Array.prototype) == Object.prototype)

let protoRabbit = {
    f() {
        console.log('fcall')
    }
}
let r = Object.create(protoRabbit)
r.f()
