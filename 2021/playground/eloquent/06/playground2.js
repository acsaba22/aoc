function f() {
    console.log(this)
}

f()

rabbit = {name:'bunny', f}
rabbit.f()