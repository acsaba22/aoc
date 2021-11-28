let rabbit = {}
console.log(typeof(rabbit))
console.log(typeof('hello'))
console.log(typeof(5))
console.log(typeof([1,2]))

rabbit.speak = function() {
    console.log('hello');
};
rabbit.speak();
console.log(typeof(rabbit.speak));
console.log(rabbit.speak);
console.log('end');
