import UIKit

var str = "Hello, playground"

func multiply(_ firstFactor :Int) -> (Int) -> Int {
    return { secondFactor in
        if 0 == secondFactor { return 0 }
        return firstFactor + multiply(firstFactor)(secondFactor - 1)
    }
}


func multiply2(_ factor: Int, _ factor2: Int) -> Int {
    if 0 == factor2 { return 0 }
    else {
        return factor + multiply2(factor, factor2 - 1)
    }
}

multiply(3)(4)
multiply2(3,4)


////////////////
func insert<A: Comparable>(_ value: A) -> ([A]) -> [A] {
    return { array in
        guard false == array.isEmpty else { return [value] }
        if value <= array.first! {
            var newArray = array
            newArray.insert(value, at: 0)
            return newArray
        }
        else {
            return [array.first!] + insert(value)(Array(array.dropFirst()))
        }
    }
}

func isort<A: Comparable>(_ array: [A]) -> [A] {
    guard false == array.isEmpty else { return [] }
    return insert(array.first!)(isort(Array(array.dropFirst())))
}

insert(3)([1,2,4])
isort([3,2,1,4])

func drop(_ value: Int) -> ([Int]) -> [Int] {
    return { array in
        guard false == array.isEmpty else { return array }
        guard 0 < value else { return array }
        return drop(value - 1)(Array(array.dropFirst()))
    }
}

drop(1)([1,2,3,4,5,6])


func fibo(_ value: Int) -> Int {
    if 0 == value { return 0 }
    else if 1 == value { return 1 }
    else { return fibo(value-1) + fibo(value-2) }
}

fibo(7)


func qsortcustom<A: Comparable>(_ array: [A]) -> [A] {
    guard false == array.isEmpty else { return [] }
    let pivot = array.first!
    let smaller = array.dropFirst().filter { $0 <= pivot }
    let bigger = array.dropFirst().filter { $0 > pivot }
    return qsortcustom(smaller) + [pivot] + qsortcustom(bigger)
}

qsortcustom([3,5,7,2,5,9])


func even(_ value: Int) -> Bool {
    guard 0 != value else { return true }
    return odd(value - 1)
}

func odd(_ value: Int) -> Bool {
    guard 0 != value else { return false }
    return even(value - 1)
}

even(5)
odd(3)


func takeWhile<A>(function: (A) -> Bool, list: Array<A>) -> Array<A> {
    guard false == list.isEmpty else { return list }
    if true == function(list.first!) {
        let newList = Array(list.dropFirst())
        return [list.first!] + takeWhile(function: function, list: newList)
    } else {
        return Array<A>()
    }
}

takeWhile(function: odd, list: [1,5,3,7])

[1,2,3].reduce(0, +)
let a = [1,2,4].reduce(0) { result, element in result + element }
print(a)

let b: [Int] = [1,2,3].reduce(Array<Int>()) { acc, element in
    var array = acc
    array.insert(element, at: 0)
    return array
}
print(b)

func foldr<A, B>(_ f: @escaping (A) -> (B) -> B) -> (B) -> ([A]) -> B {
    return { acc in
        return { list in
            guard let first = list.first else { return acc }
            let tail = Array(list.dropFirst())
            return f (first) ( foldr(f)(acc)(tail) )
        }
    }
}

let reverseInt = foldr { (element: Int) -> (Array<Int>) -> Array<Int> in
    return { acc in
        var newArray = acc
        newArray.append(element)
        return newArray
    }
}
let c = reverseInt(Array<Int>())([1,2,3])
print(c)

func jumpingOnClouds(c: [Int]) -> Int {
    var counter = 0
    var pointer = 0
    
    while (pointer < c.count - 1) {
        pointer = pointer + 2
        
        if pointer > c.count - 1 {
            pointer = pointer - 1
        }
        
        if c[pointer] == 1 { pointer = pointer - 1 }
        counter = counter + 1
        print(pointer)
    }
    return counter
}

print("no of jump: \(jumpingOnClouds(c: [0, 0, 0, 1, 0, 0]))")
print("no of jump: \(jumpingOnClouds(c: [0, 0, 1, 0, 0, 1, 0]))")
print("no of jump: \(jumpingOnClouds(c: [0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0]))")


precedencegroup PowerPrecedence { higherThan: MultiplicationPrecedence }
infix operator ^^ : PowerPrecedence
func ^^ (radix: Int, power: Int) -> Int {
    return Int(pow(Double(radix), Double(power)))
}

// Complete the countingValleys function below.
func accumulate(n: Int, ys: [Int]) -> [Int] {
    guard false == ys.isEmpty else {
        return []
    }
    let priorArr = [n + ys.reduce(0, +)]
    return priorArr + accumulate(n: n, ys: Array(ys.dropLast()))
}
