module RomanNumerals

let roman arabicNumeral = 
    let rec toRoman arabNum romNum =
        match arabNum with
        | x when x >= 1000 -> toRoman (arabNum - 1000) (romNum + "M")
        | x when x >= 900 -> toRoman (arabNum - 900) (romNum + "CM")
        | x when x >= 500 -> toRoman (arabNum - 500) (romNum + "D")
        | x when x >= 400 -> toRoman (arabNum - 400) (romNum + "CD")
        | x when x >= 100 -> toRoman (arabNum - 100) (romNum + "C")
        | x when x >= 90 -> toRoman (arabNum - 90) (romNum + "XC")
        | x when x >= 50 -> toRoman (arabNum - 50) (romNum + "L")
        | x when x >= 40 -> toRoman (arabNum - 40) (romNum + "XL")
        | x when x >= 10 -> toRoman (arabNum - 10) (romNum + "X")
        | x when x >= 9 -> toRoman (arabNum - 9) (romNum + "IX")
        | x when x >= 5 -> toRoman (arabNum - 5) (romNum + "V")
        | x when x >= 4 -> toRoman (arabNum - 4) (romNum + "IV")
        | x when x >= 1 -> toRoman (arabNum - 1) (romNum + "I")
        | _ -> romNum
    toRoman arabicNumeral ""

