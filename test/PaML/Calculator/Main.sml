structure Main =
struct
    structure C = Calculator(TextCalcInterface)

    val _ = C.main 42
end
