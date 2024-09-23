(* Quicksort -- Paulson p. 98 and answer to exercise 3.29 *)

local
type elem = int

fun qSort (arg as ([], sorted)) = arg
  | qSort ([a], sorted) = ([], a::sorted)
  | qSort (a::bs, sorted) =  (* "a" is the pivot *)
    let fun partition (arg as (_, _, []: elem list)) = arg
	  | partition (left, right, x::xr) =
	    if x<=a then partition(x::left, right, xr)
	    else partition(left, x::right, xr)
	val arg' =
	    let val (left, right, _) = partition([], [], bs)
                val sorted' = #2 (qSort (right, sorted))
	    in (left, a::sorted')
	    end
    in qSort arg'
    end

fun quickSort l = #2 (qSort(l, []))

(* Generating random numbers.  Paulson, page 96 *)

val min = 1
val max = 100000
val a = 16807.0
val m = 2147483647.0
val w = real(max - min)/m
fun seed0 () = 117.0

fun nextRand seed =
    let val t = a*seed
    in t - m*real(floor(t/m))
    end

fun randomList' (arg as (0, _, res)) = arg
  | randomList' (i, seed, res) =
    let val res' = min + floor(seed*w) :: res
    in randomList'(i-1, nextRand seed, res')
    end

fun randomList n = #3 (randomList'(n, seed0(), []))

(* Building input list, sorting it and testing the result *)

fun isSorted [] = true
  | isSorted [x: elem] = true
  | isSorted (x::(xr as (y::yr))) = x <= y andalso isSorted xr

in

val () =
    if isSorted (quickSort(randomList 10000)) then print("Ok!\n")
    else print("Oops...\n")

end
