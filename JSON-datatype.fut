type terminal = #string | #number | #literal_2 | #literal_3 | #literal_4 | #literal_5 | #literal_6 | #literal_7 | #literal_8 | #literal_9 | #literal_10 | #ignore | #empty_12
type production = #Object | #Array | #String | #Number | #Null | #True | #False | #string_18 | #number_19 | #ignore_20 | #Literal_21 | #Literal_22 | #Literal_23 | #Literal_24 | #Literal_25 | #Literal_26 | #Literal_27 | #Literal_28 | #Literal_29 | #start_30
type node = #terminal terminal (i64, i64) | #production production
type option 'a = #none | #some a

type JSON = #null | #num i64 | #bool bool | #string i64 | #list (i64, i64) | #obj (i64, i64) (i64, i64)

def testcst : option ([](i64, node)) = 
#some [(0, #production (#Array)), (0, #production (#Array)), (1, #terminal (#literal_4) (0, 1)), 
       (1, #production (#Array)), (3, #production (#Number)), (4, #terminal (#number) (1, 2)), 
       (3, #production (#Array)), (6, #terminal (#literal_2) (2, 3)), (6, #production (#Number)), 
       (8, #terminal (#number) (3, 4)), (6, #production (#Array)), (10, #terminal (#literal_2) (4, 5)), 
       (10, #production (#Number)), (12, #terminal (#number) (5, 6)), (10, #production (#Array)), 
       (14, #terminal (#literal_2) (6, 7)), (14, #production (#False)), (16, #terminal (#literal_6) (7, 12)), 
       (14, #production (#Array)), (18, #terminal (#literal_2) (12, 13)), (18, #production (#True)), 
       (20, #terminal (#literal_8) (13, 17)), (18, #production (#Array)), (1, #terminal (#literal_5) (17, 18))]

def testjson : []u8 = "[1, 2, 3, false, true]"

-- def byte_array_to_num (a:i64, b:i64) (s:[]u8) : i64 =
--    let num = s[a:b] in
--    let digits = map (\x -> i64.u8 (x - 48)) num in
--    let tens = map2 (**) (replicate (b-a) 10) (reverse (iota (b-a))) in
--    let tuple_mul (a:i64, b:i64) : i64 = a * b in 
--        reduce (+) 0 (map tuple_mul (zip digits tens))

def s_to_num  (a:i64, b:i64) (s:[]u8) =
    let xs = s[a:b:-1] in
    loop acc = 0 for i < (b-a) do
        acc + (i64.u8 xs[i] - 48) * 10**i

def read_val (s:[]u8) (x:node) : JSON =
    match x
    case (#terminal #number (a, b)) -> #num (s_to_num (a,b) s)
    case (#production #True) -> #bool true
    case (#production #False) -> #bool false
    case _ -> #null

def read_json (s:[]u8) (js:option ([](i64, node))) : option ([]JSON) =
    match js
    case #some json -> 
        let extract_val (x:(i64, node)) : JSON = 
            match x
            case (_, x) -> read_val s x in 
        let is_not_null (y:JSON) : bool = 
            match y
            case #null -> false
            case _ -> true in
        #some (filter is_not_null (map extract_val json))
    case #none -> #none


def main : bool =
    true