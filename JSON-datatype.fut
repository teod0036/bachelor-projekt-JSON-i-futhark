import "json"

type production = parser.production
type node = parser.node terminal production 
type option 'a = #none | #some a

--advanced testjson for later: "[{\"foo\": 1, \"bar\": {\"baz\": true}},{\"qux\": [3, 4, 5]}]"

type JSON = #null | #num i64 | #bool bool | #string i64 | #list (i64, i64) | #obj (i64, i64) (i64, i64)

def testjson : []u8 = "[{\"foo\": 1}, {\"bar\": 2}]"

--testjson cst
--#some [(0, #production (#Array)), (0, #production (#Array)), (1, #terminal (#literal_4) (0, 1)), 
--      (1, #production (#Array)), (3, #production (#Object)), (4, #production (#Object)), 
--      (5, #terminal (#literal_9) (1, 2)), (5, #production (#Object)), (7, #production (#Object)), 
--      (8, #terminal (#string) (2, 7)), (8, #terminal (#literal_3) (7, 8)), (8, #production (#Number)), 
--      (11, #terminal (#number) (9, 10)), (7, #production (#Object)), (5, #terminal (#literal_10) (10, 11)), 
--      (3, #production (#Array)), (15, #terminal (#literal_2) (11, 12)), (15, #production (#Object)), 
--      (17, #production (#Object)), (18, #terminal (#literal_9) (13, 14)), (18, #production (#Object)), 
--      (20, #production (#Object)), (21, #terminal (#string) (14, 19)), (21, #terminal (#literal_3) (19, 20)), 
--      (21, #production (#Number)), (24, #terminal (#number) (21, 22)), (20, #production (#Object)), 
--      (18, #terminal (#literal_10) (22, 23)), (15, #production (#Array)), (1, #terminal (#literal_5) (23, 24))]

def cst_by_depth (ns:[]node) : []i64 =
    let track_depth (x:i64) (n:node) : i64 =
        match n
        case (#terminal #literal_4  (_, _)) -> x + 1
        case (#terminal #literal_9  (_, _)) -> x + 1
        case (#terminal #literal_5  (_, _)) -> x - 1
        case (#terminal #literal_10 (_, _)) -> x - 1
        case _ -> x in
    let xy =    
        loop (depth, depths) = (0, []) for n in ns do
            (track_depth depth n, concat depths [track_depth depth n]) in
    match xy
    case (_, y) -> y
        
    

def s_to_num  (a:i64, b:i64) (s:[]u8) =
    let xs = reverse s[a:b] in
    loop acc = 0 for i < (b-a) do
        acc * 10 + (i64.u8 xs[(b-a) - 1 - i] - '0')

def read_val (s:[]u8) (x:node) : JSON =
    match x
    case (#terminal #number (a, b)) -> #num (s_to_num (a,b) s)
    case (#production #True) -> #bool true
    case (#production #False) -> #bool false
    case _ -> #null

def read_json (s:[]u8) (js:option ([](i64, node))) : []JSON =
    match js
    case #some json -> 
        let extract_val (x:(i64, node)) : JSON = 
            match x
            case (_, x) -> read_val s x in 
        let is_not_null (y:JSON) : bool = 
                match y
                case #null -> false
                case _ -> true in
        filter is_not_null (map extract_val json)
    case #none -> []

def main : []i64 =
    let js = parse testjson in
    match js
    case #some json -> 
        let extract_val (x:(i64, node)) : node = 
            match x
            case (_, x) -> x in
        let nodes = map extract_val json in
            cst_by_depth nodes
    case #none -> [0]