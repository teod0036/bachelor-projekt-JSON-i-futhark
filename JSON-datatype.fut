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

def preprocess_cst (js:option ([](i64, node))) : [](i64, node) =
    match js
    case #some json -> json
    case #none -> []

def cst_by_depth (ns:[](i64, node)) : [](i64, (i64, (i64, node))) =
    let track_depth (acc:(i64, (i64, node))) (n:(i64, (i64, node))) : (i64, (i64, node)) =
        match (acc, n)
        case ((depth, (_, _)), (_, (parent, #terminal #literal_4 (_, _)))) -> 
                (depth + 1, (parent, #terminal #literal_4  (0, 0)))
        case ((depth, (_, _)), (_, (parent, #terminal #literal_9  (_, _)))) -> 
                (depth + 1, (parent, #terminal #literal_9  (0, 0)))
        case (((depth, (_, #terminal #literal_5  (_, _))), (_, (parent, nde)))) -> 
                (depth-1, (parent, nde))
        case (((depth, (_, #terminal #literal_10 (_, _))), (_, (parent, nde)))) -> 
                (depth-1, (parent, nde))
        case ((depth, (_, _)), (_, (parent, nde))) -> 
                (depth, (parent, nde)) 
    in
    let depths = scan track_depth (0, (0, #production (#Null))) (zip (indices ns) ns) in
    --let isolate (dn:(i64, (i64, node))) : (i64, i64) = (dn.1.0, dn.0) in
    --let d = map isolate depths in
    let consolidate_parents (prev:(i64, (i64, (i64, node)))) (cur:(i64, (i64, (i64, node)))) : (i64, (i64, (i64, node))) =
        --If something breaks in the future, this function is probably at fault
        --the consolidation logic is kind of sketchy
        --(index, (depth, (parent, node)))
        if cur.1.0 == prev.1.0
        then (cur.0, (cur.1.0, (prev.1.1.0, cur.1.1.1))) 
        else (cur.0, (cur.1.0, (cur.1.1.0, cur.1.1.1))) in  
        scan consolidate_parents (0, (0, (0, #production (#Null)))) (zip (indices depths) depths)

        
    
-- converts a []u8 (string) to a number
def s_to_num  (a:i64, b:i64) (s:[]u8) =
    let xs = reverse s[a:b] in
    loop acc = 0 for i < (b-a) do
        acc * 10 + (i64.u8 xs[(b-a) - 1 - i] - '0')

def read_val (s:[]u8) (x:node) : JSON =
    match x
    case (#terminal #number (a, b)) -> #num (s_to_num (a,b) s)
    case (#terminal #literal_6 _) -> #bool false
    case (#terminal #literal_8 _) -> #bool true
    case _ -> #null

def read_json (s:[]u8) (js:option ([](i64, node))) : []JSON =
    match js
    case #some json -> 
        let extract_val (x:(i64, node)) : JSON = 
            match x
            case (_, x) -> read_val s x 
        in  let is_not_null (y:JSON) : bool = 
                match y
                case #null -> false
                case _ -> true 
            in filter is_not_null (map extract_val json)
    case #none -> []

def main : [](i64, (i64, (i64, node))) = 
    let json = preprocess_cst (parse testjson) in
    if null json then [] else cst_by_depth json